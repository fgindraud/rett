extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
// TODO impl serde support for graph (direct)

///*****************************************************************************
/// A sparse vector, where objects are accessed by indexes.
mod slot_vec {
    use std::mem;
    use std::ops;

    enum Slot<T> {
        Used(T),
        Unused(Option<usize>), // Element of a free list of unused Slots
    }
    pub struct SlotVec<T> {
        slots: Vec<Slot<T>>,
        next_unused_slot_id: Option<usize>, // Free list head
        nb_objects: usize,
    }

    impl<T> SlotVec<T> {
        /// Create an empty SlotVec.
        pub fn new() -> Self {
            SlotVec {
                slots: Vec::new(),
                next_unused_slot_id: None,
                nb_objects: 0,
            }
        }

        /// Number of stored objects.
        pub fn len(&self) -> usize {
            self.nb_objects
        }
        /// Number of slots (and maximum index).
        pub fn nb_slots(&self) -> usize {
            self.slots.len()
        }

        /// access a slot (returns none if empty slot).
        pub fn get(&self, index: usize) -> Option<&T> {
            match self.slots[index] {
                Slot::Used(ref value) => Some(value),
                _ => None,
            }
        }
        /// access a slot (returns none if empty slot): mut version.
        pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
            match self.slots[index] {
                Slot::Used(ref mut value) => Some(value),
                _ => None,
            }
        }

        /// Insert an object in any slot, returns the new object index.
        pub fn insert(&mut self, value: T) -> usize {
            let new_id = {
                if let Some(unused_id) = self.next_unused_slot_id {
                    // Pop unused slot from free list
                    let unused_slot = &mut self.slots[unused_id];
                    if let Slot::Unused(next_unused_slot_id) = *unused_slot {
                        self.next_unused_slot_id = next_unused_slot_id;
                        *unused_slot = Slot::Used(value);
                        unused_id
                    } else {
                        panic!("Used Slot in free list");
                    }
                } else {
                    // Allocate new slot
                    let end_of_vec_id = self.nb_slots();
                    self.slots.push(Slot::Used(value));
                    end_of_vec_id
                }
            };
            self.nb_objects += 1;
            new_id
        }
        /// Remove the object at the given index. Return the object that was removed.
        pub fn remove(&mut self, index: usize) -> Option<T> {
            let slot = &mut self.slots[index];
            if let Slot::Used(_) = *slot {
                let old_next_unused_slot_id =
                    mem::replace(&mut self.next_unused_slot_id, Some(index));
                let old_value = match mem::replace(slot, Slot::Unused(old_next_unused_slot_id)) {
                    Slot::Used(value) => value,
                    _ => panic!("Slot was used"),
                };
                self.nb_objects -= 1;
                Some(old_value)
            } else {
                None
            }
        }
    }

    /// Indexation with []: panics on invalid index.
    impl<T> ops::Index<usize> for SlotVec<T> {
        type Output = T;
        fn index(&self, index: usize) -> &T {
            self.get(index).expect("invalid index")
        }
    }
    impl<T> ops::IndexMut<usize> for SlotVec<T> {
        fn index_mut(&mut self, index: usize) -> &mut T {
            self.get_mut(index).expect("invalid index")
        }
    }

    #[cfg(test)]
    mod tests {
        #[test]
        fn basic_api() {
            let mut sv = super::SlotVec::new();
            assert_eq!(sv.len(), 0);
            assert_eq!(sv.nb_slots(), 0);

            let id_42 = sv.insert(42);
            assert_eq!(sv.get(id_42), Some(&42));
            assert_eq!(sv[id_42], 42);
            assert_eq!(sv.len(), 1);
            assert_eq!(sv.nb_slots(), 1);

            let id_12 = sv.insert(12);
            assert_ne!(id_42, id_12);
            assert_eq!(sv.len(), 2);
            assert_eq!(sv.nb_slots(), 2);

            assert_eq!(sv.remove(id_42), Some(42));
            assert_eq!(sv.len(), 1);
            assert_eq!(sv.get(id_42), None);
            assert_eq!(sv.nb_slots(), 2);

            // Check reuse
            let id_34 = sv.insert(34);
            assert_eq!(id_34, id_42);
            assert_ne!(id_42, id_12);
            assert_eq!(sv.nb_slots(), 2);

            sv[id_34] = 0;
        }
    }
}

///*****************************************************************************
/// Define a knowledge graph
mod graph {
    use slot_vec::SlotVec;
    use std::collections::HashMap;
    use std::hash::Hash;

    /// Opaque Index type for graph elements
    #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Serialize, Deserialize, Debug)]
    pub struct Index(usize);

    /// A directed link (edge of the graph)
    #[derive(PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
    pub struct Link {
        pub from: Index,
        pub to: Index,
    }

    /// An abstract graph entity (node of the graph).
    /// Defined only by its relationships: not comparable.
    #[derive(Clone, Serialize, Deserialize, Debug)]
    pub struct Entity;

    /// Object of the graph: Link, Entity, or Atom (parametrized).
    #[derive(Clone, Serialize, Deserialize)]
    pub enum Object<A> {
        Atom(A),
        Link(Link),
        Entity(Entity),
    }

    /// Data for each object.
    /// in_links/out_links: indexes of links pointing to/from this link.
    struct ObjectData<A> {
        object: Object<A>,
        in_links: Vec<Index>,
        out_links: Vec<Index>,
    }

    /// Store a graph.
    pub struct Graph<A> {
        objects: SlotVec<ObjectData<A>>,
        atom_indexes: HashMap<A, Index>,
        link_indexes: HashMap<Link, Index>,
    }

    /// Reference an object and its data.
    pub struct ObjectRef<'a, A: 'a> {
        index: Index,
        object_data: &'a ObjectData<A>,
    }

    /// Iterate on objects in order of increasing indexes.
    pub struct OrderedObjectIterator<'a, A: 'a> {
        next_index: usize,
        graph: &'a Graph<A>,
    }

    impl Index {
        /// Access (read only) the underlying value.
        pub fn to_usize(&self) -> usize {
            self.0
        }
    }

    impl<A> ObjectData<A> {
        fn new(object: Object<A>) -> Self {
            ObjectData {
                object: object,
                in_links: Vec::new(),
                out_links: Vec::new(),
            }
        }
    }

    impl<A: Eq + Hash + Clone> Graph<A> {
        /// Create a new empty graph.
        pub fn new() -> Self {
            Graph {
                objects: SlotVec::new(),
                atom_indexes: HashMap::new(),
                link_indexes: HashMap::new(),
            }
        }

        /// Get object, index may be unattributed
        pub fn get_object<'a>(&'a self, index: Index) -> Option<ObjectRef<'a, A>> {
            self.objects
                .get(index.to_usize())
                .map(|object_data| ObjectRef {
                    index: index,
                    object_data: object_data,
                })
        }
        /// Get object, assume valid index
        pub fn object<'a>(&'a self, index: Index) -> ObjectRef<'a, A> {
            self.get_object(index).expect("invalid index")
        }

        /// Iterate on valid objects
        pub fn objects<'a>(&'a self) -> OrderedObjectIterator<'a, A> {
            OrderedObjectIterator {
                next_index: 0,
                graph: self,
            }
        }

        /// Get index of an atom, or None if not found.
        pub fn index_of_atom(&self, atom: &A) -> Option<Index> {
            self.atom_indexes.get(&atom).map(|index_ref| *index_ref)
        }
        /// Get index of a link, or None if not found.
        pub fn index_of_link(&self, link: &Link) -> Option<Index> {
            self.link_indexes.get(&link).map(|index_ref| *index_ref)
        }

        /// Insert a new atom, return its index.
        /// If already present, only return the current index for the atom.
        pub fn insert_atom(&mut self, atom: A) -> Index {
            match self.index_of_atom(&atom) {
                Some(index) => index,
                None => {
                    let new_index = Index(
                        self.objects
                            .insert(ObjectData::new(Object::Atom(atom.clone()))),
                    );
                    self.atom_indexes.insert(atom, new_index);
                    new_index
                }
            }
        }
        /// Insert a new link, return its index.
        /// If already present, only return the current index for the link.
        pub fn insert_link(&mut self, from: Index, to: Index) -> Index {
            let link = Link { from: from, to: to }; // TODO improve ?
            match self.index_of_link(&link) {
                Some(index) => index,
                None => {
                    let new_index = Index(
                        self.objects
                            .insert(ObjectData::new(Object::Link(link.clone()))),
                    );
                    self.objects[link.from.to_usize()].out_links.push(new_index);
                    self.objects[link.to.to_usize()].in_links.push(new_index);
                    self.link_indexes.insert(link, new_index);
                    new_index
                }
            }
        }
        /// Insert a new entity. Return its index.
        pub fn insert_entity(&mut self) -> Index {
            Index(self.objects.insert(ObjectData::new(Object::Entity(Entity))))
        }
    }

    impl<'a, A> ObjectRef<'a, A> {
        pub fn index(&self) -> Index {
            self.index
        }
        pub fn object(&self) -> &'a Object<A> {
            &self.object_data.object
        }
        pub fn in_links(&self) -> &'a Vec<Index> {
            &self.object_data.in_links
        }
        pub fn out_links(&self) -> &'a Vec<Index> {
            &self.object_data.out_links
        }
        pub fn is_link(&self) -> bool {
            match self.object() {
                &Object::Link(_) => true,
                _ => false,
            }
        }
    }

    impl<'a, A: Eq + Hash + Clone> Iterator for OrderedObjectIterator<'a, A> {
        type Item = ObjectRef<'a, A>;
        fn next(&mut self) -> Option<Self::Item> {
            loop {
                let current_index = self.next_index;
                if current_index >= self.graph.objects.nb_slots() {
                    return None;
                };
                self.next_index = current_index + 1;
                if let Some(object_ref) = self.graph.get_object(Index(current_index)) {
                    return Some(object_ref);
                }
            }
        }
    }
}

///*****************************************************************************
/// Atom: represents a basic piece of data (integer, string, etc)
#[derive(PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
enum Atom {
    String(String),
    Integer(i32),
}
impl<'a> From<&'a str> for Atom {
    fn from(text: &'a str) -> Self {
        Atom::String(String::from(text))
    }
}

type Graph = graph::Graph<Atom>;

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::io;

///*****************************************************************************
/// Output graph as dot.
///
/// Added basic filtering.
/// TODO improve it. Problem: some referenced elements are not pretty printed.
/// Notion of graph view (partial) ?

fn output_as_dot_filtered(
    out: &mut io::Write,
    g: &Graph,
    elements: &HashSet<graph::Index>,
) -> io::Result<()> {
    /* Link handling.
     *
     * Links in a graph behave as objects and can have links pointing to them.
     * This is not allowed in dot.
     * Thus links with in_links()/out_links() are split into two Dot arrows with a dummy node.
     *
     * To improve readability, each link is given an arrow color.
     * This arrow color must be different from in_links() and out_links().
     * This ensures that both Dot edges can be easily recognized as part of the same link.
     *
     * Colors are selected from a fixed palette (generating one is complex).
     * This should be sufficient as theere are few conflicts in practice.
     *
     * Colors are chosen by a greedy algorithm:
     * color (link) = min unused index among lower index link neighbors.
     */
    let color_palette = [
        "#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255",
        "#AA4499",
    ];

    // Pretty print for some types
    impl fmt::Display for graph::Index {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            self.to_usize().fmt(f)
        }
    }
    impl fmt::Display for Atom {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                &Atom::String(ref s) => write!(f, "\\\"{}\\\"", s),
                &Atom::Integer(i) => i.fmt(f),
            }
        }
    }

    // Table to store color assignements for previously printed Links.
    let mut link_color_indexes: HashMap<graph::Index, usize> = HashMap::new();

    // Select a color index for a link, assuming all lesser indexed Links have been colored.
    let mut choose_color_index_for_link = |object_ref: &graph::ObjectRef<Atom>| {
        if let &graph::Object::Link(ref link) = object_ref.object() {
            let index = object_ref.index();
            // Build a list of colors of all links we are in conflict with
            let conflicting_color_indexes = {
                let mut conflicting_color_indexes = Vec::new();
                {
                    let mut conflict_with_color_of_link = |i: &graph::Index| {
                        if *i < index {
                            conflicting_color_indexes.push(link_color_indexes[i])
                        }
                    };
                    // Conflicts with links we are pointing to/from
                    if g.object(link.from).is_link() {
                        conflict_with_color_of_link(&link.from)
                    };
                    if g.object(link.to).is_link() {
                        conflict_with_color_of_link(&link.to)
                    };
                    // Conflicts with links that are pointing to/from us
                    object_ref
                        .in_links()
                        .iter()
                        .for_each(&mut conflict_with_color_of_link);
                    object_ref
                        .out_links()
                        .iter()
                        .for_each(&mut conflict_with_color_of_link);
                }
                conflicting_color_indexes
            };
            // Select first unused color index
            let mut color_index = 0;
            while conflicting_color_indexes.contains(&color_index) {
                color_index += 1
            }
            assert!(
                color_index <= color_palette.len(),
                "output_as_dot: nb_colors = {} exceeds the color palette size ({})",
                color_index,
                color_palette.len()
            );
            // Store it for next calls to choose_link_color()
            link_color_indexes.insert(index, color_index);
            color_index
        } else {
            panic!("object_ref must be a link");
        }
    };

    // Output graph
    writeln!(out, "digraph {{")?;
    for object_ref in g.objects()
        .filter(|object_ref| elements.contains(&object_ref.index()))
    {
        let index = object_ref.index();
        match object_ref.object() {
            &graph::Object::Atom(ref a) => {
                writeln!(out, "\t{0} [shape=box,label=\"{0}: {1}\"];", index, a)?;
            }
            &graph::Object::Link(ref link) => {
                let color = color_palette[choose_color_index_for_link(&object_ref)];
                if object_ref.in_links().is_empty() && object_ref.out_links().is_empty() {
                    writeln!(
                        out,
                        "\t{0} -> {1} [fontcolor=grey,color=\"{3}\",label=\"{2}\"];",
                        link.from, link.to, index, color
                    )?;
                } else {
                    writeln!(out,
                    "\t{0} [shape=none,fontcolor=grey,margin=0.02,height=0,width=0,label=\"{0}\"];",
                    index
                )?;
                    writeln!(
                        out,
                        "\t{0} -> {1} [dir=none,color=\"{2}\"];",
                        link.from, index, color
                    )?;
                    writeln!(out, "\t{0} -> {1} [color=\"{2}\"];", index, link.to, color)?;
                }
            }
            &graph::Object::Entity(_) => {
                writeln!(out, "\t{0} [shape=hexagon,label=\"{0}\"];", index)?;
            }
        }
    }
    writeln!(out, "}}")
}

fn output_as_dot(out: &mut io::Write, g: &Graph) -> io::Result<()> {
    let all_elements = g.objects()
        .map(|object_ref| object_ref.index())
        .collect::<HashSet<_>>();
    output_as_dot_filtered(out, g, &all_elements)
}

/*******************************************************************************
 * Matching of graphs against each other.
 * TODO return iterator over possible matches
 */

fn match_graph(pattern: &Graph, target: &Graph) -> Option<HashMap<graph::Index, graph::Index>> {
    // Used to build a mapping iteratively
    struct MappingBuilder {
        mapping: HashMap<graph::Index, graph::Index>,
        matched_pattern_objects_to_inspect: HashSet<graph::Index>,
    }
    impl MappingBuilder {
        fn new() -> Self {
            MappingBuilder {
                mapping: HashMap::new(),
                matched_pattern_objects_to_inspect: HashSet::new(),
            }
        }

        // Returns true if ok, false if conflicts with current matching
        fn add(&mut self, pattern_object: graph::Index, target_object: graph::Index) -> bool {
            if let Some(&current_matched_target_object) = self.mapping.get(&pattern_object) {
                current_matched_target_object == target_object
            } else {
                self.mapping.insert(pattern_object, target_object);
                self.matched_pattern_objects_to_inspect
                    .insert(pattern_object);
                true
            }
        }

        fn next_matched_pattern_object_to_inspect(&mut self) -> Option<graph::Index> {
            self.matched_pattern_objects_to_inspect.drain().next()
        }
    }

    let mut mapping = MappingBuilder::new();

    // Match atoms, which are unambiguous
    for pattern_object_ref in pattern.objects() {
        if let &graph::Object::Atom(ref a) = pattern_object_ref.object() {
            if let Some(target_object) = target.index_of_atom(a) {
                mapping.add(pattern_object_ref.index(), target_object);
            }
        }
    }

    // Match the rest
    while let Some(matched_pattern_object) = mapping.next_matched_pattern_object_to_inspect() {
        // Match neighboring stuff that is unambiguous (and not matched)
        // Add them to list of matched stuff
        let matched_pattern_object_ref = pattern.object(matched_pattern_object);
        let matched_target_object_ref = target.object(mapping.mapping[&matched_pattern_object]);

        // Match in_links/out_links if unique between pair of matched elements
        if matched_pattern_object_ref.in_links().len() == 1
            && matched_target_object_ref.in_links().len() == 1
        {
            if !mapping.add(
                matched_pattern_object_ref.in_links()[0],
                matched_target_object_ref.in_links()[0],
            ) {
                return None;
            }
        }
        if matched_pattern_object_ref.out_links().len() == 1
            && matched_target_object_ref.out_links().len() == 1
        {
            if !mapping.add(
                matched_pattern_object_ref.out_links()[0],
                matched_target_object_ref.out_links()[0],
            ) {
                return None;
            }
        }

        // Match ends if matched object is link
        if let &graph::Object::Link(ref pattern_link) = matched_pattern_object_ref.object() {
            if let &graph::Object::Link(ref matched_link) = matched_target_object_ref.object() {
                // From
            } else {
                return None; // Must be a link
            }
        }
    }

    Some(mapping.mapping)
}

/*******************************************************************************
 * Test
 */
fn create_name_prop(g: &mut Graph) -> graph::Index {
    let name_entity = g.insert_entity();
    let name_text = g.insert_atom(Atom::from("name"));
    let name_entity_description = g.insert_link(name_text, name_entity);
    let _name_entity_description_description = g.insert_link(name_entity, name_entity_description);
    name_entity
}

fn create_named_entity(g: &mut Graph, name_entity: graph::Index, text: &str) -> graph::Index {
    let entity = g.insert_entity();
    let atom = g.insert_atom(Atom::from(text));
    let link = g.insert_link(atom, entity);
    let _link_description = g.insert_link(name_entity, link);
    entity
}

fn set_test_data(g: &mut Graph) {
    let name = create_name_prop(g);

    let joe = create_named_entity(g, name, "joe");
    let bob = create_named_entity(g, name, "bob");

    let pj = create_named_entity(g, name, "pj");
    g.insert_link(pj, joe);
    g.insert_link(pj, bob);

    let fight = create_named_entity(g, name, "fight");
    let joe_in_fight = g.insert_link(joe, fight);
    let bob_in_fight = g.insert_link(bob, fight);

    let was_present = create_named_entity(g, name, "was_present");
    g.insert_link(was_present, joe_in_fight);
    g.insert_link(was_present, bob_in_fight);

    let win = create_named_entity(g, name, "win");
    g.insert_link(win, bob_in_fight);

    let date = create_named_entity(g, name, "date");
    let some_date = g.insert_atom(Atom::Integer(2018));
    let fight_date = g.insert_link(some_date, fight);
    g.insert_link(date, fight_date);
}

fn main() {
    let mut graph = Graph::new();
    set_test_data(&mut graph);
    //output_as_dot(&mut io::stdout(), &graph).unwrap();
    if false {
        let mut pattern = Graph::new();
        let name_prop = create_name_prop(&mut pattern);

        let mapping = match_graph(&pattern, &graph);
        eprintln!("MAPPING {:?}", &mapping);
    }
    {
        // Print the matched part of graph
        let self_mapping = match_graph(&graph, &graph).expect("match failure");
        let matched_elements = self_mapping.keys().cloned().collect::<HashSet<_>>();
        output_as_dot_filtered(&mut io::stdout(), &graph, &matched_elements);
    }
}
