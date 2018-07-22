use super::serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::collections::HashMap;
use std::convert::AsRef;
use std::fmt;
use std::ops::Deref;

// TODO remove / update elements semantics
// TODO pattern matching as needed for the wiki output

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Serialize, Deserialize)]
pub enum Atom {
    Text(String),
    Integer(i32),
}
impl Atom {
    pub fn text<T: Into<String>>(text: T) -> Self {
        Atom::Text(text.into())
    }
}
impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Atom::Text(ref s) => s.fmt(f),
            Atom::Integer(i) => i.fmt(f),
        }
    }
}

/// Index for graph elements.
pub type Index = usize;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
pub struct Link {
    pub from: Index,
    pub to: Index,
}
impl Link {
    pub fn new(from: Index, to: Index) -> Self {
        Link { from: from, to: to }
    }
}
impl From<(Index, Index)> for Link {
    fn from(pair: (Index, Index)) -> Link {
        Link::new(pair.0, pair.1)
    }
}

/** Object of the graph:
 * All objects are identified by their index in the graph (which is constant after creation).
 * All objects can be pointed to/from by a link.
 *
 * Atom: a basic piece of concrete data.
 * Must be hashmap compatible (comparable).
 * In a graph, atoms are unique, and can be searched from their value.
 *
 * Link: a directed arrow between two graph objects.
 * Links are also unique and can be searched from their value.
 * Links can link any two elements of the graph (no restriction).
 * It is up to the user to give semantics to a link.
 * A common pattern is to "annotate a link with an atom" with an atom representing a relation type.
 * It consists of creating another link from the atom to the annotated link.
 *
 * Abstract: abstract graph object with no data.
 * Abstract objects are exclusively defined by their links (relations with other objects).
 * They are not comparable, and must be searched by pattern matching of their relation.
 */
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Object {
    Atom(Atom),
    Link(Link),
    Abstract,
}

/** Data for each graph object.
 * In addition to the object, store local topology for fast traversal.
 * in_links/out_links: indexes of links pointing to/from this link.
 * TODO Vec<String> notes that are not part of topology ?
 */
struct ObjectData {
    object: Object,
    in_links: Vec<Index>,
    out_links: Vec<Index>,
}
impl ObjectData {
    fn new(object: Object) -> Self {
        ObjectData {
            object: object,
            in_links: Vec::new(),
            out_links: Vec::new(),
        }
    }
}

pub struct Graph {
    objects: Vec<Option<ObjectData>>,
    atom_indexes: HashMap<Atom, Index>,
    link_indexes: HashMap<Link, Index>,
}

impl Graph {
    /// Create a new empty graph.
    pub fn new() -> Self {
        Graph {
            objects: Vec::new(),
            atom_indexes: HashMap::new(), //TODO replace with something supporting partial/fuzzy searches
            link_indexes: HashMap::new(),
        }
    }

    /// Get object, index may be invalid
    pub fn get_object<'a>(&'a self, index: Index) -> Option<ObjectRef<'a>> {
        match self.objects.get(index) {
            Some(&Some(ref object_data)) => Some(ObjectRef {
                index: index,
                object_data: object_data,
                graph: self,
            }),
            _ => None,
        }
    }
    /// Get object, assume valid index
    pub fn object<'a>(&'a self, index: Index) -> ObjectRef<'a> {
        self.get_object(index).expect("invalid index")
    }

    /// Iterate on valid objects
    pub fn objects<'a>(&'a self) -> OrderedObjectIterator<'a> {
        OrderedObjectIterator {
            next_index: 0,
            graph: self,
        }
    }

    /// Get index of an atom, or None if not found.
    pub fn get_atom(&self, atom: &Atom) -> Option<Index> {
        self.atom_indexes.get(&atom).cloned()
    }
    /// Get index of a link, or None if not found.
    pub fn get_link(&self, link: &Link) -> Option<Index> {
        self.link_indexes.get(&link).cloned()
    }

    /// Get the index of an atom, inserting it if not found.
    pub fn use_atom(&mut self, atom: Atom) -> Index {
        match self.get_atom(&atom) {
            Some(index) => index,
            None => {
                let new_index = self.insert_object(Object::Atom(atom.clone()));
                self.register_atom(new_index, atom);
                new_index
            }
        }
    }
    /// Get the index of an atom, inserting it if not found.
    pub fn use_link(&mut self, link: Link) -> Index {
        match self.get_link(&link) {
            Some(index) => index,
            None => {
                let new_index = self.insert_object(Object::Link(link.clone()));
                self.register_link(new_index, link);
                new_index
            }
        }
    }
    /// Create a new abstract object, return its index.
    pub fn create_abstract(&mut self) -> Index {
        self.insert_object(Object::Abstract)
    }

    fn insert_object(&mut self, object: Object) -> Index {
        // Find unused index
        for index in 0..self.objects.len() {
            let mut cell = &mut self.objects[index];
            if cell.is_none() {
                *cell = Some(ObjectData::new(object));
                return index;
            }
        }
        // Or allocate new one
        let index = self.objects.len();
        self.objects.push(Some(ObjectData::new(object)));
        index
    }
    fn object_mut(&mut self, index: Index) -> &mut ObjectData {
        self.objects[index].as_mut().expect("Invalid index")
    }
    fn register_atom(&mut self, index: Index, atom: Atom) {
        let old = self.atom_indexes.insert(atom, index);
        assert_eq!(old, None);
    }
    fn register_link(&mut self, index: Index, link: Link) {
        self.object_mut(link.from).out_links.push(index);
        self.object_mut(link.to).in_links.push(index);
        let old = self.link_indexes.insert(link, index);
        assert_eq!(old, None);
    }
}

/// Reference an object and its data. Has AsRef and Deref to behave like an Object.
#[derive(Clone, Copy)]
pub struct ObjectRef<'a> {
    index: Index,
    object_data: &'a ObjectData,
    graph: &'a Graph,
}
impl<'a> ObjectRef<'a> {
    pub fn index(&self) -> Index {
        self.index
    }
    pub fn graph(&self) -> &Graph {
        &self.graph
    }
    pub fn in_links(&self) -> &'a Vec<Index> {
        &self.object_data.in_links
    }
    pub fn out_links(&self) -> &'a Vec<Index> {
        &self.object_data.out_links
    }
}
impl<'a> AsRef<Object> for ObjectRef<'a> {
    fn as_ref(&self) -> &Object {
        &self.object_data.object
    }
}
impl<'a> Deref for ObjectRef<'a> {
    type Target = Object;
    fn deref(&self) -> &Object {
        &self.object_data.object
    }
}

impl Object {
    pub fn is_atom(&self) -> bool {
        match *self {
            Object::Atom(_) => true,
            _ => false,
        }
    }
    pub fn is_link(&self) -> bool {
        match *self {
            Object::Link(_) => true,
            _ => false,
        }
    }
    pub fn is_abstract(&self) -> bool {
        match *self {
            Object::Abstract => true,
            _ => false,
        }
    }
    // FIXME not very structured like. keep or improve depending on use cases
    pub fn as_link(&self) -> &Link {
        match *self {
            Object::Link(ref l) => l,
            _ => panic!("not a link"),
        }
    }
}

/// Iterate on objects in order of increasing indexes.
pub struct OrderedObjectIterator<'a> {
    next_index: usize,
    graph: &'a Graph,
}
impl<'a> Iterator for OrderedObjectIterator<'a> {
    type Item = ObjectRef<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let current_index = self.next_index;
            if current_index >= self.graph.objects.len() {
                return None;
            };
            self.next_index = current_index + 1;
            if let Some(object_ref) = self.graph.get_object(current_index) {
                return Some(object_ref);
            }
        }
    }
}

/******************************************************************************
 * IO using serde.
 * The graph is serialized as a sequence of Option<Object>.
 * Atoms, topology and indexes are conserved.
 */
impl Serialize for Graph {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeSeq;
        let mut seq = serializer.serialize_seq(Some(self.objects.len()))?;
        for cell in &self.objects {
            seq.serialize_element(&cell.as_ref().map(|obj_data| &obj_data.object))?;
        }
        seq.end()
    }
}

impl<'d> Deserialize<'d> for Graph {
    fn deserialize<D: Deserializer<'d>>(deserializer: D) -> Result<Self, D::Error> {
        use serde::de::{SeqAccess, Visitor};
        use std::fmt;

        // Define a Visitor tag for our case, and pass it to deserializer.
        struct GraphVisitor;
        impl<'s> Visitor<'s> for GraphVisitor {
            type Value = Graph;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("graph::Graph")
            }

            fn visit_seq<S: SeqAccess<'s>>(self, mut seq: S) -> Result<Self::Value, S::Error> {
                let mut graph = Graph::new();
                // Fill graph with raw objects
                while let Some(cell) = seq.next_element::<Option<Object>>()? {
                    graph.objects.push(cell.map(|obj| ObjectData::new(obj)))
                }
                // Now that all objects are defined, restore ObjectData.
                for index in 0..graph.objects.len() {
                    let maybe_object: Option<Object> = graph.objects[index]
                        .as_ref()
                        .map(|obj_data| obj_data.object.clone());
                    match maybe_object {
                        Some(Object::Atom(atom)) => graph.register_atom(index, atom),
                        Some(Object::Link(link)) => graph.register_link(index, link),
                        _ => (),
                    }
                }
                Ok(graph)
            }
        }
        deserializer.deserialize_seq(GraphVisitor)
    }
}

/******************************************************************************
 * Tests.
 */
#[cfg(test)]
mod tests {
    use super::super::serde_json;
    use super::*;

    // This equality operator is for test only. Abstract objects are supposed to be non-comparable.
    impl PartialEq for Object {
        fn eq(&self, other: &Object) -> bool {
            match (self, other) {
                (Object::Atom(ref l), Object::Atom(ref r)) => l == r,
                (Object::Link(ref l), Object::Link(ref r)) => l == r,
                (Object::Abstract, Object::Abstract) => true,
                _ => false,
            }
        }
    }

    #[test]
    fn io() {
        // Dummy graph
        let mut graph = Graph::new();
        let i0 = graph.create_abstract();
        let i1 = graph.use_atom(Atom::text("Abstract"));
        let i2 = graph.use_link(Link::new(i1, i0));
        let _i3 = graph.use_link(Link::new(i0, i2));
        // Serialize and deserialize
        let serialized = serde_json::to_string(&graph).expect("Serialization failure");
        let deserialized: Graph =
            serde_json::from_str(&serialized).expect("Deserialization failure");
        // Compare
        for object_ref in graph.objects() {
            let deserialized_object_ref = deserialized.object(object_ref.index());
            assert_eq!(object_ref.object(), deserialized_object_ref.object());
        }
    }
}
