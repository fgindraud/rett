extern crate serde;
#[macro_use]
extern crate serde_derive;

/*
mod slot_vec {
    struct Slot<T> {
        Used(T),
        Unused,
    }

    pub struct SlotVec<T> {
    }
}*/

/// Define a knowledge graph
mod graph {
    use std::hash::Hash;

    /// Opaque Index type for graph elements.
    #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Serialize, Deserialize, Debug)]
    pub struct Index(usize);

    /// A directed link (edge of the graph).
    #[derive(PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
    pub struct Link {
        from: Index,
        to: Index,
    }

    /// An abstract graph entity (node of the graph).
    /// Defined only by its relationships: considered different from others.
    #[derive(Eq, Hash, Clone, Serialize, Deserialize)]
    pub struct Entity;
    impl PartialEq for Entity {
        fn eq(&self, _rhs: &Entity) -> bool {
            false
        }
    }

    /// Object of the graph: Link, Entity, or Atom (parametrized).
    #[derive(PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
    pub enum Object<A> {
        Atom(A),
        Link(Link),
        Entity(Entity),
    }

    /// Data for each object.
    /// In addition to the object, stored ids of links pointing from/to the object.
    pub struct ObjectData<A> {
        object: Object<A>,
        in_links: Vec<Index>,
        out_links: Vec<Index>,
    }

    pub struct Graph<A> {
        objects: Vec<Option<ObjectData<A>>>, // TODO SlotVec<T>
    }
}

mod indexed_set;
use indexed_set::IndexedSet;

/*******************************************************************************
 * Database
 */
type DatabaseIndex = indexed_set::Index;

// Atom: represents a basic piece of data (integer, string, etc)
#[derive(PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
enum Atom {
    String(String),
    Integer(i32),
}

// Link: a directed arrow between two elements.
#[derive(PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
struct Link {
    from: DatabaseIndex,
    to: DatabaseIndex,
}

// Entity: an abstract object, defined by its relations with others.
// Cannot be compared with each other.
#[derive(Eq, Hash, Clone, Serialize, Deserialize)]
struct Entity;
impl PartialEq for Entity {
    fn eq(&self, _: &Entity) -> bool {
        false
    }
}

// Object: Sum type of the three above.
#[derive(PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
enum Object {
    Atom(Atom),
    Link(Link),
    Entity(Entity),
}
impl Object {
    // Nice constructors
    fn text(text: &str) -> Object {
        Object::Atom(Atom::String(String::from(text)))
    }
    fn link(from: DatabaseIndex, to: DatabaseIndex) -> Object {
        Object::Link(Link { from: from, to: to })
    }
    fn entity() -> Object {
        Object::Entity(Entity)
    }
    // AAA
    fn is_link(&self) -> bool {
        match self {
            &Object::Link(_) => true,
            _ => false,
        }
    }
}

struct Database {
    objects: IndexedSet<Object>,
}

impl Database {
    pub fn new() -> Database {
        Database {
            objects: IndexedSet::new(),
        }
    }
    pub fn insert(&mut self, object: Object) -> DatabaseIndex {
        let id = self.objects.insert(object);
        self.register(id);
        id
    }

    fn register(&mut self, new_object_id: DatabaseIndex) {
        // TODO
    }
}
impl From<IndexedSet<Object>> for Database {
    fn from(is: IndexedSet<Object>) -> Self {
        let mut db = Database { objects: is };
        //for (id, _) in db.objects.iter() {
        //    db.register(id);
        //} FIXME
        db
    }
}

// Serialize / Deserialize: only export the array.
impl ::serde::Serialize for Database {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ::serde::Serializer,
    {
        self.objects.serialize(serializer)
    }
}
impl<'de> ::serde::Deserialize<'de> for Database {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: ::serde::Deserializer<'de>,
    {
        match IndexedSet::<Object>::deserialize(deserializer) {
            Ok(objects) => Ok(Database::from(objects)),
            Err(e) => Err(e),
        }
    }
}

/*******************************************************************************
 * Output as dot.
 */
fn output_as_dot(objects: &IndexedSet<Object>) {
    use std::collections::HashMap;
    use std::cmp::{max, min};

    /* Color palette for Link arrows in dot.
     * Color palette selection is complicated.
     * For now, use a fixed size palette, which should be sufficient as there are few conflicts.
     */
    let color_palette = [
        "#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255",
        "#AA4499",
    ];

    let link_color_indexes = {
        /* Link arrow color selection.
         *
         * This algorithm select different colors for Link arrows to improve readability.
         * The rule is to select different colors for Link that touch (descriptions).
         * This is equivalent to coloring nodes of a graph of connected 'links'.
         *
         * Step 1: Determine the list of neighbor Links of each Link.
         * Step 2: Attribute a color index to each Link.
         * Step 3: Select a color (currently, a fixed palette).
         *
         * The algorithm always does a pass through Links in increasing index order, for all steps.
         * Step 2 is a simple greedy algorithm:
         * color (link) = min unused index among lower index link neighbors.
         * Thus Step 1 only create a neighbor list of lower index neighbors.
         */

        // Step 1
        let mut lower_index_neighbors = HashMap::new();
        for (index, elem) in objects {
            if let &Object::Link(ref link) = elem {
                if objects[link.from].is_link() {
                    lower_index_neighbors
                        .entry(max(index, link.from))
                        .or_insert(Vec::new())
                        .push(min(index, link.from));
                }
                if objects[link.to].is_link() {
                    lower_index_neighbors
                        .entry(max(index, link.to))
                        .or_insert(Vec::new())
                        .push(min(index, link.to));
                }
            }
        }
        let lower_index_neighbors = lower_index_neighbors;

        // Step 2
        let mut nb_colors = 0;
        let mut link_color_indexes = HashMap::new();
        for id in objects
            .into_iter()
            .filter_map(|(id, ref elem)| if elem.is_link() { Some(id) } else { None })
        {
            let color_index = match lower_index_neighbors.get(&id) {
                Some(ref link_neighbors) => {
                    // Get colors of all neighbors of lower indexes
                    let neighbour_color_indexes = link_neighbors
                        .iter()
                        .map(|n| link_color_indexes[n])
                        .collect::<Vec<_>>();
                    // Select first unused color index
                    let mut color_index = 0;
                    while neighbour_color_indexes.contains(&color_index) {
                        color_index += 1
                    }
                    color_index
                }
                None => 0,
            };
            nb_colors = max(nb_colors, color_index + 1);
            link_color_indexes.insert(id, color_index);
        }

        // Step 3
        assert!(
            nb_colors <= color_palette.len(),
            "output_as_dot: nb_colors = {} exceeds the color palette size ({})",
            nb_colors,
            color_palette.len()
        );
        link_color_indexes
    };

    // Print graph
    use std::fmt;
    impl fmt::Display for Atom {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                &Atom::String(ref s) => write!(f, "\\\"{}\\\"", s),
                &Atom::Integer(i) => i.fmt(f),
            }
        }
    }
    println!("digraph {{");
    for (index, elem) in objects {
        match elem {
            &Object::Atom(ref a) => {
                println!("\t{0} [shape=box,label=\"{0} = {1}\"];", index, a);
            }
            &Object::Link(ref link) => {
                println!(
                    "\t{0} [shape=none,fontcolor=grey,margin=0.02,height=0,width=0,label=\"{0}\"];",
                    index
                );
                let color = color_palette[link_color_indexes[&index]];
                println!("\t{0} -> {1} [color=\"{2}\"];", link.from, index, color);
                println!("\t{0} -> {1} [color=\"{2}\"];", index, link.to, color);
            }
            &Object::Entity(_) => {
                println!("\t{0} [shape=box,label=\"{0}\"];", index);
            }
        }
    }
    println!("}}");
}

/*******************************************************************************
 * TODO queries, with hash map for referencing
 */

/*******************************************************************************
 * Test
 */
extern crate serde_json;

fn create_name_prop(db: &mut Database) -> DatabaseIndex {
    let name_entity = db.insert(Object::entity());
    let name_text = db.insert(Object::text("name"));
    let name_entity_description = db.insert(Object::link(name_text, name_entity));
    let _name_entity_description_description =
        db.insert(Object::link(name_entity, name_entity_description));
    name_entity
}

fn create_named_entity(db: &mut Database, name_entity: DatabaseIndex, text: &str) -> DatabaseIndex {
    let entity = db.insert(Object::entity());
    let atom = db.insert(Object::text(text));
    let link = db.insert(Object::link(atom, entity));
    let _link_description = db.insert(Object::link(name_entity, link));
    entity
}

fn set_test_data(db: &mut Database) {
    let name = create_name_prop(db);

    let joe = create_named_entity(db, name, "joe");
    let bob = create_named_entity(db, name, "bob");

    let pj = create_named_entity(db, name, "pj");
    db.insert(Object::link(pj, joe));
    db.insert(Object::link(pj, bob));

    let fight = create_named_entity(db, name, "fight");
    let joe_in_fight = db.insert(Object::link(joe, fight));
    let bob_in_fight = db.insert(Object::link(bob, fight));

    let was_present = create_named_entity(db, name, "was_present");
    db.insert(Object::link(was_present, joe_in_fight));
    db.insert(Object::link(was_present, bob_in_fight));

    let win = create_named_entity(db, name, "win");
    db.insert(Object::link(win, bob_in_fight));

    let date = create_named_entity(db, name, "date");
    let some_date = db.insert(Object::Atom(Atom::Integer(2018)));
    let fight_date = db.insert(Object::link(some_date, fight));
    db.insert(Object::link(date, fight_date));
}

fn main() {
    let mut database = Database::new();
    set_test_data(&mut database);
    output_as_dot(&database.objects);

    //let serialized = serde_json::to_string(&database).unwrap();
    //println!("serialized = {}", serialized);
    //
    //    let deserialized: Database = serde_json::from_str(&serialized).unwrap();
    //    // TODO to Database, check if it worked
    //    output_as_dot(&deserialized.objects)
}
