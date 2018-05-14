extern crate serde;
#[macro_use]
extern crate serde_derive;

use std::fmt;

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
}

struct Database {
    objects: IndexedSet<Object>,
}

impl Database {
    fn new() -> Database {
        Database {
            objects: IndexedSet::new(),
        }
    }
    fn insert(&mut self, object: Object) -> DatabaseIndex {
        let id = self.objects.insert(object);
        // TODO register in tables
        id
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
            Ok(objects) => Ok(Database { objects: objects }),
            Err(e) => Err(e),
        }
    }
}

/* Output as dot.
 */
fn output_as_dot(db: &Database) {
    impl fmt::Display for Atom {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                &Atom::String(ref s) => write!(f, "\\\"{}\\\"", s),
                &Atom::Integer(i) => i.fmt(f),
            }
        }
    }
    println!("digraph {{");
    for (index, elem) in db.objects.into_iter() {
        match elem {
            &Object::Atom(ref a) => {
                println!("\t{0} [shape=box,label=\"{0} = {1}\"];", index, a);
            }
            &Object::Link(Link { ref from, ref to }) => {
                println!(
                    "\t{0} [shape=none,fontcolor=grey,margin=0.02,height=0,width=0,label=\"{0}\"];",
                    index
                );
                println!("\t{0} -> {1} [color=blue];", from, index);
                println!("\t{0} -> {1} [color=red];", index, to);
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
    output_as_dot(&database);

    //let serialized = serde_json::to_string(&database).unwrap();
    //println!("serialized = {}", serialized);
    //
    //    let deserialized: Database = serde_json::from_str(&serialized).unwrap();
    //    // TODO to Database, check if it worked
    //    output_as_dot(&deserialized.objects)
}
