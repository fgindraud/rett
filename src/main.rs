extern crate serde;
#[macro_use]
extern crate serde_derive;

mod indexed_set;
use indexed_set::IndexedSet;

/*******************************************************************************
 */
type DatabaseIndex = indexed_set::Index;

#[derive(PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
enum Atom {
    String(String),
}
#[derive(PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
enum Object {
    Atom(Atom),
    Entity,
    Link {
        from: DatabaseIndex,
        to: DatabaseIndex,
    },
}

struct Database {
    objects: IndexedSet<Object>,
}

impl Object {
    fn text(text: &str) -> Object {
        Object::Atom(Atom::String(String::from(text)))
    }
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
    println!("digraph {{");
    for (index, opt_elem) in db.objects.into_iter().enumerate() {
        if let Some(elem) = opt_elem {
            match elem {
                &Object::Atom(Atom::String(ref s)) => {
                    println!("\t{0} [shape=box,label=\"{0} = \\\"{1}\\\"\"];", index, s);
                }
                &Object::Link { ref from, ref to } => {
                    println!(
                        "\t{0} [shape=none,fontcolor=grey,margin=0.02,height=0,width=0,label=\"{0}\"];",
                        index
                    );
                    println!("\t{0} -> {1} [color=blue];", from.as_usize(), index);
                    println!("\t{0} -> {1} [color=red];", index, to.as_usize());
                }
                &Object::Entity => {
                    println!("\t{0} [shape=box,label=\"{0}\"];", index);
                }
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

fn create_named_entity(db: &mut Database, text: &str) -> DatabaseIndex {
    let entity = db.insert(Object::Entity);
    let atom = db.insert(Object::text(text));
    let link = db.insert(Object::Link {
        from: atom,
        to: entity,
    });
    entity
}

fn set_test_data(db: &mut Database) {
    let name = create_named_entity(db, "name");
    let joe = create_named_entity(db, "joe");
    let bob = create_named_entity(db, "bob");
    let win = create_named_entity(db, "win");
    let joe_bob_fight = create_named_entity(db, "joe_bob_fight");
    let was_present = create_named_entity(db, "was_present");
}

fn main() {
    let mut database = Database::new();
    set_test_data(&mut database);
    output_as_dot(&database);

    //    let serialized = serde_json::to_string(&database).unwrap();
    //    println!("serialized = {}", serialized);
    //
    //    let deserialized: Database = serde_json::from_str(&serialized).unwrap();
    //    // TODO to Database, check if it worked
    //    output_as_dot(&deserialized.objects)
}
