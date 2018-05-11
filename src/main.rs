extern crate serde;
#[macro_use]
extern crate serde_derive;

mod indexed_set;
use indexed_set::IndexedSet;

/*******************************************************************************
 */

#[derive(PartialEq, Eq, Hash, Serialize, Deserialize)]
enum Atom {
    String(String),
}
#[derive(PartialEq, Eq, Hash, Serialize, Deserialize)]
enum Object {
    Atom(Atom),
    Entity,
    Link { from: usize, to: usize },
}
struct Database {
    objects: IndexedSet<Object>,
}

impl Object {
    fn text(text: &str) -> Object {
        Object::Atom(Atom::String(String::from(text)))
    }
    fn link(from: usize, to: usize) -> Object {
        Object::Link { from, to }
    }
}

impl Database {
    fn new() -> Database {
        Database {
            objects: IndexedSet::new(),
        }
    }
    fn insert(&mut self, object: Object) -> usize {
        let id = self.objects.insert(object);
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

/*******************************************************************************
 * TODO output as dot : (c, link{a, b}) : a => c => b with color code on arrows
 * TODO queries, with hash map for referencing
 */

/*******************************************************************************
 * OLD test stuff
 */
fn output_as_dot(iv: &IndexedSet<Object>) {
    println!("digraph {{");
    for (index, opt_elem) in iv.into_iter().enumerate() {
        if let Some(elem) = opt_elem {
            match elem {
                &Object::Atom(Atom::String(ref s)) => {
                    println!("\t{} [shape=box,label=\"{}\"];", index, s)
                }
                &Object::Link { ref from, ref to } => {
                    println!("\t{} -> {} [label=\"{}\"];", from, to, index)
                }
                _ => {}
            }
        }
    }
    println!("}}");
}

extern crate serde_json;

fn main() {
    let mut database = Database::new();
    // Catégories de personnes
    let personnage = database.insert(Object::text("Personnage"));
    let pj = database.insert(Object::text("PJ"));
    database.insert(Object::link(pj, personnage));
    let pnj = database.insert(Object::text("PNJ"));
    database.insert(Object::link(pnj, personnage));
    // Liens entre personnes
    let ami = database.insert(Object::text("Ami de"));
    let _ennemi = database.insert(Object::text("Ennemi de"));
    // Quelques données
    let joe = database.insert(Object::text("Joe"));
    database.insert(Object::link(joe, pj));
    let alice = database.insert(Object::text("Alice"));
    database.insert(Object::link(alice, pnj));
    let joe_ami_alice = database.insert(Object::link(joe, alice));
    database.insert(Object::link(joe_ami_alice, ami));
    output_as_dot(&database.objects);

    let serialized = serde_json::to_string(&database).unwrap();
    println!("serialized = {}", serialized);

    let deserialized: Database = serde_json::from_str(&serialized).unwrap();
    // TODO to Database, check if it worked
    output_as_dot(&deserialized.objects)
}
