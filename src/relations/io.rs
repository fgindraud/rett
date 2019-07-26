use std::fmt;
use std::io;
use std::fs::File;
use std::path::Path;

use super::{Atom, Database, Element, ElementData, Relation};
use utils::SlotVec;

/// Read the database from file.
/// In case of failure returns an empty database (for bootstrap).
pub fn read_database_from_file(filename: &Path) -> Database {
    match File::open(filename) {
        Ok(file) => match Database::read_from(io::BufReader::new(file)) {
            Ok(database) => return database,
            Err(e) => eprintln!(
                "[warning] Invalid database format in file {}: {}",
                filename.display(),
                e
            ),
        },
        Err(e) => eprintln!(
            "[warning] Cannot read database from {}: {}",
            filename.display(),
            e
        ),
    }
    eprintln!(
        "[database] File {} not present; using empty database",
        filename.display()
    );
    Database::new()
}

/// Write database to a file.
pub fn write_database_to_file(filename: &Path, database: &Database) -> Result<(), String> {
    File::create(filename)
        .and_then(|f| database.write_to(io::BufWriter::new(f)))
        .map_err(|e| {
            format!(
                "Cannot write database to {}: {}",
                filename.display(),
                e
            )
        })
}

/******************************************************************************
 * IO using a simple text format.
 *
 * The slot-vector of elements is printed with one line per slot, in order.
 * The first char of the line indicates which type of element the line represents.
 * Empty lines are empty slots.
 */
impl Database {
    /// Write database in a simple text format to any io.
    pub fn write_to<W: io::Write>(&self, mut w: W) -> io::Result<()> {
        for element_slot in self.elements.as_ref().iter() {
            match element_slot {
                Some(element) => match element.value {
                    Element::Abstract => write!(w, "A\n"),
                    Element::Atom(ref atom) => match atom {
                        Atom::Text(ref s) => write!(w, "T {}\n", EscapedAtomText(s)),
                    },
                    Element::Relation(ref rel) => match rel.complement {
                        Some(c) => write!(w, "R {} {} {}\n", rel.subject, rel.descriptor, c),
                        None => write!(w, "R {} {}\n", rel.subject, rel.descriptor),
                    },
                },
                None => write!(w, "\n"),
            }?
        }
        Ok(())
    }

    /// Read database in a simple text format from any io.
    pub fn read_from<R: io::BufRead>(reader: R) -> io::Result<Database> {
        // Read list of elements
        let element_for = |line: &str| -> Result<Element, &str> {
            let (type_char, tail) = split_first(line).unwrap();
            match type_char {
                'A' => match tail {
                    "" => Ok(Element::Abstract),
                    _ => Err("Abstract: trailing text"),
                },
                'T' => match split_first(tail) {
                    Some((' ', text)) => Ok(Element::Atom(Atom::from(text))),
                    _ => Err("Text: missing space"),
                },
                'R' => match split_first(tail) {
                    Some((' ', text)) => {
                        let mut it = text.split(' ').map(|s| s.parse::<usize>());
                        let fields = [it.next(), it.next(), it.next(), it.next()];
                        match fields {
                            [Some(Ok(s)), Some(Ok(d)), Some(Ok(c)), None] => {
                                Ok(Element::Relation(Relation {
                                    subject: s,
                                    descriptor: d,
                                    complement: Some(c),
                                }))
                            }
                            [Some(Ok(s)), Some(Ok(d)), None, None] => {
                                Ok(Element::Relation(Relation {
                                    subject: s,
                                    descriptor: d,
                                    complement: None,
                                }))
                            }
                            _ => Err("Relation: bad field format or count"),
                        }
                    }
                    _ => Err("Relation: missing space"),
                },
                _ => Err("Unrecognized type char"),
            }
        };
        let parsed_elements: io::Result<Vec<Option<ElementData>>> = reader
            .lines()
            .map(|maybe_line| {
                maybe_line.and_then(|line| {
                    if line.is_empty() {
                        Ok(None)
                    } else {
                        match element_for(&line) {
                            Ok(e) => Ok(Some(ElementData::new(e))),
                            Err(reason) => Err(io::Error::new(
                                io::ErrorKind::Other,
                                format!("Cannot parse line '{}': {}", line, reason),
                            )),
                        }
                    }
                })
            })
            .collect();
        Database::new_from(parsed_elements?).map_err(|s| io::Error::new(io::ErrorKind::Other, s))
    }

    fn new_from(elements: Vec<Option<ElementData>>) -> Result<Database, String> {
        let mut db = Database {
            elements: SlotVec::from(elements),
            ..Database::new()
        };
        // Check and register elements
        let nb_slots = db.elements.capacity();
        for index in 0..nb_slots {
            if let Some(element) = db.elements.as_ref()[index]
                .as_ref()
                .map(|ed| ed.value.clone())
            {
                match element {
                    Element::Abstract => Ok(()),
                    Element::Atom(atom) => db.register_atom(index, atom),
                    Element::Relation(relation) => db.register_relation(index, relation),
                }
                .map_err(|s| format!("Bad Element at index {}: {}", index, s))?;
            }
        }
        Ok(db)
    }
}

struct EscapedAtomText<'a>(&'a str);
impl<'a> fmt::Display for EscapedAtomText<'a> {
    // Remove all \n. TODO replace with ' ' or something else ?
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for s in self.0.split('\n') {
            s.fmt(f)?
        }
        Ok(())
    }
}

fn split_first(s: &str) -> Option<(char, &str)> {
    s.chars().next().map(|first: char| {
        let (_, tail) = s.split_at(first.len_utf8());
        (first, tail)
    })
}

/******************************************************************************
 * Tests.
 */
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn io() {
        // Create a very small database
        let mut db = Database::new();
        let name = Atom::from("Name");
        let name_i = db.insert_atom(name.clone());
        let object_i = db.create_abstract_element();
        let is_named = Atom::from("is named");
        let is_named_i = db.insert_atom(is_named.clone());
        let relation = Relation {
            subject: object_i,
            descriptor: is_named_i,
            complement: Some(name_i),
        };
        let _relation_i = db.insert_relation(relation.clone()).unwrap();

        // Serialization
        let mut serialized: Vec<u8> = Vec::new();
        db.write_to(&mut serialized).expect("serialization failure");
        let expected_serialized = b"T Name\nA\nT is named\nR 1 2 0\n";
        assert_eq!(serialized, expected_serialized);

        // Deserialization
        let db_clone = Database::read_from(serialized.as_slice()).expect("deserialization failure");
        assert_eq!(db.elements.capacity(), db_clone.elements.capacity());
        for i in 0..db.elements.capacity() {
            let both_slots_match = match (db.elements.get(i), db_clone.elements.get(i)) {
                (None, None) => true,
                (Some(dbo), Some(dbc)) => {
                    let element_match = match (&dbo.value, &dbc.value) {
                        (Element::Abstract, Element::Abstract) => true,
                        (Element::Atom(ref l), Element::Atom(ref r)) => l == r,
                        (Element::Relation(ref l), Element::Relation(ref r)) => l == r,
                        _ => false,
                    };
                    element_match
                        && dbo.subject_of == dbc.subject_of
                        && dbo.descriptor_of == dbc.descriptor_of
                        && dbo.complement_of == dbc.complement_of
                }
                _ => false,
            };
            assert!(both_slots_match);
        }
        assert_eq!(db.index_of_text_atoms, db_clone.index_of_text_atoms);
        assert_eq!(db.index_of_relations, db_clone.index_of_relations);
    }
}
