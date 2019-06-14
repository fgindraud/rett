use std::collections::HashMap;
use std::fmt;
use std::io;
use std::marker::PhantomData;
use utils::Set;

/// Error type for graph operations
#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    InvalidIndex,
}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::InvalidIndex => "invalid index".fmt(f),
        }
    }
}
impl std::error::Error for Error {}

/// All database elements are referenced by an index, and share the same index space.
pub type Index = usize;

// Internal typedefs for clarity, indicate that the element must be of a certain type.
type AtomIndex = usize;
type RelationIndex = usize;

/// Abtract object, not self contained, described by its relations.
pub struct Abstract;

/// Atom of data that is known, self contained, indexable.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Atom {
    Text(String),
    // TODO integers ?
    // TODO tuple of atoms ? (for dates, etc)
}

/// Binary relation between any two elements, tagged by a third one.
/// If the second entity is omitted, this is a simple description.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Relation {
    subject: Index,
    descriptor: Index,
    complement: Option<Index>,
}

impl From<String> for Atom {
    fn from(s: String) -> Atom {
        Atom::Text(s)
    }
}
impl From<&str> for Atom {
    fn from(s: &str) -> Atom {
        Atom::Text(s.into())
    }
}

pub enum Element {
    Abstract,
    Atom(Atom),
    Relation(Relation),
}
struct ElementData {
    value: Element,
    subject_of: Set<RelationIndex>,
    descriptor_of: Set<RelationIndex>,
    complement_of: Set<RelationIndex>,
}
impl ElementData {
    fn new(e: Element) -> Self {
        Self {
            value: e,
            subject_of: Set::new(),
            descriptor_of: Set::new(),
            complement_of: Set::new(),
        }
    }
}

pub struct Database {
    elements: SlotVec<ElementData>,
    index_of_atoms: HashMap<Atom, AtomIndex>,
    index_of_relations: HashMap<Relation, RelationIndex>,
}

impl Database {
    pub fn new() -> Database {
        Database {
            elements: SlotVec::new(),
            index_of_atoms: HashMap::new(),
            index_of_relations: HashMap::new(),
        }
    }

    // Add new entities to the database.
    pub fn create_abstract_element(&mut self) -> Index {
        self.elements.insert(ElementData::new(Element::Abstract))
    }
    pub fn insert_atom(&mut self, atom: Atom) -> Index {
        match self.index_of_atom(&atom) {
            Some(index) => index,
            None => {
                let index = self
                    .elements
                    .insert(ElementData::new(Element::Atom(atom.clone())));
                self.index_of_atoms.insert(atom, index);
                index
            }
        }
    }
    pub fn insert_relation(&mut self, relation: Relation) -> Result<Index, Error> {
        let all_indexes_valid = {
            let is_valid = |i: Index| self.elements.get(i).is_ok();
            is_valid(relation.subject)
                && is_valid(relation.descriptor)
                && relation.complement.map_or(true, is_valid)
        };
        if !all_indexes_valid {
            return Err(Error::InvalidIndex);
        }
        Ok(match self.index_of_relation(&relation) {
            Some(index) => index,
            None => {
                let index = self
                    .elements
                    .insert(ElementData::new(Element::Relation(relation.clone())));
                self.elements[relation.subject].subject_of.insert(index);
                self.elements[relation.descriptor]
                    .descriptor_of
                    .insert(index);
                if let Some(i) = relation.complement {
                    self.elements[i].complement_of.insert(index)
                }
                self.elements[relation.subject].subject_of.insert(index);
                self.index_of_relations.insert(relation, index);
                index
            }
        })
    }

    pub fn element(&self, i: Index) -> Result<Ref<Element>, Error> {
        self.elements.get(i).map(|_| Ref::new(self, i))
    }

    // Retrieve index of indexable entities.
    pub fn index_of_atom(&self, atom: &Atom) -> Option<Index> {
        self.index_of_atoms.get(atom).cloned()
    }
    pub fn index_of_relation(&self, relation: &Relation) -> Option<Index> {
        self.index_of_relations.get(relation).cloned()
    }
}

/// A Ref<'a, E> is a valid index into the database to an "element of type E".
/// If E is Atom/Object/Relation, this is a ref to the specific variant.
/// If E is Element, this is a ref to any type (but still valid index).
#[derive(Clone, Copy)]
pub struct Ref<'a, ElementType> {
    database: &'a Database,
    index: Index,
    marker: PhantomData<&'a ElementType>,
}
/// Wrap a Set<Index> so that it returns Ref<Relation> instead.
#[derive(Clone, Copy)]
pub struct RelationRefSet<'a> {
    database: &'a Database,
    set: &'a Set<Index>,
}
/// Enum of ref structs, to perform exploration.
pub enum ElementRef<'a> {
    Abstract(Ref<'a, Abstract>),
    Atom(Ref<'a, Atom>),
    Relation(Ref<'a, Relation>),
}

impl<'a, E> Ref<'a, E> {
    fn new(db: &'a Database, i: Index) -> Self {
        Self {
            database: db,
            index: i,
            marker: PhantomData,
        }
    }
    pub fn database(&self) -> &Database {
        self.database
    }
    pub fn index(&self) -> Index {
        self.index
    }
    pub fn subject_of(&self) -> RelationRefSet<'a> {
        RelationRefSet::new(self.database, &self.data().subject_of)
    }
    pub fn descriptor_of(&self) -> RelationRefSet<'a> {
        RelationRefSet::new(self.database, &self.data().descriptor_of)
    }
    pub fn complement_of(&self) -> RelationRefSet<'a> {
        RelationRefSet::new(self.database, &self.data().complement_of)
    }
    fn data(&self) -> &'a ElementData {
        &self.database.elements[self.index]
    }
}
impl<'a> Ref<'a, Element> {
    pub fn cases(&self) -> ElementRef<'a> {
        match self.data().value {
            Element::Abstract => ElementRef::Abstract(Ref::new(self.database, self.index)),
            Element::Atom(_) => ElementRef::Atom(Ref::new(self.database, self.index)),
            Element::Relation(_) => ElementRef::Relation(Ref::new(self.database, self.index)),
        }
    }
}
impl<'a> Ref<'a, Atom> {
    pub fn value(&self) -> &Atom {
        match self.data().value {
            Element::Atom(ref atom) => atom,
            _ => panic!("Ref<Atom> must be an atom"),
        }
    }
}
impl<'a> Ref<'a, Relation> {
    pub fn value(&self) -> &Relation {
        match self.data().value {
            Element::Relation(ref rel) => rel,
            _ => panic!("Ref<Relation> must be a relation"),
        }
    }
    pub fn subject(&self) -> Ref<'a, Element> {
        Ref::new(self.database, self.value().subject)
    }
    pub fn descriptor(&self) -> Ref<'a, Element> {
        Ref::new(self.database, self.value().descriptor)
    }
    pub fn complement(&self) -> Option<Ref<'a, Element>> {
        self.value().complement.map(|i| Ref::new(self.database, i))
    }
}
impl<'a> RelationRefSet<'a> {
    fn new(db: &'a Database, set: &'a Set<Index>) -> Self {
        Self {
            database: db,
            set: set,
        }
    }
    pub fn len(&self) -> usize {
        self.set.len()
    }
    pub fn get(&self, i: usize) -> Ref<'a, Relation> {
        Ref::new(self.database, self.set[i])
    }
    pub fn iter(&self) -> impl Iterator<Item = Ref<'a, Relation>> {
        let database = self.database; // Explicitely clone ref
        self.set.into_iter().map(move |i| Ref::new(database, *i))
    }
}

/// Vector where elements never change indexes. Removal generate holes.
struct SlotVec<T> {
    inner: Vec<Option<T>>,
}
impl<T> SlotVec<T> {
    fn new() -> Self {
        SlotVec { inner: Vec::new() }
    }
    fn get(&self, i: usize) -> Result<&T, Error> {
        match self.inner.get(i) {
            Some(&Some(ref e)) => Ok(e),
            _ => Err(Error::InvalidIndex),
        }
    }
    fn get_mut(&mut self, i: usize) -> Result<&mut T, Error> {
        match self.inner.get_mut(i) {
            Some(&mut Some(ref mut e)) => Ok(e),
            _ => Err(Error::InvalidIndex),
        }
    }
    fn insert(&mut self, e: T) -> usize {
        // Find unused index
        for index in 0..self.inner.len() {
            let mut cell = &mut self.inner[index];
            if cell.is_none() {
                *cell = Some(e);
                return index;
            }
        }
        // Or allocate new one
        let index = self.inner.len();
        self.inner.push(Some(e));
        index
    }
}
impl<T> std::ops::Index<usize> for SlotVec<T> {
    type Output = T;
    fn index(&self, i: usize) -> &Self::Output {
        self.get(i).unwrap()
    }
}
impl<T> std::ops::IndexMut<usize> for SlotVec<T> {
    fn index_mut(&mut self, i: usize) -> &mut Self::Output {
        self.get_mut(i).unwrap()
    }
}

/******************************************************************************
 * IO using a simple text format.
 *
 * The slot-vector of elements is printed with one line per slot, in order.
 * The first char of the line indicates which type of element the line represents.
 * Empty lines are empty slots.
 */
impl Database {
    pub fn write_to<W: io::Write>(&self, mut w: W) -> io::Result<()> {
        for element_slot in self.elements.inner.iter() {
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
    pub fn read_from<R: io::BufRead>(reader: R) -> io::Result<Database> {
        let mut db = Database::new();
        // Fill empty db with raw elements
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
        for maybe_line in reader.lines() {
            let line = maybe_line?;
            let slot = if !line.is_empty() {
                match element_for(&line) {
                    Ok(e) => Some(ElementData::new(e)),
                    Err(reason) => {
                        return Err(io::Error::new(
                            io::ErrorKind::Other,
                            format!("Cannot parse line '{}': {}", line, reason),
                        ));
                    }
                }
            } else {
                None
            };
            db.elements.inner.push(slot)
        }
        // Check referenced indexes, update index maps
        let nb_slots = db.elements.inner.len();
        for index in 0..nb_slots {
            let elements = &db.elements;
            let validate_index = |i: Index| match elements.get(i) {
                Ok(_) => Ok(()),
                Err(_) => Err("invalid index"),
            };
            if let Some(ref data) = elements.inner[index] {
                let check_and_register = match data.value {
                    Element::Atom(ref a) => match db.index_of_atoms.insert(a.clone(), index) {
                        Some(_previous) => Err("atom duplicated"),
                        None => Ok(()),
                    },
                    Element::Relation(ref r) => {
                        match db.index_of_relations.insert(r.clone(), index) {
                            Some(_previous) => Err("relation duplicated"),
                            None => Ok(()),
                        }
                        .and_then(|()| validate_index(r.subject))
                        .and_then(|()| validate_index(r.descriptor))
                        .and_then(|()| r.complement.map_or(Ok(()), validate_index))
                    }
                    Element::Abstract => Ok(()),
                };
                check_and_register.map_err(|s| {
                    io::Error::new(
                        io::ErrorKind::Other,
                        format!("Bad Element at index {}: {}", index, s),
                    )
                })?;
            }
        }
        // Update *_of sets. Must be done carefully, as we scan and mutate db.elements.
        for index in 0..nb_slots {
            // Clone indexes if this is a relation, to avoid keeping a ref to db.elements.
            let maybe_relation = match db.elements.inner[index] {
                Some(ElementData {
                    value: Element::Relation(ref r),
                    ..
                }) => Some(r.clone()),
                _ => None,
            };
            // Add to sets, indexes are already validated at the previous steps.
            if let Some(relation) = maybe_relation {
                db.elements
                    .get_mut(relation.subject)
                    .unwrap()
                    .subject_of
                    .insert(index);
                db.elements
                    .get_mut(relation.descriptor)
                    .unwrap()
                    .descriptor_of
                    .insert(index);
                if let Some(complement) = relation.complement {
                    db.elements
                        .get_mut(complement)
                        .unwrap()
                        .complement_of
                        .insert(index);
                }
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
    fn basic() {
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
        let relation_i = db.insert_relation(relation.clone()).unwrap();

        // Test raw API.
        assert_eq!(Some(name_i), db.index_of_atom(&name));
        assert_eq!(name_i, db.insert_atom(name));
        let name2 = Atom::from("name");
        assert_eq!(None, db.index_of_atom(&name2));
        assert_ne!(name_i, db.insert_atom(name2));

        assert_eq!(Some(relation_i), db.index_of_relation(&relation));
        assert_eq!(Ok(relation_i), db.insert_relation(relation));
        let relation2 = Relation {
            subject: 42, // Bad index
            descriptor: is_named_i,
            complement: None,
        };
        assert_eq!(None, db.index_of_relation(&relation2));
        assert_eq!(Err(Error::InvalidIndex), db.insert_relation(relation2));

        // Test ref api
        assert!(db.element(name_i).is_ok());
        assert!(db.element(42).is_err());
        let r_name = db.element(name_i).unwrap();
        assert_eq!(name_i, r_name.index());
        assert!(match r_name.cases() {
            ElementRef::Atom(_) => true,
            _ => false,
        });
        assert_eq!(r_name.subject_of().len(), 0);
        assert_eq!(r_name.descriptor_of().len(), 0);
        assert_eq!(r_name.complement_of().len(), 1);
        let r_name = match r_name.cases() {
            ElementRef::Atom(a) => a,
            _ => panic!("not atom"),
        };
        assert_eq!(r_name.value(), &Atom::from("Name"));

        let complement = r_name.complement_of().get(0);
        assert_eq!(complement.index(), relation_i);
    }

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
        let relation_i = db.insert_relation(relation.clone()).unwrap();

        // Serialization
        let mut serialized: Vec<u8> = Vec::new();
        db.write_to(&mut serialized).expect("serialization failure");
        let expected_serialized = b"T Name\nA\nT is named\nR 1 2 0\n";
        assert_eq!(serialized, expected_serialized);

        // Deserialization
        let db_clone = Database::read_from(serialized.as_slice()).expect("deserialization failure");
        assert_eq!(db.elements.inner.len(), db_clone.elements.inner.len());
        for i in 0..db.elements.inner.len() {
            let both_slots_match = match (db.elements.get(i), db_clone.elements.get(i)) {
                (Err(_), Err(_)) => true,
                (Ok(dbo), Ok(dbc)) => {
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
        assert_eq!(db.index_of_atoms, db_clone.index_of_atoms);
        assert_eq!(db.index_of_relations, db_clone.index_of_relations);
    }
}
