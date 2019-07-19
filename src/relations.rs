use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::io;
use std::marker::PhantomData;

/// Error type for graph operations
#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    InvalidIndex,
    DuplicatedElement,
}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::InvalidIndex => "invalid index".fmt(f),
            Error::DuplicatedElement => "duplicated element".fmt(f),
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
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Atom {
    Text(String),
    // TODO integers ?
    // TODO tuple of atoms ? (for dates, etc)
}

/// Binary relation between any two elements, tagged by a third one.
/// If the second entity is omitted, this is a simple description.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Relation {
    pub subject: Index,
    pub descriptor: Index,
    pub complement: Option<Index>,
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

#[derive(Clone, Debug)]
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
    index_of_text_atoms: HashMap<String, AtomIndex>,
    index_of_relations: HashMap<Relation, RelationIndex>,
}

impl Database {
    pub fn new() -> Database {
        Database {
            elements: SlotVec::new(),
            index_of_text_atoms: HashMap::new(),
            index_of_relations: HashMap::new(),
        }
    }

    /// Add a new abstract element.
    pub fn create_abstract_element(&mut self) -> Index {
        self.elements.insert(ElementData::new(Element::Abstract))
    }

    /// Add an atom, or return index if already present.
    pub fn insert_atom(&mut self, atom: Atom) -> Index {
        match self.index_of_atom(&atom) {
            Some(index) => index,
            None => {
                let data = ElementData::new(Element::Atom(atom.clone()));
                let index = self.elements.insert(data);
                self.register_atom(index, atom).unwrap();
                index
            }
        }
    }
    // Add a newly inserted Atom (at index) to tables. No-op on error.
    fn register_atom(&mut self, index: Index, atom: Atom) -> Result<(), Error> {
        let insert = match atom {
            Atom::Text(s) => self.index_of_text_atoms.insert(s, index),
        };
        match insert {
            Some(_) => Err(Error::DuplicatedElement),
            None => Ok(()),
        }
    }

    /// Add a relation, or return index if already present.
    pub fn insert_relation(&mut self, relation: Relation) -> Result<Index, Error> {
        match self.index_of_relation(&relation) {
            Some(index) => Ok(index),
            None => {
                let data = ElementData::new(Element::Relation(relation.clone()));
                let index = self.elements.insert(data);
                match self.register_relation(index, relation) {
                    Ok(()) => Ok(index),
                    Err(e) => {
                        self.elements.remove(index); // Revert insertion.
                        Err(e)
                    }
                }
            }
        }
    }
    // Add a newly inserted Relation (at index) to tables. No-op on error.
    fn register_relation(&mut self, index: Index, rel: Relation) -> Result<(), Error> {
        let indexes_valid = self.elements.valid(rel.subject)
            && self.elements.valid(rel.descriptor)
            && rel.complement.map_or(true, |c| self.elements.valid(c));
        if !indexes_valid {
            return Err(Error::InvalidIndex);
        }
        if self.index_of_relations.insert(rel.clone(), index).is_some() {
            return Err(Error::DuplicatedElement);
        }
        self.elements[rel.subject].subject_of.insert(index);
        self.elements[rel.descriptor].descriptor_of.insert(index);
        if let Some(complement) = rel.complement {
            self.elements[complement].complement_of.insert(index);
        }
        Ok(())
    }

    /// Access element by index.
    pub fn element(&self, i: Index) -> Result<Ref<Element>, Error> {
        if self.elements.valid(i) {
            Ok(Ref::new(self, i))
        } else {
            Err(Error::InvalidIndex)
        }
    }

    // Retrieve index of indexable entities.
    pub fn index_of_atom(&self, atom: &Atom) -> Option<Index> {
        match atom {
            Atom::Text(s) => self.index_of_text_atom(s),
        }
    }
    pub fn index_of_text_atom<Q>(&self, text: &Q) -> Option<Index>
    where
        String: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.index_of_text_atoms.get(text).cloned()
    }
    pub fn index_of_relation(&self, relation: &Relation) -> Option<Index> {
        self.index_of_relations.get(relation).cloned()
    }

    /// Get atom ref if it exists, by name.
    pub fn get_text_atom<'a, Q>(&'a self, text: &Q) -> Option<Ref<'a, Atom>>
    where
        String: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.index_of_text_atom(text)
            .map(|index| Ref::new(self, index))
    }

    /// Iterate on all elements.
    pub fn iter<'a>(&'a self) -> ElementIterator<'a> {
        ElementIterator::new(self)
    }
}

/// A Ref<'a, E> is a valid index into the database to an "element of type E".
/// If E is Atom/Object/Relation, this is a ref to the specific variant.
/// If E is Element, this is a ref to any type (but still valid index).
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
impl<'a, E> Clone for Ref<'a, E> {
    // Manual impl because derive(Clone) requires E: Clone, which is not needed.
    fn clone(&self) -> Self {
        Self::new(self.database, self.index)
    }
}
impl<'a, E> Copy for Ref<'a, E> {}
impl<'a> Ref<'a, Element> {
    pub fn value(&self) -> &Element {
        &self.data().value
    }
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

/// Iterator on elements in the database, by increasing ids.
pub struct ElementIterator<'a> {
    database: &'a Database,
    index: Index,
}
impl<'a> ElementIterator<'a> {
    fn new(database: &'a Database) -> Self {
        ElementIterator {
            database: database,
            index: 0,
        }
    }
}
impl<'a> Iterator for ElementIterator<'a> {
    type Item = Ref<'a, Element>;
    fn next(&mut self) -> Option<Self::Item> {
        let end_index = self.database.elements.capacity();
        loop {
            if self.index == end_index {
                return None;
            }
            let current_index = self.index;
            self.index += 1;
            if self.database.elements.valid(current_index) {
                return Some(Ref::new(self.database, current_index));
            }
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.database.elements.capacity() - self.index))
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
    fn valid(&self, i: usize) -> bool {
        i < self.inner.len() && self.inner[i].is_some()
    }
    fn get(&self, i: usize) -> Option<&T> {
        match self.inner.get(i) {
            Some(&Some(ref e)) => Some(e),
            _ => None,
        }
    }
    fn get_mut(&mut self, i: usize) -> Option<&mut T> {
        match self.inner.get_mut(i) {
            Some(&mut Some(ref mut e)) => Some(e),
            _ => None,
        }
    }
    fn insert(&mut self, e: T) -> usize {
        // Find unused index
        for index in 0..self.inner.len() {
            let cell = &mut self.inner[index];
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
    fn remove(&mut self, i: usize) -> Option<T> {
        match self.inner.get_mut(i) {
            Some(slot) => slot.take(),
            None => None,
        }
    }
    fn capacity(&self) -> usize {
        self.inner.len()
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

/// Set based on a sorted vector.
#[derive(Debug, PartialEq, Eq)]
pub struct Set<T: Ord> {
    inner: Vec<T>,
}
impl<T: Ord> Set<T> {
    pub fn new() -> Self {
        Set { inner: Vec::new() }
    }
    pub fn contains(&self, e: &T) -> bool {
        self.inner.binary_search(e).is_ok()
    }
    pub fn insert(&mut self, e: T) {
        if let Err(insertion_index) = self.inner.binary_search(&e) {
            self.inner.insert(insertion_index, e)
        }
    }
    pub fn remove(&mut self, e: &T) {
        if let Ok(index) = self.inner.binary_search(e) {
            self.inner.remove(index);
        }
    }
}
impl<T: Ord> std::ops::Deref for Set<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        self.inner.deref()
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
        // Check and register elements
        let nb_slots = db.elements.inner.len();
        for index in 0..nb_slots {
            if let Some(element) = db.elements.inner[index].as_ref().map(|ed| ed.value.clone()) {
                match element {
                    Element::Abstract => Ok(()),
                    Element::Atom(atom) => db.register_atom(index, atom),
                    Element::Relation(relation) => db.register_relation(index, relation),
                }
                .map_err(|s| {
                    io::Error::new(
                        io::ErrorKind::Other,
                        format!("Bad Element at index {}: {}", index, s),
                    )
                })?;
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
    fn traits() {
        // Testing clone and copy. Compile test only.
        let mut db = Database::new();
        let object_i = db.create_abstract_element();
        let object_ref = db.element(object_i).unwrap();
        let copy = object_ref.clone();
        let _copy2 = copy;
        let _copy3 = copy;
    }

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

        // Test iterator (relies on incremental index allocation)
        let valid_indexes: Vec<_> = db.iter().map(|r| r.index()).collect();
        assert_eq!(
            valid_indexes,
            vec![name_i, object_i, is_named_i, relation_i]
        );

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
        let _relation_i = db.insert_relation(relation.clone()).unwrap();

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
