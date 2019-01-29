use std;
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::io;
use std::marker::PhantomData;

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
        add_new_element_to_data_vec(&mut self.elements, Element::Abstract)
    }
    pub fn insert_atom(&mut self, atom: Atom) -> Index {
        match self.index_of_atom(&atom) {
            Some(index) => index,
            None => {
                let index =
                    add_new_element_to_data_vec(&mut self.elements, Element::Atom(atom.clone()));
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
                let index = add_new_element_to_data_vec(
                    &mut self.elements,
                    Element::Relation(relation.clone()),
                );
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

fn add_new_element_to_data_vec(v: &mut SlotVec<ElementData>, e: Element) -> usize {
    v.insert(ElementData {
        value: e,
        subject_of: Set::new(),
        descriptor_of: Set::new(),
        complement_of: Set::new(),
    })
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

/******************************************************************************
 * Utils.
 */

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

/// Vector with sorted elements and set api.
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
 * Prelude for easy import.
 */
pub mod prelude {
    pub use super::Database;
    pub use super::Index;
    pub use super::{Abstract, Atom, Relation};
}

/******************************************************************************
 * IO using serde. FIXME use simple text format.
 *
 * Serialized as multiple Vec<Opt<DataStruct>>.
 * Foreach DataStruct, only serialize the inner object (not the backlinks).
 * After a row of serialize + deserialize, indexes are conserved.
 */
pub fn from_reader<R: io::Read>(reader: R) -> io::Result<Database> {
    unimplemented!()
}
pub fn to_writer<W: io::Write>(writer: W, database: &Database) -> io::Result<()> {
    unimplemented!()
}

/*
#[derive(Deserialize)]
struct NotValidatedCorpus {
    objects: SlotVec<ObjectData>,
    nouns: SlotVec<NounData>,
    verbs: SlotVec<VerbData>,
    sentences: SlotVec<SentenceData>,
}

/// Deserialize : backlinks must be restored and indexes validated.
impl<'d> Deserialize<'d> for Corpus {
    fn deserialize<D: Deserializer<'d>>(deserializer: D) -> Result<Self, D::Error> {
        let deserialized = NotValidatedCorpus::deserialize(deserializer)?;

        // Extract data in a new corpus
        let mut corpus = Corpus::new();
        corpus.objects = deserialized.objects;
        corpus.nouns = deserialized.nouns;
        corpus.verbs = deserialized.verbs;
        corpus.sentences = deserialized.sentences;

        // Restore back links and validate
        use serde::de::Error as DE;
        for index in 0..corpus.sentences.inner.len() {
            if corpus.sentences.get(index).is_ok() {
                // Clone Sentence struct to avoid clash with mutable refs for Corpus changes.
                let sentence = corpus.sentences[index].sentence.clone();
                let s_index = SentenceIndex(index);
                // Restore links, validate indexes at the same time.
                match sentence.subject {
                    Subject::Object(o) => corpus
                        .objects
                        .get_mut(o.0)
                        .map_err(DE::custom)?
                        .subject_of
                        .insert(s_index),
                    Subject::Sentence(s) => corpus
                        .sentences
                        .get_mut(s.0)
                        .map_err(DE::custom)?
                        .subject_of
                        .insert(s_index),
                }
                corpus
                    .verbs
                    .get_mut(sentence.verb.0)
                    .map_err(DE::custom)?
                    .verb_of
                    .insert(s_index);
                match sentence.complement {
                    Complement::None => (),
                    Complement::Noun(n) => corpus
                        .nouns
                        .get_mut(n.0)
                        .map_err(DE::custom)?
                        .complement_of
                        .insert(s_index),
                    Complement::Object(o) => corpus
                        .objects
                        .get_mut(o.0)
                        .map_err(DE::custom)?
                        .complement_of
                        .insert(s_index),
                }
            }
        }

        Ok(corpus)
    }
}
*/

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

    /*
    #[test]
    fn io() {
        let mut corpus = Corpus::new();
        let name = Noun("Name".to_string());
        let ni = corpus.insert_noun(name.clone());
        let oi = corpus.create_object();
        let vi = corpus.insert_verb(Verb::IsNamed);
        let sentence = Sentence {
            subject: Subject::Object(oi),
            verb: vi,
            complement: Complement::Noun(ni),
        };
        let _si = corpus.insert_sentence(sentence.clone()).unwrap();
    
        let serialized = serde_json::to_string(&corpus).expect("Serialization failure");
        let deserialized: Corpus =
            serde_json::from_str(&serialized).expect("Deserialization failure");
    
        for i in 0..corpus.objects.inner.len() {
            assert_eq!(
                corpus.objects.get(i).map(|o| &o.description),
                deserialized.objects.get(i).map(|o| &o.description)
            );
            assert_eq!(
                corpus.objects.get(i).map(|o| &o.subject_of),
                deserialized.objects.get(i).map(|o| &o.subject_of)
            );
            assert_eq!(
                corpus.objects.get(i).map(|o| &o.complement_of),
                deserialized.objects.get(i).map(|o| &o.complement_of)
            );
        }
        for i in 0..corpus.nouns.inner.len() {
            assert_eq!(
                corpus.nouns.get(i).map(|n| &n.noun),
                deserialized.nouns.get(i).map(|n| &n.noun)
            );
            assert_eq!(
                corpus.nouns.get(i).map(|n| &n.complement_of),
                deserialized.nouns.get(i).map(|n| &n.complement_of)
            );
        }
        for i in 0..corpus.verbs.inner.len() {
            assert_eq!(
                corpus.verbs.get(i).map(|v| &v.verb),
                deserialized.verbs.get(i).map(|v| &v.verb)
            );
            assert_eq!(
                corpus.verbs.get(i).map(|v| &v.verb_of),
                deserialized.verbs.get(i).map(|v| &v.verb_of)
            );
        }
        for i in 0..corpus.sentences.inner.len() {
            assert_eq!(
                corpus.sentences.get(i).map(|s| &s.sentence),
                deserialized.sentences.get(i).map(|s| &s.sentence)
            );
            assert_eq!(
                corpus.sentences.get(i).map(|s| &s.subject_of),
                deserialized.sentences.get(i).map(|s| &s.subject_of)
            );
        }
    }*/
}
