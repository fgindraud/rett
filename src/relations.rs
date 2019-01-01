use std;
use std::collections::HashMap;
use std::default::Default;
use std::fmt;
use std::hash::Hash;
use std::io;

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

/// All database entities are references by typed indexes.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Entity {
    Concrete(AtomIndex),
    Abstract(ObjectIndex),
    Relation(RelationIndex),
}
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct AtomIndex(pub usize);
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ObjectIndex(pub usize);
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct RelationIndex(pub usize);

/// Abtract object, not self contained, described by its relations.
struct Object;

/// Atom of data that is known, self contained, indexable.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Atom {
    Text(String),
}

/// Binary relation between two entities, tagged by a third one.
/// If the second entity is omitted, this is a simple description.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Relation {
    subject: Entity,
    descriptor: Entity,
    complement: Option<Entity>,
}

struct EntityData<E> {
    entity: E,
    subject_of: Set<RelationIndex>,
    descriptor_of: Set<RelationIndex>,
    complement_of: Set<RelationIndex>,
}
struct IndexableEntity<EntityType: Eq + Hash, Index> {
    entities: SlotVec<EntityData<EntityType>>,
    indexes: HashMap<EntityType, Index>,
}
impl<E: Eq + Hash, I> Default for IndexableEntity<E, I> {
    // Derived Default trait would require Default impl for E,D,I which are not needed.
    fn default() -> Self {
        IndexableEntity {
            entities: SlotVec::default(),
            indexes: HashMap::default(),
        }
    }
}

#[derive(Default)]
pub struct Database {
    objects: SlotVec<EntityData<Object>>,
    atoms: IndexableEntity<Atom, AtomIndex>,
    relations: IndexableEntity<Relation, RelationIndex>,
}

impl Database {
    pub fn new() -> Database {
        Self::default()
    }

    // Add new entities to the database.
    pub fn create_object(&mut self) -> ObjectIndex {
        ObjectIndex(add_new_entity_to_data_vec(&mut self.objects, Object {}))
    }
    pub fn insert_atom(&mut self, atom: Atom) -> AtomIndex {
        match self.index_of_atom(&atom) {
            Some(index) => index,
            None => {
                let index = add_new_entity_to_data_vec(&mut self.atoms.entities, atom.clone());
                let index = AtomIndex(index);
                self.atoms.indexes.insert(atom, index);
                index
            }
        }
    }
    pub fn insert_relation(&mut self, relation: Relation) -> Result<RelationIndex, Error> {
        let all_indexes_valid = {
            let is_valid = |e: &Entity| match *e {
                Entity::Concrete(a) => self.atom(a).is_ok(),
                Entity::Abstract(o) => self.object(o).is_ok(),
                Entity::Relation(r) => self.relation(r).is_ok(),
            };
            is_valid(&relation.subject)
                && is_valid(&relation.descriptor)
                && relation.complement.as_ref().map_or(true, is_valid)
        };
        if !all_indexes_valid {
            return Err(Error::InvalidIndex);
        }
        Ok(match self.index_of_relation(&relation) {
            Some(index) => index,
            None => {
                let index =
                    add_new_entity_to_data_vec(&mut self.relations.entities, relation.clone());
                let index = RelationIndex(index);
                // FIXME register in subject_of/... fields
                self.relations.indexes.insert(relation, index);
                index
            }
        })
    }

    // Temporary accessors (FIXME use fat ref)
    pub fn object(&self, i: ObjectIndex) -> Result<&Object, Error> {
        self.objects.get(i.0).map(|d| &d.entity)
    }
    pub fn atom(&self, i: AtomIndex) -> Result<&Atom, Error> {
        self.atoms.entities.get(i.0).map(|d| &d.entity)
    }
    pub fn relation(&self, i: RelationIndex) -> Result<&Relation, Error> {
        self.relations.entities.get(i.0).map(|d| &d.entity)
    }

    // Retrieve index of indexable entities.
    pub fn index_of_atom(&self, atom: &Atom) -> Option<AtomIndex> {
        self.atoms.indexes.get(atom).cloned()
    }
    pub fn index_of_relation(&self, relation: &Relation) -> Option<RelationIndex> {
        self.relations.indexes.get(relation).cloned()
    }
}

fn add_new_entity_to_data_vec<E>(v: &mut SlotVec<EntityData<E>>, e: E) -> usize {
    v.insert(EntityData {
        entity: e,
        subject_of: Set::new(),
        descriptor_of: Set::new(),
        complement_of: Set::new(),
    })
}

/*
    fn get_ref(&self, i: I) -> Result<Ref<I>, Error> {
        self.get(i.clone()).map(|e| Ref {
            corpus: self,
            index: i,
            element: e,
        })
    }
pub struct Ref<'a, I>
where
    I: CorpusElementIndex,
    I::ElementData: 'a,
{
    pub corpus: &'a Corpus,
    pub index: I,
    pub element: &'a I::ElementData,
}

        Ok(match self.index_of(&s) {
            Some(index) => index,
            None => {
                let d = SentenceData {
                    sentence: s.clone(),
                    subject_of: Set::new(),
                };
                let new_index = SentenceIndex(self.sentences.insert(d));
                match s.subject {
                    Subject::Object(o) => self.objects[o.0].subject_of.insert(new_index),
                    Subject::Sentence(s) => self.sentences[s.0].subject_of.insert(new_index),
                }
                self.verbs[s.verb.0].verb_of.insert(new_index);
                match s.complement {
                    Complement::None => (),
                    Complement::Noun(n) => self.nouns[n.0].complement_of.insert(new_index),
                    Complement::Object(o) => self.objects[o.0].complement_of.insert(new_index),
                }
                self.sentence_indexes.insert(s, new_index);
                new_index
            }
        })
    }
}
*/

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
impl<T> std::default::Default for SlotVec<T> {
    fn default() -> Self {
        Self::new()
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
impl<T: Ord> std::default::Default for Set<T> {
    fn default() -> Self {
        Self::new()
    }
}

/******************************************************************************
 * Prelude for easy import.
 */
pub mod prelude {
    pub use super::Database;
    pub use super::{AtomIndex, ObjectIndex, RelationIndex};
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


/******************************************************************************
 * Tests.
 */
#[cfg(test)]
mod tests {
use super::super::serde_json;
use super::*;

#[test]
fn basic() {
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
let si = corpus.insert_sentence(sentence.clone()).unwrap();

assert_eq!(Some(ni), corpus.index_of(&name));
assert_eq!(ni, corpus.insert_noun(name));
let name2 = Noun("name".to_string());
assert_eq!(None, corpus.index_of(&name2));
assert_ne!(ni, corpus.insert_noun(name2));

assert_eq!(Some(vi), corpus.index_of(&Verb::IsNamed));
assert_eq!(vi, corpus.insert_verb(Verb::IsNamed));
let verb2 = Verb::Text("is named".to_string());
assert_eq!(None, corpus.index_of(&verb2));
assert_ne!(vi, corpus.insert_verb(verb2));

assert_eq!(Some(si), corpus.index_of(&sentence));
assert_eq!(Ok(si), corpus.insert_sentence(sentence));
let sentence2 = Sentence {
subject: Subject::Object(ObjectIndex(42)), // Bad index
verb: vi,
complement: Complement::Noun(ni),
};
assert_eq!(None, corpus.index_of(&sentence2));
assert_eq!(Err(Error::InvalidIndex), corpus.insert_sentence(sentence2));
}

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
}
}
*/
