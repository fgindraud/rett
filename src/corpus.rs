use serde::{Deserialize, Deserializer};
use std;
use std::collections::HashMap;
use std::fmt;

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

// Index types
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct ObjectIndex(usize);
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct NounIndex(usize);
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct VerbIndex(usize);
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct SentenceIndex(usize);

/// A single piece of text data, usable as a complement in a sentence.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Noun(String);

/// A verb used to link elements in a sentence. Can be "is" + adjective.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Verb {
    IsNamed,
    Text(String),
}

/// A sentence describes either an object, or another sentence (tagging).
#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Subject {
    Object(ObjectIndex),
    Sentence(SentenceIndex),
}
/// Additional description element.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum Complement {
    None,
    Noun(NounIndex),
    Object(ObjectIndex),
}
#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct Sentence {
    subject: Subject,
    verb: VerbIndex,
    complement: Complement,
}

// Store elements and back links
#[derive(Serialize, Deserialize)]
pub struct ObjectData {
    description: String,
    #[serde(skip)]
    subject_of: Set<SentenceIndex>,
    #[serde(skip)]
    complement_of: Set<SentenceIndex>,
}
#[derive(Serialize, Deserialize)]
pub struct NounData {
    noun: Noun,
    #[serde(skip)]
    complement_of: Set<SentenceIndex>,
}
#[derive(Serialize, Deserialize)]
pub struct VerbData {
    verb: Verb,
    #[serde(skip)]
    verb_of: Set<SentenceIndex>,
}
#[derive(Serialize, Deserialize)]
pub struct SentenceData {
    sentence: Sentence,
    #[serde(skip)]
    subject_of: Set<SentenceIndex>,
}

/// Relation graph as a corpus of sentences.
#[derive(Default, Serialize)]
pub struct Corpus {
    objects: SlotVec<ObjectData>,
    nouns: SlotVec<NounData>,
    verbs: SlotVec<VerbData>,
    sentences: SlotVec<SentenceData>,
    #[serde(skip)]
    noun_indexes: HashMap<Noun, NounIndex>,
    #[serde(skip)]
    verb_indexes: HashMap<Verb, VerbIndex>,
    #[serde(skip)]
    sentence_indexes: HashMap<Sentence, SentenceIndex>,
}

/// Basic methods for an index type of the corpus.
pub trait CorpusElementIndex {
    type ElementData;
    fn new(raw_index: usize) -> Self;
    fn to_raw_index(&self) -> usize;
    // Used to enable ElementForIndex<I> automatic implementation
    fn get_element<'a>(&self, corpus: &'a Corpus) -> Result<&'a Self::ElementData, Error>;
}
impl CorpusElementIndex for ObjectIndex {
    type ElementData = ObjectData;
    fn new(i: usize) -> Self {
        ObjectIndex(i)
    }
    fn to_raw_index(&self) -> usize {
        self.0
    }
    fn get_element<'a>(&self, corpus: &'a Corpus) -> Result<&'a Self::ElementData, Error> {
        corpus.objects.get(self.to_raw_index())
    }
}
impl CorpusElementIndex for NounIndex {
    type ElementData = NounData;
    fn new(i: usize) -> Self {
        NounIndex(i)
    }
    fn to_raw_index(&self) -> usize {
        self.0
    }
    fn get_element<'a>(&self, corpus: &'a Corpus) -> Result<&'a Self::ElementData, Error> {
        corpus.nouns.get(self.to_raw_index())
    }
}
impl CorpusElementIndex for VerbIndex {
    type ElementData = VerbData;
    fn new(i: usize) -> Self {
        VerbIndex(i)
    }
    fn to_raw_index(&self) -> usize {
        self.0
    }
    fn get_element<'a>(&self, corpus: &'a Corpus) -> Result<&'a Self::ElementData, Error> {
        corpus.verbs.get(self.to_raw_index())
    }
}
impl CorpusElementIndex for SentenceIndex {
    type ElementData = SentenceData;
    fn new(i: usize) -> Self {
        SentenceIndex(i)
    }
    fn to_raw_index(&self) -> usize {
        self.0
    }
    fn get_element<'a>(&self, corpus: &'a Corpus) -> Result<&'a Self::ElementData, Error> {
        corpus.sentences.get(self.to_raw_index())
    }
}

/// Access elements by index, for multiple index types.
pub trait ElementForIndex<I: CorpusElementIndex> {
    fn get(&self, i: I) -> Result<&I::ElementData, Error>;
    fn valid(&self, i: I) -> bool {
        self.get(i).is_ok()
    }
    fn get_ref(&self, i: I) -> Result<Ref<I>, Error>;
}
impl<I: CorpusElementIndex + Clone> ElementForIndex<I> for Corpus {
    fn get(&self, i: I) -> Result<&I::ElementData, Error> {
        i.get_element(self)
    }
    fn get_ref(&self, i: I) -> Result<Ref<I>, Error> {
        self.get(i.clone()).map(|e| Ref {
            corpus: self,
            index: i,
            element: e,
        })
    }
}
impl<I: CorpusElementIndex + Clone> std::ops::Index<I> for Corpus {
    type Output = I::ElementData;
    fn index(&self, i: I) -> &Self::Output {
        self.get(i).unwrap()
    }
}
// TODO add api
pub struct Ref<'a, I>
where
    I: CorpusElementIndex,
    I::ElementData: 'a,
{
    pub corpus: &'a Corpus,
    pub index: I,
    pub element: &'a I::ElementData,
}

/// Get index for an element (if indexed)
pub trait IndexForElement<E> {
    type Index;
    fn index_of(&self, e: &E) -> Option<Self::Index>;
}
impl IndexForElement<Noun> for Corpus {
    type Index = NounIndex;
    fn index_of(&self, n: &Noun) -> Option<Self::Index> {
        self.noun_indexes.get(n).cloned()
    }
}
impl IndexForElement<Verb> for Corpus {
    type Index = VerbIndex;
    fn index_of(&self, v: &Verb) -> Option<Self::Index> {
        self.verb_indexes.get(v).cloned()
    }
}
impl IndexForElement<Sentence> for Corpus {
    type Index = SentenceIndex;
    fn index_of(&self, s: &Sentence) -> Option<Self::Index> {
        self.sentence_indexes.get(s).cloned()
    }
}

impl Corpus {
    /// Create an empty Corpus.
    pub fn new() -> Corpus {
        Corpus::default()
    }

    pub fn create_object(&mut self) -> ObjectIndex {
        let d = ObjectData {
            description: String::new(),
            subject_of: Set::new(),
            complement_of: Set::new(),
        };
        ObjectIndex(self.objects.insert(d))
    }
    pub fn insert_noun(&mut self, n: Noun) -> NounIndex {
        match self.index_of(&n) {
            Some(index) => index,
            None => {
                let d = NounData {
                    noun: n.clone(),
                    complement_of: Set::new(),
                };
                let new_index = NounIndex(self.nouns.insert(d));
                self.noun_indexes.insert(n, new_index);
                new_index
            }
        }
    }
    pub fn insert_verb(&mut self, v: Verb) -> VerbIndex {
        match self.index_of(&v) {
            Some(index) => index,
            None => {
                let d = VerbData {
                    verb: v.clone(),
                    verb_of: Set::new(),
                };
                let new_index = VerbIndex(self.verbs.insert(d));
                self.verb_indexes.insert(v, new_index);
                new_index
            }
        }
    }
    pub fn insert_sentence(&mut self, s: Sentence) -> Result<SentenceIndex, Error> {
        let subject_valid = match s.subject {
            Subject::Object(o) => self.valid(o),
            Subject::Sentence(s) => self.valid(s),
        };
        let complement_valid = match s.complement {
            Complement::None => true,
            Complement::Noun(n) => self.valid(n),
            Complement::Object(o) => self.valid(o),
        };
        if !(subject_valid && self.valid(s.verb) && complement_valid) {
            return Err(Error::InvalidIndex);
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

/******************************************************************************
 * Utils.
 */

/// Vector where elements never change indexes. Removal generate holes.
#[derive(Serialize, Deserialize)]
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
 * IO using serde.
 *
 * Serialized as multiple Vec<Opt<DataStruct>>.
 * Foreach DataStruct, only serialize the inner object (not the backlinks).
 * After a row of serialize + deserialize, indexes are conserved.
 */

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
 * Prelude for easy import.
 */
pub mod prelude {
    pub use super::Corpus;
    pub use super::{NounIndex, ObjectIndex, SentenceIndex, VerbIndex};

    // Traits
    pub use super::{CorpusElementIndex, ElementForIndex, IndexForElement};
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
