use std;
use std::collections::HashMap;
use std::fmt;

/// Error type for graph operations
#[derive(Debug)]
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
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ObjectIndex(usize);
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct NounIndex(usize);
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct VerbIndex(usize);
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct SentenceIndex(usize);

/// A single piece of text data, usable as a complement in a sentence.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Noun(String);

/// A verb used to link elements in a sentence. Can be "is" + adjective.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Verb(String);

/// A sentence describes either an object, or another sentence (tagging).
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum Subject {
    Object(ObjectIndex),
    Sentence(SentenceIndex),
}
/// Additional description element.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum Complement {
    None,
    Noun(NounIndex),
    Object(ObjectIndex),
}
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Sentence {
    subject: Subject,
    verb: VerbIndex,
    complement: Complement,
}

// Store elements and back links
struct ObjectData {
    description: String,
    subject_of: Set<SentenceIndex>,
    complement_of: Set<SentenceIndex>,
}
struct NounData {
    noun: Noun,
    complement_of: Set<SentenceIndex>,
}
struct VerbData {
    verb: Verb,
    verb_of: Set<SentenceIndex>,
}
struct SentenceData {
    sentence: Sentence,
    subject_of: Set<SentenceIndex>,
}

/// Relation graph as a corpus of sentences.
struct Corpus {
    objects: SlotVec<ObjectData>,
    nouns: SlotVec<NounData>,
    verbs: SlotVec<VerbData>,
    sentences: SlotVec<SentenceData>,
    noun_indexes: HashMap<Noun, NounIndex>,
    verb_indexes: HashMap<Verb, VerbIndex>,
    sentence_indexes: HashMap<Sentence, SentenceIndex>,
}

/// Access elements by index, for multiple index types.
trait ElementForIndex<I> {
    type Data;
    fn get(&self, i: I) -> Result<&Self::Data, Error>;
    fn valid(&self, i: I) -> bool {
        self.get(i).is_ok()
    }
}
impl<I> std::ops::Index<I> for Corpus
where
    Corpus: ElementForIndex<I>,
{
    type Output = <Self as ElementForIndex<I>>::Data;
    fn index(&self, i: I) -> &Self::Output {
        self.get(i).unwrap()
    }
}
impl ElementForIndex<ObjectIndex> for Corpus {
    type Data = ObjectData;
    fn get(&self, i: ObjectIndex) -> Result<&Self::Data, Error> {
        self.objects.get(i.0)
    }
}
impl ElementForIndex<NounIndex> for Corpus {
    type Data = NounData;
    fn get(&self, i: NounIndex) -> Result<&Self::Data, Error> {
        self.nouns.get(i.0)
    }
}
impl ElementForIndex<VerbIndex> for Corpus {
    type Data = VerbData;
    fn get(&self, i: VerbIndex) -> Result<&Self::Data, Error> {
        self.verbs.get(i.0)
    }
}
impl ElementForIndex<SentenceIndex> for Corpus {
    type Data = SentenceData;
    fn get(&self, i: SentenceIndex) -> Result<&Self::Data, Error> {
        self.sentences.get(i.0)
    }
}

/// Get index for an element (if indexed)
trait IndexForElement<E> {
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

/// Insert an element if not already present.
impl Corpus {
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
                };
                self.verbs[s.verb.0].verb_of.insert(new_index);
                match s.complement {
                    Complement::None => (),
                    Complement::Noun(n) => self.nouns[n.0].complement_of.insert(new_index),
                    Complement::Object(o) => self.objects[o.0].complement_of.insert(new_index),
                };
                new_index
            }
        })
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

/// Vector with sorted elements and set api.
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
