use std;
use std::collections::HashMap;
use std::fmt;

/// Error type for graph operations
#[derive(Debug)]
pub enum Error {
    InvalidIndex,
    CannotRemoveLinked,
}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::InvalidIndex => "invalid index".fmt(f),
            Error::CannotRemoveLinked => "cannot remove a referenced object".fmt(f),
        }
    }
}
impl std::error::Error for Error {}

// Index types
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ObjectIndex(usize);
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct LinkIndex(usize);
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct TagIndex(usize);
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct AtomIndex(usize);

// Element types
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Link {
    from: ObjectIndex,
    to: ObjectIndex,
}
struct Tag {
    tag: AtomIndex,
}
enum Atom {
    Text(String),
}

// Element data types: store element and back links
struct ObjectData {
    description: String,
    in_links: Set<LinkIndex>,
    out_links: Set<LinkIndex>,
}
struct LinkData {
    link: Link,
}
struct TagData {
    tag: Tag,
}
struct AtomData {
    atom: Atom,
}

/// Relation graph
struct Graph {
    objects: SlotVec<ObjectData>,
    links: SlotVec<LinkData>,
    tags: SlotVec<TagData>,
    atoms: SlotVec<AtomData>,
    link_indexes: HashMap<Link, LinkIndex>,
}

/// Access elements by index, for multiple index types.
trait ElementForIndex<I> {
    type Data;
    fn get(&self, i: I) -> Result<&Self::Data, Error>;
    fn valid(&self, i: I) -> bool {
        self.get(i).is_ok()
    }
}
impl<I> std::ops::Index<I> for Graph
where
    Graph: ElementForIndex<I>,
{
    type Output = <Self as ElementForIndex<I>>::Data;
    fn index(&self, i: I) -> &Self::Output {
        self.get(i).unwrap()
    }
}
impl ElementForIndex<ObjectIndex> for Graph {
    type Data = ObjectData;
    fn get(&self, i: ObjectIndex) -> Result<&Self::Data, Error> {
        self.objects.get(i.0)
    }
}
impl ElementForIndex<LinkIndex> for Graph {
    type Data = LinkData;
    fn get(&self, i: LinkIndex) -> Result<&Self::Data, Error> {
        self.links.get(i.0)
    }
}
impl ElementForIndex<TagIndex> for Graph {
    type Data = TagData;
    fn get(&self, i: TagIndex) -> Result<&Self::Data, Error> {
        self.tags.get(i.0)
    }
}
impl ElementForIndex<AtomIndex> for Graph {
    type Data = AtomData;
    fn get(&self, i: AtomIndex) -> Result<&Self::Data, Error> {
        self.atoms.get(i.0)
    }
}

/// Get index for an element (if indexed)
trait IndexForElement<E> {
    type Index;
    fn index_of(&self, e: &E) -> Option<Self::Index>;
}
impl IndexForElement<Link> for Graph {
    type Index = LinkIndex;
    fn index_of(&self, l: &Link) -> Option<Self::Index> {
        self.link_indexes.get(l).cloned()
    }
}

/// Insert an element if not already present.
impl Graph {
    pub fn create_object(&mut self) -> ObjectIndex {
        let d = ObjectData {
            description: String::new(),
            in_links: Set::new(),
            out_links: Set::new(),
        };
        ObjectIndex(self.objects.insert(d))
    }
    pub fn insert_link(&mut self, l: Link) -> Result<LinkIndex, Error> {
        if self.valid(l.from) && self.valid(l.to) {
            Ok(match self.index_of(&l) {
                Some(index) => index,
                None => {
                    let d = LinkData { link: l.clone() };
                    let new_index = LinkIndex(self.links.insert(d));
                    self.objects[l.from.0].out_links.insert(new_index);
                    self.objects[l.to.0].in_links.insert(new_index);
                    self.link_indexes.insert(l, new_index);
                    new_index
                }
            })
        } else {
            Err(Error::InvalidIndex)
        }
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
