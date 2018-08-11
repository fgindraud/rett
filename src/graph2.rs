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
    in_links: SortedVec<LinkIndex>,
    out_links: SortedVec<LinkIndex>,
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
trait ElementForIndex<'g, I> {
    type Ref;
    fn get(&'g self, i: I) -> Result<Self::Ref, Error>;
    fn element(&'g self, i: I) -> Self::Ref {
        self.get(i).unwrap()
    }
    fn valid(&'g self, i: I) -> bool {
        self.get(i).is_ok()
    }
}
impl<'g> ElementForIndex<'g, ObjectIndex> for Graph {
    type Ref = ObjectRef<'g>;
    fn get(&'g self, i: ObjectIndex) -> Result<Self::Ref, Error> {
        self.objects.get(i.0).map(|e| ObjectRef {
            index: i,
            data: e,
            graph: self,
        })
    }
}
impl<'g> ElementForIndex<'g, LinkIndex> for Graph {
    type Ref = LinkRef<'g>;
    fn get(&'g self, i: LinkIndex) -> Result<Self::Ref, Error> {
        self.links.get(i.0).map(|e| LinkRef {
            index: i,
            data: e,
            graph: self,
        })
    }
}
impl<'g> ElementForIndex<'g, TagIndex> for Graph {
    type Ref = TagRef<'g>;
    fn get(&'g self, i: TagIndex) -> Result<Self::Ref, Error> {
        self.tags.get(i.0).map(|e| TagRef {
            index: i,
            data: e,
            graph: self,
        })
    }
}
impl<'g> ElementForIndex<'g, AtomIndex> for Graph {
    type Ref = AtomRef<'g>;
    fn get(&'g self, i: AtomIndex) -> Result<Self::Ref, Error> {
        self.atoms.get(i.0).map(|e| AtomRef {
            index: i,
            data: e,
            graph: self,
        })
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
            in_links: SortedVec::new(),
            out_links: SortedVec::new(),
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

pub struct ObjectRef<'g> {
    index: ObjectIndex,
    data: &'g ObjectData,
    graph: &'g Graph,
}
pub struct LinkRef<'g> {
    index: LinkIndex,
    data: &'g LinkData,
    graph: &'g Graph,
}
pub struct TagRef<'g> {
    index: TagIndex,
    data: &'g TagData,
    graph: &'g Graph,
}
pub struct AtomRef<'g> {
    index: AtomIndex,
    data: &'g AtomData,
    graph: &'g Graph,
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
struct SortedVec<T: Ord> {
    inner: Vec<T>,
}
impl<T: Ord> SortedVec<T> {
    fn new() -> Self {
        SortedVec { inner: Vec::new() }
    }
    fn contains(&self, e: &T) -> bool {
        self.inner.binary_search(e).is_ok()
    }
    fn insert(&mut self, e: T) {
        if let Err(insertion_index) = self.inner.binary_search(&e) {
            self.inner.insert(insertion_index, e)
        }
    }
    fn remove(&mut self, e: &T) {
        if let Ok(index) = self.inner.binary_search(e) {
            self.inner.remove(index);
        }
    }
}
impl<'a, T: Ord> std::ops::Deref for SortedVec<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        self.inner.deref()
    }
}
