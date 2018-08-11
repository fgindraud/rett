use std;
use std::collections::HashMap;
use std::fmt;

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

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ObjectIndex(usize);
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct LinkIndex(usize);
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct TagIndex(usize);
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct AtomIndex(usize);

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

// TODO
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

struct Graph {
    objects: SlotVec<ObjectData>,
    links: SlotVec<LinkData>,
    tags: SlotVec<TagData>,
    atoms: SlotVec<AtomData>,
    link_indexes: HashMap<Link, LinkIndex>,
}

trait IndexForElement<E> {
    type Index;
    fn get(&self, e: &E) -> Option<Self::Index>;
    fn insert(&mut self, e: E) -> Self::Index;
}
impl IndexForElement<Link> for Graph {
    type Index = LinkIndex;
    fn get(&self, l: &Link) -> Option<Self::Index> {
        self.link_indexes.get(l).cloned()
    }
    fn insert(&mut self, l: Link) -> Self::Index {
        // TODO check link indexes, should return error...
        match self.get(&l) {
            Some(index) => index,
            None => {
                let d = LinkData { link: l.clone() };
                let new_index = LinkIndex(self.links.insert(d));
                self.objects[l.from.0].out_links.insert(new_index);
                self.objects[l.to.0].in_links.insert(new_index);
                self.link_indexes.insert(l, new_index);
                new_index
            }
        }
    }
}
// TODO for atoms, tag

/// Access elements by index, for multiple index types.
trait ElementForIndex<I> {
    type Ref;
    fn get(&self, i: I) -> Result<Self::Ref, Error>;
    fn element(&self, i: I) -> Self::Ref {
        self.get(i).unwrap()
    }
    fn valid(&self, i: I) -> bool {
        self.get(i).is_ok()
    }
}
impl<'a> ElementForIndex<ObjectIndex> for &'a Graph {
    type Ref = ElementRef<'a, ObjectIndex, ObjectData>;
    fn get(&self, i: ObjectIndex) -> Result<Self::Ref, Error> {
        self.objects.get(i.0).map(|e| ElementRef {
            index: i,
            data: e,
            graph: self,
        })
    }
}
impl<'a> ElementForIndex<LinkIndex> for &'a Graph {
    type Ref = ElementRef<'a, LinkIndex, LinkData>;
    fn get(&self, i: LinkIndex) -> Result<Self::Ref, Error> {
        self.links.get(i.0).map(|e| ElementRef {
            index: i,
            data: e,
            graph: self,
        })
    }
}
impl<'a> ElementForIndex<TagIndex> for &'a Graph {
    type Ref = ElementRef<'a, TagIndex, TagData>;
    fn get(&self, i: TagIndex) -> Result<Self::Ref, Error> {
        self.tags.get(i.0).map(|e| ElementRef {
            index: i,
            data: e,
            graph: self,
        })
    }
}
impl<'a> ElementForIndex<AtomIndex> for &'a Graph {
    type Ref = ElementRef<'a, AtomIndex, AtomData>;
    fn get(&self, i: AtomIndex) -> Result<Self::Ref, Error> {
        self.atoms.get(i.0).map(|e| ElementRef {
            index: i,
            data: e,
            graph: self,
        })
    }
}

// Fat references for easy graph traversal
struct ElementRef<'a, I, D: 'a> {
    index: I,
    data: &'a D,
    graph: &'a Graph,
}
impl<'a, I: Clone, D: 'a> ElementRef<'a, I, D> {
    fn index(&self) -> I {
        self.index.clone()
    }
    fn graph(&self) -> &'a Graph {
        self.graph
    }
}
// TODO impls for specific types
//

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
