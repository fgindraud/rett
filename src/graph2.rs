use std;
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

#[derive(Clone, Copy)]
pub struct ObjectIndex(usize);
#[derive(Clone, Copy)]
pub struct LinkIndex(usize);
#[derive(Clone, Copy)]
pub struct TagIndex(usize);
#[derive(Clone, Copy)]
pub struct AtomIndex(usize);

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
    objects: Vec<Option<ObjectData>>,
    links: Vec<Option<LinkData>>,
    tags: Vec<Option<TagData>>,
    atoms: Vec<Option<AtomData>>,
}

trait Indexable<I> {
    type Index;
    fn get(&self, i: &I) -> Option<Self::Index>;
    fn insert(&self, i: I) -> Self::Index;
}
// TODO for atoms, tag, links

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

trait AccessElement<I> {
    type Ref;
    fn get(&self, i: I) -> Result<Self::Ref, Error>;
    fn element(&self, i: I) -> Self::Ref {
        self.get(i).unwrap()
    }
    fn valid(&self, i: I) -> bool {
        self.get(i).is_ok()
    }
}
impl<'a> AccessElement<ObjectIndex> for &'a Graph {
    type Ref = ElementRef<'a, ObjectIndex, ObjectData>;
    fn get(&self, i: ObjectIndex) -> Result<Self::Ref, Error> {
        match self.objects.get(i.0) {
            Some(&Some(ref e)) => Ok(ElementRef {
                index: i,
                data: e,
                graph: self,
            }),
            _ => Err(Error::InvalidIndex),
        }
    }
}
// TODO for others

fn option_vec_insert<T>(vec: &mut Vec<Option<T>>, t: T) -> usize {
    // Find unused index
    for index in 0..vec.len() {
        let mut cell = &mut vec[index];
        if cell.is_none() {
            *cell = Some(t);
            return index;
        }
    }
    // Or allocate new one
    let index = vec.len();
    vec.push(Some(t));
    index
}
