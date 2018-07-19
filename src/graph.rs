use std::collections::HashMap;

// TODO serde IO
// TODO remove / update elements semantics
// TODO better object access ?
// TODO pattern matching as needed for the wiki output

#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
pub enum Atom {
    String(String),
    Integer(i32),
}
impl Atom {
    pub fn text<T: Into<String>>(text: T) -> Self {
        Atom::String(text.into())
    }
}

/// Index for graph elements.
pub type Index = usize;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
pub struct Link {
    pub from: Index,
    pub to: Index,
}
impl Link {
    pub fn new(from: Index, to: Index) -> Self {
        Link { from: from, to: to }
    }
}
impl From<(Index, Index)> for Link {
    fn from(pair: (Index, Index)) -> Link {
        Link::new(pair.0, pair.1)
    }
}

/** Object of the graph:
 * All objects are identified by their index in the graph (which is constant after creation).
 * All objects can be pointed to/from by a link.
 *
 * Atom: a basic piece of concrete data.
 * Must be hashmap compatible (comparable).
 * In a graph, atoms are unique, and can be searched from their value.
 *
 * Link: a directed arrow between two graph objects.
 * Links are also unique and can be searched from their value.
 * Links can link any two elements of the graph (no restriction).
 * It is up to the user to give semantics to a link.
 * A common pattern is to "annotate a link with an atom" with an atom representing a relation type.
 * It consists of creating another link from the atom to the annotated link.
 *
 * Abstract: abstract graph object with no data.
 * Abstract objects are exclusively defined by their links (relations with other objects).
 * They are not comparable, and must be searched by pattern matching of their relation.
 */
#[derive(Debug, Serialize, Deserialize)]
pub enum Object {
    Atom(Atom),
    Link(Link),
    Abstract,
}

/// Data for each object.
/// in_links/out_links: indexes of links pointing to/from this link.
struct ObjectData {
    object: Object,
    in_links: Vec<Index>,
    out_links: Vec<Index>,
}

pub struct Graph {
    objects: Vec<Option<ObjectData>>,
    atom_indexes: HashMap<Atom, Index>,
    link_indexes: HashMap<Link, Index>,
}

/// Reference an object and its data.
pub struct ObjectRef<'a> {
    index: Index,
    object_data: &'a ObjectData,
}

/// Iterate on objects in order of increasing indexes.
pub struct OrderedObjectIterator<'a> {
    next_index: usize,
    graph: &'a Graph,
}

impl Object {
    pub fn is_link(&self) -> bool {
        match self {
            &Object::Link(_) => true,
            _ => false,
        }
    }
    pub fn as_link(&self) -> &Link {
        match self {
            &Object::Link(ref l) => l,
            _ => panic!("not a link"),
        }
    }
}

impl ObjectData {
    fn new(object: Object) -> Self {
        ObjectData {
            object: object,
            in_links: Vec::new(),
            out_links: Vec::new(),
        }
    }
}

impl Graph {
    /// Create a new empty graph.
    pub fn new() -> Self {
        Graph {
            objects: Vec::new(),
            atom_indexes: HashMap::new(),
            link_indexes: HashMap::new(),
        }
    }

    /// Get object, index may be invalid
    pub fn get_object<'a>(&'a self, index: Index) -> Option<ObjectRef<'a>> {
        match self.objects.get(index) {
            Some(&Some(ref object_data)) => Some(ObjectRef {
                index: index,
                object_data: object_data,
            }),
            _ => None,
        }
    }
    /// Get object, assume valid index
    pub fn object<'a>(&'a self, index: Index) -> ObjectRef<'a> {
        self.get_object(index).expect("invalid index")
    }

    /// Iterate on valid objects
    pub fn objects<'a>(&'a self) -> OrderedObjectIterator<'a> {
        OrderedObjectIterator {
            next_index: 0,
            graph: self,
        }
    }

    /// Get index of an atom, or None if not found.
    pub fn get_atom(&self, atom: &Atom) -> Option<Index> {
        self.atom_indexes.get(&atom).cloned()
    }
    /// Get index of a link, or None if not found.
    pub fn get_link(&self, link: &Link) -> Option<Index> {
        self.link_indexes.get(&link).cloned()
    }

    /// Get the index of an atom, inserting it if not found.
    pub fn use_atom(&mut self, atom: Atom) -> Index {
        match self.get_atom(&atom) {
            Some(index) => index,
            None => {
                let new_index = self.insert_object(Object::Atom(atom.clone()));
                self.atom_indexes.insert(atom, new_index);
                new_index
            }
        }
    }
    /// Get the index of an atom, inserting it if not found.
    pub fn use_link(&mut self, link: Link) -> Index {
        match self.get_link(&link) {
            Some(index) => index,
            None => {
                let new_index = self.insert_object(Object::Link(link.clone()));
                self.object_mut(link.from).out_links.push(new_index);
                self.object_mut(link.to).in_links.push(new_index);
                self.link_indexes.insert(link, new_index);
                new_index
            }
        }
    }
    /// Create a new abstract object, return its index.
    pub fn create_abstract(&mut self) -> Index {
        self.insert_object(Object::Abstract)
    }

    fn insert_object(&mut self, object: Object) -> Index {
        // Find unused index
        for index in 0..self.objects.len() {
            let mut cell = &mut self.objects[index];
            if cell.is_none() {
                *cell = Some(ObjectData::new(object));
                return index;
            }
        }
        // Or allocate new one
        let index = self.objects.len();
        self.objects.push(Some(ObjectData::new(object)));
        index
    }
    fn object_mut(&mut self, index: Index) -> &mut ObjectData {
        self.objects[index].as_mut().expect("Invalid index")
    }
}

impl<'a> ObjectRef<'a> {
    pub fn index(&self) -> Index {
        self.index
    }
    pub fn object(&self) -> &'a Object {
        &self.object_data.object
    }
    pub fn in_links(&self) -> &'a Vec<Index> {
        &self.object_data.in_links
    }
    pub fn out_links(&self) -> &'a Vec<Index> {
        &self.object_data.out_links
    }
}

impl<'a> Iterator for OrderedObjectIterator<'a> {
    type Item = ObjectRef<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let current_index = self.next_index;
            if current_index >= self.graph.objects.len() {
                return None;
            };
            self.next_index = current_index + 1;
            if let Some(object_ref) = self.graph.get_object(current_index) {
                return Some(object_ref);
            }
        }
    }
}
