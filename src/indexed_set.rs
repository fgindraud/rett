use std::hash::Hash;
use std::collections::HashMap;

/*******************************************************************************
 * A vector of immutable indexed cells.
 * Cells can be created and destroyed.
 * After creation, cells are immutable.
 * TODO iterable: filter_map (enumerate ())
 */

pub trait IndexedSetCapableType: PartialEq + Eq + Hash {}
impl<T: PartialEq + Eq + Hash> IndexedSetCapableType for T {}

pub enum Cell<T> {
    Used(T),
    Unused, // TODO Pointer to next unused cell
}
pub struct IndexedSet<T>
where
    T: IndexedSetCapableType,
{
    cells: Vec<Cell<T>>,
    indexes: HashMap<T, usize>,
    nb_elements: usize,
}

impl<T> Cell<T> {
    fn value(&self) -> Option<&T> {
        match self {
            &Cell::Used(ref v) => Some(v),
            _ => None,
        }
    }
}

impl<T> IndexedSet<T>
where
    T: IndexedSetCapableType,
{
    pub fn new() -> IndexedSet<T> {
        IndexedSet {
            cells: Vec::new(),
            indexes: HashMap::new(),
            nb_elements: 0,
        }
    }
    pub fn insert(&mut self, value: T) -> usize {
        let new_index = self.cells.len();
        self.cells.push(Cell::Used(value)); // TODO reuse existing if possible
        self.nb_elements += 1;
        new_index
    }
    pub fn remove(&mut self, index: usize) {
        self.cells[index] = Cell::Unused; // TODO add to free list
        self.nb_elements -= 1
    }
    pub fn len(&self) -> usize {
        self.nb_elements
    }
    pub fn capacity(&self) -> usize {
        self.cells.len()
    }
    pub fn cell(&self, index: usize) -> Option<&T> {
        return self.cells[index].value();
    }
    // Iterator over all cells, returns elem_ref: Option<&T>
    pub fn cell_iter<'a>(
        &'a self,
    ) -> ::std::iter::Map<::std::slice::Iter<'a, Cell<T>>, fn(&Cell<T>) -> Option<&T>> {
        self.cells.iter().map(Cell::value as _)
    }
}

impl<T> ::std::ops::Index<usize> for IndexedSet<T>
where
    T: IndexedSetCapableType,
{
    // Index panics on empty cells
    type Output = T;
    fn index(&self, index: usize) -> &T {
        self.cell(index).unwrap()
    }
}

impl<'a, T> ::std::iter::IntoIterator for &'a IndexedSet<T>
where
    T: IndexedSetCapableType,
{
    // Iterator over non empty cells, returns (index: usize, elem_ref: &T)
    type Item = (usize, &'a T);
    type IntoIter = ::std::iter::FilterMap<
        ::std::iter::Enumerate<::std::slice::Iter<'a, Cell<T>>>,
        fn((usize, &Cell<T>)) -> Option<(usize, &T)>,
    >;
    fn into_iter(self) -> Self::IntoIter {
        self.cells
            .iter()
            .enumerate()
            .filter_map(|(i, ref v)| match v.value() {
                Some(ref t) => Some((i, t)),
                None => None,
            })
    }
}

/* Serialize / Deserialize.
 * The vector is stored as an array of Option<T>.
 * Unused cells are kept, to avoid complex id conversion if ids are used by user code.
 */
impl<T> ::serde::Serialize for IndexedSet<T>
where
    T: ::serde::Serialize + IndexedSetCapableType,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ::serde::Serializer,
    {
        use serde::ser::SerializeSeq;
        let mut seq = serializer.serialize_seq(Some(self.capacity()))?;
        for elem_ref in self.cell_iter() {
            seq.serialize_element(&elem_ref)?;
        }
        seq.end()
    }
}

impl<'de, T> ::serde::Deserialize<'de> for IndexedSet<T>
where
    T: ::serde::Deserialize<'de> + IndexedSetCapableType,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: ::serde::Deserializer<'de>,
    {
        // Define a type tag "Visitor"
        use std::marker::PhantomData;
        struct SeqVisitor<T> {
            marker: PhantomData<T>, // Rust complains if T not used in Visitor.
        }

        // Operations for this type tag
        use std::fmt;
        impl<'de, T> ::serde::de::Visitor<'de> for SeqVisitor<T>
        where
            T: ::serde::Deserialize<'de> + IndexedSetCapableType,
        {
            type Value = IndexedSet<T>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("IndexedSet<T>")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: ::serde::de::SeqAccess<'de>,
            {
                let mut vector = IndexedSet::new();

                while let Some(cell) = try!(seq.next_element()) {
                    match cell {
                        // Add values manually to prevent reuse of cells or such
                        // TODO non pub "raw push" method ?
                        Some(value) => {
                            vector.cells.push(Cell::Used(value));
                            vector.nb_elements += 1
                        }
                        None => vector.cells.push(Cell::Unused),
                    }
                }

                Ok(vector)
            }
        }

        // Deserializer for IndexedSet<T>: use ops from type tag
        let visitor = SeqVisitor {
            marker: PhantomData,
        };
        deserializer.deserialize_seq(visitor)
    }
    // There is a deserialize_in_place stuff for Vec<T>, not implemented
}
