use std::hash::Hash;
use std::collections::HashMap;

/*******************************************************************************
 * A vector of immutable indexed cells.
 * Cells can be created and destroyed.
 * Cells are unique.
 * After creation, cells are immutable.
 */

// PartialEq + Eq + Hash: index_of and unicity
// Clone: to store the object in both Vec and HashMap.
pub trait IndexedSetCapableType: PartialEq + Eq + Hash + Clone {}
impl<T: PartialEq + Eq + Hash + Clone> IndexedSetCapableType for T {}

/* Contains a Vec<T> for direct indexing.
 * Also contains a Map<T, index>.
 */
pub struct IndexedSet<T>
where
    T: IndexedSetCapableType,
{
    cells: Vec<Cell<T>>,
    indexes: HashMap<T, usize>,
    nb_elements: usize,
}
// Each cell of the vector can be in use or unused.
pub enum Cell<T> {
    Used(T),
    Unused, // TODO Pointer to next unused cell
}

impl<T> Cell<T> {
    // Simple accessor
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
    // Create a new empty set
    pub fn new() -> IndexedSet<T> {
        IndexedSet {
            cells: Vec::new(),
            indexes: HashMap::new(),
            nb_elements: 0,
        }
    }

    // Number of elements in the set
    pub fn len(&self) -> usize {
        self.nb_elements
    }

    // Number of cells
    pub fn capacity(&self) -> usize {
        self.cells.len()
    }

    // Access cell through index (if the index is in use)
    pub fn cell(&self, index: usize) -> Option<&T> {
        self.cells[index].value()
    }

    // Get index of a value (if in the set)
    pub fn index_of(&self, object: &T) -> Option<usize> {
        match self.indexes.get(object) {
            Some(i) => Some(*i),
            None => None,
        }
    }

    // Add a new element in the set.
    // Returns its index.
    // If the element already exists, return its current index instead.
    pub fn insert(&mut self, value: T) -> usize {
        match self.index_of(&value) {
            Some(id) => id,
            None => self.push_new_entry(value),
        } // TODO use Entry api
    }

    // Push an entry without checking if it is already defined; return new index.
    fn push_new_entry(&mut self, value: T) -> usize {
        let new_index = self.cells.len();
        self.cells.push(Cell::Used(value.clone())); // TODO reuse existing cell if possible
        let previous_value = self.indexes.insert(value, new_index);
        assert_eq!(previous_value, None);
        self.nb_elements += 1;
        new_index
    }
    // Push a single unused cell at the end
    fn push_unused(&mut self) {
        self.cells.push(Cell::Unused)
    }

    // Remove object from the set through its id.
    // Does nothing if the index does not exist.
    pub fn remove_id(&mut self, index: usize) {
        //        let cell = &mut self.cells[index];
        //        if let &Cell::Used(ref v) = cell {
        //            let previous_value = self.indexes.remove(v);
        //            assert_eq!(previous_value, Some(index));
        //            self.nb_elements -= 1
        //        }
        //        *cell = Cell::Unused; // TODO add to free list
    }
}

// Indexing operator: panics if the cell is unused
impl<T> ::std::ops::Index<usize> for IndexedSet<T>
where
    T: IndexedSetCapableType,
{
    type Output = T;
    fn index(&self, index: usize) -> &T {
        self.cell(index).unwrap()
    }
}

// Iterator over all cells, returns elem_ref: Option<&T>
impl<'a, T> ::std::iter::IntoIterator for &'a IndexedSet<T>
where
    T: IndexedSetCapableType,
{
    type Item = Option<&'a T>;
    type IntoIter = ::std::iter::Map<::std::slice::Iter<'a, Cell<T>>, fn(&Cell<T>) -> Option<&T>>;
    fn into_iter(self) -> Self::IntoIter {
        self.cells.iter().map(Cell::value as _)
    }
}

/*******************************************************************************
 * Serialize / Deserialize.
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
        for elem_ref in self {
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
                        Some(value) => {
                            vector.push_new_entry(value);
                        }
                        None => vector.push_unused(),
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

/*******************************************************************************
 * Tests
 */
#[cfg(test)]
mod tests {
    #[test]
    fn basic_api() {
        let mut is = super::IndexedSet::new();
        assert_eq!(is.len(), 0);
        assert_eq!(is.capacity(), 0);

        let id_42 = is.insert(42);
        assert_eq!(is.index_of(&42), Some(id_42));
        assert_eq!(is.cell(id_42), Some(&42));
        assert_eq!(is.len(), 1);

        let id_42_b = is.insert(42);
        let id_12 = is.insert(12);
        assert_eq!(id_42, id_42_b);
        assert_ne!(id_42, id_12);
        assert_eq!(is.len(), 2);
    }
}
