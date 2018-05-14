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

// Opaque index for IndexedSet.
#[derive(PartialEq, Eq, Clone, Copy, Hash, Serialize, Deserialize, Debug)]
pub struct Index(usize);

/* Contains a Vec<T> for direct indexing.
 * Also contains a Map<T, index>.
 */
pub struct IndexedSet<T> {
    cells: Vec<Cell<T>>,
    indexes: HashMap<T, Index>,
    nb_elements: usize,
}

// Each cell of the vector can be in use or unused.
pub enum Cell<T> {
    Used(T),
    Unused, // TODO Pointer to next unused cell
}

impl Index {
    pub fn as_usize(&self) -> usize {
        self.0
    }
}

impl<T> Cell<T> {
    // Convert to Option<&T>, abstracting away the free list system.
    fn value(&self) -> Option<&T> {
        match self {
            &Cell::Used(ref v) => Some(v),
            &Cell::Unused => None,
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

    // Access cell through index (if the index is in use)
    pub fn cell(&self, index: Index) -> Option<&T> {
        self.cells[index.as_usize()].value()
    }

    // Get index of a value (if in the set)
    pub fn index_of(&self, object: &T) -> Option<Index> {
        match self.indexes.get(object) {
            Some(i) => Some(*i),
            None => None,
        }
    }

    // Add a new element in the set.
    // Returns its index.
    // If the element already exists, return its current index instead.
    pub fn insert(&mut self, value: T) -> Index {
        match self.index_of(&value) {
            Some(id) => id,
            None => self.push_new_entry(value),
        }
    }

    // Push an entry without checking if it is already defined; return new index.
    fn push_new_entry(&mut self, value: T) -> Index {
        let new_index = Index(self.cells.len());
        self.cells.push(Cell::Used(value.clone())); // TODO reuse existing cell if possible
        let previous_value = self.indexes.insert(value, new_index);
        debug_assert_eq!(previous_value, None);
        self.nb_elements += 1;
        new_index
    }
    // Push a single unused cell at the end
    fn push_unused(&mut self) {
        self.cells.push(Cell::Unused)
    }

    // Remove object from the set through its id.
    // Does nothing if the index does not exist.
    pub fn remove_id(&mut self, index: Index) {
        let cell = &mut self.cells[index.as_usize()];
        if let &mut Cell::Used(_) = cell {
            self.nb_elements -= 1;
            let previous_value = self.indexes.remove(cell.value().unwrap());
            debug_assert_eq!(previous_value, Some(index));
            *cell = Cell::Unused // TODO add to free list
        }
    }
}

// Indexing operator: panics if the cell is unused
impl<T> ::std::ops::Index<Index> for IndexedSet<T>
where
    T: IndexedSetCapableType,
{
    type Output = T;
    fn index(&self, index: Index) -> &T {
        self.cell(index).unwrap()
    }
}

// Iterator over all elements, returns (index, elem_ref)
impl<'a, T> ::std::iter::IntoIterator for &'a IndexedSet<T>
where
    T: IndexedSetCapableType,
{
    type Item = (Index, &'a T);
    type IntoIter = ::std::iter::FilterMap<
        ::std::iter::Enumerate<::std::slice::Iter<'a, Cell<T>>>,
        fn((usize, &'a Cell<T>)) -> Option<Self::Item>,
    >;
    fn into_iter(self) -> Self::IntoIter {
        self.cells
            .iter()
            .enumerate()
            .filter_map(|(raw_index, ref cell)| match cell.value() {
                Some(ref v) => Some((Index(raw_index), v)),
                None => None,
            })
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
        let mut seq = serializer.serialize_seq(Some(self.cells.len()))?;
        for cell in &self.cells {
            seq.serialize_element(&cell.value())?;
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

        let id_42 = is.insert(42);
        assert_eq!(is.index_of(&42), Some(id_42));
        assert_eq!(is.cell(id_42), Some(&42));
        assert_eq!(is[id_42], 42);
        assert_eq!(is.len(), 1);

        let id_42_b = is.insert(42);
        let id_12 = is.insert(12);
        assert_eq!(id_42, id_42_b);
        assert_ne!(id_42, id_12);
        assert_eq!(is.len(), 2);

        is.remove_id(id_42);
        assert_eq!(is.len(), 1);
        assert_eq!(is.cell(id_42), None);
    }
}
