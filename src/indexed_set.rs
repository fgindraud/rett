use std::hash::Hash;
use std::collections::HashMap;
use std::fmt;
use std::iter;
use std::ops;

/*******************************************************************************
 * A set of indexed objects of type T.
 * After insertions, objects are immutable.
 * Objects are unique: cannot be inserted twice.
 */

// PartialEq + Eq + Hash: index_of and unicity
// Clone: to store the object in both Vec and HashMap.
pub trait IndexedSetCapableType: PartialEq + Eq + Hash + Clone {}
impl<T: PartialEq + Eq + Hash + Clone> IndexedSetCapableType for T {}

// Opaque index for IndexedSet.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Serialize, Deserialize, Debug)]
pub struct Index(usize);

/* Contains a Vec<T> for direct indexing.
 * Also contains a Map<T, index>.
 */
pub struct IndexedSet<T> {
    slots: Vec<Slot<T>>,
    indexes: HashMap<T, Index>,
    nb_elements: usize,
}

// Each slot of the vector can be in use or unused.
pub enum Slot<T> {
    Used(T),
    Unused, // TODO Pointer to next unused slot
}

impl Index {
    pub fn as_usize(&self) -> usize {
        self.0
    }
}
impl fmt::Display for Index {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.as_usize().fmt(f)
    }
}

impl<T> Slot<T> {
    // Convert to Option<&T>, abstracting away the free list system.
    fn value(&self) -> Option<&T> {
        match self {
            &Slot::Used(ref v) => Some(v),
            &Slot::Unused => None,
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
            slots: Vec::new(),
            indexes: HashMap::new(),
            nb_elements: 0,
        }
    }

    // Number of elements in the set
    pub fn len(&self) -> usize {
        self.nb_elements
    }

    // For every existing index, index.as_usize() < nb_indexes()
    // Represent the size of the internal array.
    // Indexes may not be valid !
    pub fn nb_indexes(&self) -> usize {
        self.slots.len()
    }

    // Access slot through index (if the index is in use)
    pub fn get(&self, index: Index) -> Option<&T> {
        self.slots[index.as_usize()].value()
    }

    // Get index of a value (if in the set)
    pub fn index_of(&self, object: &T) -> Option<Index> {
        self.indexes.get(object).map(|i| *i)
    }

    // Iterate on all indexes

    // Iterate on slots (may be empty)
    pub fn slot_iter<'a>(&'a self) -> SlotIterator<'a, T> {
        SlotIterator::new(&self.slots)
    }

    // TODO add iter() method
    pub fn iter(&self) -> <&Self as iter::IntoIterator>::IntoIter {
        self.into_iter()
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
        let new_index = Index(self.slots.len());
        self.slots.push(Slot::Used(value.clone())); // TODO reuse existing slot if possible
        let previous_value = self.indexes.insert(value, new_index);
        debug_assert_eq!(previous_value, None);
        self.nb_elements += 1;
        new_index
    }
    // Push a single unused slot at the end
    fn push_unused(&mut self) {
        self.slots.push(Slot::Unused)
    }

    // Remove object from the set through its id.
    // Does nothing if the index does not exist.
    pub fn remove_id(&mut self, index: Index) {
        let slot = &mut self.slots[index.as_usize()];
        if let &mut Slot::Used(_) = slot {
            self.nb_elements -= 1;
            let previous_value = self.indexes.remove(slot.value().unwrap());
            debug_assert_eq!(previous_value, Some(index));
            *slot = Slot::Unused // TODO add to free list
        }
    }
}

// Indexing operator: panics if the slot is unused
impl<T> ops::Index<Index> for IndexedSet<T>
where
    T: IndexedSetCapableType,
{
    type Output = T;
    fn index(&self, index: Index) -> &T {
        self.get(index).unwrap()
    }
}

// Iterator on indexes
pub struct IndexIterator {
    index: usize,
    nb_slots: usize,
}
impl IndexIterator {
    fn new(nb_slots: usize) -> Self {
        IndexIterator {
            index: 0,
            nb_slots: nb_slots,
        }
    }
}
impl iter::Iterator for IndexIterator {
    type Item = Index;
    fn next(&mut self) -> Option<Self::Item> {
        let current_index = self.index;
        self.index += 1;
        if self.index <= self.nb_slots {
            Some(Index(current_index))
        } else {
            None
        }
    }
}

// Iterator on slots
pub struct SlotIterator<'a, T: 'a> {
    slots: &'a Vec<Slot<T>>,
    index: usize,
}
impl<'a, T> SlotIterator<'a, T> {
    fn new(slots: &'a Vec<Slot<T>>) -> Self {
        SlotIterator {
            slots: slots,
            index: 0,
        }
    }
}
impl<'a, T> iter::Iterator for SlotIterator<'a, T> {
    type Item = (Index, Option<&'a T>);
    fn next(&mut self) -> Option<Self::Item> {
        let current_index = self.index;
        self.index += 1;
        if self.index <= self.slots.len() {
            Some((Index(current_index), self.slots[current_index].value()))
        } else {
            None
        }
    }
}

// Iterator over all elements, returns (index, elem_ref).
// Indexes are in increasing order.
impl<'a, T> iter::IntoIterator for &'a IndexedSet<T>
where
    T: IndexedSetCapableType,
{
    type Item = (Index, &'a T);
    type IntoIter = iter::FilterMap<
        iter::Enumerate<::std::slice::Iter<'a, Slot<T>>>,
        fn((usize, &'a Slot<T>)) -> Option<Self::Item>,
    >;
    fn into_iter(self) -> Self::IntoIter {
        self.slots
            .iter()
            .enumerate()
            .filter_map(|(raw_index, ref slot)| match slot.value() {
                Some(ref v) => Some((Index(raw_index), v)),
                None => None,
            })
    }
}

/*******************************************************************************
 * Serialize / Deserialize.
 * The vector is stored as an array of Option<T>.
 * Unused slots are kept, to avoid complex id conversion if ids are used by user code.
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
        let mut seq = serializer.serialize_seq(Some(self.slots.len()))?;
        for slot in &self.slots {
            seq.serialize_element(&slot.value())?;
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

                while let Some(slot) = try!(seq.next_element()) {
                    match slot {
                        // Add values manually to prevent reuse of slots or such
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
        assert_eq!(is.get(id_42), Some(&42));
        assert_eq!(is[id_42], 42);
        assert_eq!(is.len(), 1);

        let id_42_b = is.insert(42);
        let id_12 = is.insert(12);
        assert_eq!(id_42, id_42_b);
        assert_ne!(id_42, id_12);
        assert_eq!(is.len(), 2);

        is.remove_id(id_42);
        assert_eq!(is.len(), 1);
        assert_eq!(is.get(id_42), None);
    }
}
