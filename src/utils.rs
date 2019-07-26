use std::ops;

/// Vector where elements never change indexes. Removal generate holes.
pub struct SlotVec<T> {
    inner: Vec<Option<T>>,
}
impl<T> SlotVec<T> {
    pub fn new() -> Self {
        SlotVec { inner: Vec::new() }
    }
    pub fn valid(&self, i: usize) -> bool {
        i < self.inner.len() && self.inner[i].is_some()
    }
    pub fn get(&self, i: usize) -> Option<&T> {
        match self.inner.get(i) {
            Some(&Some(ref e)) => Some(e),
            _ => None,
        }
    }
    pub fn get_mut(&mut self, i: usize) -> Option<&mut T> {
        match self.inner.get_mut(i) {
            Some(&mut Some(ref mut e)) => Some(e),
            _ => None,
        }
    }
    pub fn insert(&mut self, e: T) -> usize {
        // Find unused index
        for index in 0..self.inner.len() {
            let cell = &mut self.inner[index];
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
    pub fn remove(&mut self, i: usize) -> Option<T> {
        match self.inner.get_mut(i) {
            Some(slot) => slot.take(),
            None => None,
        }
    }
    pub fn capacity(&self) -> usize {
        self.inner.len()
    }
}
impl<T> ops::Index<usize> for SlotVec<T> {
    type Output = T;
    fn index(&self, i: usize) -> &Self::Output {
        self.get(i).unwrap()
    }
}
impl<T> ops::IndexMut<usize> for SlotVec<T> {
    fn index_mut(&mut self, i: usize) -> &mut Self::Output {
        self.get_mut(i).unwrap()
    }
}
impl<T> AsRef<[Option<T>]> for SlotVec<T> {
    fn as_ref(&self) -> &[Option<T>] {
        self.inner.as_ref()
    }
}
impl<T> From<Vec<Option<T>>> for SlotVec<T> {
    fn from(inner: Vec<Option<T>>) -> Self {
        SlotVec { inner }
    }
}

/// Set based on a sorted vector.
#[derive(Debug, PartialEq, Eq)]
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
impl<T: Ord> ops::Deref for Set<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        self.inner.deref()
    }
}

/// Remove prefix and return tail of string if successful
pub fn remove_prefix<'a>(s: &'a str, prefix: &str) -> Option<&'a str> {
    match s.get(..prefix.len()) {
        Some(p) if p == prefix => Some(&s[prefix.len()..]),
        _ => None,
    }
}
