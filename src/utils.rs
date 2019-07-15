use std::borrow::Borrow;
use std::iter::FromIterator;
use std::ops::Deref;

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
impl<T: Ord> Deref for Set<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        self.inner.deref()
    }
}

/// Associative map based on a sorted vector.
#[derive(Debug, PartialEq, Eq)]
pub struct Map<K: Ord, V> {
    inner: Vec<(K, V)>,
}
impl<K: Ord, V> Map<K, V> {
    pub fn new() -> Self {
        Map { inner: Vec::new() }
    }
    pub fn get<Q>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Ord + ?Sized,
    {
        self.inner
            .binary_search_by_key(&k, |p| p.0.borrow())
            .map(|index| &self.inner[index].1)
            .ok()
    }
}
impl<K: Ord, V> Deref for Map<K, V> {
    type Target = [(K, V)];
    fn deref(&self) -> &[(K, V)] {
        self.inner.deref()
    }
}
impl<K: Ord, V> From<Vec<(K, V)>> for Map<K, V> {
    fn from(mut v: Vec<(K, V)>) -> Self {
        v.sort_unstable_by(|lhs, rhs| lhs.0.cmp(&rhs.0));
        Map { inner: v }
    }
}
impl<K: Ord, V> FromIterator<(K, V)> for Map<K, V> {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = (K, V)>,
    {
        Map::from(Vec::from_iter(iter))
    }
}
