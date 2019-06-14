/// Vector with sorted elements and set api.
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
impl<T: Ord> std::ops::Deref for Set<T> {
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
    pub fn get(&self, k: &K) -> Option<&V> {
        self.inner
            .binary_search_by_key(&k, |p| &p.0)
            .map(|index| &self.inner[index].1)
            .ok()
    }
}
impl<K: Ord, V> std::ops::Deref for Map<K, V> {
    type Target = [(K, V)];
    fn deref(&self) -> &[(K, V)] {
        self.inner.deref()
    }
}
