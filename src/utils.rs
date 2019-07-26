use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;
use std::iter::FromIterator;
use std::ops;

/// Remove prefix and return tail of string if successful
pub fn remove_prefix<'a>(s: &'a str, prefix: &str) -> Option<&'a str> {
    match s.get(..prefix.len()) {
        Some(p) if p == prefix => Some(&s[prefix.len()..]),
        _ => None,
    }
}

/// Vector where elements never change indexes.
/// Removal generate holes.
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
impl<T> FromIterator<Option<T>> for SlotVec<T> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = Option<T>>,
    {
        Self::from(Vec::from_iter(iter))
    }
}

/// Set based on a sorted vector.
/// Elements are unique.
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
    /// Insert element if not present.
    pub fn insert(&mut self, e: T) {
        if let Err(insertion_index) = self.inner.binary_search(&e) {
            self.inner.insert(insertion_index, e)
        }
    }
    /// Remove element e if present, return removed value.
    pub fn remove(&mut self, e: &T) -> Option<T> {
        if let Ok(index) = self.inner.binary_search(e) {
            Some(self.inner.remove(index))
        } else {
            None
        }
    }
}
impl<T: Ord> AsRef<[T]> for Set<T> {
    fn as_ref(&self) -> &[T] {
        &self.inner
    }
}
impl<T: Ord> From<Vec<T>> for Set<T> {
    fn from(mut inner: Vec<T>) -> Self {
        inner.sort_unstable();
        Set { inner }
    }
}
impl<T: Ord> FromIterator<T> for Set<T> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        Self::from(Vec::from_iter(iter))
    }
}

/// Map based on a sorted vector.
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
            .binary_search_by(|p| p.0.borrow().cmp(k))
            .map(|index| &self.inner[index].1)
            .ok()
    }
    pub fn get_mut<Q>(&mut self, k: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Ord + ?Sized,
    {
        match self.inner.binary_search_by(|p| p.0.borrow().cmp(k)) {
            Ok(index) => Some(&mut self.inner[index].1),
            Err(_) => None,
        }
    }
    /// Insert (k,v), returns old value at K if present.
    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        match self.inner.binary_search_by(|p| p.0.cmp(&k)) {
            Ok(existing_index) => {
                Some(std::mem::replace(&mut self.inner[existing_index], (k, v)).1)
            }
            Err(insertion_index) => {
                self.inner.insert(insertion_index, (k, v));
                None
            }
        }
    }
    /// Remove value for k and return it, if present.
    pub fn remove<Q>(&mut self, k: &Q) -> Option<V>
    where
        K: Borrow<Q>,
        Q: Ord + ?Sized,
    {
        if let Ok(index) = self.inner.binary_search_by(|p| p.0.borrow().cmp(k)) {
            Some(self.inner.remove(index).1)
        } else {
            None
        }
    }
}
impl<K: Ord, V> AsRef<[(K, V)]> for Map<K, V> {
    fn as_ref(&self) -> &[(K, V)] {
        &self.inner
    }
}
impl<K: Ord, V> From<Vec<(K, V)>> for Map<K, V> {
    fn from(mut inner: Vec<(K, V)>) -> Self {
        inner.sort_unstable_by(|lhs, rhs| lhs.0.cmp(&rhs.0));
        Map { inner }
    }
}
impl<K: Ord, V> FromIterator<(K, V)> for Map<K, V> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (K, V)>,
    {
        Self::from(Vec::from_iter(iter))
    }
}

/// Fuzzy search database for strings.
/// Each string must be associated to a unique D value.
/// This D value is returned in search results.
/// The search is based on decomposing strings into [char;3] sequences,
/// and returning the D with the most associated sequences.
/// Values are converted to lowercase to improve matching probability.
#[derive(Debug)]
pub struct FuzzySearcher<D: Ord + Clone + Hash> {
    kmers: HashMap<[char; 3], Map<D, usize>>,
}
impl<D: Ord + Clone + Hash> FuzzySearcher<D> {
    pub fn new() -> Self {
        FuzzySearcher {
            kmers: HashMap::new(),
        }
    }
    /// Associate D with the String.
    /// Only one association is allowed for each D.
    pub fn insert(&mut self, s: &str, d: D) {
        let chars = to_lowercase_char_vec(s);
        for kmer in chars.windows(3) {
            match self.kmers.get_mut(kmer) {
                Some(map) => match map.get_mut(&d) {
                    Some(v) => *v += 1,
                    None => {
                        map.insert(d.clone(), 1);
                        ()
                    }
                },
                None => {
                    self.kmers
                        .insert([kmer[0], kmer[1], kmer[2]], Map::from(vec![(d.clone(), 1)]));
                    ()
                }
            }
        }
    }
    /// Remove association between D and the String.
    /// The arguments must be the same as used for add(s, d).
    pub fn remove(&mut self, s: &str, d: &D) {
        let chars = to_lowercase_char_vec(s);
        for kmer in chars.windows(3) {
            let rm_entry = match self.kmers.get_mut(kmer) {
                Some(map) => {
                    // Remove all references to D at once, optimization.
                    map.remove(d);
                    map.as_ref().len() == 0
                }
                None => false,
            };
            if rm_entry {
                self.kmers.remove(kmer);
            }
        }
    }

    pub fn matches(&self, s: &str) -> Vec<(D, usize)> {
        let chars = to_lowercase_char_vec(s);
        // Accumulate number of matching kmer to each defined D.
        let mut score_table = HashMap::new();
        for kmer in chars.windows(3) {
            if let Some(map) = self.kmers.get(kmer) {
                for (d, count) in map.as_ref().iter() {
                    match score_table.get_mut(d) {
                        Some(score) => *score += count,
                        None => {
                            score_table.insert(d.clone(), *count);
                            ()
                        }
                    }
                }
            }
        }
        // Sort by decreasing match count
        let mut scores: Vec<_> = score_table.into_iter().collect();
        scores.sort_unstable_by(|lhs, rhs| rhs.1.cmp(&lhs.1));
        scores
    }
}
fn to_lowercase_char_vec(s: &str) -> Vec<char> {
    s.chars().flat_map(|c| c.to_lowercase()).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fuzzy_search() {
        let mut searcher = FuzzySearcher::new();
        searcher.insert("Hello world !", 0);
        searcher.insert("This is a beautiful world", 1);
        searcher.insert("War of Worlds...", 2);
        eprintln!("{:?}", searcher.matches("world"));
        eprintln!("{:?}", searcher.matches("This is war !"));
    }
}
