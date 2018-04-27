/*******************************************************************************
 * A vector of immutable indexed cells.
 * Cells can be created and destroyed.
 * After creation, cells are immutable.
 * TODO iterable: filter_map (enumerate ())
 */
mod immutable_vector {
    pub enum Cell<T> {
        Used(T),
        Unused, // TODO Pointer to next unused cell
    }
    impl<T> Cell<T> {
        fn value(&self) -> Option<&T> {
            match self {
                &Cell::Used(ref v) => Some(v),
                _ => None,
            }
        }
    }
    pub struct ImmutableVector<T> {
        cells: Vec<Cell<T>>,
        nb_elements: usize,
    }
    impl<T> ImmutableVector<T> {
        pub fn new() -> ImmutableVector<T> {
            ImmutableVector {
                cells: Vec::new(),
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
        pub fn cell(&self, index: usize) -> Option<&T> {
            return self.cells[index].value();
        }
        // Iterator over all cells, returns (index: usize, elem_ref: Option<&T>)
        pub fn cell_iter<'a>(
            &'a self,
        ) -> ::std::iter::Enumerate<
            ::std::iter::Map<::std::slice::Iter<'a, Cell<T>>, fn(&Cell<T>) -> Option<&T>>,
        > {
            self.cells.iter().map(Cell::value as _).enumerate()
        }
    }
    impl<T> ::std::ops::Index<usize> for ImmutableVector<T> {
        // Index panics on empty cells
        type Output = T;
        fn index(&self, index: usize) -> &T {
            self.cell(index).unwrap()
        }
    }
    impl<'a, T> ::std::iter::IntoIterator for &'a ImmutableVector<T> {
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
}

use immutable_vector::ImmutableVector;

/*******************************************************************************
 * TODO new Element enum
 * TODO file I/O
 * TODO output as dot : (c, link{a, b}) : a => c => b with color code on arrows
 * TODO queries, with hash map for referencing
 */

/*******************************************************************************
 * OLD test stuff
 */
enum Entity {
    Atom(String),
    Link { from: usize, to: usize },
}
impl Entity {
    fn atom(text: &str) -> Entity {
        Entity::Atom(String::from(text))
    }
    fn link(from: usize, to: usize) -> Entity {
        Entity::Link { from, to }
    }
}
fn output_as_dot(iv: &ImmutableVector<Entity>) {
    println!("digraph {{");
    for (index, elem) in iv {
        match elem {
            &Entity::Atom(ref s) => println!("\t{} [shape=box,label=\"{}\"];", index, s),
            &Entity::Link { ref from, ref to } => {
                println!("\t{} -> {} [label=\"{}\"];", from, to, index)
            }
        }
    }
    println!("}}");
}

fn main() {
    let mut iv = ImmutableVector::new();
    // Catégories de personnes
    let personnage = iv.insert(Entity::atom("Personnage"));
    let pj = iv.insert(Entity::atom("PJ"));
    iv.insert(Entity::link(pj, personnage));
    let pnj = iv.insert(Entity::atom("PNJ"));
    iv.insert(Entity::link(pnj, personnage));
    // Liens entre personnes
    let ami = iv.insert(Entity::atom("Ami de"));
    let _ennemi = iv.insert(Entity::atom("Ennemi de"));
    // Quelques données
    let joe = iv.insert(Entity::atom("Joe"));
    iv.insert(Entity::link(joe, pj));
    let alice = iv.insert(Entity::atom("Alice"));
    iv.insert(Entity::link(alice, pnj));
    let joe_ami_alice = iv.insert(Entity::link(joe, alice));
    iv.insert(Entity::link(joe_ami_alice, ami));
    output_as_dot(&iv);
}
