/* A vector of immutable indexed cells.
 * Cells can be created and destroyed.
 * After creation, cells are immutable.
 * TODO iterable: filter_map (enumerate ())
 */
enum ImmutableVectorCell<T> {
    Used(T),
    Unused, // TODO Pointer to next unused cell
}
impl<T> ImmutableVectorCell<T> {
    fn value(&self) -> Option<&T> {
        match self {
            &ImmutableVectorCell::Used(ref v) => Some(v),
            _ => None,
        }
    }
}
struct ImmutableVector<T> {
    cells: Vec<ImmutableVectorCell<T>>,
    nb_elements: usize,
}
impl<T> ImmutableVector<T> {
    fn new() -> ImmutableVector<T> {
        ImmutableVector {
            cells: Vec::new(),
            nb_elements: 0,
        }
    }
    fn insert(&mut self, value: T) -> usize {
        let new_index = self.cells.len();
        self.cells.push(ImmutableVectorCell::Used(value));
        self.nb_elements += 1;
        new_index
    }
    fn remove(&mut self, index: usize) {
        self.cells[index] = ImmutableVectorCell::Unused;
        self.nb_elements -= 1
    }
    fn len(&self) -> usize {
        self.nb_elements
    }
}
impl<T> std::ops::Index<usize> for ImmutableVector<T> {
    type Output = T;
    fn index(&self, i: usize) -> &T {
        self.cells[i].value().unwrap()
    }
}
impl<'a, T> std::iter::IntoIterator for &'a ImmutableVector<T> {
    type Item = &'a ImmutableVectorCell<T>;
    type IntoIter =
        <&'a std::vec::Vec<ImmutableVectorCell<T>> as std::iter::IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        (&self.cells).into_iter()
    }
}

// OLD test stuff
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
    for index in 0..iv.len() {
        match &iv[index] {
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
