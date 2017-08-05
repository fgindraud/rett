
enum Entity {
    Atom(String),
    Link { from: i32, to: i32 },
}
struct IndexedEntityVector {
    entities: Vec<Entity>,
}

impl Entity {
    fn atom(text: &str) -> Entity {
        Entity::Atom(String::from(text))
    }
    fn link(from: i32, to: i32) -> Entity {
        Entity::Link { from, to }
    }
}
impl IndexedEntityVector {
    fn new() -> IndexedEntityVector {
        IndexedEntityVector { entities: Vec::new() }
    }
    fn size(&self) -> i32 {
        self.entities.len() as i32
    }
    fn create(&mut self, content: Entity) -> i32 {
        let index: i32 = self.size();
        self.entities.push(content);
        index
    }
    fn entity(&self, index: i32) -> &Entity {
        &self.entities[index as usize]
    }
}

fn output_as_dot(iv: &IndexedEntityVector) {
    println!("digraph {{");
    for index in 0..iv.size() {
        match iv.entity(index) {
            &Entity::Atom(ref s) => println!("\t{} [shape=box,label=\"{}\"];", index, s),
            &Entity::Link { from, to } => println!("\t{} -> {} [label=\"{}\"];", from, to, index),
        }
    }
    println!("}}");
}

fn main() {
    let mut iv = IndexedEntityVector::new();

    // Catégories de personnes
    let personnage = iv.create(Entity::atom("Personnage"));
    let pj = iv.create(Entity::atom("PJ"));
    iv.create(Entity::link(pj, personnage));
    let pnj = iv.create(Entity::atom("PNJ"));
    iv.create(Entity::link(pnj, personnage));

    // Liens entre personnes
    let ami = iv.create(Entity::atom("Ami de"));
    let ennemi = iv.create(Entity::atom("Ennemi de"));

    // Quelques données
    let joe = iv.create(Entity::atom("Joe"));
    iv.create(Entity::link(joe, pj));
    let alice = iv.create(Entity::atom("Alice"));
    iv.create(Entity::link(alice, pnj));
    let joe_ami_alice = iv.create(Entity::link(joe, alice));
    iv.create(Entity::link(joe_ami_alice, ami));

    /* Modèle pas trop mal.
     * Il faudrait imposer que toute flèche soit taggée.
     * ("est un", "inclus dans" pour les trucs basiques).
     *
     * Il faut un map Entity -> Index
     *
     * Affichage dot:
     * Soit le truc actuel, passer par les index de liens.
     * Soit labeller les liens avec les tags.
     * Cas link -> link pertinent ? Ordonnancement de liens par exemple.
     *
     * Tester Queries ?
     *
     * Automatismes comme liens reflexifs / transitifs : niveau query ou lors de l'input du graph ?
     *
     */

    output_as_dot(&iv);
}
