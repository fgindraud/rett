// Graph IO
extern crate serde;
#[macro_use]
extern crate serde_derive;

// Wiki
#[macro_use]
extern crate rouille;

/** Knowledge graph.
 */
mod graph;
use graph::{Atom, Graph, Index, Link, Object};

/*******************************************************************************
 */
mod wiki {
    use super::graph::Graph;
    use super::rouille;
    use rouille::Response;
    use std::sync::RwLock;

    pub fn run(graph: Graph) -> ! {
        let graph = RwLock::new(graph);

        rouille::start_server("localhost:8000", move |request| {
            // TODO grow from this skeleton
            // html is the entire page content
            // /: link to all named elements ?
            // /zzz: zzz and its associated stuff
            // Horrorshow templating lib ?
            router!(request,
                (GET) (/id/{id: usize}) => {
                    let graph = graph.read().unwrap();
                    match graph.get_object(id) {
                        Some (object_ref) => {
                            Response::text (format!("Object {}: {:?}", object_ref.index(), object_ref.object()))
                        },
                        None => Response::empty_404()
                    }
                },
                _ => Response::empty_404()
            )
        })
    }
}

/*****************************************************************************
 */
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::io;

/// Output graph as dot.
///
/// Added basic filtering.
/// TODO improve it.
/// Notion of graph view (partial) ?
fn output_as_dot_filtered(
    out: &mut io::Write,
    g: &Graph,
    predicate: &Fn(Index) -> bool,
) -> io::Result<()> {
    /* Link handling.
     *
     * Links in a graph behave as objects and can have links pointing to them.
     * This is not allowed in dot.
     * Thus links with in_links()/out_links() are split into two Dot arrows with a dummy node.
     *
     * To improve readability, each link is given an arrow color.
     * This arrow color must be different from in_links() and out_links().
     * This ensures that both Dot edges can be easily recognized as part of the same link.
     *
     * Colors are selected from a fixed palette (generating one is complex).
     * This should be sufficient as theere are few conflicts in practice.
     *
     * Colors are chosen by a greedy algorithm:
     * color (link) = min unused index among lower index link neighbors.
     */
    let color_palette = [
        "#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255",
        "#AA4499",
    ];

    impl fmt::Display for Atom {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                &Atom::String(ref s) => write!(f, "\\\"{}\\\"", s),
                &Atom::Integer(i) => i.fmt(f),
            }
        }
    }

    let link_color_indexes = {
        // Table to store color assignements for previously colored Links.
        let mut link_color_indexes: HashMap<Index, usize> = HashMap::new();

        for object_ref in g.objects() {
            // Select a color index for a link, assuming all lesser indexed Links have been colored.
            if let &Object::Link(ref link) = object_ref.object() {
                let index = object_ref.index();
                // Build a list of colors of all links we are in conflict with
                let conflicting_color_indexes = {
                    let mut conflicting_color_indexes = Vec::new();
                    {
                        let mut conflict_with_color_of_link = |i: &Index| {
                            if *i < index {
                                conflicting_color_indexes.push(link_color_indexes[i])
                            }
                        };
                        // Conflicts with links we are pointing to/from
                        if g.object(link.from).object().is_link() {
                            conflict_with_color_of_link(&link.from)
                        };
                        if g.object(link.to).object().is_link() {
                            conflict_with_color_of_link(&link.to)
                        };
                        // Conflicts with links that are pointing to/from us
                        object_ref
                            .in_links()
                            .iter()
                            .for_each(&mut conflict_with_color_of_link);
                        object_ref
                            .out_links()
                            .iter()
                            .for_each(&mut conflict_with_color_of_link);
                    }
                    conflicting_color_indexes
                };
                // Select first unused color index
                let mut color_index = 0;
                while conflicting_color_indexes.contains(&color_index) {
                    color_index += 1
                }
                assert!(
                    color_index <= color_palette.len(),
                    "output_as_dot: nb_colors = {} exceeds the color palette size ({})",
                    color_index,
                    color_palette.len()
                );
                // Store it for next calls to choose_link_color()
                link_color_indexes.insert(index, color_index);
            }
        }

        link_color_indexes
    };

    // Output graph
    writeln!(out, "digraph {{")?;
    for object_ref in g.objects()
        .filter(|object_ref| (*predicate)(object_ref.index()))
    {
        let index = object_ref.index();
        match object_ref.object() {
            &Object::Atom(ref a) => {
                writeln!(out, "\t{0} [shape=box,label=\"{0}: {1}\"];", index, a)?;
            }
            &Object::Link(ref link) => {
                let color = color_palette[link_color_indexes[&index]];
                if object_ref.in_links().is_empty() && object_ref.out_links().is_empty() {
                    writeln!(
                        out,
                        "\t{0} -> {1} [fontcolor=grey,color=\"{3}\",label=\"{2}\"];",
                        link.from, link.to, index, color
                    )?;
                } else {
                    writeln!(out,
                    "\t{0} [shape=none,fontcolor=grey,margin=0.02,height=0,width=0,label=\"{0}\"];",
                    index
                )?;
                    writeln!(
                        out,
                        "\t{0} -> {1} [dir=none,color=\"{2}\"];",
                        link.from, index, color
                    )?;
                    writeln!(out, "\t{0} -> {1} [color=\"{2}\"];", index, link.to, color)?;
                }
            }
            _ => {
                writeln!(out, "\t{0} [shape=hexagon,label=\"{0}\"];", index)?;
            }
        }
    }
    writeln!(out, "}}")
}

fn output_as_dot(out: &mut io::Write, g: &Graph) -> io::Result<()> {
    output_as_dot_filtered(out, g, &|_: Index| true)
}

/*******************************************************************************
 * Matching of graphs against each other.
 * TODO return iterator over possible matches
 */
fn match_graph(pattern: &Graph, target: &Graph) -> Option<HashMap<Index, Index>> {
    // Used to build a mapping iteratively
    struct MappingBuilder {
        mapping: HashMap<Index, Index>,
        matched_pattern_objects_to_inspect: HashSet<Index>,
    }
    impl MappingBuilder {
        fn new() -> Self {
            MappingBuilder {
                mapping: HashMap::new(),
                matched_pattern_objects_to_inspect: HashSet::new(),
            }
        }

        // Returns true if ok, false if conflicts with current matching
        fn add(&mut self, pattern_object: Index, target_object: Index) -> bool {
            if let Some(&current_matched_target_object) = self.mapping.get(&pattern_object) {
                current_matched_target_object == target_object
            } else {
                self.mapping.insert(pattern_object, target_object);
                self.matched_pattern_objects_to_inspect
                    .insert(pattern_object);
                true
            }
        }

        // Pick one from matched_pattern_objects_to_inspect set
        fn next_matched_pattern_object_to_inspect(&mut self) -> Option<Index> {
            match self.matched_pattern_objects_to_inspect
                .iter()
                .cloned()
                .next()
            {
                Some(index) => {
                    self.matched_pattern_objects_to_inspect.remove(&index);
                    Some(index)
                }
                None => None,
            }
        }
    }

    let mut mapping = MappingBuilder::new();

    // Match atoms, which are unambiguous
    for pattern_object_ref in pattern.objects() {
        if let &Object::Atom(ref a) = pattern_object_ref.object() {
            if let Some(target_object) = target.get_atom(a) {
                mapping.add(pattern_object_ref.index(), target_object);
            }
        }
    }

    // Match the rest
    while let Some(matched_pattern_object) = mapping.next_matched_pattern_object_to_inspect() {
        // Match neighboring stuff that is unambiguous (and not matched)
        // Add them to list of matched stuff
        let matched_pattern_object_ref = pattern.object(matched_pattern_object);
        let matched_target_object_ref = target.object(mapping.mapping[&matched_pattern_object]);

        // Match in_links/out_links if unique between pair of matched elements
        if matched_pattern_object_ref.in_links().len() == 1
            && matched_target_object_ref.in_links().len() == 1
        {
            if !mapping.add(
                matched_pattern_object_ref.in_links()[0],
                matched_target_object_ref.in_links()[0],
            ) {
                return None;
            }
        }
        if matched_pattern_object_ref.out_links().len() == 1
            && matched_target_object_ref.out_links().len() == 1
        {
            if !mapping.add(
                matched_pattern_object_ref.out_links()[0],
                matched_target_object_ref.out_links()[0],
            ) {
                return None;
            }
        }

        // Match ends if matched object is link
        if let &Object::Link(ref pattern_link) = matched_pattern_object_ref.object() {
            let target_link = matched_target_object_ref.object().as_link();

            // Match from/to objects
            if !mapping.add(pattern_link.from, target_link.from) {
                return None;
            }
            if !mapping.add(pattern_link.to, target_link.to) {
                return None;
            }
        }

        // TODO Match in/out links if their other end is matched too
    }

    Some(mapping.mapping)
}

/*******************************************************************************
 * Test
 */
fn create_name_prop(g: &mut Graph) -> Index {
    let name_entity = g.create_abstract();
    let name_text = g.use_atom(Atom::text("name"));
    let name_entity_description = g.use_link(Link::new(name_text, name_entity));
    let _name_entity_description_description =
        g.use_link(Link::new(name_entity, name_entity_description));
    name_entity
}

fn create_named_entity(g: &mut Graph, name_entity: Index, text: &str) -> Index {
    let entity = g.create_abstract();
    let atom = g.use_atom(Atom::text(text));
    let link = g.use_link(Link::new(atom, entity));
    let _link_description = g.use_link(Link::new(name_entity, link));
    entity
}

fn set_test_data(g: &mut Graph) {
    let name = create_name_prop(g);

    let joe = create_named_entity(g, name, "joe");
    let bob = create_named_entity(g, name, "bob");

    let pj = create_named_entity(g, name, "pj");
    g.use_link(Link::new(pj, joe));
    g.use_link(Link::new(pj, bob));

    let fight = create_named_entity(g, name, "fight");
    let joe_in_fight = g.use_link(Link::new(joe, fight));
    let bob_in_fight = g.use_link(Link::new(bob, fight));

    let was_present = create_named_entity(g, name, "was_present");
    g.use_link(Link::new(was_present, joe_in_fight));
    g.use_link(Link::new(was_present, bob_in_fight));

    let win = create_named_entity(g, name, "win");
    g.use_link(Link::new(win, bob_in_fight));

    let date = create_named_entity(g, name, "date");
    let some_date = g.use_atom(Atom::Integer(2018));
    let fight_date = g.use_link(Link::new(some_date, fight));
    g.use_link(Link::new(date, fight_date));
}

use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut graph = Graph::new();
    set_test_data(&mut graph);

    if args.iter().any(|s| s == "graph") {
        output_as_dot(&mut io::stdout(), &graph).unwrap()
    }
    if args.iter().any(|s| s == "pattern") {
        let mut pattern = Graph::new();
        let name_prop = create_name_prop(&mut pattern);
        let mapping = match_graph(&pattern, &graph);
        eprintln!("MAPPING {:?}", &mapping);
    }
    if args.iter().any(|s| s == "self") {
        // Print the matched part of graph
        let self_mapping = match_graph(&graph, &graph).expect("match failure");
        output_as_dot_filtered(&mut io::stdout(), &graph, &|i: Index| {
            self_mapping.contains_key(&i)
        }).unwrap();
    }
    if args.iter().any(|s| s == "wiki") {
        wiki::run(graph)
    }
}
