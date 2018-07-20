// Graph IO
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;

// Wiki
#[macro_use]
extern crate rouille; // Simple http server
#[macro_use]
extern crate horrorshow; // HTML template engine
#[macro_use]
extern crate rust_embed; // Embed files in executable

#[macro_use]
extern crate clap; // Command line parser

/// Knowledge graph definition.
mod graph;
use graph::{Atom, Graph, Index, Link, Object};

/// Wiki interface
mod wiki;

use std::fs::File;
use std::path::Path;

fn read_graph_from_file(filename: &Path) -> Graph {
    match File::open(filename) {
        Ok(file) => match serde_json::from_reader(file) {
            Ok(graph) => return graph,
            Err(e) => eprintln!(
                "Warning: invalid graph format in file {}: {}",
                filename.display(),
                e
            ),
        },
        Err(e) => eprintln!(
            "Warning: cannot read graph from {}: {}",
            filename.display(),
            e
        ),
    }
    eprintln!("Using empty graph");
    Graph::new()
}

fn write_graph_to_file(filename: &Path, graph: &Graph) {
    match File::create(filename) {
        Ok(file) => if let Err(e) = serde_json::to_writer(file, graph) {
            eprintln!(
                "Warning: cannot write graph to {}: {}",
                filename.display(),
                e
            )
        },
        Err(e) => eprintln!(
            "Warning: cannot write graph to {}: {}",
            filename.display(),
            e
        ),
    }
}

/*****************************************************************************
 */
use std::collections::{HashMap, HashSet};
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

    let link_color_indexes = {
        // Table to store color assignements for previously colored Links.
        let mut link_color_indexes: HashMap<Index, usize> = HashMap::new();

        for object in g.objects() {
            // Select a color index for a link, assuming all lesser indexed Links have been colored.
            if let Object::Link(ref link) = *object {
                let index = object.index();
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
                        if g.object(link.from).is_link() {
                            conflict_with_color_of_link(&link.from)
                        };
                        if g.object(link.to).is_link() {
                            conflict_with_color_of_link(&link.to)
                        };
                        // Conflicts with links that are pointing to/from us
                        object
                            .in_links()
                            .iter()
                            .for_each(&mut conflict_with_color_of_link);
                        object
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
    for object in g.objects().filter(|object| (*predicate)(object.index())) {
        let index = object.index();
        match *object {
            Object::Atom(ref a) => {
                writeln!(out, "\t{0} [shape=box,label=\"{0}: {1}\"];", index, a)?;
            }
            Object::Link(ref link) => {
                let color = color_palette[link_color_indexes[&index]];
                if object.in_links().is_empty() && object.out_links().is_empty() {
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
            if let Some(&matched_target_object) = self.mapping.get(&pattern_object) {
                matched_target_object == target_object
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
    for pattern_object in pattern.objects() {
        if let Object::Atom(ref a) = *pattern_object {
            if let Some(target_object_id) = target.get_atom(a) {
                mapping.add(pattern_object.index(), target_object_id);
            }
        }
    }

    // Match the rest
    while let Some(matched_pattern_object_id) = mapping.next_matched_pattern_object_to_inspect() {
        // Match neighboring stuff that is unambiguous (and not matched)
        // Add them to list of matched stuff
        let matched_pattern_object = pattern.object(matched_pattern_object_id);
        let matched_target_object = target.object(mapping.mapping[&matched_pattern_object_id]);

        // Match in_links/out_links if unique between pair of matched elements
        if matched_pattern_object.in_links().len() == 1
            && matched_target_object.in_links().len() == 1
        {
            if !mapping.add(
                matched_pattern_object.in_links()[0],
                matched_target_object.in_links()[0],
            ) {
                return None;
            }
        }
        if matched_pattern_object.out_links().len() == 1
            && matched_target_object.out_links().len() == 1
        {
            if !mapping.add(
                matched_pattern_object.out_links()[0],
                matched_target_object.out_links()[0],
            ) {
                return None;
            }
        }

        // Match ends if matched object is link
        if let Object::Link(ref pattern_link) = *matched_pattern_object {
            let target_link = matched_target_object.as_link();

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
 * TODO update with wiki conventions when defined
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

fn do_test(what: &str, db_filename: &Path) {
    let mut graph = Graph::new();
    set_test_data(&mut graph);

    if what == "serde" {
        write_graph_to_file(db_filename, &graph)
    }
    if what == "graph" {
        output_as_dot(&mut io::stdout(), &graph).unwrap()
    }
    if what == "pattern" {
        let mut pattern = Graph::new();
        let name_prop = create_name_prop(&mut pattern);
        let mapping = match_graph(&pattern, &graph);
        eprintln!("MAPPING {:?}", &mapping);
    }
    if what == "self" {
        // Print the matched part of graph
        let self_mapping = match_graph(&graph, &graph).expect("match failure");
        output_as_dot_filtered(&mut io::stdout(), &graph, &|i: Index| {
            self_mapping.contains_key(&i)
        }).unwrap();
    }
}

fn main() {
    use clap::{AppSettings, Arg, SubCommand};
    let matches = app_from_crate!()
        .setting(AppSettings::VersionlessSubcommands)
        .setting(AppSettings::SubcommandRequired)
        .arg(
            Arg::with_name("db_file")
                .help("Path to relation file")
                .required(true),
        )
        .subcommand(
            SubCommand::with_name("wiki")
                .about("Run a server with a wiki-like interface to the database")
                .arg(
                    Arg::with_name("addr")
                        .help("Address on which the server will bind")
                        .default_value("localhost:8000"),
                ),
        )
        .subcommand(
            SubCommand::with_name("test").about("Run tests").arg(
                Arg::with_name("what")
                    .required(true)
                    .possible_values(&["serde", "graph", "pattern", "self"]),
            ),
        )
        .get_matches();

    // TODO useful tooling: merge of files

    let db_filepath = Path::new(matches.value_of_os("db_file").unwrap());

    match matches.subcommand() {
        ("wiki", Some(args)) => wiki::run(args.value_of("addr").unwrap(), db_filepath),
        ("test", Some(args)) => do_test(args.value_of("what").unwrap(), db_filepath),
        _ => panic!("Missing subcommand"),
    }
}
