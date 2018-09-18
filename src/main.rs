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

/// Knowledge database as a set of sentences.
mod corpus;
use corpus::Corpus;

/// Wiki interface
mod wiki;

use std::fs::File;
use std::path::Path;

fn read_corpus_from_file(filename: &Path) -> Corpus {
    match File::open(filename) {
        Ok(file) => match serde_json::from_reader(file) {
            Ok(corpus) => return corpus,
            Err(e) => eprintln!(
                "Warning: invalid corpus format in file {}: {}",
                filename.display(),
                e
            ),
        },
        Err(e) => eprintln!(
            "Warning: cannot read corpus from {}: {}",
            filename.display(),
            e
        ),
    }
    eprintln!("Using empty corpus");
    Corpus::new()
}
fn write_corpus_to_file(filename: &Path, corpus: &Corpus) {
    match File::create(filename) {
        Ok(file) => if let Err(e) = serde_json::to_writer(file, corpus) {
            eprintln!(
                "Warning: cannot write corpus to {}: {}",
                filename.display(),
                e
            )
        },
        Err(e) => eprintln!(
            "Warning: cannot write corpus to {}: {}",
            filename.display(),
            e
        ),
    }
}

fn main() {
    use clap::{AppSettings, Arg, SubCommand};
    let matches = app_from_crate!()
        .setting(AppSettings::VersionlessSubcommands)
        .setting(AppSettings::SubcommandRequired)
        .arg(
            Arg::with_name("corpus_file")
                .help("Path to corpus file")
                .required(true),
        )
        .subcommand(
            SubCommand::with_name("wiki")
                .about("Run a server with a wiki-like interface to the corpus")
                .arg(
                    Arg::with_name("addr")
                        .help("Address on which the server will bind")
                        .default_value("localhost:8000"),
                )
                .arg(
                    clap::Arg::with_name("nb_threads")
                        .short("n")
                        .long("nb-threads")
                        .takes_value(true)
                        .default_value("2")
                        .help("Size of the threadpool"),
                ),
        )
        .get_matches();

    // TODO useful tooling: merge of files

    let corpus_filepath = Path::new(matches.value_of_os("corpus_file").unwrap());

    match matches.subcommand() {
        ("wiki", Some(args)) => {
            let addr = args.value_of("addr").unwrap();
            let nb_threads = args.value_of("nb_threads")
                .unwrap()
                .parse()
                .expect("nb_threads: usize");
            wiki::run(addr, corpus_filepath, nb_threads)
        }
        _ => panic!("Missing subcommand"),
    }
}
