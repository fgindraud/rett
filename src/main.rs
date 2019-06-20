// Wiki
#[macro_use]
extern crate horrorshow; // HTML template engine
extern crate hyper;
extern crate percent_encoding;
extern crate signal_hook;
extern crate tokio;

#[macro_use]
extern crate clap; // Command line parser

/// Common utilities
mod utils;

/// Knowledge database as a set of sentences.
mod relations;
use relations::Database;

/// Wiki interface
mod wiki;
use std::fs::File;
use std::io;
use std::path::Path;

fn read_database_from_file(filename: &Path) -> Database {
    match File::open(filename) {
        Ok(file) => match Database::read_from(io::BufReader::new(file)) {
            Ok(database) => return database,
            Err(e) => eprintln!(
                "Warning: invalid database format in file {}: {}",
                filename.display(),
                e
            ),
        },
        Err(e) => eprintln!(
            "Warning: cannot read database from {}: {}",
            filename.display(),
            e
        ),
    }
    eprintln!("Using empty database");
    Database::new()
}
fn write_database_to_file(filename: &Path, database: &Database) {
    match File::create(filename) {
        Ok(file) => {
            if let Err(e) = database.write_to(io::BufWriter::new(file)) {
                eprintln!(
                    "Warning: cannot write database to {}: {}",
                    filename.display(),
                    e
                )
            }
        }
        Err(e) => eprintln!(
            "Warning: cannot write database to {}: {}",
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
            Arg::with_name("database_file")
                .help("Path to database file")
                .required(true),
        )
        .subcommand(
            SubCommand::with_name("wiki")
                .about("Run a server with a wiki-like interface to the database")
                .arg(
                    Arg::with_name("addr")
                        .help("Address on which the server will bind")
                        .default_value("127.0.0.1:8000"),
                ),
        )
        .get_matches();

    // TODO useful tooling: merge of files

    let database_filepath = Path::new(matches.value_of_os("database_file").unwrap());

    match matches.subcommand() {
        ("wiki", Some(args)) => {
            let addr = args.value_of("addr").unwrap();
            wiki::run(addr, database_filepath)
        }
        _ => panic!("Missing subcommand"),
    }
}
