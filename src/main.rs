#![feature(proc_macro_hygiene)]

// Wiki
extern crate hyper;
extern crate maud; // HTML template engine
extern crate percent_encoding;
extern crate signal_hook;
extern crate tokio;

#[macro_use]
extern crate clap; // Command line parser

/// Knowledge database as a set of sentences.
mod relations;
use relations::Database;

/// Wiki interface
mod wiki;
use std::borrow::Cow;
use std::fs::File;
use std::io;
use std::path::{Path, PathBuf};
use std::time::Duration;

fn read_database_from_file(filename: &Path) -> Database {
    match File::open(filename) {
        Ok(file) => match Database::read_from(io::BufReader::new(file)) {
            Ok(database) => return database,
            Err(e) => eprintln!(
                "[warning] Invalid database format in file {}: {}",
                filename.display(),
                e
            ),
        },
        Err(e) => eprintln!(
            "[warning] Cannot read database from {}: {}",
            filename.display(),
            e
        ),
    }
    eprintln!(
        "[database] File {} not present; using empty database",
        filename.display()
    );
    Database::new()
}
fn write_database_to_file(filename: &Path, database: &Database) {
    File::create(filename)
        .and_then(|f| database.write_to(io::BufWriter::new(f)))
        .unwrap_or_else(|e| {
            eprintln!(
                "[warning] Cannot write database to {}: {}",
                filename.display(),
                e
            )
        })
}

fn main() -> Result<(), String> {
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
                        .default_value("0.0.0.0:8000"),
                )
                .arg(
                    Arg::with_name("backup_file")
                        .help("Path used for backup database file")
                        .long("backup"),
                )
                .arg(
                    Arg::with_name("autosave")
                        .help("Interval (in minutes) between writing the database to disk")
                        .long("autosave")
                        .value_name("interval")
                        .default_value("10"),
                ),
        )
        .get_matches();

    // TODO useful tooling: merge of files

    let database_filepath = Path::new(matches.value_of_os("database_file").unwrap());

    match matches.subcommand() {
        ("wiki", Some(args)) => {
            let addr = {
                let addr = args.value_of("addr").unwrap();
                match addr.parse() {
                    Ok(addr) => addr,
                    _ => return Err(format!("Unable to parse address: {}", addr)),
                }
            };
            let backup_filepath = match matches.value_of_os("backup_file") {
                Some(path) => Cow::Borrowed(Path::new(path)),
                None => {
                    let mut path = database_filepath.as_os_str().to_owned();
                    path.push(".bak");
                    Cow::Owned(PathBuf::from(path))
                }
            };
            let autosave_duration = {
                let minutes_text = args.value_of("autosave").unwrap();
                let minutes: u64 = match minutes_text.parse() {
                    Ok(minutes) if minutes > 0 => minutes,
                    _ => {
                        return Err(format!(
                            "Unable to parse positive number for autosave interval: {}",
                            minutes_text
                        ))
                    }
                };
                Duration::from_secs(minutes * 60)
            };
            eprintln!("[addr] {}", addr);
            eprintln!("[database file] {}", database_filepath.display());
            eprintln!("[backup file] {}", backup_filepath.display());
            wiki::run(
                &addr,
                database_filepath,
                &backup_filepath,
                autosave_duration,
            )
        }
        _ => Err("Missing subcommand".into()),
    }
}
