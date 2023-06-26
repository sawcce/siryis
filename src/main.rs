#![deny(rust_2018_idioms)]
#![feature(result_option_inspect)]

use std::io;

use clap::{Parser, Subcommand};

mod compiler;
mod parser;
mod run;
mod project;
use run::run;

#[derive(Subcommand, Debug)]
#[command(name = "Action")]
enum Command {
    #[command(about = "Runs the siryis project at current location")]
    Run {
        root: Option<String>,
    },
    Init {
        target: Option<String>,
    },
    Build,
}

#[derive(Parser, Debug)]
#[command(author = "Sawcce", version = "0.0.1", about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

fn main() -> io::Result<()> {
    let args = Args::parse();

    match args.command {
        Command::Init { target } => {
            println!("Creating new project at: {:?}", target);
        }
        Command::Build => {
            todo!()
        }
        Command::Run { root } => run(root.unwrap_or(".".into()))?,
    }

    Ok(())
}

#[cfg(test)]
mod test;
