#![deny(rust_2018_idioms)]

use clap::{Parser, Subcommand};


mod compiler;
mod parser;
mod run;
use run::run;

#[derive(Subcommand, Debug)]
enum Command {
    #[command(about = "Runs the siryis project at current location")]
    Run,
    Build,
    Init,
}

#[derive(Parser, Debug)]
#[command(author = "Sawcce", version = "0.0.1", about, long_about = None)]
struct Args {
    #[arg(short, long, default_value_t = (".".into()))]
    root: String,
    #[command(subcommand)]
    command: Command,
}

fn main() {
    let args = Args::parse();

    match args.command {
        Command::Build => {
            todo!()
        }
        Command::Init => {
            todo!()
        }
        Command::Run => {run(args.root)}
    }
}


#[cfg(test)]
mod test;
