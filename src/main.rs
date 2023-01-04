use clap::{Arg, Parser, Subcommand};
use zub::{
    ir::{BinaryOp, Binding, Expr, IrBuilder, Node},
    vm::{Heap, Object, Value, Variant, VM},
};

use crate::parser::Fragment;

mod compiler;
mod parser;
mod run;
use run::run;

#[derive(Subcommand, Debug)]
enum Command {
    #[command(about = "Runs the siryis project at current location")]
    Run,
    Build,
}

#[derive(Parser, Debug)]
#[command(author = "Sawcce", version = "0.0.1", about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

fn main() {
    let args = Args::parse();

    match args.command {
        Command::Build => {
            todo!()
        }
        Command::Run => {run()}
    }

    println!("{args:?}");
/* 
    let mut builder = IrBuilder::new();

    let call = Call {
        name: "print".to_string(),
        arguments: vec![Fragment::List(vec![Fragment::String(
            "Hello, world!".into(),
        )])],
    };

    let a = builder.number(20.0);
    let b = builder.number(30.0);

    let sum = builder.binary(a, BinaryOp::Add, b);

    let compiled = call.compile(&mut builder);
    builder.emit(compiled);

    let sum_b = Binding::global("sum");
    builder.bind(sum_b.clone(), sum);
    let sum_var = builder.var(sum_b);

    let print_b = Binding::global("print");
    let print_v = builder.var(print_b);
    let list = builder.list(vec![sum_var.clone(), sum_var]);
    let a = builder.call(print_v, vec![list], None);
    builder.emit(a);

    let mut vm = VM::new();
    let ir = &builder.build();
    println!("{ir:?}"); */
}


#[cfg(test)]
mod test;
