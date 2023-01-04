use super::compiler::Compile;
use crate::parser::procedure::procedure;
use nom::error::VerboseError;
use siryis;
use std::{fs::read_to_string, time::Instant};
use zub::{
    ir::{Binding, IrBuilder},
    vm::{Heap, Object, Value, VM},
};

pub fn run() {
    let start = Instant::now();

    let input = read_to_string("main.srs").unwrap();
    let (_, parsed) = procedure::<VerboseError<&str>>(&input).unwrap();

    let builder = &mut IrBuilder::new();

    let compiled = parsed.compile(builder);
    let call = builder.call(compiled, Vec::new(), None);
    builder.emit(call);

    let mut vm = VM::new();

    vm.add_native("Say", print, 1);
    vm.exec(&builder.build(), true);
    let elapsed = Instant::now().duration_since(start);

    println!("Parsing + Compile + Exec: {}ms", elapsed.as_millis());
}

fn print(heap: &mut Heap<Object>, args: &[Value]) -> Value {
    let data = args[1].with_heap(heap);
    let handle = data.item.as_object().unwrap();

    let data = match unsafe { handle.get_unchecked() } {
        Object::List(list) => list,
        _ => {
            println!("Expected list in print call!");
            return Value::nil();
        }
    };
    let data = data.content.clone();

    for arg in data {
        print!("{:} ", arg.with_heap(heap));
    }
    print!("\n");
    Value::nil()
}
