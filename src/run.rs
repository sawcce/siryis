use crate::compiler::{Error, Type, show_report};
use crate::parser::Span;
use crate::parser::procedure::procedure;
use crate::{compiler::Compiler, parser::module::module};
use nom::error::VerboseError;
use rand::{thread_rng, Rng};

use std::borrow::Borrow;
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};
use std::{fs::read_to_string, time::Instant};
use zub::{
    ir::{Binding, IrBuilder},
    vm::{Heap, Object, Value, VM},
};

pub fn run(root: String) {
    let start = Instant::now();
    let root = Path::new(&root);

    let main = root.join(Path::new("main.srs"));

    let code = read_to_string(main).unwrap();
    let input = Span::new(&code);
    let (i, module) = module::<nom::error::Error<Span<'_>>>(input).unwrap();
    let body = module.body;
    println!("{i} {body:?}");

    let builder = &mut IrBuilder::new();
    let mut compiler = Compiler::new(builder);

    compiler.add_method("Say", vec![Type::list(Type::Any)], Type::None);
    compiler.add_method("Debug", vec![Type::list(Type::Any)], Type::None);
    compiler.add_method("EnableDebug", vec![Type::Boolean], Type::None);
    compiler.add_method("Random", vec![Type::Number, Type::Number], Type::Number);


    /* let index = body.iter().position(|el| el.name == "Lifecycle").unwrap();
    let lifecycle = body.get(index);
    body.remove(index); */

    let mut errors = Vec::new();

    for procedure in body {
        let compiled = match compiler.procedure(&procedure) {
            Ok(ast) => ast,
            Err(err) => {
                let error = &mut err.clone();
                errors.append(error);
                continue;
            }
        };

        if procedure.name == "Lifecycle".to_string() {
            let node = compiler.builder.call(compiled, vec![], None);
            compiler.builder.emit(node);
        } else {
            compiler.builder.emit(compiled);
        }
    }

    if errors.len() > 0 {

        let path = Path::new("main.srs");
        let path = root.join(path);

        show_report(errors, path.to_str().unwrap(), &code);

        return;
    }

    let mut vm = VM::new();

    vm.add_native("EnableDebug", enable_debug, 1);
    vm.add_native("Debug", debug, 1);
    vm.add_native("Say", print, 1);
    vm.add_native("Random", random, 2);

    let built = builder.build();
    //println!("{built:?}");

    vm.exec(&built, true);

    let elapsed = Instant::now().duration_since(start);

    println!("Parsing + Compile + Exec: {}ms", elapsed.as_millis());
}

static debug_enabled: AtomicBool = AtomicBool::new(false);

fn enable_debug(heap: &mut Heap<Object>, args: &[Value]) -> Value {
    let enabled = args[1].truthy();
    debug_enabled.store(enabled, Ordering::SeqCst);
    Value::nil()
}

fn debug(heap: &mut Heap<Object>, args: &[Value]) -> Value {
    if debug_enabled.load(Ordering::SeqCst) == false {
        return Value::nil();
    }

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

fn random(heap: &mut Heap<Object>, args: &[Value]) -> Value {
    let min = args[1].with_heap(heap).item.as_float() as u64;
    let max = args[2].with_heap(heap).item.as_float() as u64;

    let number = thread_rng().gen_range(min..max);

    Value::float(number as f64)
}
