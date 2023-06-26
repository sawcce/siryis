use crate::compiler::{show_report, Type};
use crate::parser::Span;

use crate::project::parse_project;
use crate::{compiler::Compiler, parser::module::module};

use rand::{thread_rng, Rng};
use zubbers::vm::{Function, CallContext};

use std::io;
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};
use std::{fs::read_to_string, time::Instant};
use zubbers::{
    ir::IrBuilder,
    vm::{Heap, Object, Value, VM},
};

pub fn run(root: String) -> io::Result<()> {
    let project = parse_project(&root);

    println!(
        "Running: {}, version {}",
        project.data.name, project.data.version
    );

    let start = Instant::now();
    let root = Path::new(&root);
    let main = root.join(Path::new("src/main.srs"));

    let code = read_to_string(&main)
        .inspect_err(|_e| eprintln!("Could not open file {}", main.to_str().unwrap()))?;

    let input = Span::new(&code);
    let (i, module) = module::<nom::error::Error<Span<'_>>>(input).unwrap();
    let body = module.body;

    let builder = &mut IrBuilder::new();
    let mut compiler = Compiler::new(builder);

    compiler.add_method("Say", vec![Type::list(Type::Any)], Type::None);
    compiler.add_method("Debug", vec![Type::list(Type::Any)], Type::None);
    compiler.add_method("EnableDebug", vec![Type::Boolean], Type::None);
    compiler.add_method("Random", vec![Type::Number, Type::Number], Type::Number);
    compiler.add_method(
        "Foreach",
        vec![
            Type::function(vec![Type::Any], Type::None),
            Type::list(Type::Any),
        ],
        Type::None,
    );

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

        return Ok(());
    }

    let mut vm = VM::new();

    vm.add_native("EnableDebug", enable_debug, 1);
    vm.add_native("Debug", debug, 1);
    vm.add_native("Say", print, 1);
    vm.add_native("Random", random, 2);
    vm.add_native("Foreach", foreach, 2);

    let built = builder.build();
    //println!("{built:?}");

    vm.exec(&built, true);

    let elapsed = Instant::now().duration_since(start);

    println!("Parsing + Compile + Exec: {}ms", elapsed.as_millis());

    Ok(())
}

static DEBUG_ENABLED: AtomicBool = AtomicBool::new(false);

use zubbers::vm::Callable;

fn enable_debug(context: &mut CallContext<'_>) -> Value {
    let enabled = context.get_arg(1).truthy();
    DEBUG_ENABLED.store(enabled, Ordering::SeqCst);
    Value::nil()
}

fn debug(context: &mut CallContext<'_>) -> Value {
    if DEBUG_ENABLED.load(Ordering::SeqCst) == false {
        return Value::nil();
    }

    let data = context.get_arg_with_heap(1);
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
        print!("{:} ", context.with_heap(arg));
    }
    print!("\n");
    Value::nil()
}

macro_rules! unwrap_as {
    ($data: expr, $tt: path) => {{
        let b = unsafe { $data.get_unchecked() };

        let a = match b {
            $tt(a) => a,
            _ => {
                println!("Expected $tt in call!");
                return Value::nil();
            }
        };

        drop(b);

        a
    }};
}

fn foreach(context: &mut CallContext<'_>) -> Value {
    let function = context.get_arg(1).as_object().unwrap();

    let list = context.get_arg_with_heap(2).item.as_object().unwrap();
    
    let list = if let Object::List(list) = unsafe {list.get_unchecked()} {
        list
    } else {
        println!("Expected list in call to foreach");
        return Value::nil();
    };

    for value in list.content.clone() {
        let ret_val = context.call(function, vec![value]);
        println!("Got: {}", context.with_heap(ret_val));
    }

    Value::nil()
}

fn print(context: &mut CallContext<'_>) -> Value {
    let data = context.get_arg(1);
    let handle = data.as_object().unwrap();

    let data = match unsafe { handle.get_unchecked() } {
        Object::List(list) => list,
        _ => {
            println!("Expected list in print call!");
            return Value::nil();
        }
    };

    let data = data.content.clone();

    for arg in data {
        print!("{:} ", arg.with_heap(&context.vm.heap));
    }
    print!("\n");
    Value::nil()
}

fn random(context: &mut CallContext<'_>) -> Value {
    let min = context.get_arg_with_heap(1).item.as_float() as u64;
    let max = context.get_arg_with_heap(2).item.as_float() as u64;

    let number = thread_rng().gen_range(min..max);

    Value::float(number as f64)
}
