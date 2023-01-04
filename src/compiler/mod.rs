use std::collections::HashMap;

use crate::parser::procedure::{Instruction, Procedure};
use clap::builder::Str;
use zub::{ir::{Binding, Expr, IrBuilder, Node, Literal, TypeInfo}, vm::Value};

type PType = zub::ir::Type;
use super::parser::Fragment;

enum Type {
    Primitive(PType),
}

struct Compiler<'b> {
    builder: &'b mut IrBuilder,
    values: HashMap<String, Option<SType>>
}

impl Compiler {
    pub(crate) fn new() -> Self {
    }
}

pub(crate) trait Compile {
    fn compile(&self, builder: &mut IrBuilder) -> Node<Expr>;
}

impl Compile for Procedure {
    fn compile(&self, builder: &mut IrBuilder) -> Node<Expr> {
        let binding = Binding::global(&self.name);

        let body = |builder: &mut IrBuilder| {
            let body = self.body.clone();
            for instruction in body {
                let compiled = instruction.compile(builder);
                builder.emit(compiled);
            }
            builder.ret(None);
        };

        builder.function(binding, &[], body)
    }
}

impl Compile for Instruction {
    fn compile(&self, builder: &mut IrBuilder) -> Node<Expr> {
        match self {
            Instruction::Assignment(assignment) => {
                panic!("fezfezf")
            }
            Instruction::Call(call) => {
                let binding = Binding::global(&call.name);
                let variable = builder.var(binding);
                let arguments = call
                    .arguments
                    .iter()
                    .map(|argument| argument.compile(builder))
                    .collect();

                builder.call(variable, arguments, None)
            }

        }
    }
}

impl Compile for Fragment {
    fn compile(&self, builder: &mut IrBuilder) -> Node<Expr> {
        match self {
            Self::Boolean(value) => builder.bool(*value),
            Self::None => Expr::Literal(Literal::Nil).node(TypeInfo::new(Type::Nil)),
            Self::Number(number) => builder.number(*number),
            Self::String(string) => builder.string(string),
            Self::List(elements) => {
                let list = elements
                    .iter()
                    .map(|element| element.compile(builder))
                    .collect();

                builder.list(list)
            }
        }
    }
}
