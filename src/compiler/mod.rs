use std::{
    collections::HashMap,
    fmt::Display,
    ops::Deref,
    sync::{Arc, Mutex},
};

use crate::parser::{
    procedure::{Assignment, Call, Instruction, Procedure},
    Span,
};
use clap::builder::Str;
use zub::{
    ir::{BinaryOp, Binding, Expr, ExprNode, IrBuilder, Literal, Node, TypeInfo},
    vm::Value,
};

type PType = zub::ir::Type;
use super::parser::Fragment;

#[derive(Clone, Debug)]
pub(crate) enum Error {
    VariableNotFound(String),
    TypeError {
        expected: Type,
        got: Type,
    },
    CallTypeError {
        name: String,
        expected: Vec<Type>,
        got: Vec<Type>,
    },
    CallToNonFunction(String, Type),
}

impl Error {
    fn spanned(self, span: Span) -> CompileError {
        CompileError { error: self, span }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VariableNotFound(name) => write!(f, "Reference to undeclared variable: {}", name),
            Self::CallToNonFunction(name, actual_type) => {
                write!(f, "Trying to call variable {name} of type {actual_type:?}")
            }
            Self::TypeError { expected, got } => write!(f, "Expected {expected:?} got: {got:?}"),
            Self::CallTypeError {
                name,
                expected,
                got,
            } => write!(f, "In call to: {name}, expected {expected:?} got: {got:?}"),
        }
    }
}

#[derive(Clone, Debug)]
struct CompileError<'a> {
    error: Error,
    span: Span<'a>,
}

type SirisResult<'a, T> = Result<T, CompileError<'a>>;

#[derive(Clone)]
#[repr(u8)]
pub(crate) enum Type {
    List(Box<Type>),
    Function(Vec<Type>, Box<Type>),
    Any,
    Unknown,
    String,
    Boolean,
    Number,
    None,
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        use Type::*;

        if let Any = other {
            return true;
        }

        match self {
            Function(args, ret) => {
                if let Function(o_args, o_ret) = other {
                    args == o_args && ret == o_ret
                } else {
                    false
                }
            }
            List(types) => {
                if let List(others) = other {
                    others == types
                } else {
                    false
                }
            }
            Any => true,
            Unknown => matches!(other, Unknown),
            String => matches!(other, String),
            Boolean => matches!(other, Boolean),
            Number => matches!(other, Number),
            None => matches!(other, None),
        }
    }
}

impl Type {
    pub(crate) fn list(inner: Type) -> Self {
        Self::List(Box::new(inner))
    }

    fn function(arguments: Vec<Type>, return_type: Type) -> Self {
        Self::Function(arguments, Box::new(return_type))
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Any => write!(f, "Any"),
            Self::Unknown => write!(f, "Unknown"),
            Self::String => write!(f, "String"),
            Self::Boolean => write!(f, "Boolean"),
            Self::Number => write!(f, "Number"),
            Self::None => write!(f, "None"),
            Self::Function(arguments, return_type) => {
                write!(f, "Fn({arguments:?}) -> {return_type:?}")
            }
            Self::List(elements) => write!(f, "List({elements:?})"),
        }
    }
}

pub(crate) struct Compiler<'b> {
    pub(crate) builder: &'b mut IrBuilder,
    values: Vec<Arc<Mutex<HashMap<String, Type>>>>,
}

impl<'b> From<&'b mut IrBuilder> for Compiler<'b> {
    fn from(builder: &'b mut IrBuilder) -> Self {
        Compiler {
            builder,
            values: Vec::new(),
        }
    }
}

impl<'b> Compiler<'b> {
    pub(crate) fn new(builder: &'b mut IrBuilder) -> Self {
        Compiler {
            builder,
            values: vec![Arc::new(Mutex::new(HashMap::new()))],
        }
    }

    pub(crate) fn copy(&self, builder: &'b mut IrBuilder) -> Self {
        Compiler {
            builder,
            values: self.values.clone(),
        }
    }

    pub(crate) fn copy_mut(&mut self, builder: &'b mut IrBuilder) -> Self {
        Compiler {
            builder,
            values: self.values.clone(),
        }
    }

    pub(crate) fn add_method(&mut self, name: impl ToString, args: Vec<Type>, return_type: Type) {
        self.values
            .last()
            .unwrap()
            .lock()
            .unwrap()
            .insert(name.to_string(), Type::function(args, return_type));
    }

    pub(crate) fn procedure(
        &mut self,
        procedure: &Procedure,
    ) -> Result<Node<Expr>, Vec<CompileError>> {
        let binding = Binding {
            name: procedure.name.clone(),
            depth: None,
            function_depth: 0,
        };

        let map = Arc::new(Mutex::new(HashMap::new()));
        let scope = map.clone();

        self.values.push(map);

        let mut argument_types = Vec::new();
        for argument in procedure.args.clone() {
            let mut scope = scope.lock().unwrap();
            scope.insert(argument, Type::Any);
            argument_types.push(Type::Any);
        }

        let errors = Arc::new(Mutex::new(Vec::new()));

        let body = |builder: &mut IrBuilder| {
            let mut compiler = Compiler::new(builder);
            compiler.values = self.values.clone();

            //println!("{:?}", self.values);
            let errors = errors.clone();
            let mut errors = errors.lock().unwrap();

            let body = procedure.body.clone();
            for instruction in body {
                let compiled = compiler.instruction(&instruction);
                if compiled.is_err() {
                    let err = compiled.err();
                    errors.push(err.unwrap());
                    continue;
                }

                let compiled = compiled.unwrap();
                if compiled.is_some() {
                    compiler.builder.emit(compiled.unwrap());
                }
            }

            let mut has_return = false;
            for instruction in procedure.body.clone() {
                if let Instruction::Return(value) = instruction {
                    has_return = true;
                    break;
                }
            }

            if !has_return {
                compiler.builder.ret(None);
            }
        };

        let args = procedure
            .args
            .iter()
            .map(|value| value.as_str())
            .collect::<Vec<_>>();
        let args = args.as_slice();

        let ret = self.builder.function(binding, args, body);
        self.values.pop();
        self.values.last().unwrap().clone().lock().unwrap().insert(
            procedure.name.clone(),
            Type::function(argument_types, Type::Any),
        );

        if errors.clone().lock().unwrap().len() > 0 {
            return Err(errors.lock().unwrap().to_vec());
        }

        Ok(ret)
    }

    pub(crate) fn instruction(
        &mut self,
        instruction: &Instruction,
    ) -> SirisResult<Option<Node<Expr>>> {
        match instruction {
            Instruction::Assignment(assignment) => {
                self.assignment(&assignment);
                Ok(None)
            }
            Instruction::Pipe(name, call) => {
                let (args, ret) = self.verify_call_correctness(call)?;

                let node = self.call(&call, args, ret.clone());
                self.insert_local(name.clone(), *ret);

                let binding = Binding::local(&name, self.values.len(), 0);
                self.builder.bind(binding, node?);

                Ok(None)
            }
            Instruction::Call(call) => {
                let (args, ret) = self.verify_call_correctness(call)?;
                Ok(Some(self.call(&call, args, ret)?))
            }
            Instruction::Match(value, branches) => {
                let value = self.fragment(&value)?;
                let a = self.branches(&value, &mut branches.iter())?;
                //println!("{a:?}");
                Ok(Some(a))
            }
            Instruction::Increment(name, value) => {
                let binding = Binding::local(&name, spanned(self.resolve_variable(name.clone()), )?.0, 1);
                let var = self.builder.var(binding);
                let increment = self.fragment(value)?;
                let operation = self.builder.binary(var.clone(), BinaryOp::Add, increment);
                self.builder.mutate(var, operation);
                Ok(None)
            }
            Instruction::Return(value) => {
                let value = self.fragment(value)?;
                Ok(Some(self.builder.ret_(Some(value))))
            }
            a => panic!("Unexpected: {a:?}"),
        }
    }

    fn branches(
        &mut self,
        compare: &Node<Expr>,
        branches: &mut core::slice::Iter<(Fragment, Vec<Instruction>)>,
    ) -> SirisResult<Node<Expr>> {
        println!("Branches!");
        let current = branches.nth(0).unwrap();
        let value = self.fragment(&current.0)?;

        let builder = &mut IrBuilder::new();
        let compiler = Compiler::copy(&self, builder);

        for instruction in &current.1 {
            let compiled = self.instruction(instruction)?;

            if compiled.is_some() {
                compiler.builder.emit(compiled.unwrap());
            }
        }

        let cmp = self.builder.binary(compare.clone(), BinaryOp::Equal, value);

        let then_body = self.builder.block(compiler.builder.build());
        let else_body = if branches.len() == 0 {
            None
        } else {
            Some(self.branches(compare, branches)?)
        };

        Ok(Expr::If(cmp, then_body, else_body).node(TypeInfo::nil()))

        //self.builder.if_(cmp, Box::new(truthy), Some(Self::elsy))
    }

    fn assignment(&mut self, assignment: &Assignment) -> SirisResult<()> {
        let var_type = self.get_type(&assignment.value)?;

        self.insert_local(assignment.name.to_string(), var_type);

        let binding = Binding::local(&assignment.name, self.values.len() - 1, 0);
        let value = self.fragment(&assignment.value)?;
        self.builder.bind(binding, value);
        Ok(())
    }

    fn insert_local(&mut self, name: String, var_type: Type) {
        let map = self.values.last();

        let map = map.unwrap();
        map.lock().unwrap().insert(name.clone(), var_type);
    }

    fn verify_call_correctness(&mut self, call: &Call) -> SirisResult<(Vec<Type>, Box<Type>)> {
        let (_, f_type) = self.resolve_variable(call.name.clone(), call.location)?;

        let result = match f_type {
            Type::Function(arguments, return_type) => (arguments, return_type),
            _ => {
                return Err(
                    Error::CallToNonFunction(call.name.clone(), f_type).spanned(call.location)
                );
            }
        };

        Ok(result)
    }

    fn call(&mut self, call: &Call, args: Vec<Type>, ret: Box<Type>) -> SirisResult<Node<Expr>> {
        let binding = Binding::global(&call.name);

        let variable = self.builder.var(binding);

        let mut types = Vec::new();

        let arguments = call
            .arguments
            .iter()
            .map(|argument| {
                types.push(spanned(self.get_type(argument), call.location)?);
                self.fragment(argument)
            })
            .collect::<Result<Vec<_>, _>>()?;

        if args != types {
            return Err(Error::CallTypeError {
                name: call.name.clone(),
                expected: args,
                got: types,
            }
            .spanned(call.location));
        }

        Ok(self.builder.call(variable, arguments, None))
    }

    fn fragment(&mut self, fragment: &Fragment) -> SirisResult<Node<Expr>> {
        match fragment {
            Fragment::Variable(name, span) => {
                let depth = spanned(self.resolve_variable(name.to_string()), *span)?.0;

                let binding = if depth == 0 {
                    Binding::global(name)
                } else {
                    Binding::local(name, depth, 1)
                };

                Ok(self.builder.var(binding))
            }
            Fragment::Boolean(value) => Ok(self.builder.bool(*value)),
            Fragment::None => Ok(Expr::Literal(Literal::Nil).node(TypeInfo::new(PType::Nil))),
            Fragment::Number(number) => Ok(self.builder.number(*number)),
            Fragment::String(string) => Ok(self.builder.string(&string)),
            Fragment::List(elements) => {
                let list = elements
                    .iter()
                    .map(|element| self.fragment(element))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(self.builder.list(list))
            }
        }
    }

    fn resolve_variable(&mut self, name: String) -> Result<(usize, Type), Error> {
        let iterator = self.values.iter().rev();
        for (i, map) in iterator.enumerate() {
            if let Some(var_type) = map.lock().unwrap().get(&name) {
                return Ok((self.values.len() - (i + 1), var_type.clone()));
            }
        }

        println!("Not found! {name}");
        Err(Error::VariableNotFound(name))
    }

    fn get_type(&mut self, fragment: &Fragment) -> Result<Type, Error> {
        Ok(match fragment {
            Fragment::Number(_) => Type::Number,
            Fragment::Boolean(_) => Type::Boolean,
            Fragment::List(_) => Type::list(Type::Any),
            Fragment::None => Type::None,
            Fragment::String(_) => Type::String,
            Fragment::Variable(name) => self.resolve_variable(name.clone())?.1,
            t => Type::Unknown,
        })
    }
}

fn spanned<T>(result: Result<T, Error>, span: Span) -> SirisResult<T> {
    match result {
        Ok(value) => Ok(value),
        Err(error) => Err(error.spanned(span)),
    }
}
