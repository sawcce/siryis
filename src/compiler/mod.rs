use std::{
    collections::HashMap,
    fmt::Display,
    ops::{Deref, Range},
    sync::{Arc, Mutex},
};

use crate::parser::{
    procedure::{Assignment, Call, Instruction, Procedure},
    Span, Spans,
};
use ariadne::{Color, ColorGenerator, Label, Report, Source};
use clap::builder::Str;
use nom::{Offset, Slice};
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
    fn spanned<'a>(self, spans: Spans<'a>) -> CompileError<'a> {
        CompileError { error: self, spans }
    }

    fn code(&self) -> u8 {
        match self {
            Error::VariableNotFound(_) => 10,
            Error::TypeError { expected, got } => 20,
            Error::CallTypeError {
                name,
                expected,
                got,
            } => 30,
            Error::CallToNonFunction(_, _) => 40,
        }
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
pub(crate) struct CompileError<'a> {
    error: Error,
    spans: Spans<'a>,
}

impl<'a> CompileError<'a> {
    pub(crate) fn range_from(&self, index: usize) -> Range<usize> {
        let start = self.spans.get(index).unwrap().location_offset();
        let end = self.spans.get(index + 1).unwrap().location_offset();
        start..end
    }
}

pub(crate) fn show_report<'a>(errors: Vec<CompileError<'a>>, path: &str, source: &String) {
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    let b = colors.next();
    let out = Color::Fixed(81);

    for error in errors {
        let mut builder = Report::build(ariadne::ReportKind::Error, path, 0)
            .with_code(error.error.code())
            .with_message(format!("Error: {}", error.error));

        builder =
            match error.error {
                Error::VariableNotFound(ref name) => {
                    let range = error.range_from(0);

                    builder.with_label(Label::new((path, range)).with_color(a).with_message(
                        format!("Variable \"{name}\" does not exist in current scope"),
                    ))
                }
                Error::CallTypeError {
                    ref name,
                    ref expected,
                    ref got,
                } => {
                    let name_r = error.range_from(0);
                    let value_r = error.range_from(2);

                    builder
                        .with_label(
                            Label::new((path, name_r))
                                .with_color(a)
                                .with_message(format!(
                                    "Function \"{name}\" has a type signature of {expected:?}"
                                )),
                        )
                        .with_label(Label::new((path, value_r)).with_color(b).with_message(
                            format!("And arguments have a type signature of {got:?}"),
                        ))
                }
                Error::TypeError {
                    ref expected,
                    ref got,
                } => {
                    let got_r = error.range_from(0);

                    builder
                        .with_label(
                            Label::new((path, got_r))
                                .with_color(a)
                                .with_message(format!("Got type signature of: {got:?}")),
                        )
                        .with_note(format!("Expected type signature of: {expected:?}"))
                        .with_help(match got {
                            Type::String => "Try using the built-in \"Append\" function with the pipe operator",
                            _ => "Try changing the type of this to a number"
                        })
                }
                Error::CallToNonFunction(_, _) => todo!(),
            };

        let report = builder.finish();
        report.print((path, Source::from(source))).unwrap();
    }
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

    pub(crate) fn procedure<'a, 'p>(
        &mut self,
        procedure: &'p Procedure<'a>,
    ) -> Result<Node<Expr>, Vec<CompileError<'a>>> {
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
                    errors.push(err.unwrap().clone());
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

    pub(crate) fn instruction<'l, 'x>(
        &mut self,
        instruction: &'l Instruction<'x>,
    ) -> SirisResult<'x, Option<Node<Expr>>> {
        match instruction {
            Instruction::Assignment(assignment) => {
                self.assignment(&assignment);
                Ok(None)
            }
            Instruction::Pipe(name, call) => {
                let (args, ret) = self.verify_call_correctness(call)?;

                let node = self.call(call, args, ret.clone());
                self.insert_local(name.clone(), *ret);

                let binding = Binding::local(&name, self.values.len(), 0);
                self.builder.bind(binding, node?);

                Ok(None)
            }
            Instruction::Call(call) => {
                let (args, ret) = self.verify_call_correctness(call)?;
                Ok(Some(self.call(call, args, ret)?))
            }
            Instruction::Match(value, branches) => {
                let value = self.fragment(&value)?;
                let a = self.branches(&value, &mut branches.iter())?;
                //println!("{a:?}");
                Ok(Some(a))
            }
            Instruction::Increment(increment) => {
                let (depth, type_v) = spanned(
                    self.resolve_variable(increment.name.clone()),
                    increment.spans.clone(),
                )?;

                if type_v != Type::Number {
                    return Err(CompileError {
                        error: Error::TypeError {
                            expected: Type::Number,
                            got: type_v,
                        },
                        spans: increment.spans.clone(),
                    });
                } 

                let binding = Binding::local(&increment.name, depth, 1);
                let var = self.builder.var(binding);
                let increment_value = self.fragment(&increment.value)?;

                let span = (&increment.spans[2..4]).clone().to_vec();
                let type_v = spanned(self.get_type(increment.value.clone()), span.clone())?;
                if type_v != Type::Number {
                    return Err(CompileError {
                        error: Error::TypeError {
                            expected: Type::Number,
                            got: type_v,
                        },
                        spans: span,
                    });
                } 

                let operation = self.builder.binary(var.clone(), BinaryOp::Add, increment_value);
                self.builder.mutate(var, operation);
                Ok(None)
            }
            Instruction::Return(value) => {
                let value = self.fragment(&value)?;
                Ok(Some(self.builder.ret_(Some(value))))
            }
        }
    }

    fn branches<'l, 'a>(
        &mut self,
        compare: &Node<Expr>,
        branches: &mut core::slice::Iter<'_, (Fragment<'a>, Vec<Instruction<'a>>)>,
    ) -> SirisResult<'a, Node<Expr>> {
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

    fn assignment<'a>(&mut self, assignment: &'a Assignment<'a>) -> SirisResult<'a, ()> {
        let var_type = spanned(self.get_type(assignment.value.clone()), vec![Span::new("")])?;

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

    fn verify_call_correctness<'a, 'l>(
        &mut self,
        call: &'l Call<'a>,
    ) -> SirisResult<'a, (Vec<Type>, Box<Type>)> {
        let (_, f_type) = spanned(self.resolve_variable(call.name.clone()), call.spans.clone())?;

        let result = match f_type {
            Type::Function(arguments, return_type) => (arguments, return_type),
            _ => {
                return Err(
                    Error::CallToNonFunction(call.name.clone(), f_type).spanned(call.spans.clone())
                );
            }
        };

        Ok(result)
    }

    fn call<'z, 'a>(
        &mut self,
        call: &'z Call<'a>,
        args: Vec<Type>,
        ret: Box<Type>,
    ) -> SirisResult<'a, Node<Expr>> {
        let binding = Binding::global(&call.name);

        let variable = self.builder.var(binding);

        let mut types = Vec::new();
        let mut arguments = Vec::new();

        for argument in call.arguments.clone() {
            types.push(spanned(
                self.get_type(argument.clone()),
                call.spans.clone(),
            )?);
            arguments.push(self.fragment(&argument)?);
        }

        if args != types {
            return Err(Error::CallTypeError {
                name: call.name.clone(),
                expected: args,
                got: types,
            }
            .spanned(call.spans.clone()));
        }

        Ok(self.builder.call(variable, arguments, None))
    }

    fn fragment<'a, 'l>(&mut self, fragment: &'l Fragment<'a>) -> SirisResult<'a, Node<Expr>> {
        match fragment {
            Fragment::Variable(name, span) => Ok(self.fragment_var(name, span)?.0),
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

    fn fragment_var<'a>(
        &mut self,
        name: &String,
        span: &Span<'a>,
    ) -> SirisResult<'a, (ExprNode, Span<'a>)> {
        let depth = spanned(self.resolve_variable(name.to_string()), vec![*span])?.0;

        let binding = if depth == 0 {
            Binding::global(name)
        } else {
            Binding::local(name, depth, 1)
        };

        Ok((self.builder.var(binding), *span))
    }

    fn resolve_variable(&mut self, name: String) -> Result<(usize, Type), Error> {
        let iterator = self.values.iter().rev();
        for (i, map) in iterator.enumerate() {
            if let Some(var_type) = map.lock().unwrap().get(&name) {
                return Ok((self.values.len() - (i + 1), var_type.clone()));
            }
        }

        Err(Error::VariableNotFound(name))
    }

    fn get_type<'a>(&mut self, fragment: Fragment<'a>) -> Result<Type, Error> {
        Ok(match fragment {
            Fragment::Number(_) => Type::Number,
            Fragment::Boolean(_) => Type::Boolean,
            Fragment::List(_) => Type::list(Type::Any),
            Fragment::None => Type::None,
            Fragment::String(_) => Type::String,
            Fragment::Variable(name, _) => self.resolve_variable(name.clone())?.1,
        })
    }
}

fn spanned<'a, T>(result: Result<T, Error>, span: Spans<'a>) -> SirisResult<'a, T> {
    match result {
        Ok(value) => Ok(value),
        Err(error) => Err(error.spanned(span)),
    }
}
