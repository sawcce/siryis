use nom::multi::{many0, many1, separated_list1};

use super::prelude::*;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Procedure {
    pub(crate) name: String,
    pub(crate) body: Vec<Instruction>,
}

impl Procedure {
    pub(crate) fn named(name: &str) -> Self {
        Self {
            name: name.into(),
            body: Vec::new(),
        }
    }

    pub(crate) fn with_body(name: &str, body: Vec<Instruction>) -> Self {
        Self {
            name: name.into(),
            body,
        }
    }
}

pub(crate) fn procedure<'a, E>(i: &'a str) -> IResult<&'a str, Procedure, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (i, _) = ws(tag("procedure"))(i)?;
    let (i, name) = cut(identifier)(i)?;
    let (i, body) = opt(many0(instruction))(i)?;

    Ok((
        i,
        Procedure {
            name: name.to_string(),
            body: body.unwrap_or(Vec::new()),
        },
    ))
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Instruction {
    Call(Call),
    Assignment(Assignment),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Call {
    pub(crate) name: String,
    pub(crate) arguments: Vec<Fragment>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Assignment {
    pub(crate) name: String,
    pub(crate) value: Fragment,
}

impl Call {
    pub(crate) fn with_arguments(name: &str, arguments: Vec<Fragment>) -> Self {
        Self {
            name: name.into(),
            arguments,
        }
    }
}

fn instruction<'a, E>(i: &'a str) -> IResult<&'a str, Instruction, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (i, _) = ws(tag("->"))(i)?;
    alt((assignment, call))(i)
}

fn assignment<'a, E>(i: &'a str) -> IResult<&'a str, Instruction, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (i, name) = ws(identifier)(i)?;
    let (i, _) = ws(tag("="))(i)?;
    let (i, value) = cut(ws(fragment_value))(i)?;

    Ok((
        i,
        Instruction::Assignment(Assignment {
            name: name.into(),
            value,
        }),
    ))
}

fn call<'a, E>(i: &'a str) -> IResult<&'a str, Instruction, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (i, name) = ws(identifier)(i)?;
    let (i, arguments) = opt(args)(i)?;

    Ok((
        i,
        Instruction::Call(Call {
            name: name.into(),
            arguments: arguments.unwrap_or(Vec::new()),
        }),
    ))
}

fn args<'a, E>(i: &'a str) -> IResult<&'a str, Vec<Fragment>, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    let (i, _) = ws(tag(":"))(i)?;
    cut(separated_list1(tag(","), fragment_value))(i)
}
