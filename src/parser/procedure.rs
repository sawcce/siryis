use nom::multi::{many0, many1, separated_list1};
use nom_locate::position;

use super::{prelude::*, value::fragment_value_nws, ws::positioned_ws};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Procedure<'a> {
    pub(crate) name: String,
    pub(crate) args: Vec<String>,
    pub(crate) body: Vec<Instruction<'a>>,
    pub(crate) name_span: Span<'a>,
}

impl<'a> Procedure<'a> {
    /*  pub(crate) fn named(name: &str) -> Self {
        Self {
            name: name.into(),
            body: Vec::new(),
            args: Vec::new(),
            name_span: Span::<'a>::new(name)
        }
    }

    pub(crate) fn with_body(name: &str, body: Vec<Instruction>) -> Self {
        Self {
            name: name.into(),
            body,
            args: Vec::new(),
            name_span: Span::new(name)
        }
    } */
}

pub(crate) fn procedure<'a, E>(i: Span<'a>) -> IResult<Span<'a>, Procedure, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    let (i, _) = ws(tag("procedure"))(i)?;
    let (i, name) = cut(identifier)(i)?;
    let (i, name_span) = position(i)?;

    let (i, args) = opt(|i| {
        let (i, _) = ws(tag("needs"))(i)?;
        separated_list1(tag(","), identifier)(i)
    })(i)?;

    let (i, body) = many0(instruction)(i)?;

    let args = args.unwrap_or_default();

    Ok((
        i,
        Procedure {
            name: name.to_string(),
            body,
            args,
            name_span,
        },
    ))
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Instruction<'a> {
    Return(Fragment<'a>),
    Call(Call<'a>),
    Assignment(Assignment<'a>),
    Increment(Increment<'a>),
    Pipe(String, Call<'a>),
    Match(Fragment<'a>, Vec<(Fragment<'a>, Vec<Instruction<'a>>)>),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Call<'a> {
    pub(crate) name: String,
    pub(crate) arguments: Vec<Fragment<'a>>,
    pub(crate) spans: Spans<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Assignment<'a> {
    pub(crate) name: String,
    pub(crate) value: Fragment<'a>,
}

fn instruction<'a, E>(i: Span<'a>) -> IResult<Span<'a>, Instruction, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    alt((
        send_back,
        increment,
        assignment,
        pipe_call,
        matcher,
        map(call, |call| Instruction::Call(call)),
    ))(i)
}

fn send_back<'a, E>(i: Span<'a>) -> IResult<Span<'a>, Instruction, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    let (i, _) = ws(tag("<-"))(i)?;
    let (i, value) = ws(fragment_value)(i)?;

    Ok((i, Instruction::Return(value)))
}

fn assignment<'a, E>(i: Span<'a>) -> IResult<Span<'a>, Instruction, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    let (i, _) = ws(tag("="))(i)?;
    let (i, name) = identifier(i)?;
    let (i, _) = ws(tag(":"))(i)?;
    let (i, value) = cut(ws(fragment_value))(i)?;

    Ok((
        i,
        Instruction::Assignment(Assignment {
            name: name.into(),
            value,
        }),
    ))
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Increment<'a> {
    pub(crate) name: String,
    pub(crate) value: Fragment<'a>,
    pub(crate) spans: Spans<'a>,
}

fn increment<'a, E>(i: Span<'a>) -> IResult<Span<'a>, Instruction, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    let (i, _) = ws(tag("+="))(i)?;
    let (i, (name, ns, ne)) = positioned_ws(identifier)(i)?;
    let (i, _) = opt(ws(tag(":")))(i)?;
    let (i, (value, vs, ve)) = cut(positioned_ws(fragment_value))(i)?;

    Ok((
        i,
        Instruction::Increment(Increment {
            name: name.into(),
            value,
            spans: vec![ns, ne, vs, ve],
        }),
    ))
}

fn matcher<'a, E>(i: Span<'a>) -> IResult<Span<'a>, Instruction, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    let (i, _) = ws(tag("?"))(i)?;
    let (i, value) = cut(fragment_value)(i)?;
    let (i, _) = ws(cut(tag(":")))(i)?;

    let (i, branches) = cut(many1(branch))(i)?;
    let branches = branches.into_iter().flatten().collect();

    let (i, _) = cut(ws(tag("End")))(i)?;

    Ok((i, Instruction::Match(value, branches)))
}

fn branch<'a, E>(i: Span<'a>) -> IResult<Span<'a>, Vec<(Fragment, Vec<Instruction>)>, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    let (i, values) = separated_list1(tag(","), fragment_value)(i)?;
    let (i, instructions) = many0(instruction)(i)?;

    let branches = values
        .iter()
        .map(|value| (value.clone(), instructions.clone()))
        .collect();

    Ok((i, branches))
}

fn pipe_call<'a, E>(i: Span<'a>) -> IResult<Span<'a>, Instruction, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    let (i, call) = call(i)?;
    let (i, _) = ws(tag("=>"))(i)?;
    let (i, name) = identifier(i)?;

    Ok((i, Instruction::Pipe(name.to_string(), call)))
}

fn call<'a, E>(i: Span<'a>) -> IResult<Span<'a>, Call, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    let (i, _) = ws(tag("->"))(i)?;
    let (i, (name, ns, ne)) = positioned_ws(identifier)(i)?;
    let (i, bvs) = position(i)?;
    let (i, result) = opt(args)(i)?;
    let (i, bve) = position(i)?;
    let (arguments, vs, ve) = result.unwrap_or((Vec::new(), bvs, bve));

    Ok((
        i,
        Call {
            name: name.into(),
            arguments,
            spans: vec![ns, ne, vs, ve],
        },
    ))
}

fn args<'a, E>(i: Span<'a>) -> IResult<Span<'a>, (Vec<Fragment>, Span<'a>, Span<'a>), E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    let (i, _) = ws(tag(":"))(i)?;
    positioned_ws(cut(separated_list1(ws(tag(",")), fragment_value_nws)))(i)
}
