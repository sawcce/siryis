use nom::{
    bytes::{streaming::is_not},
    character::{complete::multispace1},
};
use nom_locate::position;

use super::prelude::*;

pub(crate) fn positioned_ws<'a, F, O, E: ParseError<Span<'a>>>(
    mut inner: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, (O, Span<'a>, Span<'a>), E>
where
    F: FnMut(Span<'a>) -> IResult<Span<'a>, O, E>,
{
    delimited(many0(empty),  move |i| {
        let (i, first_span) = position(i)?;
        let (i, x) = inner(i)?;
        let (i, second_span) = position(i)?;
        Ok((i, (x, first_span, second_span)))
    }, many0(empty))
}

pub(crate) fn ws<'a, F, O, E: ParseError<Span<'a>>>(
    inner: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, O, E>
where
    F: FnMut(Span<'a>) -> IResult<Span<'a>, O, E>,
{
    delimited(many0(empty), inner, many0(empty))
}

pub(crate) fn empty<'a, E>(i: Span<'a>) -> IResult<Span<'a>, (), E>
where
    E: ParseError<Span<'a>>,
{
    alt((
        value((), multispace1),
        value((), comment),
    ))(i)
}

pub(crate) fn comment<'a, E>(i: Span<'a>) -> IResult<Span<'a>, Span, E>
where
    E: ParseError<Span<'a>>,
{
    delimited(tag("{"), is_not("}"), tag("}"))(i)
}

pub(crate) fn not_closer(i: char) -> bool {
    i != '}'
}
