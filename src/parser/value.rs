use super::{prelude::*, string::escaped_string};
use nom::{character::complete::digit1, multi::separated_list0};
use nom_locate::position;

pub(crate) fn fragment_value<'a, E>(i: Span<'a>) -> IResult<Span<'a>, Fragment, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    ws(fragment_value_nws)(i)
}

pub(crate) fn fragment_value_nws<'a, E>(i: Span<'a>) -> IResult<Span<'a>, Fragment, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    alt((escaped_string, number, boolean, none, variable, list))(i)
}

fn number<'a, E>(i: Span<'a>) -> IResult<Span<'a>, Fragment, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    let (i, sign) = opt(alt((tag("+"), tag("-"))))(i)?;
    let sign = sign.map(|a| a.to_string()).unwrap_or("".into());

    let (i, decimal) = digit1(i)?;
    let (i, fractional) = opt(|i| {
        let (i, _) = tag(".")(i)?;
        digit1(i)
    })(i)?;

    let representation = match fractional {
        Some(fractional) => format!("{sign}{decimal}.{fractional}"),
        None => format!("{sign}{decimal}"),
    };

    let number = representation.parse::<f64>().unwrap();

    Ok((i, Fragment::Number(number)))
}

fn variable<'a, E>(i: Span<'a>) -> IResult<Span<'a>, Fragment, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    let (i, ident) = identifier(i)?;
    let (i, pos) = position(i)?;
    Ok((i, Fragment::Variable(ident, pos)))
}

fn boolean<'a, E>(i: Span<'a>) -> IResult<Span<'a>, Fragment, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    alt((
        value(Fragment::Boolean(false), ws(tag("Lie"))),
        value(Fragment::Boolean(true), ws(tag("Truth"))),
    ))(i)
}

fn none<'a, E>(i: Span<'a>) -> IResult<Span<'a>, Fragment, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    value(Fragment::None, tag("None"))(i)
}

fn list<'a, E>(i: Span<'a>) -> IResult<Span<'a>, Fragment, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    map(
        delimited(
            tag("["),
            separated_list0(tag(","), fragment_value),
            tag("]"),
        ),
        |list| Fragment::List(list),
    )(i)
}
