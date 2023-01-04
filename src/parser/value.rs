use super::{prelude::*, string::escaped_string};
use nom::multi::{fold_many0, fold_many1, fold_many_m_n, separated_list0};

pub(crate) fn fragment_value<'a, E>(i: &'a str) -> IResult<&'a str, Fragment, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    ws(alt((escaped_string, boolean, none, list)))(i)
}

pub(crate) fn boolean<'a, E>(i: &'a str) -> IResult<&'a str, Fragment, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    alt((
        value(Fragment::Boolean(false), ws(tag("Lie"))),
        value(Fragment::Boolean(true), ws(tag("Truth"))),
    ))(i)
}

pub(crate) fn none<'a, E>(i: &'a str) -> IResult<&'a str, Fragment, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
    value(Fragment::None, ws(tag("None")))(i)
}

pub(crate) fn list<'a, E>(i: &'a str) -> IResult<&'a str, Fragment, E>
where
    E: ParseError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
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
