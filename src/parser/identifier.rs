use super::prelude::*;

pub(crate) fn identifier<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    alpha1(i)
}