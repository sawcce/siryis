use nom::{combinator::verify, multi::many1, Err};

use super::prelude::*;

pub(crate) fn identifier<'a, E: ParseError<Span<'a>>>(i: Span<'a>) -> IResult<Span<'a>, String, E> {
    let (i, identifier) = many1(alt((alpha1, tag("_"))))(i)?;
    let identifier = identifier
        .iter()
        .map(|value| String::from(value.to_string().clone()))
        .collect::<String>();

    if(identifier == "End") {
        return Err(Err::Error(ParseError::from_char(i, 'E')))
    }

    Ok((i, identifier))
}
