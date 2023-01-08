use super::{
    prelude::*,
    procedure::{procedure, Procedure}, Span,
};

pub(crate) struct Module<'a> {
    name: String,
    pub(crate) body: Vec<Procedure<'a>>
}

pub(crate) fn module<'a, E>(i: Span<'a>) -> IResult<Span<'a>, Module, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::num::ParseIntError>,
{
    let (i, _) = ws(tag("module"))(i)?;
    let (i, name) = identifier(i)?;

    let (i, body) = many0(procedure)(i)?;
    

    Ok((i, Module {
        name,
        body
    }))
}
