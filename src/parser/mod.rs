pub(crate) mod identifier;
pub(crate) mod procedure;
pub(crate) mod string;
pub(crate) mod value;
pub(crate) mod ws;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Fragment {
    Number(f64),
    String(String),
    List(Vec<Fragment>),
    None,
    Boolean(bool),
}

pub mod prelude {
    pub(crate) use super::identifier::identifier;
    pub(crate) use super::value::fragment_value;
    pub(crate) use super::ws::ws;
    pub(crate) use super::Fragment;
    pub(crate) use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::multispace0,
        combinator::{cut, map, opt, value},
        error::FromExternalError,
        error::ParseError,
        sequence::{delimited},
        multi::many0,
        IResult,
    };
    pub(crate) use nom_unicode::complete::{alpha0, alpha1};
}
