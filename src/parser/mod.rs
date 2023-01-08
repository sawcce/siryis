#![allow(rust_2018_idioms)]

use nom_locate::LocatedSpan;

pub(super) mod identifier;
pub(super) mod module;
pub(super) mod procedure;
pub(super) mod string;
pub(super) mod value;
pub(super) mod ws;

#[derive(Debug, Clone, PartialEq)]
pub(super) enum Fragment<'a> {
    Number(f64),
    String(String),
    List(Vec<Fragment<'a>>),
    Variable(String, Span<'a>),
    None,
    Boolean(bool),
}

pub(super) type Span<'a> = LocatedSpan<&'a str>;
pub(super) type Spans<'a> = Vec<Span<'a>>;

pub mod prelude {
    pub(super) use super::identifier::identifier;
    pub(super) use super::value::fragment_value;
    pub(super) use super::ws::ws;
    pub(super) use super::Fragment;
    pub(super) use super::{Span, Spans};
    pub(super) use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::multispace0,
        combinator::{cut, map, opt, value},
        error::FromExternalError,
        error::ParseError,
        multi::many0,
        sequence::delimited,
        IResult,
    };
    pub(super) use nom_unicode::complete::{alpha0, alpha1};
}
