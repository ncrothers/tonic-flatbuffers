use winnow::{
    combinator::{repeat, trace},
    error::{ContextError, ParseError},
    Parser,
};

use crate::flatbuffers::item::{Item, ItemParser};

pub fn parse_file(file: &str) -> anyhow::Result<Vec<Item>> {
    trace("parse_file", repeat(0.., ItemParser))
        .parse(file)
        .map_err(|e| anyhow::format_err!("{e}"))
}
