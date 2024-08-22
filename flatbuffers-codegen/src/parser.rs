use winnow::{
    combinator::{repeat, trace},
    Parser,
};

use crate::flatbuffers::item::{item, Item};

pub fn parse_file(file: &str) -> anyhow::Result<Vec<Item>> {
    trace("parse_file", repeat(0.., item))
        .parse(file)
        .map_err(|e| anyhow::format_err!("{e}"))
}
