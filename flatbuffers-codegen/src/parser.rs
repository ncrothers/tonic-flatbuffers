use winnow::{
    combinator::{repeat, trace},
    error::StrContext,
    Parser,
};

use crate::flatbuffers::item::{Item, ItemParser};

pub fn parse_file(file: &str) -> anyhow::Result<Vec<Item>> {
    trace("parse_file", repeat(0.., ItemParser))
        .context(StrContext::Expected(
            winnow::error::StrContextValue::Description("flatbuffer statement"),
        ))
        .parse(file)
        .map_err(|e| anyhow::format_err!("{e}"))
}
