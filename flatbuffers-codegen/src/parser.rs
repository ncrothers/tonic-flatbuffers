use winnow::{
    combinator::{repeat, trace},
    Parser,
};

use crate::flatbuffers::item::{item, Item};

pub fn collect_includes(file: &str) -> Vec<&str> {
    let pat = regex::Regex::new(r#"^[ \t]*include[ \t\n]+"([\.\w]+\.fbs)""#).unwrap();

    pat.captures_iter(file)
        .flat_map(|val| val.iter().skip(1).collect::<Vec<_>>())
        .filter_map(|val| val.map(|x| x.as_str()))
        .collect()
}

pub fn parse_file(file: &str) -> anyhow::Result<Vec<Item>> {
    trace("parse_file", repeat(0.., item))
        .parse(file)
        .map_err(|e| anyhow::format_err!("{e}"))
}
