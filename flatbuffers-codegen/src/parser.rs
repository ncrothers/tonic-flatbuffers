use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

use winnow::{
    combinator::{repeat, trace},
    Parser,
};

use crate::flatbuffers::item::{item, Item};

pub struct ParserState<'a> {
    cur_namespace: RefCell<&'a str>,
    namespace_decls: HashMap<&'a str, TypeDecls<'a>>,
}

impl<'a> ParserState<'a> {
    pub fn new() -> Self {
        Self {
            cur_namespace: RefCell::new(""),
            namespace_decls: HashMap::new(),
        }
    }

    pub fn namespace(&self) -> &'a str {
        &self.cur_namespace.borrow()
    }

    pub fn set_namespace(&self, namespace: &'a str) {
        *self.cur_namespace.borrow_mut() = namespace;
    }
}

pub struct TypeDecls<'a> {
    structs: HashSet<&'a str>,
    enums: HashSet<&'a str>,
    unions: HashSet<&'a str>,
    tables: HashSet<&'a str>,
}

pub struct Schema<'a> {
    namespace_decls: HashMap<&'a str, TypeDecls<'a>>,
    namespaces: HashMap<&'a str, Vec<Item<'a>>>,
}

pub fn collect_type_decls<'a>(file: &'a str, ty: &'static str) -> Vec<&'a str> {
    let pat = regex::Regex::new(&format!(r#"^[ \t]*{ty}[ \t\n\r]+([\w\.]+)"#)).unwrap();

    pat.captures_iter(file)
        .flat_map(|val| val.iter().skip(1).collect::<Vec<_>>())
        .filter_map(|val| val.map(|x| x.as_str()))
        .collect()
}

pub fn collect_includes(file: &str) -> Vec<&str> {
    let pat = regex::Regex::new(r#"^[ \t]*include[ \t\n\r]+"([\.\w]+\.fbs)""#).unwrap();

    pat.captures_iter(file)
        .flat_map(|val| val.iter().skip(1).collect::<Vec<_>>())
        .filter_map(|val| val.map(|x| x.as_str()))
        .collect()
}

pub fn parse_file<'a>(file: &'a str, state: &ParserState<'a>) -> anyhow::Result<Vec<Item<'a>>> {
    // Reset the current namespace
    *state.cur_namespace.borrow_mut() = "";

    trace("parse_file", repeat(0.., item(state)))
        .parse(file)
        .map_err(|e| anyhow::format_err!("{e}"))
}
