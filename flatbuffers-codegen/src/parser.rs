use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

use winnow::{
    combinator::{alt, repeat, rest, trace},
    error::ContextError,
    token::take_until,
    PResult, Parser,
};

use crate::flatbuffers::item::{item, namespace, Item};

#[derive(Debug)]
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

    pub fn extend_decls(&mut self, decls: HashMap<&'a str, TypeDecls<'a>>) {
        for (ns, decls) in decls {
            self.namespace_decls
                .entry(ns)
                .and_modify(|existing| {
                    existing.enums.extend(&decls.enums);
                    existing.structs.extend(&decls.structs);
                    existing.tables.extend(&decls.tables);
                    existing.unions.extend(&decls.unions);
                })
                .or_insert(decls);
        }
    }
}

impl<'a> Default for ParserState<'a> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Default)]
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
    let pat = format!(r#"(?:[\n\r]+|^)[ \t]*{ty}[ \t\n\r]+([\w]+)"#);
    let pat = regex::Regex::new(&pat).unwrap();

    pat.captures_iter(file)
        .flat_map(|val| {
            println!("capture: {val:?}");
            val.iter().skip(1).collect::<Vec<_>>()
        })
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

pub fn get_namespaced_decls(file: &str) -> PResult<HashMap<&str, TypeDecls<'_>>> {
    let mut file = file;

    let input = &mut file;

    let mut map = HashMap::new();

    let state = ParserState::new();

    while !input.is_empty() {
        let pre_namespace_text = alt((take_until(0.., "namespace"), rest)).parse_next(input)?;

        println!("pre_namespace_text:\n===");
        println!("{pre_namespace_text}");
        println!("===");

        let structs = collect_type_decls(pre_namespace_text, "struct");
        let enums = collect_type_decls(pre_namespace_text, "enum");
        let tables = collect_type_decls(pre_namespace_text, "table");
        println!("tables: {tables:?}");
        let unions = collect_type_decls(pre_namespace_text, "union");

        let ns_entry: &mut TypeDecls = map.entry(state.namespace()).or_default();

        ns_entry.structs.extend(structs);
        ns_entry.enums.extend(enums);
        ns_entry.tables.extend(tables);
        ns_entry.unions.extend(unions);

        if !input.is_empty() {
            namespace(&state).parse_next(input)?;
        }
    }

    Ok(map)
}
