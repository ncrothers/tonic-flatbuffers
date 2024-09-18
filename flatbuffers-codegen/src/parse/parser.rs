use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

use winnow::{
    combinator::{alt, repeat, rest, trace},
    token::take_until,
    PResult, Parser,
};

use super::{flatbuffers::item::{item, namespace, Item}, utils::{ByteSize, Namespace}};

#[derive(Debug, PartialEq)]
pub enum DeclType {
    Enum,
    Struct,
    Table,
    Union,
}

impl DeclType {
    pub const ANY: &'static [Self] = &[Self::Enum, Self::Struct, Self::Table, Self::Union];
}

#[derive(Debug, PartialEq)]
pub struct NamedType<'a> {
    pub ident: &'a str,
    pub namespace: Namespace<'a>,
    pub decl_type: DeclType,
}

impl<'a> NamedType<'a> {
    pub fn new(ident: &'a str, namespace: Namespace<'a>, decl_type: DeclType) -> Self {
        Self {
            ident,
            namespace,
            decl_type,
        }
    }
}

impl<'a> ByteSize for NamedType<'a> {
    fn size(&self) -> usize {
        todo!("implement once NamedType has access to the actual type")
    }
}

#[derive(Debug)]
pub struct ParserState<'a> {
    cur_namespace: RefCell<Namespace<'a>>,
    namespace_decls: HashMap<Namespace<'a>, TypeDecls<'a>>,
}

impl<'a> ParserState<'a> {
    pub fn new() -> Self {
        Self {
            cur_namespace: RefCell::new(Namespace::new("")),
            namespace_decls: HashMap::new(),
        }
    }

    pub fn namespace(&self) -> Namespace<'a> {
        self.cur_namespace.borrow().clone()
    }

    pub fn set_namespace(&self, namespace: Namespace<'a>) {
        *self.cur_namespace.borrow_mut() = namespace;
    }

    pub fn extend_decls(&mut self, decls: HashMap<Namespace<'a>, TypeDecls<'a>>) {
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

    fn resolve_ident_recursive(
        &self,
        ns: &Namespace,
        ident: &str,
        decl_types: &[DeclType],
    ) -> Option<NamedType<'a>> {
        // Try to match namespace
        if let Some((ns, decls)) = self.namespace_decls.get_key_value(ns) {
            // Try to match against all the provided types
            for ty in decl_types {
                match ty {
                    DeclType::Enum => {
                        if let Some(ident) = decls.enums.get(ident) {
                            return Some(NamedType::new(ident, Namespace::new(ns.raw), DeclType::Enum));
                        }
                    }
                    DeclType::Struct => {
                        if let Some(ident) = decls.structs.get(ident) {
                            return Some(NamedType::new(ident, Namespace::new(ns.raw), DeclType::Struct));
                        }
                    }
                    DeclType::Table => {
                        if let Some(ident) = decls.tables.get(ident) {
                            return Some(NamedType::new(ident, Namespace::new(ns.raw), DeclType::Table));
                        }
                    }
                    DeclType::Union => {
                        if let Some(ident) = decls.unions.get(ident) {
                            return Some(NamedType::new(ident, Namespace::new(ns.raw), DeclType::Union));
                        }
                    }
                };
            }

            // When no namespace left to check, we can return
            if !ns.raw.is_empty() {
                return None;
            }
        }

        // If no match, try to split and try the next highest scope
        if let Some((ns, ident)) = ident.rsplit_once('.') {
            self.resolve_ident_recursive(&Namespace::new_raw(ns), ident, decl_types)
        } else {
            None
        }
    }

    pub fn resolve_any(&self, ident: &'a str, ty: &[DeclType]) -> Option<NamedType<'a>> {
        let (ns, ident) = ident.rsplit_once('.').unwrap_or(("", ident));

        self.resolve_ident_recursive(&Namespace::new_raw(ns), ident, ty)
    }
}

impl<'a> Default for ParserState<'a> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug, Default)]
pub struct TypeDecls<'a> {
    structs: HashSet<&'a str>,
    enums: HashSet<&'a str>,
    unions: HashSet<&'a str>,
    tables: HashSet<&'a str>,
}

impl<'a> TypeDecls<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_structs(&mut self, values: impl IntoIterator<Item = &'a str>) {
        self.structs.extend(values);
    }
    pub fn add_enums(&mut self, values: impl IntoIterator<Item = &'a str>) {
        self.enums.extend(values);
    }
    pub fn add_tables(&mut self, values: impl IntoIterator<Item = &'a str>) {
        self.tables.extend(values);
    }
    pub fn add_unions(&mut self, values: impl IntoIterator<Item = &'a str>) {
        self.unions.extend(values);
    }
}

pub struct Schema<'a> {
    namespace_decls: HashMap<&'a str, TypeDecls<'a>>,
    namespaces: HashMap<&'a str, Vec<Item<'a>>>,
}

pub fn collect_type_decls<'a>(file: &'a str, ty: &'static str) -> Vec<&'a str> {
    let pat = format!(r#"(?:[\n\r]+|^)[ \t]*{ty}[ \t\n\r]+([\w]+)"#);
    let pat = regex::Regex::new(&pat).unwrap();

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
    *state.cur_namespace.borrow_mut() = Namespace::new("");

    trace("parse_file", repeat(0.., item(state)))
        .parse(file)
        .map_err(|e| anyhow::format_err!("{e}"))
}

pub fn get_namespaced_decls(file: &str) -> PResult<HashMap<Namespace, TypeDecls<'_>>> {
    let mut file = file;

    let input = &mut file;

    let mut map = HashMap::new();

    let state = ParserState::new();

    while !input.is_empty() {
        let pre_namespace_text = alt((take_until(0.., "namespace"), rest)).parse_next(input)?;

        let structs = collect_type_decls(pre_namespace_text, "struct");
        let enums = collect_type_decls(pre_namespace_text, "enum");
        let tables = collect_type_decls(pre_namespace_text, "table");
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
