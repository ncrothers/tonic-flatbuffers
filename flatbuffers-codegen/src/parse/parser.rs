use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf}, rc::Rc,
};

use winnow::{
    combinator::{alt, repeat, rest, trace},
    token::take_until,
    PResult, Parser,
};

use super::{
    flatbuffers::item::{item, namespace, Item},
    utils::{Alignment, ByteSize, Namespace, NamespaceWrapped},
};

#[derive(Debug, PartialEq)]
pub enum DeclType {
    Enum,
    Struct,
    Table,
    Union,
}

impl DeclType {
    /// Slice of all [`DeclType`] variants. Used to match any [`DeclType`] when resolving an identifier
    pub const ANY: &'static [Self] = &[Self::Enum, Self::Struct, Self::Table, Self::Union];
}

#[derive(Debug, PartialEq)]
pub struct NamedType<'a> {
    pub ident: &'a str,
    pub namespace: Namespace<'a>,
    pub decl_type: DeclType,
}

impl<'a> NamedType<'a> {
    pub fn new<N>(ident: &'a str, namespace: N, decl_type: DeclType) -> Self
    where
        N: Into<Namespace<'a>> + 'a,
    {
        Self {
            ident,
            namespace: namespace.into(),
            decl_type,
        }
    }
}

impl<'a> ByteSize for NamedType<'a> {
    fn size(&self, parsed_types: &ParsedTypes) -> usize {
        // TODO: Remove unwrap
        let resolved_item = parsed_types.resolve_named(self).unwrap();
        let resolved_item = &*resolved_item.borrow();
        resolved_item.size(parsed_types)
    }
}

impl<'a> Alignment for NamedType<'a> {
    fn alignment(&self, parsed_types: &ParsedTypes) -> usize {
        let resolved_item = parsed_types.resolve_named(self).unwrap();
        let resolved_item = &*resolved_item.borrow();

        if let Item::Struct(struct_) = resolved_item {
            struct_.alignment(parsed_types)
        } else {
            resolved_item.size(parsed_types)
        }
    }
}

#[derive(Debug)]
pub struct ParserState<'a> {
    cur_namespace: RefCell<Namespace<'a>>,
    parsed_names: RefCell<HashMap<Namespace<'a>, HashSet<&'a str>>>,
    namespace_decls: HashMap<NamespaceWrapped<'a>, TypeDecls<'a>>,
}

impl<'a> ParserState<'a> {
    pub fn new() -> Self {
        Self {
            cur_namespace: RefCell::new(Namespace::from("")),
            parsed_names: RefCell::new(HashMap::new()),
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
            let ns = NamespaceWrapped(ns);
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

    pub fn is_already_defined(&self, ns: &Namespace<'_>, ident: &str) -> bool {
        self.parsed_names
            .borrow()
            .get(ns)
            .map(|already_defined| already_defined.contains(ident))
            .unwrap_or(false)
    }

    pub fn add_parsed(&self, ns: Namespace<'a>, ident: &'a str) {
        self.parsed_names
            .borrow_mut()
            .entry(ns)
            .or_default()
            .insert(ident);
    }

    fn resolve_ident_recursive(
        &self,
        ns: &Namespace<'_>,
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
                            return Some(NamedType::new(ident, ns.0.clone(), DeclType::Enum));
                        }
                    }
                    DeclType::Struct => {
                        if let Some(ident) = decls.structs.get(ident) {
                            return Some(NamedType::new(ident, ns.0.clone(), DeclType::Struct));
                        }
                    }
                    DeclType::Table => {
                        if let Some(ident) = decls.tables.get(ident) {
                            return Some(NamedType::new(ident, ns.0.clone(), DeclType::Table));
                        }
                    }
                    DeclType::Union => {
                        if let Some(ident) = decls.unions.get(ident) {
                            return Some(NamedType::new(ident, ns.0.clone(), DeclType::Union));
                        }
                    }
                };
            }

            // When no namespace left to check, we can return
            if !ns.0.raw.is_empty() {
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

    pub fn resolve_any(&self, ident: &str, ty: &[DeclType]) -> Option<NamedType<'a>> {
        let (ns, ident) = ident.rsplit_once('.').unwrap_or(("", ident));

        self.resolve_ident_recursive(&Namespace::new_raw(ns), ident, ty)
    }
}

impl<'a> Default for ParserState<'a> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Default)]
pub struct ParsedTypes<'a>(pub(crate) HashMap<Namespace<'a>, HashMap<&'a str, Rc<RefCell<Item<'a>>>>>);

impl<'a> ParsedTypes<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, item: Item<'a>) {
        if let Some((ns, ident)) = item
            .ident()
            .and_then(|ident| item.namespace().map(|ns| (ns, ident)))
        {
            self.0.entry(ns.clone()).or_default().insert(ident, Rc::new(RefCell::new(item)));
        }
    }

    /// Given a [`NamedType`], return a reference to the [`Item`] it actually points to.
    ///
    /// TODO: Care must be taken that only types defined in files that are included in
    /// the file the parent type was defined
    pub fn resolve_named(&self, named_type: &NamedType<'_>) -> Option<Rc<RefCell<Item<'a>>>> {
        self.0
            .get(&named_type.namespace)
            .and_then(|items| items.get(named_type.ident).cloned())
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Namespace<'a>, &HashMap<&'a str, Rc<RefCell<Item<'a>>>>)> {
        self.0.iter()
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
    *state.cur_namespace.borrow_mut() = Namespace::default();

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

/// Converts a relative path into absolute
pub fn absolute<P: AsRef<Path>>(path: P) -> std::io::Result<PathBuf> {
    let cur_dir = std::env::current_dir()?;

    Ok(path_clean::clean(cur_dir.join(path)))
}

/// Load _all_ files recursively until everything that we will need to parse is in memory
pub fn load_file_strs(
    files: &[PathBuf],
    include_paths: &[PathBuf],
) -> anyhow::Result<HashMap<PathBuf, String>> {
    let mut loaded = HashMap::new();

    for file in files {
        load_file_recursive(file, &mut loaded, include_paths)?;
    }

    Ok(loaded)
}

fn load_file_recursive(
    file_path: &Path,
    loaded: &mut HashMap<PathBuf, String>,
    include_paths: &[PathBuf],
) -> anyhow::Result<()> {
    let file_str = fs::read_to_string(file_path)?;

    // Get all included files and load them if not already loaded
    let includes = get_include_paths(&file_str, include_paths)?;
    for include in includes {
        // Load this include file if we haven't already
        if !loaded.contains_key(&include) {
            load_file_recursive(&include, loaded, include_paths)?;
        }
    }

    loaded.insert(file_path.to_owned(), file_str);

    Ok(())
}

pub fn get_include_paths(file: &str, include_paths: &[PathBuf]) -> anyhow::Result<Vec<PathBuf>> {
    // Get the include declarations from the file
    let includes = collect_includes(file)
        .into_iter()
        .map(|path_str| resolve_include(path_str, include_paths))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(includes)
}

fn resolve_include(path_str: &str, include_paths: &[PathBuf]) -> anyhow::Result<PathBuf> {
    let path = PathBuf::from(path_str);

    // Import a file on the first matched include, in order. This is how
    // protoc and flatc do it; even if there are multiple files with the
    // same name that resolve the import, only import the first one
    include_paths
        .iter()
        .filter_map(|include_path| {
            let joined = include_path.join(&path);
            if std::fs::File::open(&joined).is_ok() {
                Some(joined)
            } else {
                None
            }
        })
        .next()
        .ok_or_else(|| {
            anyhow::format_err!(
                "couldn't locate file \"{path_str}\" in any of the include directories"
            )
        })
}

pub fn align_structs(items: &ParsedTypes) {
    for ns_items in items.0.values() {
        for item in ns_items.values() {
            match &mut *item.borrow_mut() {
                Item::Struct(struct_) => {
                    struct_.position_fields(&items);
                }
                _ => ()
            }
        }
    }
}