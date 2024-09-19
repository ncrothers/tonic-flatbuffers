use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fs,
    ops::{Deref, DerefMut},
    path::{Path, PathBuf},
    rc::Rc,
};

use winnow::{
    combinator::{alt, repeat, rest, trace},
    token::take_until,
    PResult, Parser,
};

use super::{
    flatbuffers::{
        item::{item, Item},
        r#enum::Enum,
        r#struct::Struct,
        table::Table,
        union::Union,
    },
    utils::{ByteSize, Namespace, NamespaceWrapped},
};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
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

#[derive(Clone, Debug)]
pub enum NamedType<'a> {
    Unparsed {
        name: &'a str,
        namespace: Namespace<'a>,
    },
    Parsed(Rc<Item<'a>>),
}
// pub struct NamedType<'a>(pub Rc<Item<'a>>);

impl<'a> NamedType<'a> {
    pub fn new_parsed(item: Rc<Item<'a>>) -> Self {
        Self::Parsed(item)
    }

    pub fn new_unparsed<N>(name: &'a str, namespace: N) -> Self
    where
        N: Into<Namespace<'a>> + 'a
    {
        Self::Unparsed { name, namespace: namespace.into() }
    }

    pub fn ident(&self) -> &'a str {
        match self {
            NamedType::Unparsed { name, namespace: _ } => name,
            NamedType::Parsed(rc) => rc.ident(),
        }
    }

    pub fn namespace(&self) -> Option<&Namespace<'a>> {
        match self {
            NamedType::Unparsed { name: _, namespace } => Some(namespace),
            NamedType::Parsed(rc) => rc.namespace(),
        }
    }
}

impl<'a> PartialEq for NamedType<'a> {
    fn eq(&self, other: &Self) -> bool {
        // Don't compare the item_ref when checking equality
        self.ident() == other.ident() && self.namespace() == other.namespace()
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
    namespace_decls: HashMap<NamespaceWrapped<'a>, ParsedTypes<'a>>,
    parsed_types: HashMap<NamespaceWrapped<'a>, ParsedTypes<'a>>,
}

impl<'a> ParserState<'a> {
    pub fn new() -> Self {
        Self {
            cur_namespace: RefCell::new(Namespace::from("")),
            namespace_decls: HashMap::new(),
        }
    }

    pub fn namespace(&self) -> Namespace<'a> {
        self.cur_namespace.borrow().clone()
    }

    pub fn set_namespace(&self, namespace: Namespace<'a>) {
        *self.cur_namespace.borrow_mut() = namespace;
    }

    pub fn extend_decls(&mut self, decls: HashMap<Namespace<'a>, ParsedTypes<'a>>) {
        for (ns, mut decls) in decls {
            let ns = NamespaceWrapped(ns);
            self.namespace_decls
                .entry(ns)
                .and_modify(|existing| {
                    for (decl_type, data) in decls.data.drain() {
                        existing.data.entry(decl_type).or_default().extend(data);
                    }
                })
                .or_insert(decls);
        }
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
                if let Some(item) = decls.get_decl(ident, *ty) {
                    return Some(NamedType::new(item));
                }
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

#[derive(Clone, Debug, Default)]
pub struct TypeDecls<'a> {
    data: HashMap<DeclType, HashSet<&'a str>>,
}

impl<'a> TypeDecls<'a> {
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Clone, Debug, Default)]
pub struct ParsedTypes<'a> {
    // TODO: Switched from HashSet to Vec because Items can't be hashed
    // use a different data structure for faster lookups?
    data: HashMap<DeclType, Vec<Rc<Item<'a>>>>,
}

impl<'a> ParsedTypes<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_decl(&self, ident: &str, decl_type: DeclType) -> Option<Rc<Item<'a>>> {
        self.data
            .get(&decl_type)
            .and_then(|items| {
                items.iter().find(|item| match item.as_ref() {
                    Item::Enum(val) => val.name == ident,
                    Item::Struct(val) => val.name == ident,
                    Item::Table(val) => val.name == ident,
                    Item::Union(val) => val.name == ident,
                    _ => unreachable!("Non-item attempted to match in TypeDecls::get_decl"),
                })
            }).cloned()
    }

    pub fn add_structs(&mut self, values: impl IntoIterator<Item = Struct<'a>>) {
        self.data
            .entry(DeclType::Struct)
            .or_default()
            .extend(values.into_iter().map(|item| Rc::new(Item::Struct(item))));
    }

    pub fn add_enums(&mut self, values: impl IntoIterator<Item = Enum<'a>>) {
        self.data
            .entry(DeclType::Enum)
            .or_default()
            .extend(values.into_iter().map(|item| Rc::new(Item::Enum(item))));
    }

    pub fn add_tables(&mut self, values: impl IntoIterator<Item = Table<'a>>) {
        self.data
            .entry(DeclType::Table)
            .or_default()
            .extend(values.into_iter().map(|item| Rc::new(Item::Table(item))));
    }

    pub fn add_unions(&mut self, values: impl IntoIterator<Item = Union<'a>>) {
        self.data
            .entry(DeclType::Union)
            .or_default()
            .extend(values.into_iter().map(|item| Rc::new(Item::Union(item))));
    }

    pub fn add_all(&mut self, values: impl IntoIterator<Item = Item<'a>>) {
        for item in values.into_iter() {
            self.add_any(item);
        }
    }

    pub fn add_any(&mut self, item: Item<'a>) {
        match &item {
            Item::Enum(_) => {
                self.data
                    .entry(DeclType::Enum)
                    .or_default()
                    .push(Rc::new(item));
            }
            Item::Struct(_) => {
                self.data
                    .entry(DeclType::Struct)
                    .or_default()
                    .push(Rc::new(item));
            }
            Item::Table(_) => {
                self.data
                    .entry(DeclType::Table)
                    .or_default()
                    .push(Rc::new(item));
            }
            Item::Union(_) => {
                self.data
                    .entry(DeclType::Union)
                    .or_default()
                    .push(Rc::new(item));
            }
            _ => (),
        }
    }
}

impl<'a> From<HashMap<DeclType, Vec<Rc<Item<'a>>>>> for ParsedTypes<'a> {
    fn from(value: HashMap<DeclType, Vec<Rc<Item<'a>>>>) -> Self {
        Self { data: value }
    }
}

pub struct Schema<'a> {
    namespace_decls: HashMap<&'a str, ParsedTypes<'a>>,
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

pub fn parse_file<'a>(
    file_str: &'a str,
    state: &'a ParserState<'a>,
    include_paths: &[PathBuf],
    loaded: &'a HashMap<PathBuf, String>,
    parsed_types: &mut HashMap<Namespace<'a>, ParsedTypes<'a>>,
    parsed_files: &mut HashSet<PathBuf>,
) -> anyhow::Result<Vec<Item<'a>>> {
    // Reset the current namespace
    *state.cur_namespace.borrow_mut() = Namespace::default();

    let mut buffer = file_str;
    let buffer = &mut buffer;

    let mut items = Vec::new();

    while !buffer.is_empty() {
        let item = item(state)
            .parse_next(buffer)
            .map_err(|e| anyhow::format_err!("{e}"))?;

        if let Item::Include(include_path) = &item {
            let include_path = resolve_include(include_path, include_paths)?;

            let prev_namespace = state.namespace();

            if !parsed_files.contains(&include_path) {
                let include_file_str = loaded.get(&include_path).unwrap();

                let items = parse_file(
                    include_file_str,
                    state,
                    include_paths,
                    loaded,
                    parsed_types,
                    parsed_files,
                )?;

                for item in items {}
            }

            *state.cur_namespace.borrow_mut() = prev_namespace;
        }

        items.push(item);
    }

    Ok(items)
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
            load_file_recursive(file_path, loaded, include_paths)?;
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
