use std::collections::HashSet;

use convert_case::{Case, Casing};
use lazy_static::lazy_static;
use quote::format_ident;

use crate::parse::{parser::NamedType, utils::Namespace};

lazy_static! {
    static ref RESERVED: HashSet<&'static str> = HashSet::from([
        // https://doc.rust-lang.org/book/second-edition/appendix-01-keywords.html
        "as",
        "break",
        "const",
        "continue",
        "crate",
        "else",
        "enum",
        "extern",
        "false",
        "fn",
        "for",
        "if",
        "impl",
        "in",
        "let",
        "loop",
        "match",
        "mod",
        "move",
        "mut",
        "pub",
        "ref",
        "return",
        "Self",
        "self",
        "static",
        "struct",
        "super",
        "trait",
        "true",
        "type",
        "unsafe",
        "use",
        "where",
        "while",
        // future possible keywords
        "abstract",
        "alignof",
        "become",
        "box",
        "do",
        "final",
        "macro",
        "offsetof",
        "override",
        "priv",
        "proc",
        "pure",
        "sizeof",
        "typeof",
        "unsized",
        "virtual",
        "yield",
        // other rust terms we should not use
        "std",
        "usize",
        "isize",
        "u8",
        "i8",
        "u16",
        "i16",
        "u32",
        "i32",
        "u64",
        "i64",
        "u128",
        "i128",
        "f32",
        "f64",
        // Terms that we use ourselves
        "follow",
        "push",
        "size",
        "alignment",
        "to_little_endian",
        "from_little_endian",
        "ENUM_MAX",
        "ENUM_MIN",
        "ENUM_VALUES",
    ]);
}

pub fn deconflict_name(ident: &str, case: Option<Case>) -> String {
    let ident = if let Some(case) = case {
        &ident.to_case(case)
    } else {
        ident
    };

    if RESERVED.contains(ident) {
        format!("{ident}_")
    } else {
        ident.to_owned()
    }
}

pub fn into_valid_ident(ident: &str, case: Case) -> syn::Ident {
    let ident = ident.to_case(case);
    if RESERVED.contains(ident.as_str()) {
        format_ident!("{ident}_")
    } else {
        syn::Ident::new(&ident, proc_macro2::Span::call_site())
    }
}

pub fn into_valid_type(ident: &str) -> syn::Type {
    if RESERVED.contains(ident) {
        syn::parse_str(&format!("{ident}_")).unwrap()
    } else {
        syn::parse_str(&ident).unwrap()
    }
}

pub fn named_type_to_rust_name(named_type: &NamedType, from_ns: &Namespace) -> String {
    let path = from_ns.path_to(&named_type.namespace);

    if path.is_empty() {
        named_type.ident.to_owned()
    } else {
        format!(
            "{}::{}",
            path.join("::"),
            deconflict_name(named_type.ident, Some(Case::Pascal))
        )
    }
}
