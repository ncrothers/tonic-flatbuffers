use std::{hash::Hash, str::FromStr};

use winnow::{
    ascii::till_line_ending,
    combinator::{cut_err, opt, repeat, separated, trace},
    error::{AddContext, ContextError, ErrMode, StrContext, StrContextValue},
    stream::{AsChar, Stream},
    token::{literal, one_of, take_till, take_while},
    PResult, Parser,
};

use super::{
    flatbuffers::primitives::{DefaultValue, ScalarType, TableFieldType},
    parser::{DeclType, NamedType, ParserState},
};

macro_rules! impl_typename {
    ($($type:ty),*) => {
        $(
            impl TypeName for $type {
                fn type_name() -> &'static str {
                    stringify!($type)
                }
            }
        )*
    };
}

pub trait TypeName {
    fn type_name() -> &'static str;
}

impl_typename!(i8, u8, i16, u16, i32, u32, i64, u64, f32, f64);

/// Wrapper around a namespace that stores both the raw and split namespace
/// for easy access to the components
#[derive(Clone, Debug, Default)]
pub struct Namespace<'a> {
    pub raw: &'a str,
    pub components: Vec<&'a str>,
}

/// Wrapper around [`Namespace`] that's only used in the `HashMap`s that store
/// the already-parsed items under each namespace. You should never have to
/// interact with this object directly.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) struct NamespaceWrapped<'a>(pub(crate) Namespace<'a>);

impl<'a> Namespace<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a [`Namespace`] with _only_ the `raw` field. This should
    /// **only be used** when creating a namespace where only the raw identifier
    /// is needed; `components` will be empty regardless of `raw`.
    pub(crate) fn new_raw(ns: &'a str) -> Self {
        Self {
            raw: ns,
            components: Vec::new(),
        }
    }
}

impl<'a> From<&'a str> for Namespace<'a> {
    /// Creates a namespace from a string, including splitting it into components.
    fn from(value: &'a str) -> Self {
        let components = if value.is_empty() {
            // If the namespace is empty (root namespace), no components
            Vec::new()
        } else {
            value.split('.').collect()
        };

        Self {
            raw: value,
            components,
        }
    }
}

impl Hash for Namespace<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Only hash the raw namespace
        self.raw.hash(state);
    }
}

impl PartialEq for Namespace<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}

impl Eq for Namespace<'_> {}

impl std::fmt::Display for Namespace<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Use the raw's implementation
        self.raw.fmt(f)
    }
}

impl<'a: 'b, 'b> std::borrow::Borrow<Namespace<'b>> for NamespaceWrapped<'a> {
    fn borrow(&self) -> &Namespace<'b> {
        &self.0
    }
}

pub trait ByteSize {
    /// Returns the size in bytes of the type when serialized
    fn size(&self) -> usize;
}

pub fn parse_to_scalar<T>(input: &mut &str) -> PResult<T>
where
    T: FromStr,
{
    take_while(1.., |c: char| {
        !(AsChar::is_space(c)
            || (c.is_ascii_punctuation() && c != '_' && c != '.' && c != '-' && c != '+'))
    })
    .parse_to()
    .parse_next(input)
}

/// Returns non-whitespace tokens prefixed by whitespace. Unlike `consume_whitespace`,
/// whitespace is purely considered to be spaces or tabs, not line endings.
pub fn default_value<'a, 's>(
    field_type: &'a TableFieldType<'s>,
) -> impl Parser<&'s str, DefaultValue<'s>, ContextError> + 'a {
    move |input: &mut &'s str| {
        trace("default_value", |input: &mut &'s str| {
            let checkpoint = input.checkpoint();
            // Remove whitespace at the front
            whitespace_all(input)?;

            match field_type {
                TableFieldType::Scalar(scalar) => {
                    if opt(literal("null")).parse_next(input)?.is_some() {
                        return Ok(DefaultValue::Null);
                    }

                    match scalar {
                        ScalarType::Int8 => parse_to_scalar
                            .map(DefaultValue::Int8)
                            .context(StrContext::Expected(StrContextValue::Description("int8")))
                            .parse_next(input),
                        ScalarType::UInt8 => parse_to_scalar
                            .map(DefaultValue::UInt8)
                            .context(StrContext::Expected(StrContextValue::Description("uint8")))
                            .parse_next(input),
                        ScalarType::Bool => parse_to_scalar
                            .map(DefaultValue::Bool)
                            .context(StrContext::Expected(StrContextValue::Description("bool")))
                            .parse_next(input),
                        ScalarType::Int16 => parse_to_scalar
                            .map(DefaultValue::Int16)
                            .context(StrContext::Expected(StrContextValue::Description("int16")))
                            .parse_next(input),
                        ScalarType::UInt16 => parse_to_scalar
                            .map(DefaultValue::UInt16)
                            .context(StrContext::Expected(StrContextValue::Description("uint16")))
                            .parse_next(input),
                        ScalarType::Int32 => parse_to_scalar
                            .map(DefaultValue::Int32)
                            .context(StrContext::Expected(StrContextValue::Description("int32")))
                            .parse_next(input),
                        ScalarType::UInt32 => parse_to_scalar
                            .map(DefaultValue::UInt32)
                            .context(StrContext::Expected(StrContextValue::Description("uint32")))
                            .parse_next(input),
                        ScalarType::Float32 => parse_to_scalar
                            .map(DefaultValue::Float32)
                            .context(StrContext::Expected(StrContextValue::Description(
                                "float32",
                            )))
                            .parse_next(input),
                        ScalarType::Int64 => parse_to_scalar
                            .map(DefaultValue::Int64)
                            .context(StrContext::Expected(StrContextValue::Description("int64")))
                            .parse_next(input),
                        ScalarType::UInt64 => parse_to_scalar
                            .map(DefaultValue::UInt64)
                            .context(StrContext::Expected(StrContextValue::Description("uint64")))
                            .parse_next(input),
                        ScalarType::Float64 => parse_to_scalar
                            .map(DefaultValue::Float64)
                            .context(StrContext::Expected(StrContextValue::Description(
                                "float64",
                            )))
                            .parse_next(input),
                    }
                }
                TableFieldType::String => {
                    let value = string_literal
                        .context(StrContext::Expected(StrContextValue::Description(
                            "string literal",
                        )))
                        .parse_next(input)?;

                    Ok(DefaultValue::String(value))
                }
                TableFieldType::Vector(_) => {
                    // Flatbuffers only allows for empty vectors as a default
                    literal('[').parse_next(input)?;
                    whitespace_and_comments_opt(input)?;
                    literal(']').parse_next(input)?;

                    Ok(DefaultValue::Vector)
                }
                TableFieldType::Named(_) => {
                    // Allow null as a default type for named types
                    if opt(literal("null")).parse_next(input)?.is_some() {
                        Ok(DefaultValue::Null)
                    } else {
                        Err(ErrMode::Cut(
                            ContextError::new()
                                .add_context(input, &checkpoint, StrContext::Label("default value"))
                                .add_context(
                                    input,
                                    &checkpoint,
                                    StrContext::Expected(StrContextValue::Description(
                                        "default values for this type are not supported",
                                    )),
                                ),
                        ))
                    }
                }
            }
        })
        .context(StrContext::Label("default value"))
        .parse_next(input)
    }
}

/// Parse a namespaced ident, checking against the allowed types from [`ParserState`]
pub fn resolved_ident<'a, 's: 'a>(
    state: &'a ParserState<'s>,
    decl_types: &'a [DeclType],
) -> impl Parser<&'s str, NamedType<'s>, ContextError> + 'a {
    |input: &mut _| {
        trace("resolved_ident", |input: &mut _| {
            whitespace_all(input)?;

            let ident = cut_err(separated(1.., ident, ".").map(|()| ()).take().verify_map(
                |ident| {
                    // Try the ident literally, then prepend the cur_namespace
                    state.resolve_any(ident, decl_types).or_else(|| {
                        let ident = format!("{}.{}", state.namespace(), ident);
                        state.resolve_any(&ident, decl_types)
                    })
                },
            ))
            .context(StrContext::Label("type"))
            .context(StrContext::Expected(StrContextValue::Description(
                "valid type, could not be found",
            )))
            .parse_next(input)?;

            Ok(ident)
        })
        .parse_next(input)
    }
}

pub fn ident<'s>(input: &mut &'s str) -> PResult<&'s str> {
    fn is_valid(input: char) -> bool {
        input.is_ascii_alphanumeric() || input == '_'
    }

    trace("ident", |input: &mut _| {
        whitespace_all(input)?;
        let start = input.checkpoint();
        // Make sure the first character is a valid ident start
        one_of(('a'..='z', 'A'..='Z', '_')).parse_next(input)?;
        // If it is, now parse the entire thing
        input.reset(&start);
        let ident = take_while(1.., is_valid).parse_next(input)?;

        Ok(ident)
    })
    .parse_next(input)
}

/// Parses an [`ident`] and checks if this type has already been defined in the
/// current namespace. If so, it returns `Err`.
pub(crate) fn item_ident<'a, 's: 'a>(
    state: &'a ParserState<'s>,
) -> impl Parser<&'s str, &'s str, ContextError> + 'a {
    |input: &mut _| {
        trace("item_ident", |input: &mut &'s str| {
            let before_ident = input.checkpoint();

            let ident = cut_err(ident).parse_next(input)?;

            // Check if this name has already been defined in this namespace
            if state.is_already_defined(&state.namespace(), ident) {
                input.reset(&before_ident);
                Err(ErrMode::Cut(ContextError::new().add_context(
                    input,
                    &before_ident,
                    StrContext::Label("; name already defined"),
                )))
            } else {
                Ok(ident)
            }
        })
        .parse_next(input)
    }
}

pub fn comment<'s>(input: &mut &'s str) -> PResult<Option<&'s str>> {
    trace("comment", |input: &mut _| {
        whitespace_all(input)?;
        // Consume the comment start
        literal("//").parse_next(input)?;

        // If we still have another / left, this is documentation, and should be kept
        let is_doc = input.starts_with('/');

        // Remove the leading slash when it's a documentation comment
        if is_doc {
            literal("/").parse_next(input)?;
        }

        // Consume whitespace until comment
        take_while(0.., AsChar::is_space).parse_next(input)?;

        let comment = till_line_ending(input)?;

        // Only return the comment if it was documentation
        if is_doc {
            Ok(Some(comment))
        } else {
            Ok(None)
        }
    })
    .parse_next(input)
}

/// Consumes whitespace only, including line endings
pub fn whitespace_all<'s>(input: &mut &'s str) -> PResult<&'s str> {
    take_while(0.., (AsChar::is_newline, AsChar::is_space)).parse_next(input)
}

fn whitespace_and_comments_min<'s>(
    min_comments: usize,
) -> impl Parser<&'s str, Vec<&'s str>, ContextError> {
    move |input: &mut _| {
        whitespace_all(input)?;
        let comments: Vec<Option<&'s str>> = repeat(min_comments.., comment).parse_next(input)?;
        let comments: Vec<&'s str> = comments.into_iter().flatten().collect();

        if !input.is_empty() {
            whitespace_all(input)?;
        }

        Ok(comments)
    }
}

/// Consumes both whitespace/newlines and comments, returning any comments found along the way
pub fn whitespace_and_comments_opt<'s>(input: &mut &'s str) -> PResult<Vec<&'s str>> {
    whitespace_and_comments_min(0).parse_next(input)
}

/// Consumes both whitespace/newlines and comments, returning any comments found along the way
pub fn whitespace_and_comments_req<'s>(input: &mut &'s str) -> PResult<Vec<&'s str>> {
    whitespace_and_comments_min(1).parse_next(input)
}

pub fn string_literal<'s>(input: &mut &'s str) -> PResult<<&'s str as Stream>::Slice> {
    trace("string_literal", |input: &mut _| {
        whitespace_all(input)?;
        // Try to consume an opening quotation
        literal("\"")
            .context(StrContext::Expected(StrContextValue::Description(
                "opening quotation",
            )))
            .parse_next(input)?;
        // Consume the contents of the string, disallowing multi-line strings
        let value = take_till(0.., |c| c == '"' || AsChar::is_newline(c)).parse_next(input)?;

        // Try to consume a closing quotation
        literal("\"")
            .context(StrContext::Expected(StrContextValue::Description(
                "closing quotation",
            )))
            .parse_next(input)?;

        Ok(value)
    })
    .parse_next(input)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use rstest::rstest;

    use super::super::parser::TypeDecls;

    use super::*;

    #[rstest]
    #[case("foo")]
    #[case("_foo")]
    #[case("foo_")]
    #[case("_f_1_o_o")]
    fn ident_pass(#[case] item_str: &str) {
        assert_eq!(ident.parse(item_str), Ok(item_str));
    }

    #[rstest]
    #[case("1foo")]
    #[case("1+foo")]
    fn ident_fail(#[case] item_str: &str) {
        assert!(ident.parse(item_str).is_err());
    }

    #[rstest]
    #[case::simple("foo", NamedType { ident: "foo", namespace: "".into(), decl_type: DeclType::Struct })]
    #[case::nested("namespace.foo", NamedType { ident: "foo", namespace: "namespace".into(), decl_type: DeclType::Struct })]
    #[case::nested("one.two.three.foo", NamedType { ident: "foo", namespace: "one.two.three".into(),decl_type: DeclType::Struct })]
    fn resolved_ident_pass(#[case] item_str: &str, #[case] output: NamedType) {
        let mut state = ParserState::new();

        let mut foo_decl = TypeDecls::new();
        foo_decl.add_structs(["foo"]);

        let decls = HashMap::from([
            ("".into(), foo_decl.clone()),
            ("namespace".into(), foo_decl.clone()),
            ("one.two.three".into(), foo_decl.clone()),
        ]);

        state.extend_decls(decls);

        assert_eq!(
            resolved_ident(&state, DeclType::ANY).parse(item_str),
            Ok(output)
        );
    }

    #[rstest]
    #[case::malformed(".foo")]
    #[case::malformed("hello.")]
    #[case::not_defined("empty.foo")]
    fn resolved_ident_fail(#[case] item_str: &str) {
        let mut state = ParserState::new();

        let mut foo_decl = TypeDecls::new();
        foo_decl.add_structs(["foo"]);

        let decls = HashMap::from([
            ("".into(), foo_decl.clone()),
            ("namespace".into(), foo_decl.clone()),
            ("one.two.three".into(), foo_decl.clone()),
        ]);

        state.extend_decls(decls);

        assert!(resolved_ident(&state, DeclType::ANY)
            .parse(item_str)
            .is_err());
    }

    #[rstest]
    #[case::single(" /// this is a comment", vec!["this is a comment"])]
    #[case::multiple(
        "\n/// Comment here\n/// Another line!",
        vec!["Comment here", "Another line!"],
    )]
    #[case::non_document(
        "/// This is a comment\n// this is not!",
        vec!["This is a comment"],
    )]
    #[case::non_document("// This is not a comment", Vec::new())]
    #[case::none("", Vec::new())]
    fn comments_pass(#[case] item_str: &str, #[case] output: Vec<&str>) {
        assert_eq!(whitespace_and_comments_opt.parse(item_str), Ok(output));
    }
}
