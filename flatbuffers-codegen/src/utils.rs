use std::str::FromStr;

use winnow::{
    ascii::till_line_ending,
    combinator::{cut_err, repeat, separated, trace},
    error::{AddContext, ContextError, ErrMode, StrContext, StrContextValue},
    stream::{AsChar, Stream},
    token::{literal, one_of, take_till, take_while},
    PResult, Parser,
};

use crate::{
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
                TableFieldType::Scalar(scalar) => match scalar {
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
                },
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
                _ => Err(ErrMode::Cut(ContextError::new().add_context(
                    input,
                    &checkpoint,
                    StrContext::Label(
                        "default value; default values for this type are not supported",
                    ),
                ))),
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
                    state
                        .resolve_any(ident, decl_types)
                        .or_else(|| {
                            state.resolve_any(
                                &format!("{}.{}", state.namespace(), ident),
                                decl_types,
                            )
                        })
                        .map(|decl_type| NamedType { ident, decl_type })
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

    use crate::parser::TypeDecls;

    use super::*;

    #[test]
    fn ident_() {
        let valid = ["foo", "_foo", "foo_", "_f_1_o_o"];

        for item in valid {
            assert_eq!(ident.parse(item), Ok(item));
        }

        let invalid = ["1foo", "", "111foo"];

        for item in invalid {
            assert!(ident.parse(item).is_err());
        }
    }

    #[test]
    fn resolved_ident_() {
        let mut state = ParserState::new();

        let mut foo_decl = TypeDecls::new();
        foo_decl.add_structs(["foo"]);

        let decls = HashMap::from([
            ("", foo_decl.clone()),
            ("namespace", foo_decl.clone()),
            ("one.two.three", foo_decl.clone()),
        ]);

        state.extend_decls(decls);

        let valid = ["foo", "namespace.foo", "one.two.three.foo"];

        for item in valid {
            assert_eq!(
                resolved_ident(&state, DeclType::ANY).parse(item),
                Ok(NamedType {
                    ident: item,
                    decl_type: DeclType::Struct
                })
            );
        }

        let invalid = [".foo", "hello.", "test.test."];

        for item in invalid {
            assert!(resolved_ident(&state, DeclType::ANY).parse(item).is_err());
        }
    }

    #[test]
    fn comments() {
        let valid = [
            (" /// this is a comment", vec!["this is a comment"]),
            (
                "\n/// Comment here\n/// Another line!",
                vec!["Comment here", "Another line!"],
            ),
            (
                "/// This is a comment\n// this is not!",
                vec!["This is a comment"],
            ),
            ("// This is not a comment", Vec::new()),
            ("", Vec::new()),
        ];

        for (item_str, item) in valid {
            assert_eq!(whitespace_and_comments_opt.parse(item_str), Ok(item));
        }
    }

    #[test]
    fn type_name() {
        assert_eq!(u32::type_name(), "u32");
        assert_eq!(f64::type_name(), "f64");
        assert_eq!(i8::type_name(), "i8");
    }
}
