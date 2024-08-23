use winnow::{
    ascii::till_line_ending,
    combinator::{repeat, separated, trace},
    error::{AddContext, ContextError, ErrMode, StrContext, StrContextValue},
    stream::{AsChar, Stream},
    token::{literal, one_of, take_till, take_while},
    PResult, Parser,
};

use crate::flatbuffers::primitives::{DefaultValue, TableFieldType};

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

/// Returns non-whitespace tokens prefixed by whitespace. Unlike `consume_whitespace`,
/// whitespace is purely considered to be spaces or tabs, not line endings.
pub fn default_value<'a, 's>(
    field_type: &'a TableFieldType<'s>,
) -> impl Parser<&'s str, DefaultValue<'s>, ContextError> + 'a {
    move |input: &mut &'s str| {
        let checkpoint = input.checkpoint();

        trace("default_value", |input: &mut _| {
            // Remove whitespace at the front
            whitespace_all(input)?;
            // take_while(0.., AsChar::is_space).parse_next(input)?;

            take_while(1.., |c: char| {
                !(AsChar::is_space(c)
                    || (c.is_ascii_punctuation() && c != '_' && c != '.' && c != '-' && c != '+'))
            })
            .parse_next(input)
        })
        .parse_next(input)
        .and_then(|default| {
            match field_type {
                TableFieldType::Scalar(scalar) => {
                    DefaultValue::parse(default, *scalar).ok_or_else(|| {
                        ErrMode::Cut(
                            ContextError::new()
                                .add_context(input, &checkpoint, StrContext::Label("default value"))
                                .add_context(
                                    input,
                                    &checkpoint,
                                    StrContext::Expected(StrContextValue::Description(
                                        "invalid default value for field type",
                                    )),
                                ),
                        )
                    })
                }
                TableFieldType::String | TableFieldType::Vector(_) => Err(ErrMode::Cut(
                    ContextError::new()
                        .add_context(input, &checkpoint, StrContext::Label("default value"))
                        .add_context(
                            input,
                            &checkpoint,
                            StrContext::Expected(StrContextValue::Description(
                                "non-scalar values can't have a default",
                            )),
                        ),
                )),
                TableFieldType::Named(_) => {
                    // TODO: How to validate this? Only allowed for enums, and must be a valid enum variant
                    Ok(DefaultValue::Named(default))
                }
            }
        })
    }
}

pub fn namespaced_ident<'s>(input: &mut &'s str) -> PResult<&'s str> {
    trace("namespaced_ident", |input: &mut _| {
        whitespace_all(input)?;
        separated(1.., ident, ".")
            .map(|()| ())
            .take()
            .parse_next(input)
    })
    .parse_next(input)
}

pub fn ident<'s>(input: &mut &'s str) -> PResult<&'s str> {
    fn is_valid(input: char) -> bool {
        input.is_ascii_alphanumeric() || input == '_'
    }

    trace("namespaced_ident", |input: &mut _| {
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
        literal("\"").parse_next(input)?;
        // Consume the contents of the string, disallowing multi-line strings
        let value = take_till(0.., |c| c == '"' || AsChar::is_newline(c)).parse_next(input)?;

        // Try to consume a closing quotation
        literal("\"")
            .context(StrContext::Label("string literal"))
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
    fn namespaced_ident_() {
        let valid = ["foo", "namespace.foo", "one.two.three"];

        for item in valid {
            assert_eq!(namespaced_ident.parse(item), Ok(item));
        }

        let invalid = [".foo", "hello.", "test.test."];

        for item in invalid {
            assert!(namespaced_ident.parse(item).is_err());
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
