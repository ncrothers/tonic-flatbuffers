use winnow::{
    ascii::till_line_ending,
    combinator::{delimited, eof, not, repeat, trace},
    error::{AddContext, ContextError, ErrMode, ParserError, StrContext, StrContextValue},
    stream::{AsChar, Compare, Stream, StreamIsPartial},
    token::{literal, one_of, take_till, take_while},
    PResult, Parser,
};

macro_rules! impl_typename {
    ($($type:ty),*) => {
        $(
            paste::paste! {
                impl TypeName for $type {
                    fn type_name() -> &'static str {
                        stringify!($type)
                    }
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
pub struct WhitespacePrefixedParser;

impl<'s, E> Parser<&'s str, <&'s str as Stream>::Slice, E> for WhitespacePrefixedParser
where
    E: ParserError<&'s str> + AddContext<&'s str, StrContext>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<<&'s str as Stream>::Slice, E> {
        // Remove whitespace at the front
        take_while(0.., |c| AsChar::is_space(c)).parse_next(input)?;

        take_while(1.., |c: char| {
            !AsChar::is_space(c) && !(c.is_ascii_punctuation() && c != '_')
        })
        .parse_next(input)
    }
}

pub struct IdentParser;

impl IdentParser {
    pub fn is_valid(input: char) -> bool {
        input.is_ascii_alphanumeric() || input == '_'
    }

    /// Checks whether the character is valid in the context of a namespaced ident.
    pub fn is_valid_namespace(input: char) -> bool {
        Self::is_valid(input) || input == '.'
    }
}

impl<'s, E> Parser<&'s str, <&'s str as Stream>::Slice, E> for IdentParser
where
    E: ParserError<&'s str> + AddContext<&'s str, StrContext>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<<&'s str as Stream>::Slice, E> {
        consume_whitespace(input)?;
        let start = input.checkpoint();
        // Make sure the first character is a valid ident start
        one_of(('a'..='z', 'A'..='Z', '_')).parse_next(input)?;
        // If it is, now parse the entire thing
        input.reset(&start);
        take_while(1.., IdentParser::is_valid).parse_next(input)
    }
}

pub fn parse_ident<'s>(input: &mut &'s str) -> PResult<&'s str> {
    IdentParser.parse_next(input)
}

struct CommentParser;

impl<'s, E> Parser<&'s str, Option<&'s str>, E> for CommentParser
where
    E: ParserError<&'s str> + AddContext<&'s str, StrContext>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<Option<&'s str>, E> {
        trace("comment_parser", |input: &mut _| {
            consume_whitespace(input)?;
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
}

/// Consumes whitespace only, including line endings
pub fn consume_whitespace<'s>(input: &mut &'s str) -> PResult<&'s str> {
    take_while(0.., (AsChar::is_newline, AsChar::is_space)).parse_next(input)
}

/// Consumes both whitespace/newlines and comments, returning any comments found along the way
pub fn consume_whitespace_and_comments<'s>(input: &mut &'s str) -> PResult<Vec<&'s str>> {
    consume_whitespace(input)?;
    let comments: Vec<Option<&'s str>> = repeat(0.., CommentParser).parse_next(input)?;
    consume_whitespace(input)?;
    let comments: Vec<&'s str> = comments.into_iter().flatten().collect();

    Ok(comments)
}

pub struct StringLiteral;

impl<'s, E> Parser<&'s str, <&'s str as Stream>::Slice, E> for StringLiteral
where
    E: ParserError<&'s str> + AddContext<&'s str, StrContext>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<<&'s str as Stream>::Slice, E> {
        consume_whitespace(input)?;
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
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ident() {
        let valid = ["foo", "_foo", "foo_", "_f_1_o_o"];

        for item in valid {
            let mut value = item;
            assert_eq!(parse_ident(&mut value), Ok(item));
        }

        let invalid = ["1foo", "", "111foo"];

        for item in invalid {
            let mut value = item;
            assert!(parse_ident(&mut value).is_err());
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
                "/// This is a comment\nthis is not!",
                vec!["This is a comment"],
            ),
            ("This is not a comment", Vec::new()),
        ];

        for (item_str, item) in valid {
            let mut value = item_str;
            assert_eq!(consume_whitespace_and_comments(&mut value), Ok(item));
        }
    }

    #[test]
    fn type_name() {
        assert_eq!(u32::type_name(), "u32");
        assert_eq!(f64::type_name(), "f64");
        assert_eq!(i8::type_name(), "i8");
    }
}
