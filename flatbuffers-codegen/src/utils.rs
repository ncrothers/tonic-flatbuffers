use winnow::{
    combinator::delimited,
    error::{AddContext, ContextError, ErrMode, ParserError, StrContext, StrContextValue},
    stream::{AsChar, Compare, Stream, StreamIsPartial},
    token::{literal, one_of, take_till, take_while},
    PResult, Parser,
};

pub struct IdentParser;

impl IdentParser {
    pub fn is_valid(input: char) -> bool {
        input.is_ascii_alphanumeric() || input == '_'
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

/// Consumes both whitespace and newlines
pub fn consume_whitespace<'s>(input: &mut &'s str) -> PResult<&'s str> {
    take_while(0.., (AsChar::is_newline, AsChar::is_space)).parse_next(input)
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
}
