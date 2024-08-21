use winnow::{
    combinator::{alt, delimited, trace},
    error::{AddContext, ContextError, ErrMode, ParserError, StrContext},
    token::{literal, take_while},
    PResult, Parser,
};

use crate::utils::{
    consume_whitespace, consume_whitespace_and_comments, IdentParser, StringLiteral,
};

use super::{
    r#enum::{Enum, EnumParser},
    r#struct::{Struct, StructParser},
    table::{Table, TableParser},
    union::{Union, UnionParser},
};

#[derive(Debug, PartialEq)]
pub enum Item<'a> {
    Attribute(&'a str),
    Comment(Vec<&'a str>),
    Enum(Enum<'a>),
    FileExtension(&'a str),
    FileIdentifier(&'a str),
    Include(&'a str),
    Namespace(&'a str),
    RootType(&'a str),
    Struct(Struct<'a>),
    Table(Table<'a>),
    Union(Union<'a>),
}

pub struct ItemParser;

impl<'s, E> Parser<&'s str, Item<'s>, E> for ItemParser
where
    E: AddContext<&'s str, StrContext> + ParserError<&'s str>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<Item<'s>, E> {
        let item = alt((
            AttributeDeclParser.map(Item::Attribute),
            EnumParser.map(Item::Enum),
            FileExtensionParser.map(Item::FileExtension),
            FileIdentifierParser.map(Item::FileIdentifier),
            IncludeParser.map(Item::Include),
            NamespaceParser.map(Item::Namespace),
            RootTypeParser.map(Item::RootType),
            StructParser.map(Item::Struct),
            TableParser.map(Item::Table),
            UnionParser.map(Item::Union),
        ))
        .context(StrContext::Expected(
            winnow::error::StrContextValue::Description("flatbuffer statement"),
        ))
        .parse_next(input)?;

        consume_whitespace(input)?;

        Ok(item)
    }
}

struct AttributeDeclParser;

impl<'s, E> Parser<&'s str, &'s str, E> for AttributeDeclParser
where
    E: AddContext<&'s str, StrContext> + ParserError<&'s str>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<&'s str, E> {
        consume_whitespace_and_comments(input)?;

        literal("attribute").parse_next(input)?;

        consume_whitespace_and_comments(input)?;

        let attr_val = StringLiteral.parse_next(input)?;

        consume_whitespace_and_comments(input)?;
        literal(";").parse_next(input)?;

        Ok(attr_val)
    }
}

struct FileExtensionParser;

impl<'s, E> Parser<&'s str, &'s str, E> for FileExtensionParser
where
    E: AddContext<&'s str, StrContext> + ParserError<&'s str>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<&'s str, E> {
        consume_whitespace_and_comments(input)?;

        literal("file_extension").parse_next(input)?;

        consume_whitespace_and_comments(input)?;

        let attr_val = StringLiteral.parse_next(input)?;

        consume_whitespace_and_comments(input)?;
        literal(";").parse_next(input)?;

        Ok(attr_val)
    }
}

struct FileIdentifierParser;

impl<'s, E> Parser<&'s str, &'s str, E> for FileIdentifierParser
where
    E: AddContext<&'s str, StrContext> + ParserError<&'s str>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<&'s str, E> {
        consume_whitespace_and_comments(input)?;

        literal("file_identifier").parse_next(input)?;

        consume_whitespace_and_comments(input)?;

        let attr_val = delimited(
            "\"",
            take_while(4..=4, |c: char| c.is_ascii() && c != '"'),
            "\"",
        )
        .parse_next(input)?;

        consume_whitespace_and_comments(input)?;
        literal(";").parse_next(input)?;

        Ok(attr_val)
    }
}

struct IncludeParser;

impl<'s, E> Parser<&'s str, &'s str, E> for IncludeParser
where
    E: AddContext<&'s str, StrContext> + ParserError<&'s str>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<&'s str, E> {
        consume_whitespace_and_comments(input)?;

        literal("include").parse_next(input)?;

        consume_whitespace_and_comments(input)?;

        let attr_val = StringLiteral.parse_next(input)?;

        consume_whitespace_and_comments(input)?;
        literal(";").parse_next(input)?;

        Ok(attr_val)
    }
}

struct NamespaceParser;

impl<'s, E> Parser<&'s str, &'s str, E> for NamespaceParser
where
    E: AddContext<&'s str, StrContext> + ParserError<&'s str>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<&'s str, E> {
        trace("namespace", |input: &mut _| {
            consume_whitespace_and_comments(input)?;

            literal("namespace").parse_next(input)?;
            consume_whitespace_and_comments(input)?;

            let namespace = take_while(1.., IdentParser::is_valid_namespace).parse_next(input)?;

            consume_whitespace_and_comments(input)?;
            literal(";").parse_next(input)?;

            Ok(namespace)
        })
        .parse_next(input)
    }
}

struct RootTypeParser;

impl<'s, E> Parser<&'s str, &'s str, E> for RootTypeParser
where
    E: AddContext<&'s str, StrContext> + ParserError<&'s str>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<&'s str, E> {
        consume_whitespace_and_comments(input)?;

        literal("root_type").parse_next(input)?;
        consume_whitespace_and_comments(input)?;

        let namespace = take_while(1.., IdentParser::is_valid_namespace).parse_next(input)?;

        consume_whitespace_and_comments(input)?;
        literal(";").parse_next(input)?;

        Ok(namespace)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn r#struct() {
        assert_eq!(
            ItemParser.parse("attribute \"testing\";"),
            Ok(Item::Attribute("testing"))
        );
        assert!(matches!(
            ItemParser.parse("enum test:int {}"),
            Ok(Item::Enum(_))
        ));
        assert_eq!(
            ItemParser.parse("file_identifier \"TEST\";"),
            Ok(Item::FileIdentifier("TEST"))
        );
        assert!(ItemParser.parse("file_identifier \"TESTa\";").is_err());
        assert!(ItemParser.parse("file_identifier \"TES\";").is_err());
        assert_eq!(
            ItemParser.parse("file_extension \"ext\";"),
            Ok(Item::FileExtension("ext"))
        );
        assert_eq!(
            ItemParser.parse("include \"file.fbs\";"),
            Ok(Item::Include("file.fbs"))
        );
        assert_eq!(
            ItemParser.parse("namespace one.two.three;"),
            Ok(Item::Namespace("one.two.three"))
        );
        assert_eq!(
            ItemParser.parse("root_type Table;"),
            Ok(Item::RootType("Table"))
        );
        assert!(matches!(
            ItemParser.parse("struct test {}"),
            Ok(Item::Struct(_))
        ));
        assert!(matches!(
            ItemParser.parse("table test {}"),
            Ok(Item::Table(_))
        ));
        assert!(matches!(
            ItemParser.parse("union test {}"),
            Ok(Item::Union(_))
        ));
    }
}
