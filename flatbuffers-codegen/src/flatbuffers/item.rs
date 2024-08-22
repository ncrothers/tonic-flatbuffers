use winnow::{
    combinator::{alt, delimited, trace},
    error::{AddContext, ContextError, ErrMode, ParserError, StrContext},
    token::{literal, take_while},
    PResult, Parser,
};

use crate::utils::{
    namespaced_ident, string_literal, whitespace_all, whitespace_and_comments_opt,
    whitespace_and_comments_req,
};

use super::{
    r#enum::{enum_item, Enum},
    r#struct::{struct_item, Struct},
    table::{table_item, Table},
    union::{union_item, Union},
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
            attribute_decl.map(Item::Attribute),
            enum_item.map(Item::Enum),
            file_extension.map(Item::FileExtension),
            file_identifier.map(Item::FileIdentifier),
            include.map(Item::Include),
            namespace.map(Item::Namespace),
            root_type.map(Item::RootType),
            struct_item.map(Item::Struct),
            table_item.map(Item::Table),
            union_item.map(Item::Union),
            // As last resort, parse to end of file and keep any comments
            whitespace_and_comments_req.map(Item::Comment),
        ))
        .context(StrContext::Expected(
            winnow::error::StrContextValue::Description("flatbuffer statement"),
        ))
        .parse_next(input)?;

        whitespace_all(input)?;

        Ok(item)
    }
}

fn attribute_decl<'s>(input: &mut &'s str) -> PResult<&'s str> {
    trace("attribute_decl", |input: &mut _| {
        whitespace_and_comments_opt(input)?;

        literal("attribute").parse_next(input)?;

        whitespace_and_comments_opt(input)?;

        let attr_val = string_literal.parse_next(input)?;

        whitespace_and_comments_opt(input)?;
        literal(";").parse_next(input)?;

        Ok(attr_val)
    })
    .parse_next(input)
}

fn file_extension<'s>(input: &mut &'s str) -> PResult<&'s str> {
    trace("file_extension", |input: &mut _| {
        whitespace_and_comments_opt(input)?;

        literal("file_extension").parse_next(input)?;

        whitespace_and_comments_opt(input)?;

        let attr_val = string_literal.parse_next(input)?;

        whitespace_and_comments_opt(input)?;
        literal(";").parse_next(input)?;

        Ok(attr_val)
    })
    .parse_next(input)
}

fn file_identifier<'s>(input: &mut &'s str) -> PResult<&'s str> {
    trace("file_identifier", |input: &mut _| {
        whitespace_and_comments_opt(input)?;

        literal("file_identifier").parse_next(input)?;

        whitespace_and_comments_opt(input)?;

        let attr_val = delimited(
            "\"",
            take_while(4..=4, |c: char| c.is_ascii() && c != '"'),
            "\"",
        )
        .parse_next(input)?;

        whitespace_and_comments_opt(input)?;
        literal(";").parse_next(input)?;

        Ok(attr_val)
    })
    .parse_next(input)
}

pub fn include<'s>(input: &mut &'s str) -> PResult<&'s str> {
    trace("include", |input: &mut _| {
        whitespace_and_comments_opt(input)?;

        literal("include").parse_next(input)?;

        whitespace_and_comments_opt(input)?;

        let attr_val = string_literal.parse_next(input)?;

        whitespace_and_comments_opt(input)?;
        literal(";").parse_next(input)?;

        Ok(attr_val)
    })
    .parse_next(input)
}

pub fn namespace<'s>(input: &mut &'s str) -> PResult<&'s str> {
    trace("namespace", |input: &mut _| {
        whitespace_and_comments_opt(input)?;

        literal("namespace").parse_next(input)?;
        whitespace_and_comments_opt(input)?;

        let namespace = namespaced_ident.parse_next(input)?;

        whitespace_and_comments_opt(input)?;
        literal(";").parse_next(input)?;

        Ok(namespace)
    })
    .parse_next(input)
}

pub fn root_type<'s>(input: &mut &'s str) -> PResult<&'s str> {
    trace("root_type", |input: &mut _| {
        whitespace_and_comments_opt(input)?;

        literal("root_type").parse_next(input)?;
        whitespace_and_comments_opt(input)?;

        let namespace = namespaced_ident.parse_next(input)?;

        whitespace_and_comments_opt(input)?;
        literal(";").parse_next(input)?;

        Ok(namespace)
    })
    .parse_next(input)
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
