use winnow::{
    combinator::{alt, delimited, separated, trace},
    error::ContextError,
    token::{literal, take_while},
    Parser,
};

use crate::{
    parser::{DeclType, ParserState},
    utils::{
        ident, resolved_ident, string_literal, whitespace_all, whitespace_and_comments_opt,
        whitespace_and_comments_req,
    },
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

pub fn item<'a, 's: 'a>(
    state: &'a ParserState<'s>,
) -> impl Parser<&'s str, Item<'s>, ContextError> + 'a {
    move |input: &mut _| {
        trace("item", |input: &mut _| {
            let item = alt((
                attribute_decl(state).map(Item::Attribute),
                enum_item(state).map(Item::Enum),
                file_extension(state).map(Item::FileExtension),
                file_identifier(state).map(Item::FileIdentifier),
                include(state).map(Item::Include),
                namespace(state).map(Item::Namespace),
                root_type(state).map(Item::RootType),
                struct_item(state).map(Item::Struct),
                table_item(state).map(Item::Table),
                union_item(state).map(Item::Union),
                // As last resort, parse to end of file and keep any comments
                whitespace_and_comments_req.map(Item::Comment),
            ))
            .parse_next(input)?;

            whitespace_all(input)?;

            Ok(item)
        })
        .parse_next(input)
    }
}

fn attribute_decl<'a, 's: 'a>(
    _state: &'a ParserState<'s>,
) -> impl Parser<&'s str, &'s str, ContextError> + 'a {
    move |input: &mut _| {
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
}

fn file_extension<'a, 's: 'a>(
    _state: &'a ParserState<'s>,
) -> impl Parser<&'s str, &'s str, ContextError> + 'a {
    move |input: &mut _| {
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
}

fn file_identifier<'a, 's: 'a>(
    _state: &'a ParserState<'s>,
) -> impl Parser<&'s str, &'s str, ContextError> + 'a {
    move |input: &mut _| {
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
}

pub fn include<'a, 's: 'a>(
    _state: &'a ParserState<'s>,
) -> impl Parser<&'s str, &'s str, ContextError> + 'a {
    move |input: &mut _| {
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
}

pub fn namespace<'a, 's: 'a>(
    state: &'a ParserState<'s>,
) -> impl Parser<&'s str, &'s str, ContextError> + 'a {
    move |input: &mut _| {
        trace("namespace", |input: &mut _| {
            whitespace_and_comments_opt(input)?;

            literal("namespace").parse_next(input)?;
            whitespace_and_comments_opt(input)?;

            let namespace = separated(1.., ident, ".")
                .map(|()| ())
                .take()
                .parse_next(input)?;

            whitespace_and_comments_opt(input)?;
            literal(";").parse_next(input)?;

            state.set_namespace(namespace);

            Ok(namespace)
        })
        .parse_next(input)
    }
}

pub fn root_type<'a, 's: 'a>(
    state: &'a ParserState<'s>,
) -> impl Parser<&'s str, &'s str, ContextError> + 'a {
    move |input: &mut _| {
        trace("root_type", |input: &mut _| {
            whitespace_and_comments_opt(input)?;

            literal("root_type").parse_next(input)?;
            whitespace_and_comments_opt(input)?;

            let namespace = resolved_ident(state, &[DeclType::Table]).parse_next(input)?;

            whitespace_and_comments_opt(input)?;
            literal(";").parse_next(input)?;

            Ok(namespace)
        })
        .parse_next(input)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::parser::TypeDecls;

    use super::*;

    #[test]
    fn r#struct() {
        let mut state_ = ParserState::new();
        let mut decl = TypeDecls::new();
        decl.add_tables(["Table"]);

        state_.extend_decls(HashMap::from([("", decl)]));

        let state = &mut state_;

        assert_eq!(
            item(state).parse("attribute \"testing\";"),
            Ok(Item::Attribute("testing"))
        );
        assert!(matches!(
            item(state).parse("enum test:int {}"),
            Ok(Item::Enum(_))
        ));
        assert_eq!(
            item(state).parse("file_identifier \"TEST\";"),
            Ok(Item::FileIdentifier("TEST"))
        );
        assert!(item(state).parse("file_identifier \"TESTa\";").is_err());
        assert!(item(state).parse("file_identifier \"TES\";").is_err());
        assert_eq!(
            item(state).parse("file_extension \"ext\";"),
            Ok(Item::FileExtension("ext"))
        );
        assert_eq!(
            item(state).parse("include \"file.fbs\";"),
            Ok(Item::Include("file.fbs"))
        );
        assert_eq!(
            item(state).parse("root_type Table;"),
            Ok(Item::RootType("Table"))
        );
        assert!(matches!(
            item(state).parse("struct test {}"),
            Ok(Item::Struct(_))
        ));
        assert!(matches!(
            item(state).parse("table test {}"),
            Ok(Item::Table(_))
        ));
        assert!(matches!(
            item(state).parse("union test {}"),
            Ok(Item::Union(_))
        ));
        assert_eq!(
            item(state).parse("namespace one.two.three;"),
            Ok(Item::Namespace("one.two.three"))
        );
    }
}
