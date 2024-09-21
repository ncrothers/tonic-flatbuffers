use winnow::{
    combinator::{alt, delimited, separated, trace},
    error::ContextError,
    token::{literal, take_while},
    Parser,
};

use crate::parse::{
    parser::{DeclType, ParsedTypes, ParserState},
    utils::{
        ident, resolved_ident, string_literal, whitespace_all, whitespace_and_comments_opt,
        whitespace_and_comments_req, ByteSize, Namespace,
    },
};

use super::{
    r#enum::{enum_item, Enum},
    r#struct::{struct_item, Struct},
    rpc_service::{rpc_service_item, RpcService},
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
    Namespace(Namespace<'a>),
    RpcService(RpcService<'a>),
    RootType(&'a str),
    Struct(Struct<'a>),
    Table(Table<'a>),
    Union(Union<'a>),
}

impl<'a> Item<'a> {
    /// If this is an item definition that has an identifier (one of the below types),
    /// returns its identifier
    ///
    /// Types that return an identifier:
    /// * `Item::Enum`
    /// * `Item::RpcService`
    /// * `Item::Struct`
    /// * `Item::Table`
    /// * `Item::Union`
    pub fn ident(&self) -> Option<&'a str> {
        match self {
            Item::Enum(item) => Some(item.name),
            Item::RpcService(item) => Some(item.name),
            Item::Struct(item) => Some(item.name),
            Item::Table(item) => Some(item.name),
            Item::Union(item) => Some(item.name),
            _ => None,
        }
    }

    /// If this is an item definition that has an namespace (one of the below types),
    /// returns its namespace
    ///
    /// Types that return a namespace:
    /// * `Item::Enum`
    /// * `Item::RpcService`
    /// * `Item::Struct`
    /// * `Item::Table`
    /// * `Item::Union`
    pub fn namespace(&self) -> Option<&Namespace<'a>> {
        match self {
            Item::Enum(item) => Some(&item.namespace),
            Item::RpcService(item) => Some(&item.namespace),
            Item::Struct(item) => Some(&item.namespace),
            Item::Table(item) => Some(&item.namespace),
            Item::Union(item) => Some(&item.namespace),
            _ => None,
        }
    }
}

impl<'a> ByteSize for Item<'a> {
    fn size(&self, parsed_types: &ParsedTypes) -> usize {
        match self {
            Item::Enum(item) => item.size(parsed_types),
            Item::Struct(item) => item.size(parsed_types),
            Item::Table(item) => todo!(), //item.size(parsed_types),
            Item::Union(item) => todo!(), //item.size(parsed_types),
            _ => unreachable!("make this an error instead of unreachable"),
        }
    }
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
                rpc_service_item(state).map(Item::RpcService),
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
) -> impl Parser<&'s str, Namespace<'s>, ContextError> + 'a {
    move |input: &mut _| {
        trace("namespace", |input: &mut _| {
            whitespace_and_comments_opt(input)?;

            literal("namespace").parse_next(input)?;
            whitespace_and_comments_opt(input)?;

            let namespace = separated(1.., ident, ".")
                .map(|()| ())
                .take()
                .parse_next(input)?;

            let namespace = Namespace::from(namespace);

            whitespace_and_comments_opt(input)?;
            literal(";").parse_next(input)?;

            state.set_namespace(namespace.clone());

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

            let namespace = resolved_ident(state, &[DeclType::Table])
                .map(|value| value.ident)
                .parse_next(input)?;

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

    use rstest::rstest;

    use crate::parse::parser::TypeDecls;

    use super::*;

    #[rstest]
    #[case::attribute("attribute \"testing\";", |x| matches!(x, Item::Attribute("testing")))]
    #[case::file_identifier(
        "file_identifier \"TEST\";",
        |x| matches!(x, Item::FileIdentifier("TEST"))
    )]
    #[case::file_extension(
        "file_extension \"ext\";",
        |x| matches!(x, Item::FileExtension("ext"))
    )]
    #[case::include(
        "include \"file.fbs\";",
        |x| matches!(x, Item::Include("file.fbs"))
    )]
    #[case::root_type(
        "root_type Table;",
        |x| matches!(x, Item::RootType("Table"))
    )]
    #[case::namespace(
        "namespace one.two.three;",
        |x| matches!(x, Item::Namespace(Namespace { raw, components }) if raw == "one.two.three" && components == vec!["one", "two", "three"])
    )]
    #[case::enum_(
        "enum test:int {}",
        |x| matches!(x, Item::Enum(_))
    )]
    #[case::struct_(
        "struct test {}",
        |x| matches!(x, Item::Struct(_))
    )]
    #[case::table(
        "table test {}",
        |x| matches!(x, Item::Table(_))
    )]
    #[case::union(
        "union test {}",
        |x| matches!(x, Item::Union(_))
    )]
    fn item_pass(
        #[case] item_str: &'static str,
        #[case] output: impl FnOnce(Item<'static>) -> bool,
    ) {
        let mut state_ = ParserState::new();
        let mut decl = TypeDecls::new();
        decl.add_tables(["Table"]);

        state_.extend_decls(HashMap::from([("".into(), decl)]));

        let state = &state_;

        let item = item(state).parse(item_str);
        assert!(item.is_ok());
        let item = item.unwrap();

        assert!(output(item));

        // assert!(item(state).parse("file_identifier \"TESTa\";").is_err());
        // assert!(item(state).parse("file_identifier \"TES\";").is_err());
    }

    #[rstest]
    #[case::file_identifier_long("file_identifier \"TESTa\";")]
    #[case::file_identifier_short("file_identifier \"TES\";")]
    fn item_fail(#[case] item_str: &str) {
        let state = ParserState::new();

        assert!(item(&state).parse(item_str).is_err());
    }
}
