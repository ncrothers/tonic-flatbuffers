use winnow::{
    combinator::{alt, delimited, separated, trace},
    error::ContextError,
    token::{literal, take_while},
    Parser,
};

use crate::parse::{
    parser::{DeclType, NamedType, ParserState},
    utils::{
        ident, resolved_ident, string_literal, whitespace_all, whitespace_and_comments_opt,
        whitespace_and_comments_req, Namespace,
    },
};

use super::{
    r#enum::{enum_item, Enum},
    r#struct::{struct_item, Struct},
    rpc_service::{rpc_service_item, RpcService},
    table::{table_item, Table},
    union::{union_item, Union},
};

macro_rules! impl_item_from {
    ($($item_type:tt),*) => {
        $(
            impl<'a> From<$item_type<'a>> for Item<'a> {
                fn from(value: $item_type<'a>) -> Self {
                    Self::$item_type(value)
                }
            }
        )*
    };
}

#[derive(Clone, Debug, PartialEq)]
pub enum Item<'a> {
    Attribute(&'a str),
    Comment(Vec<&'a str>),
    Enum(Enum<'a>),
    FileExtension(&'a str),
    FileIdentifier(&'a str),
    Include(&'a str),
    Namespace(Namespace<'a>),
    RpcService(RpcService<'a>),
    RootType(NamedType<'a>),
    Struct(Struct<'a>),
    Table(Table<'a>),
    Union(Union<'a>),
}

impl<'a> Item<'a> {
    /// Return a `&str` representing the identifier of the item. For comments, it's just
    /// the literal `"comment"`; for everything else, there is a name or literal
    /// associated with it, so it returns that.
    pub fn ident(&self) -> &'a str {
        match self {
            Item::Attribute(ident)
            | Item::FileExtension(ident)
            | Item::FileIdentifier(ident)
            | Item::Include(ident) => ident,
            Item::RootType(item) => item.ident(),
            Item::Comment(_) => "comment",
            Item::Enum(enum_) => enum_.name,
            Item::Namespace(namespace) => namespace.raw,
            Item::RpcService(rpc_service) => rpc_service.name,
            Item::Struct(struct_) => struct_.name,
            Item::Table(table) => table.name,
            Item::Union(union) => union.name,
        }
    }

    pub fn namespace(&self) -> Option<&Namespace<'a>> {
        match self {
            Item::Attribute(_)
            | Item::FileExtension(_)
            | Item::FileIdentifier(_)
            | Item::Include(_)
            | Item::RootType(_)
            | Item::Namespace(_)
            | Item::Comment(_) => None,
            Item::Enum(enum_) => Some(&enum_.namespace),
            Item::RpcService(rpc_service) => Some(&rpc_service.namespace),
            Item::Struct(struct_) => Some(&struct_.namespace),
            Item::Table(table) => Some(&table.namespace),
            Item::Union(union) => Some(&union.namespace),
        }
    }
}

impl_item_from!(Enum, Struct, Table, Union, RpcService);

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
) -> impl Parser<&'s str, NamedType<'s>, ContextError> + 'a {
    move |input: &mut _| {
        trace("root_type", |input: &mut _| {
            whitespace_and_comments_opt(input)?;

            literal("root_type").parse_next(input)?;
            whitespace_and_comments_opt(input)?;

            let root_type = resolved_ident(state, &[DeclType::Table])
                .map(|value| value)
                .parse_next(input)?;

            whitespace_and_comments_opt(input)?;
            literal(";").parse_next(input)?;

            Ok(root_type)
        })
        .parse_next(input)
    }
}

#[cfg(feature = "builder")]
#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use rstest::rstest;

    use crate::parse::parser::ParsedTypes;

    use super::*;

    macro_rules! assert_item_parse {
        ($state:expr, $str_val:expr, $func:expr) => {
            let item = item($state).parse($str_val);
            assert!(item.is_ok());
            let item = item.unwrap();

            let output = $func;

            assert!(output(item));
        };
    }

    #[test]
    fn item_pass() {
        let mut state_ = ParserState::new();
        let mut decl = ParsedTypes::new();
        decl.add_tables([Table::builder().name("Table").build()]);

        state_.extend_decls(HashMap::from([("".into(), decl)]));

        let state = &state_;

        assert_item_parse!(&state, "attribute \"testing\";", |x| matches!(
            x,
            Item::Attribute("testing")
        ));
        assert_item_parse!(&state, "file_identifier \"TEST\";", |x| matches!(
            x,
            Item::FileIdentifier("TEST")
        ));
        assert_item_parse!(&state, "file_extension \"ext\";", |x| matches!(
            x,
            Item::FileExtension("ext")
        ));
        assert_item_parse!(&state, "include \"file.fbs\";", |x| matches!(
            x,
            Item::Include("file.fbs")
        ));
        assert_item_parse!(&state, "root_type Table;", |x| matches!(
            x,
            Item::RootType(NamedType(_))
        ));
        assert_item_parse!(
            &state,
            "namespace one.two.three;",
            |x| matches!(x, Item::Namespace(Namespace { raw, components }) if raw == "one.two.three" && components == vec!["one", "two", "three"])
        );
        assert_item_parse!(&state, "enum test:int {}", |x| matches!(x, Item::Enum(_)));
        assert_item_parse!(&state, "struct test {}", |x| matches!(x, Item::Struct(_)));
        assert_item_parse!(&state, "table test {}", |x| matches!(x, Item::Table(_)));
        assert_item_parse!(&state, "union test {}", |x| matches!(x, Item::Union(_)));
    }

    #[rstest]
    #[case::file_identifier_long("file_identifier \"TESTa\";")]
    #[case::file_identifier_short("file_identifier \"TES\";")]
    fn item_fail(#[case] item_str: &str) {
        let mut state = ParserState::new();

        let mut foo_decl = ParsedTypes::new();
        foo_decl.add_tables([Table::builder().name("Table").build()]);

        let decls = HashMap::from([("".into(), foo_decl.clone())]);

        state.extend_decls(decls);

        assert!(item(&state).parse(item_str).is_err());
    }
}
