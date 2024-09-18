use std::collections::HashSet;

use winnow::{
    combinator::{cut_err, opt, repeat, trace},
    error::{AddContext, ContextError, ErrMode, StrContext, StrContextValue},
    stream::Stream,
    token::literal,
    Parser,
};

use crate::parse::{
    parser::{DeclType, NamedType, ParserState},
    utils::{ident, resolved_ident, whitespace_all, whitespace_and_comments_opt, Namespace},
};

use super::attributes::{attribute_list, Attribute, AttributeTarget};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum StreamingMode {
    None,
    Client,
    Server,
    Bidirectional,
}

#[cfg_attr(feature = "builder", derive(typed_builder::TypedBuilder))]
#[cfg_attr(feature = "builder", builder(field_defaults(default)))]
#[derive(Clone, Debug, PartialEq)]
pub struct RpcMethod<'a> {
    #[cfg_attr(feature = "builder", builder(!default))]
    pub name: &'a str,
    #[cfg_attr(feature = "builder", builder(!default))]
    pub parameter: NamedType<'a>,
    #[cfg_attr(feature = "builder", builder(!default))]
    pub return_type: NamedType<'a>,
    pub comments: Vec<&'a str>,
    pub attributes: Vec<Attribute<'a>>,
}

#[cfg_attr(feature = "builder", derive(typed_builder::TypedBuilder))]
#[cfg_attr(feature = "builder", builder(field_defaults(default)))]
#[derive(Clone, Debug, PartialEq)]
pub struct RpcService<'a> {
    #[cfg_attr(feature = "builder", builder(!default))]
    pub name: &'a str,
    pub namespace: Namespace<'a>,
    pub methods: Vec<RpcMethod<'a>>,
    pub comments: Vec<&'a str>,
    pub attributes: Vec<Attribute<'a>>,
}

fn rpc_method_parameter<'a, 's: 'a>(
    state: &'s ParserState<'s>,
) -> impl Parser<&'s str, NamedType<'s>, ContextError> + 'a {
    move |input: &mut _| {
        trace("rpc_method_parameter", |input: &mut _| {
            literal("(").parse_next(input)?;

            whitespace_and_comments_opt(input)?;

            let param = resolved_ident(state, &[DeclType::Table])
                .context(StrContext::Expected(StrContextValue::Description(
                    "table parameter",
                )))
                .parse_next(input)?;

            whitespace_and_comments_opt(input)?;

            literal(")").parse_next(input)?;

            Ok(param)
        })
        .parse_next(input)
    }
}

fn rpc_method<'a, 's: 'a>(
    state: &'s ParserState<'s>,
    field_idents: &'a mut HashSet<&'s str>,
) -> impl Parser<&'s str, RpcMethod<'s>, ContextError> + 'a {
    move |input: &mut _| {
        trace("rpc_method", |input: &mut _| {
            let comments = whitespace_and_comments_opt(input)?;

            let ident_chk = input.checkpoint();
            // Get the field ident
            let ident = ident
                .context(StrContext::Expected(StrContextValue::Description(
                    "rpc_service method identifier",
                )))
                .parse_next(input)?;

            if field_idents.contains(&ident) {
                input.reset(&ident_chk);
                return Err(ErrMode::Cut(ContextError::new().add_context(
                    input,
                    &ident_chk,
                    StrContext::Label("; duplicate method name"),
                )));
            }

            field_idents.insert(ident);

            whitespace_and_comments_opt(input)?;

            // Parse the method parameter type
            let parameter = cut_err(rpc_method_parameter(state)).parse_next(input)?;

            whitespace_and_comments_opt(input)?;

            cut_err(literal(":"))
                .context(StrContext::Expected(StrContextValue::StringLiteral(":")))
                .parse_next(input)?;

            whitespace_and_comments_opt(input)?;

            // Parse the return type
            let return_type =
                cut_err(resolved_ident(state, &[DeclType::Table])).parse_next(input)?;

            whitespace_and_comments_opt(input)?;

            let attrs = opt(attribute_list(state, AttributeTarget::RpcMethod))
                .parse_next(input)?
                .map(|attrs| attrs.attrs);

            whitespace_and_comments_opt(input)?;

            cut_err(literal(";"))
                .context(StrContext::Expected(StrContextValue::StringLiteral(";")))
                .parse_next(input)?;

            Ok(RpcMethod {
                name: ident,
                parameter,
                return_type,
                comments,
                attributes: attrs.unwrap_or_default(),
            })
        })
        .parse_next(input)
    }
}

pub fn rpc_service_item<'a, 's: 'a>(
    state: &'s ParserState<'s>,
) -> impl Parser<&'s str, RpcService<'s>, ContextError> + 'a {
    move |input: &mut _| {
        trace("rpc_service", |input: &mut _| {
            let comments = whitespace_and_comments_opt(input)?;
            // Parse the keyword
            literal("rpc_service").parse_next(input)?;

            // Get the ident
            let ident = ident.parse_next(input)?;

            let attrs = opt(attribute_list(state, AttributeTarget::StructItem))
                .parse_next(input)?
                .map(|attrs| attrs.attrs);

            whitespace_and_comments_opt(input)?;
            // Consume the opening bracket
            cut_err(literal("{"))
                .context(StrContext::Expected(StrContextValue::StringLiteral("{")))
                .parse_next(input)?;

            // Consume whitespace instead of comments here so the any comments get
            // added to the field
            whitespace_all(input)?;

            let mut field_idents = HashSet::new();
            // Parse methods
            let methods = repeat(0.., rpc_method(state, &mut field_idents)).parse_next(input)?;

            whitespace_and_comments_opt(input)?;
            cut_err(literal("}"))
                .context(StrContext::Expected(StrContextValue::StringLiteral("}")))
                .parse_next(input)?;

            Ok(RpcService {
                name: ident,
                namespace: state.namespace(),
                methods,
                comments,
                attributes: attrs.unwrap_or_default(),
            })
        })
        .parse_next(input)
    }
}

#[cfg(feature = "builder")]
#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use rstest::rstest;

    use crate::parse::{flatbuffers::table::Table, parser::{DeclType, NamedType, TypeDecls}, utils::test_utils::placeholder_item};

    use super::*;

    #[rstest]
    #[case::simple(
        r#"rpc_service Hello {
            HelloWorld(Table1) : Table1;
        }"#,
        RpcService::builder()
            .name("Hello")
            .methods(vec![
                RpcMethod::builder()
                    .name("HelloWorld")
                    .parameter(NamedType::new("Table1", "", DeclType::Table, placeholder_item()))
                    .return_type(NamedType::new("Table1", "", DeclType::Table, placeholder_item()))
                    .build()
            ])
            .build()
    )]
    #[case::comments(
        r#"/// Service comment
        rpc_service Hello {
            HelloWorld(Table1) : Table1;
        }"#,
        RpcService::builder()
            .name("Hello")
            .methods(vec![
                RpcMethod::builder()
                    .name("HelloWorld")
                    .parameter(NamedType::new("Table1", "", DeclType::Table, placeholder_item()))
                    .return_type(NamedType::new("Table1", "", DeclType::Table, placeholder_item()))
                    .build()
            ])
            .comments(vec!["Service comment"])
            .build()
    )]
    fn rpc_service_pass(#[case] item_str: &str, #[case] output: RpcService) {
        let mut state = ParserState::new();
        let mut decl = TypeDecls::new();
        decl.add_tables([Table::builder().name("Table1").build()]);

        state.extend_decls(HashMap::from([("".into(), decl)]));

        let res = rpc_service_item(&state)
            .parse(item_str)
            .inspect_err(|e| println!("{e}"));

        assert_eq!(res, Ok(output));
    }

    #[rstest]
    #[case::duplicate(
        r#"rpc_service Hello_There {
            HelloWorld(Table1):Table1;
            HelloWorld(Table1):Table1;
        }"#
    )]
    fn rpc_service_fail(#[case] item_str: &str) {
        let mut state = ParserState::new();
        let mut decl = TypeDecls::new();
        decl.add_tables([Table::builder().name("Table1").build()]);

        state.extend_decls(HashMap::from([("".into(), decl)]));

        assert!(rpc_service_item(&state).parse(item_str).is_err());
    }

    #[rstest]
    #[case::simple(
        "HelloWorld(Table1) : Table1;",
        RpcMethod::builder()
            .name("HelloWorld")
            .parameter(NamedType::new("Table1", "", DeclType::Table, placeholder_item()))
            .return_type(NamedType::new("Table1", "", DeclType::Table, placeholder_item()))
            .build()
    )]
    #[case::whitespace(
        " \n HelloWorld \n ( \n Table1 \n ) \n : \n Table1 \n;",
        RpcMethod::builder()
            .name("HelloWorld")
            .parameter(NamedType::new("Table1", "", DeclType::Table, placeholder_item()))
            .return_type(NamedType::new("Table1", "", DeclType::Table, placeholder_item()))
            .build()
    )]
    #[case::comments(
        r#"/// Method comment
        HelloWorld(Table1) : Table1;"#,
        RpcMethod::builder()
            .name("HelloWorld")
            .parameter(NamedType::new("Table1", "", DeclType::Table, placeholder_item()))
            .return_type(NamedType::new("Table1", "", DeclType::Table, placeholder_item()))
            .comments(vec!["Method comment"])
            .build()
    )]
    #[case::streaming(
        "HelloWorld(Table1) : Table1 (streaming: \"bidi\");",
        RpcMethod::builder()
            .name("HelloWorld")
            .parameter(NamedType::new("Table1", "", DeclType::Table, placeholder_item()))
            .return_type(NamedType::new("Table1", "", DeclType::Table, placeholder_item()))
            .attributes(vec![Attribute::Streaming(StreamingMode::Bidirectional)])
            .build()
    )]
    fn rpc_method_pass(#[case] item_str: &str, #[case] output: RpcMethod) {
        let mut state = ParserState::new();
        let mut decl = TypeDecls::new();
        decl.add_tables([Table::builder().name("Table1").build()]);

        state.extend_decls(HashMap::from([("".into(), decl)]));

        let res = rpc_method(&state, &mut HashSet::new())
            .parse(item_str)
            .inspect_err(|e| println!("{e}"));

        assert_eq!(res, Ok(output));
    }

    #[rstest]
    #[case::missing_parameter("HelloWorld :Table1;")]
    #[case::missing_parentheses("HelloWorld(Table1 :Table1;")]
    #[case::missing_semicolon("HelloWorld(Table1):Table1")]
    fn rpc_method_fail(#[case] item_str: &str) {
        let mut state = ParserState::new();
        let mut decl = TypeDecls::new();
        decl.add_tables([Table::builder().name("Table1").build()]);

        state.extend_decls(HashMap::from([("".into(), decl)]));

        assert!(rpc_method(&state, &mut HashSet::new())
            .parse(item_str)
            .is_err());
    }
}
