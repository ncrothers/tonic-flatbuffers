use std::collections::HashSet;

use winnow::{
    combinator::{cut_err, opt, repeat, trace},
    error::{AddContext, ContextError, ErrMode, StrContext, StrContextValue},
    stream::Stream,
    token::literal,
    Parser,
};

use crate::{
    parser::{DeclType, NamedType, ParserState},
    utils::{ident, resolved_ident, whitespace_all, whitespace_and_comments_opt},
};

use super::attributes::{attribute_list, Attribute, AttributeTarget};

#[derive(Debug, PartialEq)]
pub enum StreamingMode {
    None,
    Client,
    Server,
    Bidirectional,
}

#[derive(Debug, PartialEq)]
pub struct RpcMethod<'a> {
    name: &'a str,
    parameter: NamedType<'a>,
    return_type: NamedType<'a>,
    comments: Vec<&'a str>,
    attributes: Vec<Attribute<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct RpcService<'a> {
    name: &'a str,
    namespace: &'a str,
    methods: Vec<RpcMethod<'a>>,
    comments: Vec<&'a str>,
    attributes: Vec<Attribute<'a>>,
}

fn rpc_method_parameter<'a, 's: 'a>(
    state: &'a ParserState<'s>,
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
    state: &'a ParserState<'s>,
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
    state: &'a ParserState<'s>,
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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::parser::{DeclType, NamedType, TypeDecls};

    use super::*;

    #[test]
    fn rpc_service() {
        let service1_str = r#"
            rpc_service Hello {
                HelloWorld(Table1) : Table1 (streaming: "bidi");
            }"#;

        let service1 = RpcService {
            name: "Hello",
            namespace: "",
            methods: vec![RpcMethod {
                name: "HelloWorld",
                parameter: NamedType::new("Table1", DeclType::Table),
                return_type: NamedType::new("Table1", DeclType::Table),
                comments: Vec::new(),
                attributes: vec![Attribute::Streaming(StreamingMode::Bidirectional)],
            }],
            comments: Vec::new(),
            attributes: Vec::new(),
        };

        let valid = [(service1_str, service1)];

        let mut state = ParserState::new();
        let mut decl = TypeDecls::new();
        decl.add_tables(["Table1"]);

        state.extend_decls(HashMap::from([("", decl)]));

        for (item_str, item) in valid {
            let value = item_str;
            let res = rpc_service_item(&state)
                .parse(value)
                .inspect_err(|e| println!("{e}"));
            assert_eq!(res, Ok(item));
        }

        let service_invalid1 = r#"
            rpc_service Hello_There {
                HelloWorld(MissingTable):Table1;
            }"#;

        let struct_invalid2 = r#"
            rpc_service Hello_There {
                HelloWorld(Table1) Table1;
            }"#;

        let struct_invalid3 = r#"
            rpc_service Hello There {}"#;

        let invalid = [service_invalid1, struct_invalid2, struct_invalid3];

        for item in invalid {
            assert!(rpc_service_item(&state).parse(item).is_err());
        }
    }
}
