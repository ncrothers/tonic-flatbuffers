use std::collections::HashSet;

use winnow::{
    combinator::{opt, repeat, trace},
    error::{AddContext, ContextError, ErrMode, StrContext, StrContextValue},
    stream::Stream,
    token::literal,
    Parser,
};

use crate::parse::{
    parser::ParserState,
    utils::{ident, whitespace_all, whitespace_and_comments_opt, ByteSize},
};

use super::{
    attributes::{attribute_list, Attribute, AttributeTarget},
    primitives::{struct_field_type, StructFieldType},
};

#[derive(Debug, PartialEq)]
pub struct StructField<'a> {
    pub name: &'a str,
    pub field_type: StructFieldType<'a>,
    pub comments: Vec<&'a str>,
    pub attributes: Vec<Attribute<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Struct<'a> {
    pub name: &'a str,
    pub namespace: &'a str,
    pub fields: Vec<StructField<'a>>,
    pub comments: Vec<&'a str>,
    pub attributes: Vec<Attribute<'a>>,
}

impl<'a> ByteSize for Struct<'a> {
    fn size(&self) -> usize {
        self.fields
            .iter()
            .map(|field| {
                field.size()
            })
            .sum()
    }
}

impl<'a> ByteSize for StructField<'a> {
    fn size(&self) -> usize {
        self.field_type.size()
    }
}

fn struct_field<'a, 's: 'a>(
    state: &'a ParserState<'s>,
    field_idents: &'a mut HashSet<&'s str>,
) -> impl Parser<&'s str, StructField<'s>, ContextError> + 'a {
    move |input: &mut _| {
        trace("struct_field", |input: &mut _| {
            let comments = whitespace_and_comments_opt(input)?;
            let ident_chk = input.checkpoint();
            // Get the field ident
            let ident = ident
                .context(StrContext::Expected(StrContextValue::Description(
                    "struct field identifier",
                )))
                .parse_next(input)?;

            if field_idents.contains(&ident) {
                input.reset(&ident_chk);
                return Err(ErrMode::Cut(ContextError::new().add_context(
                    input,
                    &ident_chk,
                    StrContext::Label("; duplicate field name"),
                )));
            }

            field_idents.insert(ident);

            whitespace_and_comments_opt.parse_next(input)?;

            literal(":")
                .context(StrContext::Expected(StrContextValue::StringLiteral(":")))
                .parse_next(input)?;

            whitespace_and_comments_opt(input)?;
            // Consume the opening bracket
            let field_type = struct_field_type(state).parse_next(input)?;

            whitespace_and_comments_opt(input)?;

            let attrs = opt(attribute_list(state, AttributeTarget::StructField))
                .parse_next(input)?
                .map(|attrs| attrs.attrs);

            whitespace_and_comments_opt(input)?;

            literal(";")
                .context(StrContext::Expected(StrContextValue::StringLiteral(";")))
                .parse_next(input)?;

            Ok(StructField {
                name: ident,
                field_type,
                comments,
                attributes: attrs.unwrap_or_default(),
            })
        })
        .parse_next(input)
    }
}

pub fn struct_item<'a, 's: 'a>(
    state: &'a ParserState<'s>,
) -> impl Parser<&'s str, Struct<'s>, ContextError> + 'a {
    move |input: &mut _| {
        trace("struct", |input: &mut _| {
            let comments = whitespace_and_comments_opt(input)?;
            // Parse the keyword
            literal("struct").parse_next(input)?;

            // Get the struct ident
            let ident = ident.parse_next(input)?;

            let attrs = opt(attribute_list(state, AttributeTarget::StructItem))
                .parse_next(input)?
                .map(|attrs| attrs.attrs);

            whitespace_and_comments_opt(input)?;
            // Consume the opening bracket
            literal("{")
                .context(StrContext::Expected(StrContextValue::StringLiteral("{")))
                .parse_next(input)?;

            // Consume whitespace instead of comments here so the any comments get
            // added to the field
            whitespace_all(input)?;

            let mut field_idents = HashSet::new();
            // Consume as many struct fields as possible
            let fields = repeat(0.., struct_field(state, &mut field_idents)).parse_next(input)?;

            whitespace_and_comments_opt(input)?;
            literal("}")
                .context(StrContext::Expected(StrContextValue::StringLiteral("}")))
                .parse_next(input)?;

            Ok(Struct {
                name: ident,
                namespace: state.namespace(),
                fields,
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

    use rstest::rstest;

    use crate::parse::{flatbuffers::primitives::ScalarType, parser::TypeDecls};

    use super::*;

    #[rstest]
    #[case::simple(
        r#"struct Hello {
            foo:uint32;
        }"#,
        Struct {
            name: "Hello",
            namespace: "",
            fields: vec![StructField {
                name: "foo",
                field_type: StructFieldType::Scalar(ScalarType::UInt32),
                comments: Vec::new(),
                attributes: Vec::new(),
            }],
            comments: Vec::new(),
            attributes: Vec::new(),
        }
    )]
    #[case::comments(
        r#"// This is NOT documentation
        /// This is a comment!
        struct Hello {
            foo:uint32;
            /// This should be ignored
        }"#,
        Struct {
            name: "Hello",
            namespace: "",
            fields: vec![StructField {
                name: "foo",
                field_type: StructFieldType::Scalar(ScalarType::UInt32),
                comments: Vec::new(),
                attributes: Vec::new(),
            }],
            comments: vec!["This is a comment!"],
            attributes: Vec::new(),
        }
    )]
    #[case::attributes(
        r#"struct Hello (force_align: 10) {
            foo:uint32;
        }"#,
        Struct {
            name: "Hello",
            namespace: "",
            fields: vec![StructField {
                name: "foo",
                field_type: StructFieldType::Scalar(ScalarType::UInt32),
                comments: Vec::new(),
                attributes: Vec::new(),
            }],
            comments: Vec::new(),
            attributes: vec![Attribute::ForceAlign(10)],
        }
    )]
    fn struct_pass(#[case] item_str: &str, #[case] output: Struct) {
        let state = ParserState::new();

        assert_eq!(struct_item(&state).parse(item_str), Ok(output));
    }

    #[rstest]
    #[case::enum_style(
        r#"struct Hello_There : uint32 {
            foo:uint32;
        }"#
    )]
    #[case::extra_semicolon(
        r#"struct Hello_There {
            foo:uint32;;
        }"#
    )]
    #[case::unclosed_bracket(
        r#"struct Hello_There {
            foo:uint32;"#
    )]
    #[case::duplicate_field(
        r#"struct Hello {
            foo:uint32;
            foo:uint32;
        }"#
    )]
    fn struct_fail(#[case] item_str: &str) {
        let state = ParserState::new();

        assert!(struct_item(&state).parse(item_str).is_err());
    }

    #[rstest]
    #[case::simple(
        "foo:uint32;",
        StructField {
            name: "foo",
            field_type: StructFieldType::Scalar(ScalarType::UInt32),
            comments: Vec::new(),
            attributes: Vec::new(),
        }
    )]
    #[case::comments(
        r#"/// Foo comment
        foo:uint32;"#,
        StructField {
            name: "foo",
            field_type: StructFieldType::Scalar(ScalarType::UInt32),
            comments: vec!["Foo comment"],
            attributes: Vec::new(),
        }
    )]
    #[case::attributes(
        "foo:uint32 (custom_attr);",
        StructField {
            name: "foo",
            field_type: StructFieldType::Scalar(ScalarType::UInt32),
            comments: Vec::new(),
            attributes: vec![Attribute::Custom { name: "custom_attr", value: None }],
        }
    )]
    #[case::whitespace(
        "\n foo \n : \n uint32 \n ;",
        StructField {
            name: "foo",
            field_type: StructFieldType::Scalar(ScalarType::UInt32),
            comments: Vec::new(),
            attributes: Vec::new(),
        }
    )]
    fn struct_field_pass(#[case] item_str: &str, #[case] output: StructField) {
        let state = ParserState::new();

        assert_eq!(
            struct_field(&state, &mut HashSet::new()).parse(item_str),
            Ok(output)
        );
    }

    #[rstest]
    #[case::missing_colon("foo uint32;")]
    #[case::missing_semicolon("foo:uint32")]
    #[case::invalid_type("foo:Table1;")]
    fn struct_field_fail(#[case] item_str: &str) {
        let mut state = ParserState::new();

        let mut foo_decl = TypeDecls::new();
        foo_decl.add_tables(["Table1"]);

        let decls = HashMap::from([("", foo_decl.clone())]);

        state.extend_decls(decls);

        assert!(struct_field(&state, &mut HashSet::new())
            .parse(item_str)
            .is_err());
    }
}
