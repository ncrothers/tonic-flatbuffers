use winnow::{
    combinator::opt,
    error::{AddContext, ContextError, ErrMode, ParserError, StrContext, StrContextValue},
    token::literal,
    PResult, Parser,
};

use crate::utils::{consume_whitespace, consume_whitespace_and_comments, parse_ident, IdentParser};

use super::{
    attributes::{Attribute, AttributeSectionParser},
    primitives::{ParseStructFieldType, StructFieldType},
};

#[derive(Debug, PartialEq)]
pub struct StructField<'a> {
    name: &'a str,
    field_type: StructFieldType<'a>,
    comments: Vec<&'a str>,
    attributes: Vec<Attribute<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Struct<'a> {
    name: &'a str,
    fields: Vec<StructField<'a>>,
    comments: Vec<&'a str>,
    attributes: Vec<Attribute<'a>>,
}

struct StructFieldParser;

impl<'s, E> Parser<&'s str, StructField<'s>, E> for StructFieldParser
where
    E: AddContext<&'s str, StrContext> + ParserError<&'s str>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<StructField<'s>, E> {
        let comments = consume_whitespace_and_comments(input)?;
        // Get the field ident
        let ident = IdentParser
            .context(StrContext::Expected(StrContextValue::Description(
                "struct field identifier",
            )))
            .parse_next(input)?;

        literal(":")
            .context(StrContext::Expected(StrContextValue::StringLiteral(":")))
            .parse_next(input)?;

        consume_whitespace_and_comments(input)?;
        // Consume the opening bracket
        let field_type = ParseStructFieldType.parse_next(input)?;

        consume_whitespace_and_comments(input)?;

        let attrs = opt(AttributeSectionParser).parse_next(input)?;

        consume_whitespace_and_comments(input)?;

        literal(";")
            .context(StrContext::Expected(StrContextValue::StringLiteral(";")))
            .parse_next(input)?;

        Ok(StructField {
            name: ident,
            field_type,
            comments,
            attributes: attrs.unwrap_or_default(),
        })
    }
}

struct ParseStruct;

impl<'s, E> Parser<&'s str, Struct<'s>, E> for ParseStruct
where
    E: AddContext<&'s str, StrContext> + ParserError<&'s str>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<Struct<'s>, E> {
        let comments = consume_whitespace_and_comments(input)?;
        // Parse the keyword
        literal("struct").parse_next(input)?;

        // Get the struct ident
        let ident = parse_ident(input)?;

        let attrs = opt(AttributeSectionParser).parse_next(input)?;

        consume_whitespace_and_comments(input)?;
        // Consume the opening bracket
        literal("{")
            .context(StrContext::Expected(StrContextValue::StringLiteral("{")))
            .parse_next(input)?;

        // Consume whitespace instead of comments here so the any comments get
        // added to the field
        consume_whitespace(input)?;

        let mut fields = Vec::new();

        // Consume as many struct fields as possible
        while let Some(field) = opt(StructFieldParser).parse_next(input)? {
            fields.push(field);
        }

        consume_whitespace_and_comments(input)?;
        literal("}")
            .context(StrContext::Expected(StrContextValue::StringLiteral("}")))
            .parse_next(input)?;

        Ok(Struct {
            name: ident,
            fields,
            comments,
            attributes: attrs.unwrap_or_default(),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::flatbuffers::primitives::{Array, ArrayItemType, ScalarType};

    use super::*;

    #[test]
    fn r#struct() {
        let struct1_str = r#"
            struct Hello {
                foo:uint32;
            }"#;

        let struct1 = Struct {
            name: "Hello",
            fields: vec![StructField {
                name: "foo",
                field_type: StructFieldType::Scalar(ScalarType::UInt32),
                comments: Vec::new(),
                attributes: Vec::new(),
            }],
            comments: Vec::new(),
            attributes: Vec::new(),
        };

        let struct2_str = r#"
            // This is NOT documentation
            /// This is a comment!
            struct Hello_There (force_align: 10) {
                /// This is field documentation
                foo:[int32:50];
                /// Bar comment
                bar:
                    /// This is a random comment that shouldn't be loaded
                    float  ;
                another: [
                    Struct2:
                    5
                ];
                /// This should be ignored
            }"#;

        let struct2 = Struct {
            name: "Hello_There",
            fields: vec![
                StructField {
                    name: "foo",
                    field_type: StructFieldType::Array(Array {
                        item_type: ArrayItemType::Scalar(ScalarType::Int32),
                        length: 50,
                    }),
                    comments: vec!["This is field documentation"],
                    attributes: Vec::new(),
                },
                StructField {
                    name: "bar",
                    field_type: StructFieldType::Scalar(ScalarType::Float32),
                    comments: vec!["Bar comment"],
                    attributes: Vec::new(),
                },
                StructField {
                    name: "another",
                    field_type: StructFieldType::Array(Array {
                        item_type: ArrayItemType::Named("Struct2"),
                        length: 5,
                    }),
                    comments: Vec::new(),
                    attributes: Vec::new(),
                },
            ],
            comments: vec!["This is a comment!"],
            attributes: vec![Attribute::ForceAlign(10)],
        };

        let valid = [(struct1_str, struct1), (struct2_str, struct2)];

        for (item_str, item) in valid {
            let mut value = item_str;
            let res = ParseStruct.parse(value).inspect_err(|e| println!("{e}"));
            assert_eq!(res, Ok(item));
        }

        let struct_invalid1 = r#"
            struct Hello_There {
                foo:uint32
            }
        "#;

        let struct_invalid2 = r#"
            struct Hello_There {
                foo:[uint32]
            }
        "#;

        let struct_invalid3 = r#"
            struct Hello There {}
        "#;

        let invalid = [struct_invalid1, struct_invalid2, struct_invalid3];

        for item in invalid {
            let mut value = item;
            assert!(ParseStruct.parse_next(&mut value).is_err());
        }
    }
}
