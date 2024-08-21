use winnow::{
    combinator::terminated,
    error::{AddContext, ContextError, ErrMode, StrContext},
    seq,
    stream::Stream,
    token::{literal, take_while},
    PResult, Parser,
};

use crate::utils::{consume_whitespace, parse_ident, IdentParser};

use super::primitives::{ParseStructFieldType, StructFieldType};

#[derive(Debug, PartialEq)]
pub struct StructField<'a> {
    name: &'a str,
    field_type: StructFieldType<'a>,
}

#[derive(Debug, PartialEq)]
pub struct Struct<'a> {
    name: &'a str,
    fields: Vec<StructField<'a>>,
}

struct StructFieldParser;

impl<'s, E> Parser<&'s str, StructField<'s>, E> for StructFieldParser
where
    E: AddContext<&'s str, StrContext>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<StructField<'s>, E> {
        consume_whitespace(input)?;
        // Get the field ident
        let ident = parse_ident(input)?;

        literal(":").parse_next(input)?;

        consume_whitespace(input)?;
        // Consume the opening bracket
        let field_type = ParseStructFieldType.parse_next(input)?;

        consume_whitespace(input)?;

        literal(";").parse_next(input)?;

        Ok(StructField {
            name: ident,
            field_type,
        })
    }
}

struct ParseStruct;

impl<'s, E> Parser<&'s str, Struct<'s>, E> for ParseStruct
where
    E: AddContext<&'s str, StrContext>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<Struct<'s>, E> {
        consume_whitespace(input)?;
        // Parse the keyword
        literal("struct").parse_next(input)?;

        // Get the struct ident
        let ident = parse_ident(input)?;

        consume_whitespace(input)?;
        // Consume the opening bracket
        literal("{").parse_next(input)?;

        consume_whitespace(input)?;

        let mut fields = Vec::new();

        // While we aren't at the end of the struct
        while !input.starts_with("}") {
            let field = StructFieldParser.parse_next(input)?;
            fields.push(field);
            consume_whitespace(input)?;
        }

        Ok(Struct {
            name: ident,
            fields,
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
            }
        "#;

        let struct1 = Struct {
            name: "Hello",
            fields: vec![StructField {
                name: "foo",
                field_type: StructFieldType::Scalar(ScalarType::UInt32),
            }],
        };

        let struct2_str = r#"
            struct Hello_There {
                foo:[int32:50];
                bar:
                    float  ;
                another: [
                    Struct2:
                    5
                ];
            }
        "#;

        let struct2 = Struct {
            name: "Hello_There",
            fields: vec![
                StructField {
                    name: "foo",
                    field_type: StructFieldType::Array(Array {
                        item_type: ArrayItemType::Scalar(ScalarType::Int32),
                        length: 50,
                    }),
                },
                StructField {
                    name: "bar",
                    field_type: StructFieldType::Scalar(ScalarType::Float32),
                },
                StructField {
                    name: "another",
                    field_type: StructFieldType::Array(Array {
                        item_type: ArrayItemType::Named("Struct2"),
                        length: 5,
                    }),
                },
            ],
        };

        let valid = [(struct1_str, struct1), (struct2_str, struct2)];

        for (item_str, item) in valid {
            let mut value = item_str;
            assert_eq!(ParseStruct.parse_next(&mut value), Ok(item));
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
