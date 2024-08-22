use winnow::{
    combinator::{opt, trace},
    error::{StrContext, StrContextValue},
    token::literal,
    PResult, Parser,
};

use crate::utils::{ident, whitespace_all, whitespace_and_comments_opt};

use super::{
    attributes::{attribute_list, Attribute},
    primitives::{struct_field_type, StructFieldType},
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

fn struct_field<'s>(input: &mut &'s str) -> PResult<StructField<'s>> {
    trace("struct_field", |input: &mut _| {
        let comments = whitespace_and_comments_opt(input)?;
        // Get the field ident
        let ident = ident
            .context(StrContext::Expected(StrContextValue::Description(
                "struct field identifier",
            )))
            .parse_next(input)?;

        literal(":")
            .context(StrContext::Expected(StrContextValue::StringLiteral(":")))
            .parse_next(input)?;

        whitespace_and_comments_opt(input)?;
        // Consume the opening bracket
        let field_type = struct_field_type.parse_next(input)?;

        whitespace_and_comments_opt(input)?;

        let attrs = opt(attribute_list).parse_next(input)?;

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

pub fn struct_item<'s>(input: &mut &'s str) -> PResult<Struct<'s>> {
    trace("struct", |input: &mut _| {
        let comments = whitespace_and_comments_opt(input)?;
        // Parse the keyword
        literal("struct").parse_next(input)?;

        // Get the struct ident
        let ident = ident.parse_next(input)?;

        let attrs = opt(attribute_list).parse_next(input)?;

        whitespace_and_comments_opt(input)?;
        // Consume the opening bracket
        literal("{")
            .context(StrContext::Expected(StrContextValue::StringLiteral("{")))
            .parse_next(input)?;

        // Consume whitespace instead of comments here so the any comments get
        // added to the field
        whitespace_all(input)?;

        let mut fields = Vec::new();

        // Consume as many struct fields as possible
        while let Some(field) = opt(struct_field).parse_next(input)? {
            fields.push(field);
        }

        whitespace_and_comments_opt(input)?;
        literal("}")
            .context(StrContext::Expected(StrContextValue::StringLiteral("}")))
            .parse_next(input)?;

        Ok(Struct {
            name: ident,
            fields,
            comments,
            attributes: attrs.unwrap_or_default(),
        })
    })
    .parse_next(input)
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
            let value = item_str;
            let res = struct_item.parse(value).inspect_err(|e| println!("{e}"));
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
            assert!(struct_item.parse_next(&mut value).is_err());
        }
    }
}
