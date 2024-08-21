use winnow::{
    combinator::{opt, preceded},
    error::{AddContext, ContextError, ErrMode, ParserError, StrContext},
    seq,
    token::literal,
    PResult, Parser,
};

use crate::utils::{
    consume_whitespace, consume_whitespace_and_comments, parse_ident, WhitespacePrefixedParser,
};

use super::{
    attributes::{Attribute, AttributeSectionParser},
    primitives::{ParseTypeIdent, TableFieldType},
};

#[derive(Debug, PartialEq)]
pub struct TableField<'a> {
    name: &'a str,
    field_type: TableFieldType<'a>,
    default: Option<&'a str>,
    comments: Vec<&'a str>,
    attributes: Vec<Attribute<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Table<'a> {
    name: &'a str,
    fields: Vec<TableField<'a>>,
    comments: Vec<&'a str>,
    attributes: Vec<Attribute<'a>>,
}

struct TableFieldParser;

impl<'s, E> Parser<&'s str, TableField<'s>, E> for TableFieldParser
where
    E: AddContext<&'s str, StrContext> + ParserError<&'s str>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<TableField<'s>, E> {
        let comments = consume_whitespace_and_comments(input)?;
        // Get the field ident
        let ident = parse_ident(input)?;

        literal(":").parse_next(input)?;

        consume_whitespace_and_comments(input)?;
        // Consume the opening bracket
        let field_type = ParseTypeIdent.parse_next(input)?;

        consume_whitespace_and_comments(input)?;

        let default = opt(preceded("=", WhitespacePrefixedParser)).parse_next(input)?;

        consume_whitespace_and_comments(input)?;

        let attrs = opt(AttributeSectionParser).parse_next(input)?;

        literal(";").parse_next(input)?;

        Ok(TableField {
            name: ident,
            field_type,
            default,
            comments,
            attributes: attrs.unwrap_or_default(),
        })
    }
}

struct TableParser;

impl<'s, E> Parser<&'s str, Table<'s>, E> for TableParser
where
    E: AddContext<&'s str, StrContext> + ParserError<&'s str>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<Table<'s>, E> {
        let comments = consume_whitespace_and_comments(input)?;
        // Parse the keyword
        literal("table").parse_next(input)?;

        // Get the struct ident
        let ident = parse_ident(input)?;

        let attrs = opt(AttributeSectionParser).parse_next(input)?;

        consume_whitespace_and_comments(input)?;
        // Consume the opening bracket
        literal("{").parse_next(input)?;

        // Consume whitespace instead of comments here so the any comments get
        // added to the field
        consume_whitespace(input)?;

        let mut fields = Vec::new();

        // Consume as many table fields as possible
        while let Some(field) = opt(TableFieldParser).parse_next(input)? {
            fields.push(field);
        }

        consume_whitespace_and_comments(input)?;
        literal("}").parse_next(input)?;

        Ok(Table {
            name: ident,
            fields,
            comments,
            attributes: attrs.unwrap_or_default(),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::flatbuffers::primitives::{
        Array, ArrayItemType, BuiltInTable, ScalarType, VectorItemType,
    };

    use super::*;

    #[test]
    fn r#struct() {
        let struct1_str = r#"
            table Hello {
                foo:uint32;
            }
        "#;

        let struct1 = Table {
            name: "Hello",
            fields: vec![TableField {
                name: "foo",
                field_type: TableFieldType::Scalar(ScalarType::UInt32),
                default: None,
                comments: Vec::new(),
                attributes: Vec::new(),
            }],
            comments: Vec::new(),
            attributes: Vec::new(),
        };

        let struct2_str = r#"
            // This is NOT documentation
            /// This is a comment!
            table Hello_There (original_order) {
                /// This is field documentation
                foo:[int32];
                bar:
                    /// This is a random comment that shouldn't be loaded
                    float = 1.5 (deprecated);
                /// Another comment
                another: [
                    Struct2
                ];
                /// This should be ignored
            }
        "#;

        let struct2 = Table {
            name: "Hello_There",
            fields: vec![
                TableField {
                    name: "foo",
                    field_type: TableFieldType::Vector(VectorItemType::Scalar(ScalarType::Int32)),
                    default: None,
                    comments: vec!["This is field documentation"],
                    attributes: Vec::new(),
                },
                TableField {
                    name: "bar",
                    field_type: TableFieldType::Scalar(ScalarType::Float32),
                    default: Some("1.5"),
                    comments: Vec::new(),
                    attributes: vec![Attribute::Deprecated],
                },
                TableField {
                    name: "another",
                    field_type: TableFieldType::Vector(VectorItemType::Named("Struct2")),
                    default: None,
                    comments: vec!["Another comment"],
                    attributes: Vec::new(),
                },
            ],
            comments: vec!["This is a comment!"],
            attributes: vec![Attribute::OriginalOrder],
        };

        let valid = [(struct1_str, struct1), (struct2_str, struct2)];

        for (item_str, item) in valid {
            let mut value = item_str;
            assert_eq!(TableParser.parse_next(&mut value), Ok(item));
        }

        let struct_invalid1 = r#"
            table Hello_There {
                foo:uint32
            }
        "#;

        let struct_invalid2 = r#"
            table Hello_There {
                foo:[uint32:5]
            }
        "#;

        let struct_invalid3 = r#"
            table Hello There {}
        "#;

        let invalid = [struct_invalid1, struct_invalid2, struct_invalid3];

        for item in invalid {
            let mut value = item;
            assert!(TableParser.parse_next(&mut value).is_err());
        }
    }
}
