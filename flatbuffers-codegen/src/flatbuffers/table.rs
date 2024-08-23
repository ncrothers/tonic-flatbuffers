use winnow::{
    combinator::{cut_err, opt, preceded, trace},
    error::{ContextError, StrContext, StrContextValue},
    token::literal,
    Parser,
};

use crate::{
    parser::ParserState,
    utils::{default_value, ident, whitespace_all, whitespace_and_comments_opt},
};

use super::{
    attributes::{attribute_list, Attribute},
    primitives::{table_field_type, DefaultValue, TableFieldType},
};

#[derive(Debug, PartialEq)]
pub struct TableField<'a> {
    name: &'a str,
    field_type: TableFieldType<'a>,
    default: Option<DefaultValue<'a>>,
    comments: Vec<&'a str>,
    attributes: Vec<Attribute<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Table<'a> {
    name: &'a str,
    namespace: &'a str,
    fields: Vec<TableField<'a>>,
    comments: Vec<&'a str>,
    attributes: Vec<Attribute<'a>>,
}

fn table_field<'a, 's: 'a>(
    state: &'a ParserState<'s>,
) -> impl Parser<&'s str, TableField<'s>, ContextError> + 'a {
    move |input: &mut _| {
        trace("table_field", |input: &mut _| {
            let comments = whitespace_and_comments_opt(input)?;
            // Get the field ident
            let ident = ident.parse_next(input)?;
            whitespace_and_comments_opt(input)?;

            literal(":").parse_next(input)?;

            whitespace_and_comments_opt(input)?;

            let field_type = table_field_type.parse_next(input)?;

            whitespace_and_comments_opt(input)?;

            let default = opt(preceded("=", default_value(&field_type))).parse_next(input)?;

            whitespace_and_comments_opt(input)?;

            let attrs = opt(attribute_list(state)).parse_next(input)?;

            literal(";").parse_next(input)?;

            Ok(TableField {
                name: ident,
                field_type,
                default,
                comments,
                attributes: attrs.unwrap_or_default(),
            })
        })
        .parse_next(input)
    }
}

pub fn table_item<'a, 's: 'a>(
    state: &'a ParserState<'s>,
) -> impl Parser<&'s str, Table<'s>, ContextError> + 'a {
    move |input: &mut _| {
        trace("table", |input: &mut _| {
            let comments = whitespace_and_comments_opt(input)?;
            // Parse the keyword
            literal("table")
                .context(StrContext::Expected(StrContextValue::StringLiteral(
                    "table",
                )))
                .parse_next(input)?;

            // Get the table ident
            let ident = cut_err(ident).parse_next(input)?;

            let attrs = opt(attribute_list(state)).parse_next(input)?;

            whitespace_and_comments_opt(input)?;
            // Consume the opening bracket
            cut_err(literal("{").context(StrContext::Expected(StrContextValue::CharLiteral('{'))))
                .parse_next(input)?;

            // Consume whitespace instead of comments here so the any comments get
            // added to the field
            whitespace_all(input)?;

            let mut fields = Vec::new();

            // Consume as many table fields as possible
            while let Some(field) = opt(table_field(state)).parse_next(input)? {
                fields.push(field);
            }

            whitespace_and_comments_opt(input)?;
            cut_err(literal("}").context(StrContext::Expected(StrContextValue::CharLiteral('}'))))
                .parse_next(input)?;

            Ok(Table {
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
    use crate::flatbuffers::primitives::{ScalarType, VectorItemType};

    use super::*;

    #[test]
    fn table() {
        let table1_str = r#"
            table Hello {
                foo:uint32;
            }"#;

        let table1 = Table {
            name: "Hello",
            namespace: "",
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

        let table2_str = r#"
            // This is NOT documentation
            /// This is a comment!
            table Hello_There (original_order) {
                /// This is field documentation
                foo:[int32];
                bar:
                    /// This is a random comment that shouldn't be loaded
                    float = -1e-6 (deprecated);
                /// Another comment
                another: [
                    Struct2
                ];
                enum_field: TestEnum = Variant1;
                /// This should be ignored
            }"#;

        let table2 = Table {
            name: "Hello_There",
            namespace: "",
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
                    default: Some(DefaultValue::Float32(-1.0e-6)),
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
                TableField {
                    name: "enum_field",
                    field_type: TableFieldType::Named("TestEnum"),
                    default: Some(DefaultValue::Named("Variant1")),
                    comments: Vec::new(),
                    attributes: Vec::new(),
                },
            ],
            comments: vec!["This is a comment!"],
            attributes: vec![Attribute::OriginalOrder],
        };

        let state = ParserState::new();

        let valid = [(table1_str, table1), (table2_str, table2)];

        for (item_str, item) in valid {
            assert_eq!(
                table_item(&state)
                    .parse(item_str)
                    .inspect_err(|e| println!("{e}")),
                Ok(item)
            );
        }

        let table_invalid1 = r#"
            table Hello_There {
                foo:uint32
            }"#;

        let table_invalid2 = r#"
            table Hello_There {
                foo:[uint32:5]
            }"#;

        let table_invalid3 = r#"
            table Hello There {}"#;

        let table_invalid4 = r#"
            table Test {
                foo:uint32 = 1.5;
            }"#;

        let invalid = [
            table_invalid1,
            table_invalid2,
            table_invalid3,
            table_invalid4,
        ];

        for item in invalid {
            assert!(table_item(&state).parse(item).is_err());
        }
    }
}
