use std::collections::HashSet;

use winnow::{
    combinator::{cut_err, opt, preceded, repeat, trace},
    error::{AddContext, ContextError, ErrMode, StrContext, StrContextValue},
    stream::Stream,
    token::literal,
    Parser,
};

use crate::{
    parser::{DeclType, NamedType, ParserState},
    utils::{default_value, ident, whitespace_all, whitespace_and_comments_opt},
};

use super::{
    attributes::{attribute_list, Attribute, AttributeTarget, AttributesWrapper, StrCheckpoint},
    primitives::{table_field_type, DefaultValue, TableFieldType},
};

#[derive(Debug)]
pub struct TableField<'a> {
    name: &'a str,
    field_type: TableFieldType<'a>,
    default: Option<DefaultValue<'a>>,
    comments: Vec<&'a str>,
    attributes: Option<AttributesWrapper<'a>>,
    attr_start: Option<StrCheckpoint<'a>>,
}

impl<'a> PartialEq for TableField<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.field_type == other.field_type
            && self.default == other.default
            && self.comments == other.comments
            && self.attributes == other.attributes
    }
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
    field_idents: &'a mut HashSet<&'s str>,
    require_id: &'a mut Option<bool>,
) -> impl Parser<&'s str, TableField<'s>, ContextError> + 'a {
    move |input: &mut _| {
        trace("table_field", |input: &mut _| {
            let comments = whitespace_and_comments_opt(input)?;
            let ident_chk = input.checkpoint();
            // Get the field ident
            let ident = ident.parse_next(input)?;

            if field_idents.contains(&ident) {
                input.reset(&ident_chk);
                return Err(ErrMode::Cut(ContextError::new().add_context(
                    input,
                    &ident_chk,
                    StrContext::Label("; duplicate field name"),
                )));
            }

            field_idents.insert(ident);

            whitespace_and_comments_opt(input)?;

            cut_err(literal(":")).parse_next(input)?;

            whitespace_and_comments_opt(input)?;

            let field_type = cut_err(table_field_type(state)).parse_next(input)?;

            whitespace_and_comments_opt(input)?;

            // If the next token is =, require a default
            let default = if input.starts_with('=') {
                Some(cut_err(preceded("=", default_value(&field_type))).parse_next(input)?)
            } else {
                None
            };

            whitespace_and_comments_opt(input)?;

            let attrs_start = input.checkpoint();

            let attrs = if input.starts_with('(') {
                Some(
                    attribute_list(state, AttributeTarget::TableField).parse_next(input)?, //.map(|attrs| attrs.attrs)?,
                )
            } else {
                let checkpoint = input.checkpoint();
                if let Some(require_id) = require_id {
                    if *require_id {
                        return Err(ErrMode::Cut(ContextError::new().add_context(
                            input,
                            &checkpoint,
                            StrContext::Label("attribute; id must be on all fields or none"),
                        )));
                    } else {
                        None
                    }
                } else {
                    *require_id = Some(false);
                    None
                }
            };

            literal(";").parse_next(input)?;

            Ok(TableField {
                name: ident,
                field_type,
                default,
                comments,
                attributes: attrs,
                attr_start: Some(attrs_start),
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

            let attrs = opt(attribute_list(state, AttributeTarget::TableItem))
                .parse_next(input)?
                .map(|attrs| attrs.attrs);

            whitespace_and_comments_opt(input)?;
            // Consume the opening bracket
            cut_err(literal("{").context(StrContext::Expected(StrContextValue::CharLiteral('{'))))
                .parse_next(input)?;

            // Consume whitespace instead of comments here so the any comments get
            // added to the field
            whitespace_all(input)?;

            // let mut fields = Vec::new();
            let mut require_id = None;

            let mut field_idents = HashSet::new();
            // Consume as many table fields as possible
            let fields: Vec<_> =
                cut_err(repeat(0.., table_field(state, &mut field_idents, &mut require_id))).parse_next(input)?;

            // Validate the ID attributes
            let n_fields = fields.len() as u64;
            if n_fields > 0 {
                macro_rules! err {
                    ($chk:expr) => {
                        input.reset($chk);
                        return Err(ErrMode::Cut(ContextError::new().add_context(input, $chk, StrContext::Label("id; ids must start at 0 and increase continuously"))));
                    };
                    ($chk:expr, $msg:expr) => {
                        input.reset($chk);
                        return Err(ErrMode::Cut(ContextError::new().add_context(input, $chk, StrContext::Label($msg))));
                    };
                }
                // let mut err = |chk: &'s StrCheckpoint| {

                // };

                let has_ids = fields
                    .iter()
                    .find_map(|field| {
                        field
                            .attributes
                            .as_ref()
                            .map(|attrs| {
                                attrs
                                    .attrs
                                    .iter()
                                    .find_map(|attr| if matches!(attr, Attribute::Id(_)) {
                                        Some(true)
                                    } else {
                                        None
                                    })
                        })
                        .unwrap_or(Some(false))
                    }).unwrap_or(false);

                if has_ids {
                    let mut i = 0;

                    'index: for n in 0..n_fields {
                        let mut smallest = u64::MAX;
                        let mut smallest_chk = &input.checkpoint();
                        let mut id_set = HashSet::new();

                        // Find the next index from the fields
                        for field in &fields {
                            // Check the attributes
                            if let Some(attrs) = &field.attributes {
                                let mut has_id = false;
                                // Check each attribute
                                for (attr, chk) in attrs.attrs.iter().zip(attrs.attr_chks.iter()) {
                                    // If this is the ID, check if it matches
                                    if let Attribute::Id(field_id) = attr {
                                        has_id = true;
                                        // A union takes up 2 spots, so its index should be i+1
                                        if matches!(field.field_type, TableFieldType::Named(NamedType { ident: _, decl_type: DeclType::Union })) {
                                            if *field_id == 0 {
                                                err!(chk, "id; union ids can only be 1 at the lowest");
                                            }

                                            // Check for overlap in IDs
                                            if id_set.contains(field_id) || id_set.contains(&(field_id-1)) {
                                                err!(chk, "id; id value was set twice");
                                            } else {
                                                id_set.insert(*field_id);
                                                id_set.insert(*field_id - 1);
                                            }

                                            if *field_id == i+1 {
                                                i += 2;
                                                continue 'index;
                                            }
                                            // If it's equal to what it would be if not a union, error out and notify the user
                                            else if *field_id == i {
                                                err!(chk, "id; unions use 2 entries, so the union id must skip one value");
                                            }
                                        } else {
                                            // Check for overlap in IDs
                                            if id_set.contains(field_id) {
                                                err!(chk, "id; id value was set twice");
                                            } else {
                                                id_set.insert(*field_id);
                                            }

                                            if *field_id == i {
                                                i += 1;
                                                continue 'index;
                                            } else if *field_id <= smallest && *field_id >= n {
                                                smallest = *field_id;
                                                smallest_chk = chk;
                                            }
                                        }
                                    }
                                }

                                if !has_id {
                                    err!(&field.attr_start.unwrap(), "attribute; when using the id attribute, all fields must have it");
                                }
                            } else {
                                // If there are no attributes, that means there's an error
                                err!(&field.attr_start.unwrap(), "attribute; when using the id attribute, all fields must have it");
                            }
                        }

                        err!(smallest_chk);
                    }
                }
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
    use std::collections::HashMap;

    use rstest::rstest;

    use crate::{flatbuffers::primitives::ScalarType, parser::TypeDecls};

    use super::*;

    #[rstest]
    #[case::simple(
        r#"table Hello {
            foo:uint32;
        }"#,
        Table {
            name: "Hello",
            namespace: "",
            fields: vec![TableField {
                name: "foo",
                field_type: TableFieldType::Scalar(ScalarType::UInt32),
                default: None,
                comments: Vec::new(),
                attributes: None,
                attr_start: None,
            }],
            comments: Vec::new(),
            attributes: Vec::new(),
        }
    )]
    #[case::comments(
        r#"// This is NOT documentation
        /// This is a comment!
        table Hello {
            foo:uint32;
            /// This should be ignored
        }"#,
        Table {
            name: "Hello",
            namespace: "",
            fields: vec![TableField {
                name: "foo",
                field_type: TableFieldType::Scalar(ScalarType::UInt32),
                default: None,
                comments: Vec::new(),
                attributes: None,
                attr_start: None,
            }],
            comments: vec!["This is a comment!"],
            attributes: Vec::new(),
        }
    )]
    #[case::attributes(
        r#"table Hello (original_order) {
            foo:uint32;
        }"#,
        Table {
            name: "Hello",
            namespace: "",
            fields: vec![TableField {
                name: "foo",
                field_type: TableFieldType::Scalar(ScalarType::UInt32),
                default: None,
                comments: Vec::new(),
                attributes: None,
                attr_start: None,
            }],
            comments: Vec::new(),
            attributes: vec![Attribute::OriginalOrder],
        }
    )]
    fn table_pass(#[case] item_str: &str, #[case] output: Table) {
        let state = ParserState::new();

        assert_eq!(table_item(&state).parse(item_str), Ok(output));
    }

    #[rstest]
    #[case::enum_style(
        r#"table Test : uint32 {
            foo:uint32;
        }"#
    )]
    #[case::extra_semicolon(
        r#"table Test {
            foo:uint32;;
        }"#
    )]
    #[case::unclosed_bracket(
        r#"struct Hello_There {
            foo:uint32;"#
    )]
    #[case::duplicate_field(
        r#"table Hello {
            foo:uint32;
            foo:uint32;
        }"#
    )]
    fn table_fail(#[case] item_str: &str) {
        let state = ParserState::new();

        assert!(table_item(&state).parse(item_str).is_err());
    }

    #[rstest]
    #[case::simple(
        "foo:uint32;",
        TableField {
            name: "foo",
            field_type: TableFieldType::Scalar(ScalarType::UInt32),
            default: None,
            comments: Vec::new(),
            attributes: None,
            attr_start: None,
        }
    )]
    #[case::comments(
        r#"/// Foo comment
        foo:uint32;"#,
        TableField {
            name: "foo",
            field_type: TableFieldType::Scalar(ScalarType::UInt32),
            default: None,
            comments: vec!["Foo comment"],
            attributes: None,
            attr_start: None,
        }
    )]
    #[case::attributes(
        "foo:uint32 (id: 1);",
        TableField {
            name: "foo",
            field_type: TableFieldType::Scalar(ScalarType::UInt32),
            default: None,
            comments: Vec::new(),
            attributes: Some(AttributesWrapper {
                attrs: vec![Attribute::Id(1)],
                attr_chks: Vec::new(),
                id: None
            }),
            attr_start: None,
        }
    )]
    #[case::whitespace(
        "\n foo \n : \n uint32 \n ;",
        TableField {
            name: "foo",
            field_type: TableFieldType::Scalar(ScalarType::UInt32),
            default: None,
            comments: Vec::new(),
            attributes: None,
            attr_start: None,
        }
    )]
    fn table_field_pass(#[case] item_str: &str, #[case] output: TableField) {
        let state = ParserState::new();

        let mut require_id = None;
        assert_eq!(
            table_field(&state, &mut HashSet::new(), &mut require_id).parse(item_str),
            Ok(output)
        );
    }

    #[rstest]
    #[case::missing_colon("foo uint32;")]
    #[case::missing_semicolon("foo:uint32")]
    fn table_field_fail(#[case] item_str: &str) {
        let mut state = ParserState::new();

        let mut foo_decl = TypeDecls::new();
        foo_decl.add_tables(["Table1"]);

        let decls = HashMap::from([("", foo_decl.clone())]);

        state.extend_decls(decls);

        let mut require_id = None;
        assert!(table_field(&state, &mut HashSet::new(), &mut require_id)
            .parse(item_str)
            .is_err());
    }
}
