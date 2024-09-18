use std::collections::HashSet;

use winnow::{
    ascii::digit1,
    combinator::{alt, cut_err, not, opt, separated, trace},
    error::{AddContext, ContextError, ErrMode, StrContext, StrContextValue},
    seq,
    stream::{Checkpoint, Stream},
    token::literal,
    Parser,
};

use crate::parse::{
    parser::ParserState,
    utils::{ident, string_literal, whitespace_and_comments_opt},
};

use super::rpc_service::StreamingMode;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AttributeTarget {
    EnumItem,
    EnumVariant,
    StructItem,
    StructField,
    TableItem,
    TableField,
    UnionItem,
    UnionVariant,
    RpcService,
    RpcMethod,
}

pub type StrCheckpoint<'a> = Checkpoint<&'a str, &'a str>;

#[derive(Clone, Debug, Default)]
pub struct AttributesWrapper<'a> {
    pub attrs: Vec<Attribute<'a>>,
    pub attr_chks: Vec<StrCheckpoint<'a>>,
    pub id: Option<StrCheckpoint<'a>>,
}

impl<'a> PartialEq for AttributesWrapper<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.attrs == other.attrs
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Attribute<'a> {
    BitFlags,
    Deprecated,
    FlexBuffer,
    ForceAlign(u64),
    Hash(&'a str),
    Id(u64),
    Key,
    NestedFlatBuffer(&'a str),
    OriginalOrder,
    Required,
    Streaming(StreamingMode),
    Custom {
        name: &'a str,
        value: Option<&'a str>,
    },
}

impl<'a> Attribute<'a> {
    pub fn from_ident(ident: &'a str) -> Self {
        match ident {
            "bit_flags" => Self::BitFlags,
            "deprecated" => Self::Deprecated,
            "flexbuffer" => Self::FlexBuffer,
            "force_align" => Self::ForceAlign(0),
            "hash" => Self::Hash(""),
            "id" => Self::Id(0),
            "key" => Self::Key,
            "nested_flatbuffer" => Self::NestedFlatBuffer(""),
            "original_order" => Self::OriginalOrder,
            "required" => Self::Required,
            "streaming" => Self::Streaming(StreamingMode::None),
            custom => Self::Custom {
                name: custom,
                value: None,
            },
        }
    }

    pub fn to_ident(&self) -> &'a str {
        match self {
            Attribute::BitFlags => "bit_flags",
            Attribute::Deprecated => "deprecated",
            Attribute::FlexBuffer => "flexbuffer",
            Attribute::ForceAlign(_) => "force_align",
            Attribute::Hash(_) => "hash",
            Attribute::Id(_) => "id",
            Attribute::Key => "key",
            Attribute::NestedFlatBuffer(_) => "nested_flatbuffer",
            Attribute::OriginalOrder => "original_order",
            Attribute::Required => "required",
            Attribute::Streaming(_) => "streaming",
            Attribute::Custom { name, .. } => name,
        }
    }

    pub fn is_valid(&self, target: AttributeTarget) -> bool {
        match self {
            Attribute::BitFlags => matches!(target, AttributeTarget::EnumItem),
            Attribute::Deprecated => matches!(target, AttributeTarget::TableField),
            Attribute::FlexBuffer => matches!(target, AttributeTarget::TableField),
            Attribute::ForceAlign(_) => matches!(target, AttributeTarget::StructItem),
            Attribute::Hash(_) => matches!(
                target,
                AttributeTarget::StructField | AttributeTarget::TableField
            ),
            Attribute::Id(_) => matches!(target, AttributeTarget::TableField),
            Attribute::Key => matches!(
                target,
                AttributeTarget::StructField | AttributeTarget::TableField
            ),
            Attribute::NestedFlatBuffer(_) => matches!(target, AttributeTarget::TableField),
            Attribute::OriginalOrder => matches!(target, AttributeTarget::TableItem),
            Attribute::Required => matches!(target, AttributeTarget::TableField),
            Attribute::Streaming(_) => matches!(target, AttributeTarget::RpcMethod),
            // Custom is always allowed
            Attribute::Custom { .. } => true,
        }
    }

    pub fn has_value(&self) -> bool {
        matches!(
            self,
            Self::ForceAlign(_)
                | Self::Hash(_)
                | Self::Id(_)
                | Self::NestedFlatBuffer(_)
                | Self::Streaming(_)
                | Self::Custom { .. }
        )
    }

    pub fn has_str_value(&self) -> bool {
        matches!(
            self,
            Self::Hash(_) | Self::NestedFlatBuffer(_) | Self::Streaming(_)
        )
    }

    pub fn has_u64_value(&self) -> bool {
        matches!(self, Self::ForceAlign(_) | Self::Id(_))
    }

    pub fn is_custom(&self) -> bool {
        matches!(self, Self::Custom { .. })
    }

    pub fn validate_str_value(&self, value: &str) -> bool {
        match self {
            Self::Streaming(_) => ["none", "client", "server", "bidi"].contains(&value),
            // TODO: Implement validation for other string types
            _ => true,
        }
    }

    pub fn insert_value_str(&mut self, value: &'a str) {
        match self {
            Self::Hash(val) => *val = value,
            Self::NestedFlatBuffer(val) => *val = value,
            Self::Streaming(val) => {
                *val = match value {
                    "none" => StreamingMode::None,
                    "client" => StreamingMode::Client,
                    "server" => StreamingMode::Server,
                    "bidi" => StreamingMode::Bidirectional,
                    _ => unreachable!(),
                }
            }
            _ => (),
        }
    }

    pub fn insert_value_u64(&mut self, value: u64) {
        match self {
            Self::ForceAlign(val) => *val = value,
            Self::Id(val) => *val = value,
            _ => (),
        }
    }

    pub fn insert_value_custom(&mut self, value: &'a str) {
        if let Self::Custom {
            name: _,
            value: val,
        } = self
        {
            *val = Some(value)
        }
    }
}

fn parse_attribute<'a, 's: 'a>(
    _state: &'s ParserState<'s>,
    target: AttributeTarget,
) -> impl Parser<&'s str, (Attribute<'s>, StrCheckpoint<'s>), ContextError> + 'a {
    move |input: &mut _| {
        trace("attribute", |input: &mut _| {
            whitespace_and_comments_opt(input)?;
            let attr_start = input.checkpoint();
            // Get the attribute ident
            let mut attr = cut_err(
                ident
                    .verify_map(|ident| {
                        let attr = Attribute::from_ident(ident);
                        if attr.is_valid(target) {
                            Some(attr)
                        } else {
                            None
                        }
                    })
                    .context(StrContext::Label(
                        "attribute; this attribute not allowed here",
                    )),
            )
            .parse_next(input)?;

            // Consume any whitespace and/or newlines
            whitespace_and_comments_opt(input)?;

            if attr.has_value() && !attr.is_custom() {
                cut_err(literal(':')).parse_next(input)?;

                whitespace_and_comments_opt(input)?;

                if attr.has_str_value() {
                    let value = string_literal
                        .verify(|value| attr.validate_str_value(value))
                        .context(StrContext::Label("value for this attribute"))
                        .parse_next(input)?;

                    attr.insert_value_str(value);
                } else if attr.has_u64_value() {
                    let value = digit1
                        .parse_to()
                        .context(StrContext::Expected(StrContextValue::Description(
                            "unsigned integer",
                        )))
                        .parse_next(input)?;

                    attr.insert_value_u64(value);
                }
            } else if attr.is_custom() {
                if let Some((_, _, value)) = opt(seq!(
                    literal(':'),
                    whitespace_and_comments_opt,
                    alt((string_literal, digit1,))
                ))
                .parse_next(input)?
                {
                    if !value.is_empty() {
                        attr.insert_value_custom(value);
                    }
                }
            } else {
                let checkpoint = input.checkpoint();
                cut_err(not(literal(':'))).parse_next(input)?;
                input.reset(&checkpoint);
            }

            Ok((attr, attr_start))
        })
        .parse_next(input)
    }
}

pub fn attribute_list<'a, 's: 'a>(
    state: &'s ParserState<'s>,
    target: AttributeTarget,
) -> impl Parser<&'s str, AttributesWrapper<'s>, ContextError> + 'a {
    move |input: &mut _| {
        trace("attribute_list", |input: &mut _| {
            whitespace_and_comments_opt(input)?;
            literal("(").parse_next(input)?;

            let attrs = cut_err(
                separated(1.., parse_attribute(state, target), ",")
                    .context(StrContext::Expected(StrContextValue::Description(
                        "at least one attribute",
                    )))
                    .map(|attrs: Vec<_>| {
                        let mut id = None;

                        for (attr, chk) in attrs.iter() {
                            if matches!(attr, Attribute::Id(_)) {
                                id = Some(*chk);
                                break;
                            }
                        }

                        let attr_chks = attrs.iter().map(|(_, chk)| *chk).collect();
                        let attrs = attrs.into_iter().map(|(attr, _)| attr).collect();

                        AttributesWrapper {
                            attrs,
                            attr_chks,
                            id,
                        }
                    })
                    .context(StrContext::Label(
                        "attribute; id must be on all fields or none",
                    )),
            )
            .parse_next(input)?;

            // Check for duplicate attributes
            let mut attr_idents = HashSet::new();
            for (attr, chk) in attrs.attrs.iter().zip(attrs.attr_chks.iter()) {
                let ident = attr.to_ident();

                if attr_idents.contains(&ident) {
                    input.reset(chk);
                    return Err(ErrMode::Cut(ContextError::new().add_context(
                        input,
                        chk,
                        StrContext::Label("attribute; only one of each attribute is allowed"),
                    )));
                } else {
                    attr_idents.insert(ident);
                }
            }

            whitespace_and_comments_opt(input)?;

            cut_err(literal(")")).parse_next(input)?;

            Ok(attrs)
        })
        .parse_next(input)
    }
}

#[cfg(feature = "builder")]
#[cfg(test)]
mod tests {
    use rstest::rstest;

    use super::*;

    #[rstest]
    #[case::id("id: 1", Attribute::Id(1))]
    #[case::id("id:\n2", Attribute::Id(2))]
    #[case::deprecated("deprecated", Attribute::Deprecated)]
    #[case::custom_str("custom: \"hello\"", Attribute::Custom {
        name: "custom",
        value: Some("hello"),
    })]
    #[case::custom_int("custom: 12", Attribute::Custom {
        name: "custom",
        value: Some("12"),
    })]
    #[case::custom("custom", Attribute::Custom {
        name: "custom",
        value: None,
    })]
    #[case::nested_flatbuffer(
        "nested_flatbuffer: \"other_table\"",
        Attribute::NestedFlatBuffer("other_table")
    )]
    fn single_attr_pass(#[case] item_str: &str, #[case] output: Attribute) {
        let state = ParserState::new();

        assert_eq!(
            parse_attribute(&state, AttributeTarget::TableField)
                .parse(item_str)
                .inspect_err(|e| println!("{e}"))
                .map(|(attr, _)| attr),
            Ok(output)
        );

        let invalid = [
            "id:",
            "nested_flatbuffer: \"not_closed",
            "custom:",
            "bit_flags",
        ];

        for item in invalid {
            let attr = parse_attribute(&state, AttributeTarget::TableField).parse(item);
            assert!(attr.is_err());
        }
    }

    #[rstest]
    #[case::colon_no_value("id:")]
    #[case::open_string_literal("nested_flatbuffer: \"not_closed")]
    #[case::wrong_type("id: \"1\"")]
    #[case::missing_value("bit_flags")]
    fn single_attr_fail(#[case] item_str: &str) {
        let state = ParserState::new();

        assert!(parse_attribute(&state, AttributeTarget::TableField)
            .parse(item_str)
            .inspect_err(|e| println!("{e}"))
            .is_err());
    }

    #[rstest]
    #[case::simple("(id: 1)", vec![Attribute::Id(1)])]
    #[case::whitespace("(id \n:\n2)", vec![Attribute::Id(2)])]
    #[case::comments("(deprecated, /// ignore me\n id: 1)", vec![Attribute::Deprecated, Attribute::Id(1)])]
    fn attribute_section_pass(#[case] item_str: &str, #[case] output: Vec<Attribute>) {
        let state = ParserState::new();

        assert_eq!(
            attribute_list(&state, AttributeTarget::TableField)
                .parse(item_str)
                .map(|attrs| attrs.attrs),
            Ok(output)
        );

        let invalid = ["id: 1", "(id: 1", "(id:1,)"];

        for item in invalid {
            let attr = attribute_list(&state, AttributeTarget::TableField).parse(item);
            assert!(attr.is_err());
        }
    }

    #[rstest]
    #[case::missing_parentheses("id: 1")]
    #[case::unclosed_parentheses("(id: 1")]
    #[case::trailing_comma("(id:1,)")]
    fn attribute_section_fail(#[case] item_str: &str) {
        let state = ParserState::new();

        assert!(attribute_list(&state, AttributeTarget::TableField)
            .parse(item_str)
            .is_err());
    }
}
