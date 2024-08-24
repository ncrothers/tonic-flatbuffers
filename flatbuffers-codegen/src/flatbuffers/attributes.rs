use std::collections::HashSet;

use winnow::{
    ascii::digit1,
    combinator::{cut_err, not, opt, separated, trace},
    error::{AddContext, ContextError, ErrMode, StrContext, StrContextValue},
    seq,
    stream::{AsChar, Checkpoint, Stream},
    token::{literal, take_till},
    Parser,
};

use crate::{
    parser::ParserState,
    utils::{ident, string_literal, whitespace_and_comments_opt},
};

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
}

pub type StrCheckpoint<'a> = Checkpoint<&'a str, &'a str>;

#[derive(Debug, Default)]
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

#[derive(Debug, PartialEq)]
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
                | Self::Custom { .. }
        )
    }

    pub fn has_str_value(&self) -> bool {
        matches!(self, Self::Hash(_) | Self::NestedFlatBuffer(_))
    }

    pub fn has_u64_value(&self) -> bool {
        matches!(self, Self::ForceAlign(_) | Self::Id(_))
    }

    pub fn is_custom(&self) -> bool {
        matches!(self, Self::Custom { .. })
    }

    pub fn insert_value_str(&mut self, value: &'a str) {
        match self {
            Self::Hash(val) => *val = value,
            Self::NestedFlatBuffer(val) => *val = value,
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
    _state: &'a ParserState<'s>,
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
                    let value = string_literal.parse_next(input)?;

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
                    take_till(1.., |c: char| {
                        c.is_whitespace()
                            || c.is_newline()
                            || (c.is_ascii_punctuation() && c != '_')
                    })
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
    state: &'a ParserState<'s>,
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

            literal(")").parse_next(input)?;

            Ok(attrs)
        })
        .parse_next(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn attribute() {
        let valid = [
            ("id: 1", Attribute::Id(1)),
            ("id:\n2", Attribute::Id(2)),
            ("deprecated", Attribute::Deprecated),
            (
                "custom: hello",
                Attribute::Custom {
                    name: "custom",
                    value: Some("hello"),
                },
            ),
            (
                "nested_flatbuffer: \"other_table\"",
                Attribute::NestedFlatBuffer("other_table"),
            ),
            (
                "custom",
                Attribute::Custom {
                    name: "custom",
                    value: None,
                },
            ),
        ];

        let state = ParserState::new();

        for (item_str, item) in valid {
            assert_eq!(
                parse_attribute(&state, AttributeTarget::TableField)
                    .parse(item_str)
                    .inspect_err(|e| println!("{e}"))
                    .map(|(attr, _)| attr),
                Ok(item)
            );
        }

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

    #[test]
    fn attribute_section() {
        let valid = [
            ("(id: 1)", vec![Attribute::Id(1)]),
            ("(id:\n2)", vec![Attribute::Id(2)]),
            (
                "(deprecated, /// ignore me\n id: 1)",
                vec![Attribute::Deprecated, Attribute::Id(1)],
            ),
        ];

        let state = ParserState::new();

        for (item_str, item) in valid {
            assert_eq!(
                attribute_list(&state, AttributeTarget::TableField)
                    .parse(item_str)
                    .map(|attrs| attrs.attrs),
                Ok(item)
            );
        }

        let invalid = ["id: 1", "(id: 1", "(id:1,)"];

        for item in invalid {
            let attr = attribute_list(&state, AttributeTarget::TableField).parse(item);
            assert!(attr.is_err());
        }
    }
}
