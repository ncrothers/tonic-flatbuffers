use winnow::{
    ascii::digit1,
    combinator::{separated, trace},
    error::{AddContext, ContextError, ErrMode, InputError, StrContext, StrContextValue},
    stream::{AsChar, Stream},
    token::{literal, take_till},
    Parser,
};

use crate::{
    parser::ParserState,
    utils::{ident, string_literal, whitespace_and_comments_opt},
};

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
) -> impl Parser<&'s str, Attribute<'s>, ContextError> + 'a {
    move |input: &mut _| {
        trace("attribute", |input: &mut _| {
            whitespace_and_comments_opt(input)?;
            // Get the attribute ident
            let ident = ident.parse_next(input)?;

            // Consume any whitespace and/or newlines
            whitespace_and_comments_opt(input)?;

            let mut attr = Attribute::from_ident(ident);

            let without_value = input.checkpoint();

            // If there's a value for the attribute, parse that as well
            if literal::<_, _, InputError<_>>(":")
                .parse_next(input)
                .is_ok()
            {
                if attr.has_value() {
                    whitespace_and_comments_opt(input)?;

                    if attr.has_str_value() {
                        let value = string_literal.parse_next(input)?;

                        attr.insert_value_str(value);
                    } else if attr.has_u64_value() {
                        let value = digit1(input)?.parse().map_err(|_| {
                            ErrMode::Backtrack(
                                ContextError::new()
                                    .add_context(
                                        input,
                                        &without_value,
                                        StrContext::Label("attribute value"),
                                    )
                                    .add_context(
                                        input,
                                        &without_value,
                                        StrContext::Expected(StrContextValue::Description(
                                            "unsigned integer",
                                        )),
                                    ),
                            )
                        })?;

                        attr.insert_value_u64(value);
                    } else if attr.is_custom() {
                        let value = take_till(1.., |c: char| {
                            c.is_whitespace()
                                || c.is_newline()
                                || (c.is_ascii_punctuation() && c != '_')
                        })
                        .parse_next(input)?;

                        if !value.is_empty() {
                            attr.insert_value_custom(value);
                        }
                    }
                } else {
                    let err = ContextError::new()
                        .add_context(input, &without_value, StrContext::Label("attribute"))
                        .add_context(
                            input,
                            &without_value,
                            StrContext::Expected(StrContextValue::Description(
                                "no value for this attribute type",
                            )),
                        );
                    return Err(ErrMode::Backtrack(err));
                }
            }

            Ok(attr)
        })
        .parse_next(input)
    }
}

pub fn attribute_list<'a, 's: 'a>(
    state: &'a ParserState<'s>,
) -> impl Parser<&'s str, Vec<Attribute<'s>>, ContextError> + 'a {
    move |input: &mut _| {
        trace("attribute_list", |input: &mut _| {
            whitespace_and_comments_opt(input)?;
            literal("(").parse_next(input)?;

            let attrs = separated(1.., parse_attribute(state), ",").parse_next(input)?;

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
            assert_eq!(parse_attribute(&state).parse(item_str), Ok(item));
        }

        let invalid = ["id:", "nested_flatbuffer: \"not_closed", "custom:"];

        for item in invalid {
            let attr = parse_attribute(&state).parse(item);
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
            assert_eq!(attribute_list(&state).parse(item_str), Ok(item));
        }

        let invalid = ["id: 1", "(id: 1", "(id:1,)"];

        for item in invalid {
            let attr = attribute_list(&state).parse(item);
            assert!(attr.is_err());
        }
    }
}
