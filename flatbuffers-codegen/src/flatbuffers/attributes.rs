use winnow::{ascii::digit1, error::{AddContext, ContextError, ErrMode, InputError, StrContext, StrContextValue}, stream::{AsChar, Stream}, token::{literal, one_of, take_till, take_while}, PResult, Parser};

use crate::utils::{consume_whitespace, parse_ident, StringLiteral};

const ATTRIBUTE_NO_VALUE: &[&str] = &[
    "bitflags",
    "deprecated",
    "flexbuffer",
    "key",
    "original_order",
    "required",
];

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
    }
}

impl<'a> Attribute<'a> {
    pub fn from_ident(ident: &'a str) -> Self {
        match ident {
            "bitflags" => Self::BitFlags,
            "deprecated" => Self::Deprecated,
            "flexbuffer" => Self::FlexBuffer,
            "force_align" => Self::ForceAlign(0),
            "hash" => Self::Hash(""),
            "id" => Self::Id(0),
            "key" => Self::Key,
            "nested_flatbuffer" => Self::NestedFlatBuffer(""),
            "original_order" => Self::OriginalOrder,
            "required" => Self::Required,
            custom => Self::Custom { name: custom, value: None }
        }
    }

    pub fn has_value(&self) -> bool {
        matches!(self, Self::ForceAlign(_) | Self::Hash(_) | Self::Id(_) | Self::NestedFlatBuffer(_) | Self::Custom { .. })
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
            _ => ()
        }
    }

    pub fn insert_value_u64(&mut self, value: u64) {
        match self {
            Self::ForceAlign(val) => *val = value,
            Self::Id(val) => *val = value,
            _ => ()
        }
    }

    pub fn insert_value_custom(&mut self, value: &'a str) {
        match self {
            Self::Custom { name: _, value: val } => *val = Some(value),
            _ => ()
        }
    }
}

pub fn parse_attribute<'s>(input: &mut &'s str) -> PResult<Attribute<'s>> {
    // Get the attribute ident
    let ident = parse_ident(input)?;

    // Consume any whitespace and/or newlines
    consume_whitespace(input)?;

    let mut attr = Attribute::from_ident(ident);

    let without_value = input.checkpoint();

    // If there's a value for the attribute, parse that as well
    if literal::<_, _, InputError<_>>(":").parse_next(input).is_ok() {
        if attr.has_value() {
            consume_whitespace(input)?;

            if attr.has_str_value() {
                let value = StringLiteral.parse_next(input)?;

                attr.insert_value_str(value);
            } else if attr.has_u64_value() {
                let value = digit1(input)?
                    .parse()
                    .map_err(|_| {
                        ErrMode::Cut(ContextError::new()
                            .add_context(input, &without_value, StrContext::Label("attribute value"))
                            .add_context(input, &without_value, StrContext::Expected(StrContextValue::Description("unsigned integer"))))
                    })?;

                attr.insert_value_u64(value);
            } else if attr.is_custom() {
                let value = take_till(1.., |c: char| { c.is_whitespace() || c.is_newline() || c == ',' }).parse_next(input)?;

                if !value.is_empty() {
                    attr.insert_value_custom(value);
                }
            }
        } else {
            let err = ContextError::new()
                .add_context(input, &without_value, StrContext::Label("attribute"))
                .add_context(input, &without_value, StrContext::Expected(StrContextValue::Description("no value for this attribute type")));
            return Err(ErrMode::Cut(err));
        }
    }

    Ok(attr)
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
            ("custom: hello", Attribute::Custom { name: "custom", value: Some("hello") }),
            ("custom", Attribute::Custom { name: "custom", value: None }),
        ];

        for (item_str, item) in valid {
            let mut value = item_str;
            assert_eq!(parse_attribute(&mut value), Ok(item));
        }
    
        let invalid = [
            "id:",
            "nested_flatbuffer: \"not_closed",
            "custom:",
        ];

        for item in invalid {
            let mut value = item;
            let attr = parse_attribute(&mut value);
            assert!(attr.is_err());
        }
    }
}