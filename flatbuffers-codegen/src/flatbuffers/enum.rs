use std::str::FromStr;

use winnow::{
    ascii::digit1,
    combinator::{opt, separated, trace},
    error::{AddContext, ContextError, ErrMode, ParserError, StrContext, StrContextValue},
    stream::Stream,
    token::literal,
    PResult, Parser,
};

use crate::{
    flatbuffers::attributes::AttributeSectionParser,
    utils::{consume_whitespace_and_comments, parse_ident, IdentParser, TypeName},
};

use super::{
    attributes::Attribute,
    primitives::{ParseScalarType, ScalarType},
};

#[derive(Debug, PartialEq)]
pub struct EnumVariant<'a, T> {
    name: &'a str,
    idx: Option<T>,
    comments: Vec<&'a str>,
    attributes: Vec<Attribute<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum EnumData<'a> {
    /// Alias of `byte`
    Int8(Vec<EnumVariant<'a, i8>>),
    /// Alias of `ubyte`
    UInt8(Vec<EnumVariant<'a, u8>>),
    /// Alias of `short`
    Int16(Vec<EnumVariant<'a, i16>>),
    /// Alias of `ushort`
    UInt16(Vec<EnumVariant<'a, u16>>),
    /// Alias of `int`
    Int32(Vec<EnumVariant<'a, i32>>),
    /// Alias of `uint`
    UInt32(Vec<EnumVariant<'a, u32>>),
    /// Alias of `long`
    Int64(Vec<EnumVariant<'a, i64>>),
    /// Alias of `ulong`
    UInt64(Vec<EnumVariant<'a, u64>>),
}

#[derive(Debug, PartialEq)]
pub struct Enum<'a> {
    name: &'a str,
    variants: EnumData<'a>,
    comments: Vec<&'a str>,
    attributes: Vec<Attribute<'a>>,
}

struct EnumVariantParser;

impl<'s, E, T> Parser<&'s str, EnumVariant<'s, T>, E> for EnumVariantParser
where
    E: AddContext<&'s str, StrContext> + ParserError<&'s str>,
    ErrMode<E>: From<ErrMode<ContextError>>,
    T: FromStr + TypeName,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<EnumVariant<'s, T>, E> {
        let comments = consume_whitespace_and_comments(input)?;

        let ident = IdentParser
            .context(StrContext::Expected(StrContextValue::Description(
                "enum identifier",
            )))
            .parse_next(input)?;

        consume_whitespace_and_comments(input)?;

        // Parse the index value
        let idx = if input.starts_with('=') {
            literal("=").parse_next(input)?;

            consume_whitespace_and_comments(input)?;

            let idx = Parser::parse_to(digit1).parse_next(input)?;

            Some(idx)
        } else {
            None
        };

        let attrs = opt(AttributeSectionParser).parse_next(input)?;

        consume_whitespace_and_comments(input)?;

        Ok(EnumVariant {
            name: ident,
            idx,
            comments,
            attributes: attrs.unwrap_or_default(),
        })
    }
}

pub struct EnumParser;

impl<'s, E> Parser<&'s str, Enum<'s>, E> for EnumParser
where
    E: AddContext<&'s str, StrContext> + ParserError<&'s str>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<Enum<'s>, E> {
        trace("enum", |input: &mut _| {
            let comments = consume_whitespace_and_comments(input)?;

            literal("enum").parse_next(input)?;
            consume_whitespace_and_comments(input)?;

            let ident = parse_ident(input)?;
            consume_whitespace_and_comments(input)?;

            literal(":")
                .context(StrContext::Expected(StrContextValue::StringLiteral(":")))
                .parse_next(input)?;
            consume_whitespace_and_comments(input)?;

            let before_scalar = input.checkpoint();

            let scalar = ParseScalarType.parse_next(input)?;

            if !scalar.is_integer() {
                return Err(ErrMode::Backtrack(
                    ContextError::new()
                        .add_context(input, &before_scalar, StrContext::Label("enum type"))
                        .add_context(
                            input,
                            &before_scalar,
                            StrContext::Expected(StrContextValue::Description("integer type")),
                        ),
                )
                .into());
            }

            let attrs = opt(AttributeSectionParser).parse_next(input)?;
            consume_whitespace_and_comments(input)?;

            consume_whitespace_and_comments(input)?;

            literal("{")
                .context(StrContext::Expected(StrContextValue::StringLiteral("{")))
                .parse_next(input)?;

            fn parse_variants<'s, T: FromStr + TypeName>(
                input: &mut &'s str,
            ) -> PResult<Vec<EnumVariant<'s, T>>> {
                let variants = separated(0.., EnumVariantParser, ",").parse_next(input)?;
                // Consume a trailing comma, if present
                opt(literal(",")).parse_next(input)?;

                Ok(variants)
            }

            // Parse variants according to the data type
            let variants = match scalar {
                ScalarType::Int8 => EnumData::Int8(parse_variants(input)?),
                ScalarType::UInt8 => EnumData::UInt8(parse_variants(input)?),
                ScalarType::Int16 => EnumData::Int16(parse_variants(input)?),
                ScalarType::UInt16 => EnumData::UInt16(parse_variants(input)?),
                ScalarType::Int32 => EnumData::Int32(parse_variants(input)?),
                ScalarType::UInt32 => EnumData::UInt32(parse_variants(input)?),
                ScalarType::Int64 => EnumData::Int64(parse_variants(input)?),
                ScalarType::UInt64 => EnumData::UInt64(parse_variants(input)?),
                _ => unreachable!(),
            };

            consume_whitespace_and_comments(input)?;

            literal("}")
                .context(StrContext::Expected(StrContextValue::StringLiteral("}")))
                .parse_next(input)?;

            Ok(Enum {
                name: ident,
                variants,
                comments,
                attributes: attrs.unwrap_or_default(),
            })
        })
        .parse_next(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn r#enum() {
        let enum1_str = r#"
            enum Hello : uint32 {
                Variant1 = 0,
                Variant2,
                Variant3
            }
        "#;

        let enum1 = Enum {
            name: "Hello",
            variants: EnumData::UInt32(vec![
                EnumVariant {
                    name: "Variant1",
                    idx: Some(0),
                    comments: Vec::new(),
                    attributes: Vec::new(),
                },
                EnumVariant {
                    name: "Variant2",
                    idx: None,
                    comments: Vec::new(),
                    attributes: Vec::new(),
                },
                EnumVariant {
                    name: "Variant3",
                    idx: None,
                    comments: Vec::new(),
                    attributes: Vec::new(),
                },
            ]),
            comments: Vec::new(),
            attributes: Vec::new(),
        };

        let enum2_str = r#"
            // This is NOT documentation
            /// This is a comment!
            enum Hello_There (bitflags) : int {
                Variant1,
                /// Comment
                Variant2 = 1 (custom_attr),
            }
        "#;

        let enum2 = Enum {
            name: "Hello_There",
            variants: EnumData::Int32(vec![
                EnumVariant {
                    name: "Variant1",
                    idx: None,
                    comments: Vec::new(),
                    attributes: Vec::new(),
                },
                EnumVariant {
                    name: "Variant2",
                    idx: Some(1),
                    comments: vec!["Comment"],
                    attributes: vec![Attribute::Custom {
                        name: "custom_attr",
                        value: None,
                    }],
                },
            ]),
            comments: vec!["This is a comment!"],
            attributes: vec![Attribute::BitFlags],
        };

        let valid = [(enum1_str, enum1), (enum2_str, enum2)];

        for (item_str, item) in valid {
            let mut value = item_str;
            assert_eq!(EnumParser.parse_next(&mut value), Ok(item));
        }

        let enum_invalid1 = r#"
            enum Hello_There : int32 {
                Variant bla,
            }
        "#;

        let enum_invalid2 = r#"
            enum Hello_There {
                Variant = 1,
            }
        "#;

        let enum_invalid3 = r#"
            enum Hello_There : int8 {
                1var,
            }
        "#;

        let enum_invalid4 = r#"
            enum Hello_There : int8 {
                Variant (incomplete,
            }
        "#;

        let invalid = [enum_invalid1, enum_invalid2, enum_invalid3, enum_invalid4];

        for item in invalid {
            let mut value = item;
            assert!(EnumParser.parse_next(&mut value).is_err());
        }
    }
}
