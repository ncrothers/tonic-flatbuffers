use winnow::{
    combinator::{opt, separated},
    error::{AddContext, ContextError, ErrMode, ParserError, StrContext, StrContextValue},
    token::literal,
    PResult, Parser,
};

use crate::{
    flatbuffers::attributes::AttributeSectionParser,
    utils::{consume_whitespace_and_comments, parse_ident, IdentParser},
};

use super::attributes::Attribute;

#[derive(Debug, PartialEq)]
pub struct UnionVariant<'a> {
    /// Name of the variant
    name: &'a str,
    /// Will be the same as `name` when no alias is given
    aliased_type: &'a str,
    comments: Vec<&'a str>,
    attributes: Vec<Attribute<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Union<'a> {
    name: &'a str,
    variants: Vec<UnionVariant<'a>>,
    comments: Vec<&'a str>,
    attributes: Vec<Attribute<'a>>,
}

struct UnionVariantParser;

impl<'s, E> Parser<&'s str, UnionVariant<'s>, E> for UnionVariantParser
where
    E: AddContext<&'s str, StrContext> + ParserError<&'s str>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<UnionVariant<'s>, E> {
        let comments = consume_whitespace_and_comments(input)?;

        let ident = IdentParser
            .context(StrContext::Expected(StrContextValue::Description(
                "enum identifier",
            )))
            .parse_next(input)?;

        consume_whitespace_and_comments(input)?;

        // Parse the index value
        let aliased_type = if input.starts_with(':') {
            literal(":").parse_next(input)?;

            consume_whitespace_and_comments(input)?;

            let actual_type = IdentParser.parse_next(input)?;

            Some(actual_type)
        } else {
            None
        };

        let attrs = opt(AttributeSectionParser).parse_next(input)?;

        consume_whitespace_and_comments(input)?;

        Ok(UnionVariant {
            name: ident,
            aliased_type: aliased_type.unwrap_or(ident),
            comments,
            attributes: attrs.unwrap_or_default(),
        })
    }
}

pub struct UnionParser;

impl<'s, E> Parser<&'s str, Union<'s>, E> for UnionParser
where
    E: AddContext<&'s str, StrContext> + ParserError<&'s str>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<Union<'s>, E> {
        let comments = consume_whitespace_and_comments(input)?;

        literal("union").parse_next(input)?;
        consume_whitespace_and_comments(input)?;

        let ident = parse_ident(input)?;

        let attrs = opt(AttributeSectionParser).parse_next(input)?;
        consume_whitespace_and_comments(input)?;

        literal("{")
            .context(StrContext::Expected(StrContextValue::StringLiteral("{")))
            .parse_next(input)?;

        let variants = separated(0.., UnionVariantParser, ",").parse_next(input)?;
        consume_whitespace_and_comments(input)?;
        // Consume trailing comma if present
        opt(literal(",")).parse_next(input)?;

        consume_whitespace_and_comments(input)?;

        literal("}")
            .context(StrContext::Expected(StrContextValue::StringLiteral("}")))
            .parse_next(input)?;

        Ok(Union {
            name: ident,
            variants,
            comments,
            attributes: attrs.unwrap_or_default(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn r#enum() {
        let enum1_str = r#"
            union Hello {
                Variant1,
                Variant2:Variant1,
                Variant3,
            }"#;

        let enum1 = Union {
            name: "Hello",
            variants: vec![
                UnionVariant {
                    name: "Variant1",
                    aliased_type: "Variant1",
                    comments: Vec::new(),
                    attributes: Vec::new(),
                },
                UnionVariant {
                    name: "Variant2",
                    aliased_type: "Variant1",
                    comments: Vec::new(),
                    attributes: Vec::new(),
                },
                UnionVariant {
                    name: "Variant3",
                    aliased_type: "Variant3",
                    comments: Vec::new(),
                    attributes: Vec::new(),
                },
            ],
            comments: Vec::new(),
            attributes: Vec::new(),
        };

        let enum2_str = r#"
            // This is NOT documentation
            /// This is a comment!
            union Hello_There (custom) {
                /// Comment 1
                Variant1:AnotherType,
                /// Comment 2
                Variant2 (custom_attr),
            }"#;

        let enum2 = Union {
            name: "Hello_There",
            variants: vec![
                UnionVariant {
                    name: "Variant1",
                    aliased_type: "AnotherType",
                    comments: vec!["Comment 1"],
                    attributes: Vec::new(),
                },
                UnionVariant {
                    name: "Variant2",
                    aliased_type: "Variant2",
                    comments: vec!["Comment 2"],
                    attributes: vec![Attribute::Custom {
                        name: "custom_attr",
                        value: None,
                    }],
                },
            ],
            comments: vec!["This is a comment!"],
            attributes: vec![Attribute::Custom {
                name: "custom",
                value: None,
            }],
        };

        let valid = [(enum1_str, enum1), (enum2_str, enum2)];

        for (item_str, item) in valid {
            let res = UnionParser.parse(item_str).inspect_err(|e| println!("{e}"));
            assert_eq!(res, Ok(item));
        }

        let enum_invalid1 = r#"
            union Hello_There : int32 {}
        "#;

        let enum_invalid2 = r#"
            union Hello_There {
                Variant = 1,
            }
        "#;

        let enum_invalid3 = r#"
            union Hello_There {
                Variant1:,
            }
        "#;

        let enum_invalid4 = r#"
            union Hello_There {
                Variant1,
                :Variant2
            }
        "#;

        let invalid = [enum_invalid1, enum_invalid2, enum_invalid3, enum_invalid4];

        for item in invalid {
            let mut value = item;
            assert!(UnionParser.parse_next(&mut value).is_err());
        }
    }
}
