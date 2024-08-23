use winnow::{
    combinator::{opt, separated, trace},
    error::{ContextError, StrContext, StrContextValue},
    token::literal,
    Parser,
};

use crate::{
    parser::ParserState,
    utils::{ident, namespaced_ident, whitespace_and_comments_opt},
};

use super::attributes::{attribute_list, Attribute};

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
    namespace: &'a str,
    variants: Vec<UnionVariant<'a>>,
    comments: Vec<&'a str>,
    attributes: Vec<Attribute<'a>>,
}

fn union_variant<'a, 's: 'a>(
    state: &'a ParserState<'s>,
) -> impl Parser<&'s str, UnionVariant<'s>, ContextError> + 'a {
    move |input: &mut _| {
        trace("union_variant", |input: &mut _| {
            let comments = whitespace_and_comments_opt(input)?;

            let ident = namespaced_ident
                .context(StrContext::Expected(StrContextValue::Description(
                    "enum identifier",
                )))
                .parse_next(input)?;

            whitespace_and_comments_opt(input)?;

            // Parse the index value
            let aliased_type = if input.starts_with(':') {
                literal(":").parse_next(input)?;

                whitespace_and_comments_opt(input)?;

                let actual_type = namespaced_ident.parse_next(input)?;

                Some(actual_type)
            } else {
                None
            };

            let attrs = opt(attribute_list(state)).parse_next(input)?;

            whitespace_and_comments_opt(input)?;

            Ok(UnionVariant {
                name: ident,
                aliased_type: aliased_type.unwrap_or(ident),
                comments,
                attributes: attrs.unwrap_or_default(),
            })
        })
        .parse_next(input)
    }
}

pub fn union_item<'a, 's: 'a>(
    state: &'a ParserState<'s>,
) -> impl Parser<&'s str, Union<'s>, ContextError> + 'a {
    move |input: &mut _| {
        trace("union", |input: &mut _| {
            let comments = whitespace_and_comments_opt(input)?;

            literal("union").parse_next(input)?;
            whitespace_and_comments_opt(input)?;

            let ident = ident.parse_next(input)?;

            let attrs = opt(attribute_list(state)).parse_next(input)?;
            whitespace_and_comments_opt(input)?;

            literal("{")
                .context(StrContext::Expected(StrContextValue::StringLiteral("{")))
                .parse_next(input)?;

            let variants = separated(0.., union_variant(state), ",").parse_next(input)?;
            whitespace_and_comments_opt(input)?;
            // Consume trailing comma if present
            opt(literal(",")).parse_next(input)?;

            whitespace_and_comments_opt(input)?;

            literal("}")
                .context(StrContext::Expected(StrContextValue::StringLiteral("}")))
                .parse_next(input)?;

            Ok(Union {
                name: ident,
                namespace: state.namespace(),
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
            union Hello {
                Variant1,
                Variant2:Variant1,
                Variant3,
            }"#;

        let enum1 = Union {
            name: "Hello",
            namespace: "",
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
            namespace: "",
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

        let state = ParserState::new();

        let valid = [(enum1_str, enum1), (enum2_str, enum2)];

        for (item_str, item) in valid {
            let res = union_item(&state)
                .parse(item_str)
                .inspect_err(|e| println!("{e}"));
            assert_eq!(res, Ok(item));
        }

        let enum_invalid1 = r#"
            union Hello_There : int32 {}"#;

        let enum_invalid2 = r#"
            union Hello_There {
                Variant = 1,
            }"#;

        let enum_invalid3 = r#"
            union Hello_There {
                Variant1:,
            }"#;

        let enum_invalid4 = r#"
            union Hello_There {
                Variant1,
                :Variant2
            }"#;

        let invalid = [enum_invalid1, enum_invalid2, enum_invalid3, enum_invalid4];

        for item in invalid {
            assert!(union_item(&state).parse(item).is_err());
        }
    }
}
