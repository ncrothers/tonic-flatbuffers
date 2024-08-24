use winnow::{
    combinator::{opt, separated, trace},
    error::{ContextError, StrContext, StrContextValue},
    stream::Stream,
    token::literal,
    Parser,
};

use crate::{
    parser::{DeclType, ParserState},
    utils::{ident, resolved_ident, whitespace_and_comments_opt},
};

use super::attributes::{attribute_list, Attribute, AttributeTarget};

#[derive(Debug, PartialEq)]
pub struct UnionVariant<'a> {
    /// Name of the variant
    name: &'a str,
    /// Will be the same as `name` when no alias is given
    actual_type: &'a str,
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

            let checkpoint = input.checkpoint();

            // Parse the type as a namespace, then check if there's an alias
            let ident = separated(1.., ident, ".")
                .map(|()| ())
                .take()
                .parse_next(input)?;

            whitespace_and_comments_opt(input)?;

            // If it's not aliased, reset back to the checkpoint and proceed
            // This works because, regardless, the next item we parsed must be a resolved type,
            // whether we roll back or not
            let is_aliased = if literal::<_, _, ContextError>(":")
                .parse_next(input)
                .is_err()
            {
                // Not aliased, so we need to backtrack and parse the type as a resolved type instead
                input.reset(&checkpoint);
                false
            } else {
                true
            };

            // Parse the actual type, must be a table
            let actual_type = resolved_ident(state, &[DeclType::Table])
                .context(StrContext::Label("union variant type"))
                .parse_next(input)?;

            whitespace_and_comments_opt(input)?;

            let attrs = opt(attribute_list(
                state,
                AttributeTarget::UnionVariant,
                &mut None,
            ))
            .parse_next(input)?;

            whitespace_and_comments_opt(input)?;

            // If aliased, the name is the ident
            // Otherwise, the name is the same as the actual type
            let name = if is_aliased { ident } else { actual_type };

            Ok(UnionVariant {
                name,
                actual_type,
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

            let attrs = opt(attribute_list(state, AttributeTarget::UnionItem, &mut None))
                .parse_next(input)?;
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
    use std::collections::HashMap;

    use crate::parser::TypeDecls;

    use super::*;

    #[test]
    fn r#union() {
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
                    actual_type: "Variant1",
                    comments: Vec::new(),
                    attributes: Vec::new(),
                },
                UnionVariant {
                    name: "Variant2",
                    actual_type: "Variant1",
                    comments: Vec::new(),
                    attributes: Vec::new(),
                },
                UnionVariant {
                    name: "Variant3",
                    actual_type: "Variant3",
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
                Variant1:Variant3,
                /// Comment 2
                Variant2 (custom_attr),
            }"#;

        let enum2 = Union {
            name: "Hello_There",
            namespace: "",
            variants: vec![
                UnionVariant {
                    name: "Variant1",
                    actual_type: "Variant3",
                    comments: vec!["Comment 1"],
                    attributes: Vec::new(),
                },
                UnionVariant {
                    name: "Variant2",
                    actual_type: "Variant2",
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

        let mut state = ParserState::new();
        let mut decl = TypeDecls::new();
        decl.add_tables(["Variant1", "Variant2", "Variant3"]);

        state.extend_decls(HashMap::from([("", decl)]));

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
