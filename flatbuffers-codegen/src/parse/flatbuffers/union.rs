use std::collections::HashSet;

use winnow::{
    combinator::{cut_err, opt, separated, trace},
    error::{AddContext, ContextError, ErrMode, StrContext, StrContextValue},
    stream::Stream,
    token::literal,
    Parser,
};

use crate::parse::{
    parser::{DeclType, ParserState},
    utils::{ident, resolved_ident, whitespace_and_comments_opt, Namespace},
};

use super::attributes::{attribute_list, Attribute, AttributeTarget};

#[cfg_attr(feature = "builder", derive(typed_builder::TypedBuilder))]
#[cfg_attr(feature = "builder", builder(field_defaults(default)))]
#[derive(Clone, Debug, PartialEq)]
pub struct UnionVariant<'a> {
    /// Name of the variant
    #[cfg_attr(feature = "builder", builder(!default))]
    pub name: &'a str,
    /// Will be the same as `name` when no alias is given
    #[cfg_attr(feature = "builder", builder(!default))]
    pub actual_type: &'a str,
    pub comments: Vec<&'a str>,
    pub attributes: Vec<Attribute<'a>>,
}

#[cfg_attr(feature = "builder", derive(typed_builder::TypedBuilder))]
#[cfg_attr(feature = "builder", builder(field_defaults(default)))]
#[derive(Clone, Debug, PartialEq)]
pub struct Union<'a> {
    #[cfg_attr(feature = "builder", builder(!default))]
    pub name: &'a str,
    pub namespace: Namespace<'a>,
    pub variants: Vec<UnionVariant<'a>>,
    pub comments: Vec<&'a str>,
    pub attributes: Vec<Attribute<'a>>,
}

fn union_variant<'a, 's: 'a>(
    state: &'s ParserState<'s>,
    field_idents: &'a mut HashSet<&'s str>,
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

            if field_idents.contains(&ident) {
                input.reset(&checkpoint);
                return Err(ErrMode::Cut(ContextError::new().add_context(
                    input,
                    &checkpoint,
                    StrContext::Label("; duplicate field name"),
                )));
            }

            whitespace_and_comments_opt(input)?;

            // If it's not aliased, reset back to the checkpoint and proceed
            // This works because, regardless, the next item we parsed must be a resolved type,
            // whether we roll back or not
            let is_aliased = if opt(literal::<_, _, ContextError>(":"))
                .parse_next(input)?
                .is_none()
            {
                // Not aliased, so we need to backtrack and parse the type as a resolved type instead
                input.reset(&checkpoint);
                false
            } else {
                true
            };

            // Parse the actual type, must be a table
            let actual_type = cut_err(resolved_ident(state, &[DeclType::Table]))
                .context(StrContext::Label("union variant type"))
                .map(|val| val.ident)
                .parse_next(input)?;

            whitespace_and_comments_opt(input)?;

            let attrs = opt(attribute_list(state, AttributeTarget::UnionVariant))
                .parse_next(input)?
                .map(|attrs| attrs.attrs);

            whitespace_and_comments_opt(input)?;

            // If aliased, the name is the ident
            // Otherwise, the name is the same as the actual type
            let name = if is_aliased { ident } else { actual_type };

            field_idents.insert(name);

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
    state: &'s ParserState<'s>,
) -> impl Parser<&'s str, Union<'s>, ContextError> + 'a {
    move |input: &mut _| {
        trace("union", |input: &mut _| {
            let comments = whitespace_and_comments_opt(input)?;

            literal("union").parse_next(input)?;
            whitespace_and_comments_opt(input)?;

            let ident = ident.parse_next(input)?;

            let attrs = opt(attribute_list(state, AttributeTarget::UnionItem))
                .parse_next(input)?
                .map(|attrs| attrs.attrs);
            whitespace_and_comments_opt(input)?;

            literal("{")
                .context(StrContext::Expected(StrContextValue::StringLiteral("{")))
                .parse_next(input)?;

            let mut field_idents = HashSet::new();

            let variants =
                separated(0.., union_variant(state, &mut field_idents), ",").parse_next(input)?;
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

#[cfg(feature = "builder")]
#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use rstest::rstest;

    use crate::parse::{flatbuffers::table::Table, parser::TypeDecls};

    use super::*;

    #[rstest]
    #[case::simple(
        r#"union Hello {
            Variant1,
            Variant2,
        }"#,
        Union::builder()
            .name("Hello")
            .variants(vec![
                UnionVariant::builder()
                    .name("Variant1")
                    .actual_type("Variant1")
                    .build(),
                UnionVariant::builder()
                    .name("Variant2")
                    .actual_type("Variant2")
                    .build(),
            ])
            .build()
    )]
    #[case::comments(
        r#"/// This is a comment!
        union Hello {
            Variant1,
            Variant2,
        }"#,
        Union::builder()
            .name("Hello")
            .variants(vec![
                UnionVariant::builder()
                    .name("Variant1")
                    .actual_type("Variant1")
                    .build(),
                UnionVariant::builder()
                    .name("Variant2")
                    .actual_type("Variant2")
                    .build(),
            ])
            .comments(vec!["This is a comment!"])
            .build()
    )]
    #[case::attributes(
        r#"union Hello (custom_attr) {
            Variant1,
            Variant2,
        }"#,
        Union::builder()
            .name("Hello")
            .variants(vec![
                UnionVariant::builder()
                    .name("Variant1")
                    .actual_type("Variant1")
                    .build(),
                UnionVariant::builder()
                    .name("Variant2")
                    .actual_type("Variant2")
                    .build(),
            ])
            .attributes(vec![Attribute::Custom { name: "custom_attr", value: None }])
            .build()
    )]
    #[case::variant_aliasing(
        r#"union Hello {
            Variant1,
            Variant2:Variant1,
        }"#,
        Union::builder()
            .name("Hello")
            .variants(vec![
                UnionVariant::builder()
                    .name("Variant1")
                    .actual_type("Variant1")
                    .build(),
                UnionVariant::builder()
                    .name("Variant2")
                    .actual_type("Variant1")
                    .build(),
            ])
            .build()
    )]
    fn union_pass(#[case] item_str: &str, #[case] output: Union) {
        let mut state = ParserState::new();
        let mut decl = TypeDecls::new();
        decl.add_tables([
            Table::builder().name("Variant1").build(),
            Table::builder().name("Variant2").build(),
            Table::builder().name("Variant3").build(),
        ]);

        state.extend_decls(HashMap::from([("".into(), decl)]));

        assert_eq!(union_item(&state).parse(item_str), Ok(output));
    }

    #[rstest]
    #[case::enum_style(
        r#"union Test : uint32 {
            Variant1,
        }"#
    )]
    #[case::extra_comma(
        r#"union Test {
            Variant1,,
        }"#
    )]
    #[case::unclosed_bracket(
        r#"union Test {
            Variant1,,"#
    )]
    #[case::duplicate_field(
        r#"union Test {
            Variant1,
            Variant1,
        }"#
    )]
    fn union_fail(#[case] item_str: &str) {
        let mut state = ParserState::new();
        let mut decl = TypeDecls::new();
        decl.add_tables([
            Table::builder().name("Variant1").build(),
            Table::builder().name("Variant2").build(),
            Table::builder().name("Variant3").build(),
        ]);

        state.extend_decls(HashMap::from([("".into(), decl)]));

        assert!(union_item(&state).parse(item_str).is_err());
    }
}
