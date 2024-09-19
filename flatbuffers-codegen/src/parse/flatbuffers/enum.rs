use std::{collections::HashSet, str::FromStr};

use winnow::{
    combinator::{cut_err, opt, separated, trace},
    error::{AddContext, ContextError, ErrMode, StrContext, StrContextValue},
    stream::Stream,
    token::{literal, take_while},
    Parser,
};

use crate::parse::{
    flatbuffers::primitives::scalar_type,
    parser::ParserState,
    utils::{ident, item_ident, whitespace_and_comments_opt, Namespace, TypeName},
};

use super::{
    attributes::{attribute_list, Attribute, AttributeTarget},
    primitives::ScalarType,
};

#[derive(Debug, PartialEq)]
pub struct EnumVariant<'a, T> {
    pub name: &'a str,
    pub idx: Option<T>,
    pub comments: Vec<&'a str>,
    pub attributes: Vec<Attribute<'a>>,
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
    pub name: &'a str,
    pub namespace: Namespace<'a>,
    pub variants: EnumData<'a>,
    pub comments: Vec<&'a str>,
    pub attributes: Vec<Attribute<'a>>,
}

fn enum_variant<'a, 's: 'a, T>(
    state: &'a ParserState<'s>,
    field_idents: &'a mut HashSet<&'s str>,
) -> impl Parser<&'s str, EnumVariant<'s, T>, ContextError> + 'a
where
    T: FromStr + TypeName,
{
    move |input: &mut _| {
        trace("enum_variant", |input: &mut _| {
            let comments = whitespace_and_comments_opt(input)?;

            let ident_chk = input.checkpoint();
            let ident = ident
                .context(StrContext::Expected(StrContextValue::Description(
                    "enum identifier",
                )))
                .parse_next(input)?;

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

            // Parse the index value
            let idx = if input.starts_with('=') {
                literal("=").parse_next(input)?;

                whitespace_and_comments_opt(input)?;

                let idx =
                    cut_err(take_while(1.., |c: char| c.is_ascii_digit() || c == '.').parse_to())
                        .parse_next(input)?;

                Some(idx)
            } else {
                None
            };

            let attrs = opt(attribute_list(state, AttributeTarget::EnumVariant))
                .parse_next(input)?
                .map(|attrs| attrs.attrs);

            whitespace_and_comments_opt(input)?;

            Ok(EnumVariant {
                name: ident,
                idx,
                comments,
                attributes: attrs.unwrap_or_default(),
            })
        })
        .parse_next(input)
    }
}

pub fn enum_item<'a, 's: 'a>(
    state: &'a ParserState<'s>,
) -> impl Parser<&'s str, Enum<'s>, ContextError> + 'a {
    move |input: &mut _| {
        trace("enum", |input: &mut _| {
            let comments = whitespace_and_comments_opt(input)?;

            literal("enum").parse_next(input)?;
            whitespace_and_comments_opt(input)?;

            let ident = item_ident(state).parse_next(input)?;
            whitespace_and_comments_opt(input)?;

            cut_err(literal(":"))
                .context(StrContext::Expected(StrContextValue::StringLiteral(":")))
                .parse_next(input)?;
            whitespace_and_comments_opt(input)?;

            let before_scalar = input.checkpoint();

            let scalar = cut_err(scalar_type).parse_next(input)?;

            if !scalar.is_integer() {
                input.reset(&before_scalar);
                return Err(ErrMode::Cut(
                    ContextError::new()
                        .add_context(input, &before_scalar, StrContext::Label("enum type"))
                        .add_context(
                            input,
                            &before_scalar,
                            StrContext::Expected(StrContextValue::Description("integer type")),
                        ),
                ));
            }

            let attrs = opt(attribute_list(state, AttributeTarget::EnumItem))
                .parse_next(input)?
                .map(|attrs| attrs.attrs);
            whitespace_and_comments_opt(input)?;

            cut_err(literal("{"))
                .context(StrContext::Expected(StrContextValue::StringLiteral("{")))
                .parse_next(input)?;

            fn parse_variants<'a, 's: 'a, T: FromStr + TypeName>(
                state: &'a ParserState<'s>,
            ) -> impl Parser<&'s str, Vec<EnumVariant<'s, T>>, ContextError> + 'a {
                move |input: &mut _| {
                    let mut field_idents = HashSet::new();
                    let variants = separated(0.., enum_variant(state, &mut field_idents), ",")
                        .parse_next(input)?;
                    // Consume a trailing comma, if present
                    opt(literal(",")).parse_next(input)?;

                    Ok(variants)
                }
            }

            // Parse variants according to the data type
            let variants = match scalar {
                ScalarType::Int8 => EnumData::Int8(
                    cut_err(
                        parse_variants(state)
                            .context(StrContext::Expected(StrContextValue::Description("int8"))),
                    )
                    .parse_next(input)?,
                ),
                ScalarType::UInt8 => EnumData::UInt8(
                    cut_err(
                        parse_variants(state)
                            .context(StrContext::Expected(StrContextValue::Description("uint8"))),
                    )
                    .parse_next(input)?,
                ),
                ScalarType::Int16 => EnumData::Int16(
                    cut_err(
                        parse_variants(state)
                            .context(StrContext::Expected(StrContextValue::Description("int16"))),
                    )
                    .parse_next(input)?,
                ),
                ScalarType::UInt16 => EnumData::UInt16(
                    cut_err(
                        parse_variants(state)
                            .context(StrContext::Expected(StrContextValue::Description("uint16"))),
                    )
                    .parse_next(input)?,
                ),
                ScalarType::Int32 => EnumData::Int32(
                    cut_err(
                        parse_variants(state)
                            .context(StrContext::Expected(StrContextValue::Description("int32"))),
                    )
                    .parse_next(input)?,
                ),
                ScalarType::UInt32 => EnumData::UInt32(
                    cut_err(
                        parse_variants(state)
                            .context(StrContext::Expected(StrContextValue::Description("uint32"))),
                    )
                    .parse_next(input)?,
                ),
                ScalarType::Int64 => EnumData::Int64(
                    cut_err(
                        parse_variants(state)
                            .context(StrContext::Expected(StrContextValue::Description("int64"))),
                    )
                    .parse_next(input)?,
                ),
                ScalarType::UInt64 => EnumData::UInt64(
                    cut_err(
                        parse_variants(state)
                            .context(StrContext::Expected(StrContextValue::Description("uint64"))),
                    )
                    .parse_next(input)?,
                ),
                _ => unreachable!(),
            };

            whitespace_and_comments_opt(input)?;

            cut_err(literal("}"))
                .context(StrContext::Expected(StrContextValue::StringLiteral("}")))
                .parse_next(input)?;

            // Once parsing is successful, add this name to the state
            state.add_parsed(state.namespace(), ident);

            Ok(Enum {
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
    use rstest::rstest;

    use super::*;

    #[rstest]
    #[case::simple(
        r#"enum Hello : uint32 {
            Variant1,
            Variant2,
            Variant3
        }"#,
        Enum {
            name: "Hello",
            namespace: "".into(),
            variants: EnumData::UInt32(vec![
                EnumVariant {
                    name: "Variant1",
                    idx: None,
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
        }
    )]
    #[case::comments(
        r#"// This is NOT documentation
        /// This is a comment!
        enum Hello_There : int {
            Variant1,
            /// Comment
            Variant2,
        }"#,
        Enum {
            name: "Hello_There",
            namespace: "".into(),
            variants: EnumData::Int32(vec![
                EnumVariant {
                    name: "Variant1",
                    idx: None,
                    comments: Vec::new(),
                    attributes: Vec::new(),
                },
                EnumVariant {
                    name: "Variant2",
                    idx: None,
                    comments: vec!["Comment"],
                    attributes: Vec::new(),
                },
            ]),
            comments: vec!["This is a comment!"],
            attributes: Vec::new(),
        }
    )]
    #[case::indices(
        r#"enum Hello_There : int {
            Variant1 = 0,
            Variant2 = 1,
        }"#,
        Enum {
            name: "Hello_There",
            namespace: "".into(),
            variants: EnumData::Int32(vec![
                EnumVariant {
                    name: "Variant1",
                    idx: Some(0),
                    comments: Vec::new(),
                    attributes: Vec::new(),
                },
                EnumVariant {
                    name: "Variant2",
                    idx: Some(1),
                    comments: Vec::new(),
                    attributes: Vec::new(),
                },
            ]),
            comments: Vec::new(),
            attributes: Vec::new(),
        }
    )]
    #[case::attributes(
        r#"enum Hello_There : int (bit_flags) {
            Variant1,
            Variant2 (custom_attr: "foo"),
        }"#,
        Enum {
            name: "Hello_There",
            namespace: "".into(),
            variants: EnumData::Int32(vec![
                EnumVariant {
                    name: "Variant1",
                    idx: None,
                    comments: Vec::new(),
                    attributes: Vec::new(),
                },
                EnumVariant {
                    name: "Variant2",
                    idx: None,
                    comments: Vec::new(),
                    attributes: vec![Attribute::Custom { name: "custom_attr", value: Some("foo") }],
                },
            ]),
            comments: Vec::new(),
            attributes: vec![Attribute::BitFlags],
        }
    )]
    fn enum_pass(#[case] item_str: &str, #[case] output: Enum) {
        let state = ParserState::new();

        assert_eq!(
            enum_item(&state)
                .parse(item_str)
                .inspect_err(|e| println!("{e}")),
            Ok(output)
        );
    }

    #[rstest]
    #[case::invalid_field_def(
        r#"enum Hello_There : int32 {
            Variant bla,
        }"#
    )]
    #[case::missing_datatype(
        r#"enum Hello_There {
            Variant = 1,
        }"#
    )]
    #[case::invalid_field_ident(
        r#"enum Hello_There : int8 {
            1var,
        }"#
    )]
    #[case::incomplete_attributes(
        r#"enum Hello_There : int8 {
            Variant (incomplete,
        }"#
    )]
    #[case::duplicate_variant(
        r#"enum Hello_There : int8 {
            Variant1,
            Variant1,
        }"#
    )]
    fn enum_fail(#[case] item_str: &str) {
        let state = ParserState::new();

        assert!(enum_item(&state)
            .parse(item_str)
            .inspect_err(|e| println!("{e}"))
            .is_err());
    }
}
