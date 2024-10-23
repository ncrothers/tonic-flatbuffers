use std::{collections::HashSet, marker::PhantomData, str::FromStr};

use quote::quote;
use winnow::{
    combinator::{cut_err, opt, separated, trace},
    error::{AddContext, ContextError, ErrMode, StrContext, StrContextValue},
    stream::Stream,
    token::{literal, take_while},
    Parser,
};

use crate::parse::{
    flatbuffers::primitives::scalar_type,
    parser::{ParsedTypes, ParserState},
    utils::{ident, item_ident, whitespace_and_comments_opt, ByteSize, Namespace, TypeName},
};

use super::{
    attributes::{attribute_list, Attribute, AttributeTarget},
    primitives::ScalarType,
};

#[derive(Debug, PartialEq)]
pub struct EnumVariant<'a> {
    pub name: &'a str,
    // Using `i128` to ensure all values for any data type fit here
    pub idx: Option<i128>,
    pub comments: Vec<&'a str>,
    pub attributes: Vec<Attribute<'a>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum EnumBaseType {
    /// Alias of `byte`
    Int8,
    /// Alias of `ubyte`
    UInt8,
    /// Alias of `short`
    Int16,
    /// Alias of `ushort`
    UInt16,
    /// Alias of `int`
    Int32,
    /// Alias of `uint`
    UInt32,
    /// Alias of `long`
    Int64,
    /// Alias of `ulong`
    UInt64,
}

#[derive(Debug, PartialEq)]
pub struct Enum<'a> {
    pub name: &'a str,
    pub namespace: Namespace<'a>,
    pub variants: Vec<EnumVariant<'a>>,
    pub base_type: EnumBaseType,
    pub comments: Vec<&'a str>,
    pub attributes: Vec<Attribute<'a>>,
}

impl EnumBaseType {
    pub fn from_scalar_type(scalar_type: ScalarType) -> Option<Self> {
        match scalar_type {
            ScalarType::Int8 => Some(Self::Int8),
            ScalarType::UInt8 => Some(Self::UInt8),
            ScalarType::Int16 => Some(Self::Int16),
            ScalarType::UInt16 => Some(Self::UInt16),
            ScalarType::Int32 => Some(Self::Int32),
            ScalarType::UInt32 => Some(Self::UInt32),
            ScalarType::Int64 => Some(Self::Int64),
            ScalarType::UInt64 => Some(Self::UInt64),
            _ => None,
        }
    }

    pub fn is_valid(&self, value: i128) -> bool {
        match self {
            EnumBaseType::Int8 => i8::try_from(value).is_ok(),
            EnumBaseType::UInt8 => u8::try_from(value).is_ok(),
            EnumBaseType::Int16 => i16::try_from(value).is_ok(),
            EnumBaseType::UInt16 => u16::try_from(value).is_ok(),
            EnumBaseType::Int32 => i32::try_from(value).is_ok(),
            EnumBaseType::UInt32 => u32::try_from(value).is_ok(),
            EnumBaseType::Int64 => i64::try_from(value).is_ok(),
            EnumBaseType::UInt64 => u64::try_from(value).is_ok(),
        }
    }
}

impl<'a> Enum<'a> {
    /// Return the data type of this Enum as a [`syn::Type`].
    pub fn data_type(&self) -> syn::Type {
        // Unwrap is safe because this parse is 
        match self.base_type {
            EnumBaseType::Int8 => syn::parse2(quote! { i8 }).unwrap(),
            EnumBaseType::UInt8 => syn::parse2(quote! { u8 }).unwrap(),
            EnumBaseType::Int16 => syn::parse2(quote! { i16 }).unwrap(),
            EnumBaseType::UInt16 => syn::parse2(quote! { u16 }).unwrap(),
            EnumBaseType::Int32 => syn::parse2(quote! { i32 }).unwrap(),
            EnumBaseType::UInt32 => syn::parse2(quote! { u32 }).unwrap(),
            EnumBaseType::Int64 => syn::parse2(quote! { i64 }).unwrap(),
            EnumBaseType::UInt64 => syn::parse2(quote! { u64 }).unwrap(),
        }
    }

    /// Returns whether the bitflags attribute is present on this enum.
    pub fn is_bitflags(&self) -> bool {
        self.attributes.iter().any(|attr| matches!(attr, Attribute::BitFlags))
    }
}

impl<'a> ByteSize for Enum<'a> {
    fn size(&self, _parsed_types: &ParsedTypes) -> usize {
        match self.base_type {
            EnumBaseType::Int8 | EnumBaseType::UInt8 => 1,
            EnumBaseType::Int16 | EnumBaseType::UInt16 => 2,
            EnumBaseType::Int32 | EnumBaseType::UInt32 => 4,
            EnumBaseType::Int64 | EnumBaseType::UInt64 => 8,
        }
    }
}

fn enum_variant<'a, 's: 'a>(
    state: &'a ParserState<'s>,
    field_idents: &'a mut HashSet<&'s str>,
    base_type: EnumBaseType,
) -> impl Parser<&'s str, EnumVariant<'s>, ContextError> + 'a {
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
                    cut_err(take_while(1.., |c: char| c.is_ascii_digit() || c == '.').parse_to().verify(|idx| base_type.is_valid(*idx)))
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

            // Unwrap is safe because we've already verified the type is an integer
            let base_type = EnumBaseType::from_scalar_type(scalar).unwrap();

            let attrs = opt(attribute_list(state, AttributeTarget::EnumItem))
                .parse_next(input)?
                .map(|attrs| attrs.attrs);
            whitespace_and_comments_opt(input)?;

            cut_err(literal("{"))
                .context(StrContext::Expected(StrContextValue::StringLiteral("{")))
                .parse_next(input)?;

            fn parse_variants<'a, 's: 'a>(
                state: &'a ParserState<'s>,
                base_type: EnumBaseType,
            ) -> impl Parser<&'s str, Vec<EnumVariant<'s>>, ContextError> + 'a {
                move |input: &mut _| {
                    let mut field_idents = HashSet::new();
                    let variants = separated(0.., enum_variant(state, &mut field_idents, base_type), ",")
                        .parse_next(input)?;
                    // Consume a trailing comma, if present
                    opt(literal(",")).parse_next(input)?;

                    Ok(variants)
                }
            }

            let variants = cut_err(
                    parse_variants(state, base_type)
                        .context(StrContext::Expected(StrContextValue::Description("int8"))),
                )
                .parse_next(input)?;

            // // Parse variants according to the data type
            // let variants = match scalar {
            //     ScalarType::Int8 => EnumBaseType::Int8(
            //         cut_err(
            //             parse_variants(state)
            //                 .context(StrContext::Expected(StrContextValue::Description("int8"))),
            //         )
            //         .parse_next(input)?,
            //     ),
            //     ScalarType::UInt8 => EnumBaseType::UInt8(
            //         cut_err(
            //             parse_variants(state)
            //                 .context(StrContext::Expected(StrContextValue::Description("uint8"))),
            //         )
            //         .parse_next(input)?,
            //     ),
            //     ScalarType::Int16 => EnumBaseType::Int16(
            //         cut_err(
            //             parse_variants(state)
            //                 .context(StrContext::Expected(StrContextValue::Description("int16"))),
            //         )
            //         .parse_next(input)?,
            //     ),
            //     ScalarType::UInt16 => EnumBaseType::UInt16(
            //         cut_err(
            //             parse_variants(state)
            //                 .context(StrContext::Expected(StrContextValue::Description("uint16"))),
            //         )
            //         .parse_next(input)?,
            //     ),
            //     ScalarType::Int32 => EnumBaseType::Int32(
            //         cut_err(
            //             parse_variants(state)
            //                 .context(StrContext::Expected(StrContextValue::Description("int32"))),
            //         )
            //         .parse_next(input)?,
            //     ),
            //     ScalarType::UInt32 => EnumBaseType::UInt32(
            //         cut_err(
            //             parse_variants(state)
            //                 .context(StrContext::Expected(StrContextValue::Description("uint32"))),
            //         )
            //         .parse_next(input)?,
            //     ),
            //     ScalarType::Int64 => EnumBaseType::Int64(
            //         cut_err(
            //             parse_variants(state)
            //                 .context(StrContext::Expected(StrContextValue::Description("int64"))),
            //         )
            //         .parse_next(input)?,
            //     ),
            //     ScalarType::UInt64 => EnumBaseType::UInt64(
            //         cut_err(
            //             parse_variants(state)
            //                 .context(StrContext::Expected(StrContextValue::Description("uint64"))),
            //         )
            //         .parse_next(input)?,
            //     ),
            //     _ => unreachable!(),
            // };

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
                base_type,
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
            variants: vec![
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
            ],
            base_type: EnumBaseType::UInt32,
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
            variants:vec![
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
            ],
            base_type: EnumBaseType::Int32,
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
            variants: vec![
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
            ],
            base_type: EnumBaseType::Int32,
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
            variants: vec![
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
            ],
            base_type: EnumBaseType::Int32,
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
    #[case::value_out_of_range(
        r#"enum Hello_There : uint8 {
            Variant1 = 256,
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
