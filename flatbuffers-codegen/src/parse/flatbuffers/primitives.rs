use winnow::{
    ascii::digit1,
    combinator::{alt, trace},
    error::{AddContext, ContextError, ErrMode, StrContext, StrContextValue},
    stream::Stream,
    token::literal,
    PResult, Parser,
};

use crate::parse::{
    parser::{DeclType, NamedType, ParserState},
    utils::{ident, resolved_ident, whitespace_and_comments_opt, ByteSize},
};

#[derive(Debug, PartialEq)]
pub struct Array<'a> {
    pub item_type: ArrayItemType<'a>,
    pub length: usize,
}

#[derive(Debug, PartialEq)]
pub enum ArrayItemType<'a> {
    Named(NamedType<'a>),
    Scalar(ScalarType),
}

#[derive(Debug, PartialEq)]
pub enum TableFieldType<'a> {
    Named(NamedType<'a>),
    Scalar(ScalarType),
    String,
    Vector(VectorItemType<'a>),
}

/// Type that a struct field can have, which is limited to other structs and
/// scalar types.
#[derive(Debug, PartialEq)]
pub enum StructFieldType<'a> {
    Array(Array<'a>),
    Named(NamedType<'a>),
    Scalar(ScalarType),
}

#[derive(Debug, PartialEq)]
pub enum VectorItemType<'a> {
    Named(NamedType<'a>),
    Scalar(ScalarType),
    String,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ScalarType {
    /// Alias of `byte`
    Int8,
    /// Alias of `ubyte`
    UInt8,
    Bool,
    /// Alias of `short`
    Int16,
    /// Alias of `ushort`
    UInt16,
    /// Alias of `int`
    Int32,
    /// Alias of `uint`
    UInt32,
    /// Alias of `float`
    Float32,
    /// Alias of `long`
    Int64,
    /// Alias of `ulong`
    UInt64,
    /// Alias of `double`
    Float64,
}

#[derive(Debug, PartialEq)]
pub enum DefaultValue<'a> {
    /// Alias of `byte`
    Int8(i8),
    /// Alias of `ubyte`
    UInt8(u8),
    Bool(bool),
    /// Alias of `short`
    Int16(i16),
    /// Alias of `ushort`
    UInt16(u16),
    /// Alias of `int`
    Int32(i32),
    /// Alias of `uint`
    UInt32(u32),
    /// Alias of `float`
    Float32(f32),
    /// Alias of `long`
    Int64(i64),
    /// Alias of `ulong`
    UInt64(u64),
    /// Alias of `double`
    Float64(f64),
    String(&'a str),
    Vector,
    Null,
}

impl<'a> DefaultValue<'a> {
    pub fn parse_scalar(value: &'a str, scalar_type: ScalarType) -> Option<Self> {
        match scalar_type {
            ScalarType::Int8 => value.parse().ok().map(DefaultValue::Int8),
            ScalarType::UInt8 => value.parse().ok().map(DefaultValue::UInt8),
            ScalarType::Bool => value.parse().ok().map(DefaultValue::Bool),
            ScalarType::Int16 => value.parse().ok().map(DefaultValue::Int16),
            ScalarType::UInt16 => value.parse().ok().map(DefaultValue::UInt16),
            ScalarType::Int32 => value.parse().ok().map(DefaultValue::Int32),
            ScalarType::UInt32 => value.parse().ok().map(DefaultValue::UInt32),
            ScalarType::Float32 => value.parse().ok().map(DefaultValue::Float32),
            ScalarType::Int64 => value.parse().ok().map(DefaultValue::Int64),
            ScalarType::UInt64 => value.parse().ok().map(DefaultValue::UInt64),
            ScalarType::Float64 => value.parse().ok().map(DefaultValue::Float64),
        }
    }
}

impl ScalarType {
    pub fn parse(ident: &str) -> Option<Self> {
        match ident {
            "int8" | "byte" => Some(ScalarType::Int8),
            "uint8" | "ubyte" => Some(ScalarType::UInt8),
            "bool" => Some(ScalarType::Bool),
            "int16" | "short" => Some(ScalarType::Int16),
            "uint16" | "ushort" => Some(ScalarType::UInt16),
            "int32" | "int" => Some(ScalarType::Int32),
            "uint32" | "uint" => Some(ScalarType::UInt32),
            "float32" | "float" => Some(ScalarType::Float32),
            "int64" | "long" => Some(ScalarType::Int64),
            "uint64" | "ulong" => Some(ScalarType::UInt64),
            "float64" | "double" => Some(ScalarType::Float64),
            _ => None,
        }
    }

    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            Self::Int8
                | Self::UInt8
                | Self::Int16
                | Self::UInt16
                | Self::Int32
                | Self::UInt32
                | Self::Int64
                | Self::UInt64
        )
    }
}

impl ByteSize for ScalarType {
    fn size(&self) -> usize {
        match self {
            // 8-bit types
            ScalarType::Int8 | ScalarType::UInt8 | ScalarType::Bool => 1,
            // 16-bit types
            ScalarType::Int16 | ScalarType::UInt16 => 2,
            // 32-bit types
            ScalarType::Int32 | ScalarType::UInt32 | ScalarType::Float32 => 4,
            // 64-bit types
            ScalarType::Int64 | ScalarType::UInt64 | ScalarType::Float64 => 8,
        }
    }
}

impl<'a> ByteSize for StructFieldType<'a> {
    fn size(&self) -> usize {
        match self {
            // Size of the item * number of items
            StructFieldType::Array(array) => array.item_type.size() * array.length,
            StructFieldType::Named(named_type) => named_type.size(),
            StructFieldType::Scalar(scalar_type) => scalar_type.size(),
        }
    }
}

impl<'a> ByteSize for ArrayItemType<'a> {
    fn size(&self) -> usize {
        match self {
            ArrayItemType::Named(named_type) => named_type.size(),
            ArrayItemType::Scalar(scalar_type) => scalar_type.size(),
        }
    }
}

impl<'a> ByteSize for NamedType<'a> {
    fn size(&self) -> usize {
        todo!("implement once NamedType has access to the actual type")
    }
}

pub fn array_type<'a, 's: 'a>(
    state: &'a ParserState<'s>,
) -> impl Parser<&'s str, Array<'s>, ContextError> + 'a {
    |input: &mut _| {
        trace("array_type", |input: &mut _| {
            whitespace_and_comments_opt(input)?;
            // Try to consume an opening square bracket
            literal("[").parse_next(input)?;
            // Clear out any whitespace
            whitespace_and_comments_opt(input)?;

            // Parse the item type
            let item_type = alt((
                scalar_type.map(ArrayItemType::Scalar),
                resolved_ident(state, &[DeclType::Enum, DeclType::Struct])
                    .map(ArrayItemType::Named),
            ))
            .parse_next(input)?;

            whitespace_and_comments_opt(input)?;

            // Consume the delimiter
            literal(":").parse_next(input)?;
            whitespace_and_comments_opt(input)?;

            // let length_start = input.checkpoint();
            let length = Parser::parse_to(digit1).parse_next(input)?;

            whitespace_and_comments_opt(input)?;
            // Try to consume a closing square bracket
            literal("]")
                .context(StrContext::Label("array"))
                .context(StrContext::Expected(StrContextValue::Description(
                    "closing bracket",
                )))
                .parse_next(input)?;

            Ok(Array { item_type, length })
        })
        .parse_next(input)
    }
}

pub fn struct_field_type<'a, 's: 'a>(
    state: &'a ParserState<'s>,
) -> impl Parser<&'s str, StructFieldType<'s>, ContextError> + 'a {
    |input: &mut _| {
        trace("struct_field_type", |input: &mut _| {
            whitespace_and_comments_opt(input)?;
            // Parse as vector
            let val = if input.starts_with('[') {
                let array = array_type(state).parse_next(input)?;

                StructFieldType::Array(array)
            } else {
                alt((
                    scalar_type.map(StructFieldType::Scalar),
                    resolved_ident(state, &[DeclType::Enum, DeclType::Struct])
                        .map(StructFieldType::Named),
                ))
                .parse_next(input)?
            };

            Ok(val)
        })
        .parse_next(input)
    }
}

pub fn vector_type<'a, 's: 'a>(
    state: &'a ParserState<'s>,
) -> impl Parser<&'s str, VectorItemType<'s>, ContextError> + 'a {
    |input: &mut _| {
        trace("vector_type", |input: &mut _| {
            whitespace_and_comments_opt(input)?;
            // Try to consume an opening square bracket
            literal("[").parse_next(input)?;
            // Clear out any whitespace
            whitespace_and_comments_opt(input)?;
            // Parse the inner type

            let value = alt((
                scalar_type.map(VectorItemType::Scalar),
                literal("string").map(|_| VectorItemType::String),
                resolved_ident(state, DeclType::ANY).map(VectorItemType::Named),
            ))
            .parse_next(input)?;

            whitespace_and_comments_opt(input)?;
            // Try to consume a closing square bracket
            literal("]")
                .context(StrContext::Label("vector"))
                .context(StrContext::Expected(StrContextValue::Description(
                    "closing bracket",
                )))
                .parse_next(input)?;

            Ok(value)
        })
        .parse_next(input)
    }
}

pub fn table_field_type<'a, 's: 'a>(
    state: &'a ParserState<'s>,
) -> impl Parser<&'s str, TableFieldType<'s>, ContextError> + 'a {
    |input: &mut _| {
        trace("table_field_type", |i: &mut _| {
            whitespace_and_comments_opt(i)?;
            // Parse as vector
            let val = if i.starts_with('[') {
                let ident = vector_type(state).parse_next(i)?;

                TableFieldType::Vector(ident)
            } else {
                alt((
                    scalar_type.map(TableFieldType::Scalar),
                    literal("string").map(|_| TableFieldType::String),
                    resolved_ident(state, DeclType::ANY).map(TableFieldType::Named),
                ))
                .parse_next(i)?
            };

            Ok(val)
        })
        .parse_next(input)
    }
}

pub fn scalar_type(input: &mut &str) -> PResult<ScalarType> {
    trace("scalar_type", |input: &mut _| {
        whitespace_and_comments_opt(input)?;
        let checkpoint = input.checkpoint();

        // Parse the ident from the type
        let ident = ident.parse_next(input)?;

        ScalarType::parse(ident).ok_or_else(|| {
            let err = ContextError::new()
                .add_context(input, &checkpoint, StrContext::Label("scalar"))
                .add_context(
                    input,
                    &checkpoint,
                    StrContext::Expected(StrContextValue::Description(
                        "flatbuffer-defined scalar type",
                    )),
                );
            ErrMode::Backtrack(err)
        })
    })
    .parse_next(input)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use rstest::rstest;

    use crate::parse::parser::TypeDecls;

    use super::*;

    #[rstest]
    #[case::uint8("uint8", ScalarType::UInt8)]
    #[case::int8("int8", ScalarType::Int8)]
    #[case::byte("byte", ScalarType::Int8)]
    #[case::ubyte("ubyte", ScalarType::UInt8)]
    #[case::uint16("uint16", ScalarType::UInt16)]
    #[case::int16("int16", ScalarType::Int16)]
    #[case::uint32("uint32", ScalarType::UInt32)]
    #[case::int32("int32", ScalarType::Int32)]
    #[case::uint("uint", ScalarType::UInt32)]
    #[case::int("int", ScalarType::Int32)]
    #[case::bool("bool", ScalarType::Bool)]
    #[case::float32("float32", ScalarType::Float32)]
    #[case::float("float", ScalarType::Float32)]
    #[case::float64("float64", ScalarType::Float64)]
    #[case::double("double", ScalarType::Float64)]
    fn scalar_pass(#[case] item_str: &str, #[case] output: ScalarType) {
        assert_eq!(scalar_type.parse(item_str), Ok(output));

        let invalid = ["i32", "_uint32", "_float"];

        for item in invalid {
            let mut value = item;
            assert!(scalar_type.parse_next(&mut value).is_err());
        }
    }

    #[rstest]
    #[case::rustlike_name("i32")]
    #[case::leading_underscore("_uint32")]
    #[case::trailing_underscore("uint32_")]
    fn scalar_fail(#[case] item_str: &str) {
        assert!(scalar_type.parse(item_str).is_err());
    }

    #[rstest]
    #[case::scalar("[uint32]", VectorItemType::Scalar(ScalarType::UInt32))]
    #[case::string("[string]", VectorItemType::String)]
    #[case::named_(
        "[foo]",
        VectorItemType::Named(NamedType::new("foo", "", DeclType::Struct))
    )]
    #[case::whitespace(
        "[ \nfoo\n ]",
        VectorItemType::Named(NamedType::new("foo", "", DeclType::Struct))
    )]
    fn vector_wrapped_pass(#[case] item_str: &str, #[case] output: VectorItemType) {
        let mut state = ParserState::new();

        let mut foo_decl = TypeDecls::new();
        foo_decl.add_structs(["foo"]);

        let decls = HashMap::from([("", foo_decl.clone())]);

        state.extend_decls(decls);

        assert_eq!(vector_type(&state).parse(item_str), Ok(output));
    }

    #[rstest]
    #[case::no_brackets("uint32")]
    #[case::unclosed_bracket("[uint32")]
    #[case::unopened_bracket("uint32]")]
    #[case::extra_text("[uint32 asd]")]
    fn vector_wrapped_fail(#[case] item_str: &str) {
        let state = ParserState::new();

        assert!(vector_type(&state).parse(item_str).is_err());
    }

    #[rstest]
    #[case::vector(
        "[uint32]",
        TableFieldType::Vector(VectorItemType::Scalar(ScalarType::UInt32))
    )]
    #[case::scalar("double", TableFieldType::Scalar(ScalarType::Float64))]
    #[case::string("string", TableFieldType::String)]
    fn table_field_type_pass(#[case] item_str: &str, #[case] output: TableFieldType) {
        let state = ParserState::new();

        assert_eq!(table_field_type(&state).parse(item_str), Ok(output));
    }

    #[rstest]
    #[case::unknown_type("fake_type")]
    #[case::array("[uint32:3]")]
    fn table_field_type_fail(#[case] item_str: &str) {
        let state = ParserState::new();

        assert!(table_field_type(&state).parse(item_str).is_err());
    }

    #[rstest]
    #[case::scalar(
        "[uint32:5]",
        Array {
            item_type: ArrayItemType::Scalar(ScalarType::UInt32),
            length: 5,
        },
    )]
    #[case::named(
        "[Struct1:1500000]",
        Array {
            item_type: ArrayItemType::Named(NamedType::new(
                "Struct1",
                "",
                DeclType::Struct,
            )),
            length: 1_500_000,
        },
    )]
    #[case::whitespace(
        "[\n uint32 \n : \n 10 \n]",
        Array {
            item_type: ArrayItemType::Scalar(ScalarType::UInt32),
            length: 10,
        },
    )]
    fn array_pass(#[case] item_str: &str, #[case] output: Array) {
        let mut state = ParserState::new();

        let mut foo_decl = TypeDecls::new();
        foo_decl.add_structs(["Struct1"]);

        let decls = HashMap::from([("", foo_decl.clone())]);

        state.extend_decls(decls);

        assert_eq!(array_type(&state).parse(item_str), Ok(output));
    }

    #[rstest]
    #[case::vector_styled("[uint32]")]
    #[case::missing_size("[uint32:]")]
    #[case::unclosed_bracket("[uint32:5")]
    #[case::unopened_bracket("uint32:5]")]
    #[case::nonstruct_named("[Table1:5]")]
    fn array_fail(#[case] item_str: &str) {
        let mut state = ParserState::new();

        let mut foo_decl = TypeDecls::new();
        foo_decl.add_tables(["Table1"]);

        let decls = HashMap::from([("", foo_decl.clone())]);

        state.extend_decls(decls);

        assert!(array_type(&state).parse(item_str).is_err());
    }

    #[rstest]
    #[case::scalar("uint32", StructFieldType::Scalar(ScalarType::UInt32))]
    #[case::named(
        "Struct1",
        StructFieldType::Named(
            NamedType {
                ident: "Struct1",
                namespace: "",
                decl_type: DeclType::Struct,
            }
        ),
    )]
    #[case::array(
        "[uint32:5]",
        StructFieldType::Array(Array {
            item_type: ArrayItemType::Scalar(ScalarType::UInt32),
            length: 5,
        }),
    )]
    fn struct_field_type_pass(#[case] item_str: &str, #[case] output: StructFieldType) {
        let mut state = ParserState::new();

        let mut foo_decl = TypeDecls::new();
        foo_decl.add_structs(["Struct1"]);

        let decls = HashMap::from([("", foo_decl.clone())]);

        state.extend_decls(decls);

        assert_eq!(struct_field_type(&state).parse(item_str), Ok(output));
    }

    #[rstest]
    #[case::vector("[uint32]")]
    #[case::string("string")]
    #[case::nonstruct_named("Table1")]
    fn struct_field_type_fail(#[case] item_str: &str) {
        let mut state = ParserState::new();

        let mut foo_decl = TypeDecls::new();
        foo_decl.add_tables(["Table1"]);

        let decls = HashMap::from([("", foo_decl.clone())]);

        state.extend_decls(decls);

        assert!(struct_field_type(&state).parse(item_str).is_err());
    }
}
