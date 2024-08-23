use winnow::{
    ascii::digit1,
    combinator::{alt, trace},
    error::{AddContext, ContextError, ErrMode, StrContext, StrContextValue},
    stream::Stream,
    token::literal,
    PResult, Parser,
};

use crate::{
    parser::{DeclType, ParserState},
    utils::{ident, resolved_ident, whitespace_and_comments_opt},
};

#[derive(Debug, PartialEq)]
pub struct Array<'a> {
    pub item_type: ArrayItemType<'a>,
    pub length: usize,
}

#[derive(Debug, PartialEq)]
pub enum ArrayItemType<'a> {
    Named(&'a str),
    Scalar(ScalarType),
}

#[derive(Debug, PartialEq)]
pub enum TableFieldType<'a> {
    Scalar(ScalarType),
    String,
    Named(&'a str),
    Vector(VectorItemType<'a>),
}

/// Type that a struct field can have, which is limited to other structs and
/// scalar types.
#[derive(Debug, PartialEq)]
pub enum StructFieldType<'a> {
    Array(Array<'a>),
    Named(&'a str),
    Scalar(ScalarType),
}

#[derive(Debug, PartialEq)]
pub enum VectorItemType<'a> {
    Named(&'a str),
    Scalar(ScalarType),
    String,
}

#[derive(Debug, PartialEq)]
pub enum BuiltInTable<'a> {
    Scalar(ScalarType),
    Vector(VectorItemType<'a>),
    /// UTF-8 or ASCII only
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
            // let item_type = resolved_ident(state, &[DeclType::Enum, DeclType::Struct]).parse_next(input).map(|ident| {
            //     if let Some(scalar) = ScalarType::parse(ident) {
            //         ArrayItemType::Scalar(scalar)
            //     } else {
            //         ArrayItemType::Named(ident)
            //     }
            // })?;

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

                // let ident = resolved_ident(state, &[DeclType::Enum, DeclType::Struct]).parse_next(input)?;

                // if let Some(scalar) = ScalarType::parse(ident) {
                //     StructFieldType::Scalar(scalar)
                // } else {

                //     StructFieldType::Named(ident)
                // }
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

            // let value = resolved_ident(state, DeclType::ANY).parse_next(input)?;

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

    use crate::parser::TypeDecls;

    use super::*;

    #[test]
    fn scalar() {
        let valid = [
            ("uint32", ScalarType::UInt32),
            ("uint", ScalarType::UInt32),
            ("float", ScalarType::Float32),
            ("byte", ScalarType::Int8),
        ];

        for (item_str, item) in valid {
            let mut value = item_str;
            assert_eq!(scalar_type.parse_next(&mut value), Ok(item));
        }

        let invalid = ["i32", "_uint32", "_float"];

        for item in invalid {
            let mut value = item;
            assert!(scalar_type.parse_next(&mut value).is_err());
        }
    }

    #[test]
    fn vector_wrapped() {
        let mut state = ParserState::new();

        let mut foo_decl = TypeDecls::new();
        foo_decl.add_structs(["foo"]);

        let decls = HashMap::from([
            ("", foo_decl.clone()),
            ("namespace", foo_decl.clone()),
            ("one.two.three", foo_decl.clone()),
        ]);

        state.extend_decls(decls);

        let valid = [
            ("[uint32]", VectorItemType::Scalar(ScalarType::UInt32)),
            ("[foo]", VectorItemType::Named("foo")),
            ("[ namespace.foo]", VectorItemType::Named("namespace.foo")),
            ("[float32 ]", VectorItemType::Scalar(ScalarType::Float32)),
        ];

        for (item_str, item) in valid {
            let mut value = item_str;
            assert_eq!(vector_type(&state).parse_next(&mut value), Ok(item));
        }

        let invalid = [
            "uint32",
            "[uint32",
            "uint32]",
            "[uint32 asd]",
            "a  c",
            "[invalid_type]",
        ];

        for item in invalid {
            let mut value = item;
            assert!(vector_type(&state).parse_next(&mut value).is_err());
        }
    }

    #[test]
    fn table_field_type_() {
        let mut state = ParserState::new();

        let mut foo_decl = TypeDecls::new();
        foo_decl.add_structs(["foo"]);

        let decls = HashMap::from([
            ("", foo_decl.clone()),
            ("namespace", foo_decl.clone()),
            ("one.two.three", foo_decl.clone()),
        ]);

        state.extend_decls(decls);

        let valid = [
            (
                "[uint32]",
                TableFieldType::Vector(VectorItemType::Scalar(ScalarType::UInt32)),
            ),
            (
                "[bool ]",
                TableFieldType::Vector(VectorItemType::Scalar(ScalarType::Bool)),
            ),
            (
                "[ foo]",
                TableFieldType::Vector(VectorItemType::Named("foo")),
            ),
            (
                "[double ]",
                TableFieldType::Vector(VectorItemType::Scalar(ScalarType::Float64)),
            ),
            ("double", TableFieldType::Scalar(ScalarType::Float64)),
            ("string", TableFieldType::String),
            ("namespace.foo", TableFieldType::Named("namespace.foo")),
        ];

        for (item_str, item) in valid {
            let mut value = item_str;
            assert_eq!(table_field_type(&state).parse_next(&mut value), Ok(item));
        }

        let invalid = ["[uint32", "[uint32 asd]", "invalid_type"];

        for item in invalid {
            let mut value = item;
            assert!(table_field_type(&state).parse_next(&mut value).is_err());
        }
    }

    #[test]
    fn array() {
        let mut state = ParserState::new();

        let mut foo_decl = TypeDecls::new();
        foo_decl.add_structs(["foo"]);
        foo_decl.add_tables(["bar"]);

        let decls = HashMap::from([
            ("", foo_decl.clone()),
            ("namespace", foo_decl.clone()),
            ("one.two.three", foo_decl.clone()),
        ]);

        state.extend_decls(decls);

        let valid = [
            (
                "[uint32:5]",
                Array {
                    item_type: ArrayItemType::Scalar(ScalarType::UInt32),
                    length: 5,
                },
            ),
            (
                "[ float :\n10 ]",
                Array {
                    item_type: ArrayItemType::Scalar(ScalarType::Float32),
                    length: 10,
                },
            ),
            (
                "[ namespace.foo :\n1500000 ]",
                Array {
                    item_type: ArrayItemType::Named("namespace.foo"),
                    length: 1_500_000,
                },
            ),
        ];

        for (item_str, item) in valid {
            let mut value = item_str;
            assert_eq!(array_type(&state).parse_next(&mut value), Ok(item));
        }

        let invalid = ["[uint32]", "[uint32:5", "[hello 5]", "uint32:5]", "[bar:3]"];

        for item in invalid {
            let mut value = item;
            assert!(array_type(&state).parse_next(&mut value).is_err());
        }
    }

    #[test]
    fn struct_field_type_() {
        let mut state = ParserState::new();

        let mut foo_decl = TypeDecls::new();
        foo_decl.add_structs(["foo"]);
        foo_decl.add_tables(["bar"]);

        let decls = HashMap::from([
            ("", foo_decl.clone()),
            ("namespace", foo_decl.clone()),
            ("one.two.three", foo_decl.clone()),
        ]);

        state.extend_decls(decls);

        let valid = [
            (
                "[uint32\n: \n 5]",
                StructFieldType::Array(Array {
                    item_type: ArrayItemType::Scalar(ScalarType::UInt32),
                    length: 5,
                }),
            ),
            ("float", StructFieldType::Scalar(ScalarType::Float32)),
            ("namespace.foo", StructFieldType::Named("namespace.foo")),
        ];

        for (item_str, item) in valid {
            let mut value = item_str;
            assert_eq!(struct_field_type(&state).parse_next(&mut value), Ok(item));
        }

        let invalid = ["[uint32]", "[uint32:5", "bar"];

        for item in invalid {
            let mut value = item;
            assert!(struct_field_type(&state).parse_next(&mut value).is_err());
        }
    }
}
