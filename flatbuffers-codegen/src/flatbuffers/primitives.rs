use winnow::{
    ascii::digit1,
    error::{AddContext, ContextError, ErrMode, ParserError, StrContext, StrContextValue},
    stream::Stream,
    token::literal,
    PResult, Parser,
};

use crate::utils::{consume_whitespace, consume_whitespace_and_comments, parse_ident};

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

pub struct ParseArray;

impl<'s, E> Parser<&'s str, Array<'s>, E> for ParseArray
where
    E: AddContext<&'s str, StrContext>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<Array<'s>, E> {
        consume_whitespace_and_comments(input)?;
        // Try to consume an opening square bracket
        literal("[").parse_next(input)?;
        // Clear out any whitespace
        consume_whitespace_and_comments(input)?;

        // Parse the item type
        let item_type = parse_ident(input).map(|ident| {
            if let Some(scalar) = ScalarType::parse(ident) {
                ArrayItemType::Scalar(scalar)
            } else {
                ArrayItemType::Named(ident)
            }
        })?;

        consume_whitespace_and_comments(input)?;

        // Consume the delimiter
        literal(":").parse_next(input)?;
        consume_whitespace_and_comments(input)?;

        // let length_start = input.checkpoint();
        let length = Parser::parse_to(digit1).parse_next(input)?;

        consume_whitespace_and_comments(input)?;
        // Try to consume a closing square bracket
        literal("]")
            .context(StrContext::Label("array"))
            .context(StrContext::Expected(StrContextValue::Description(
                "closing bracket",
            )))
            .parse_next(input)?;

        Ok(Array { item_type, length })
    }
}

pub struct ParseStructFieldType;

impl<'s, E> Parser<&'s str, StructFieldType<'s>, E> for ParseStructFieldType
where
    E: AddContext<&'s str, StrContext>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<StructFieldType<'s>, E> {
        consume_whitespace_and_comments(input)?;
        // Parse as vector
        let val = if input.starts_with('[') {
            let array = ParseArray.parse_next(input)?;

            StructFieldType::Array(array)
        } else {
            let ident = parse_ident(input)?;

            if let Some(scalar) = ScalarType::parse(ident) {
                StructFieldType::Scalar(scalar)
            } else {
                StructFieldType::Struct(ident)
            }
        };

        Ok(val)
    }
}

pub struct VectorWrapped;

impl<'s, E> Parser<&'s str, <&'s str as Stream>::Slice, E> for VectorWrapped
where
    E: ParserError<&'s str> + AddContext<&'s str, StrContext>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<<&'s str as Stream>::Slice, E> {
        consume_whitespace_and_comments(input)?;
        // Try to consume an opening square bracket
        literal("[").parse_next(input)?;
        // Clear out any whitespace
        consume_whitespace_and_comments(input)?;
        // Parse the inner type
        let value = parse_ident(input)?;

        consume_whitespace_and_comments(input)?;
        // Try to consume a closing square bracket
        literal("]")
            .context(StrContext::Label("vector"))
            .context(StrContext::Expected(StrContextValue::Description(
                "closing bracket",
            )))
            .parse_next(input)?;

        Ok(value)
    }
}

pub struct ParseTypeIdent;

impl<'s, E> Parser<&'s str, TableFieldType<'s>, E> for ParseTypeIdent
where
    E: ParserError<&'s str> + AddContext<&'s str, StrContext>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<TableFieldType<'s>, E> {
        consume_whitespace_and_comments(input)?;
        // Parse as vector
        let val = if input.starts_with('[') {
            let ident = VectorWrapped.parse_next(input)?;

            if ident == "string" {
                TableFieldType::Vector(VectorItemType::String)
            } else if let Some(scalar) = ScalarType::parse(ident) {
                TableFieldType::Vector(VectorItemType::Scalar(scalar))
            } else {
                TableFieldType::Vector(VectorItemType::Named(ident))
            }
        } else {
            let ident = parse_ident(input)?;

            if ident == "string" {
                TableFieldType::String
            } else if let Some(scalar) = ScalarType::parse(ident) {
                TableFieldType::Scalar(scalar)
            } else {
                TableFieldType::Named(ident)
            }
        };

        Ok(val)
    }
}

pub struct ParseScalarType;

impl<'s, E> Parser<&'s str, ScalarType, E> for ParseScalarType
where
    E: AddContext<&'s str, StrContext>,
    ErrMode<E>: From<ErrMode<ContextError>>,
{
    fn parse_next(&mut self, input: &mut &'s str) -> PResult<ScalarType, E> {
        consume_whitespace_and_comments(input)?;
        let checkpoint = input.checkpoint();

        // Parse the ident from the type
        let ident = parse_ident(input)?;

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
            ErrMode::Backtrack(err).into()
        })
    }
}

/// Type that a struct field can have, which is limited to other structs and
/// scalar types.
#[derive(Debug, PartialEq)]
pub enum StructFieldType<'a> {
    Array(Array<'a>),
    Struct(&'a str),
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

#[derive(Debug, PartialEq)]
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

pub enum NonScalar {
    Vector,
}

#[cfg(test)]
mod tests {
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
            assert_eq!(ParseScalarType.parse_next(&mut value), Ok(item));
        }

        let invalid = ["i32", "_uint32", "_float"];

        for item in invalid {
            let mut value = item;
            assert!(ParseScalarType.parse_next(&mut value).is_err());
        }
    }

    #[test]
    fn vector_wrapped() {
        let valid = [
            ("[uint32]", "uint32"),
            ("[hello]", "hello"),
            ("[ type]", "type"),
            ("[test ]", "test"),
        ];

        for (item_str, item) in valid {
            let mut value = item_str;
            assert_eq!(VectorWrapped.parse_next(&mut value), Ok(item));
        }

        let invalid = ["uint32", "[uint32", "uint32]", "[uint32 asd]", "a  c"];

        for item in invalid {
            let mut value = item;
            assert!(VectorWrapped.parse_next(&mut value).is_err());
        }
    }

    #[test]
    fn type_ident() {
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
                "[ hello]",
                TableFieldType::Vector(VectorItemType::Named("hello")),
            ),
            (
                "[double ]",
                TableFieldType::Vector(VectorItemType::Scalar(ScalarType::Float64)),
            ),
            ("double", TableFieldType::Scalar(ScalarType::Float64)),
            ("string", TableFieldType::String),
            ("hello", TableFieldType::Named("hello")),
        ];

        for (item_str, item) in valid {
            let mut value = item_str;
            assert_eq!(ParseTypeIdent.parse_next(&mut value), Ok(item));
        }

        let invalid = ["[uint32", "[uint32 asd]"];

        for item in invalid {
            let mut value = item;
            assert!(ParseTypeIdent.parse_next(&mut value).is_err());
        }
    }

    #[test]
    fn array() {
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
                "[ hello :\n1500000 ]",
                Array {
                    item_type: ArrayItemType::Named("hello"),
                    length: 1_500_000,
                },
            ),
        ];

        for (item_str, item) in valid {
            let mut value = item_str;
            assert_eq!(ParseArray.parse_next(&mut value), Ok(item));
        }

        let invalid = ["[uint32]", "[uint32:5", "[hello 5]", "uint32:5]"];

        for item in invalid {
            let mut value = item;
            assert!(ParseArray.parse_next(&mut value).is_err());
        }
    }

    #[test]
    fn struct_field_type() {
        let valid = [
            (
                "[uint32\n: \n 5]",
                StructFieldType::Array(Array {
                    item_type: ArrayItemType::Scalar(ScalarType::UInt32),
                    length: 5,
                }),
            ),
            ("float", StructFieldType::Scalar(ScalarType::Float32)),
            ("hello", StructFieldType::Struct("hello")),
        ];

        for (item_str, item) in valid {
            let mut value = item_str;
            assert_eq!(ParseStructFieldType.parse_next(&mut value), Ok(item));
        }

        let invalid = ["[uint32]", "[uint32:5"];

        for item in invalid {
            let mut value = item;
            assert!(ParseArray.parse_next(&mut value).is_err());
        }
    }
}
