use super::{r#enum::Enum, union::Union};

pub struct Array {
    item_type: ScalarType,
    length: usize,
}

pub enum Type<'a> {
    Enum(Enum<'a>),
    Primitive(Primitive<'a>),
    Struct(&'a str),
    Table(&'a str),
    Union(Union<'a>),
}

/// Type that a struct field can have, which is limited to other structs and
/// scalar types.
pub enum StructFieldType<'a> {
    Array(Array),
    Struct(&'a str),
    Scalar(ScalarType),
}

pub enum Primitive<'a> {
    Scalar(ScalarType),
    Vector(&'a str),
    /// UTF-8 or ASCII only
    String,
}

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

pub enum NonScalar {
    Vector
}