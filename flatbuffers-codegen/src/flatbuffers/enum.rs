pub enum EnumData {
    /// Alias of `byte`
    Int8(Vec<i8>),
    /// Alias of `ubyte`
    UInt8(Vec<u8>),
    /// Alias of `short`
    Int16(Vec<i16>),
    /// Alias of `ushort`
    UInt16(Vec<u16>),
    /// Alias of `int`
    Int32(Vec<i32>),
    /// Alias of `uint`
    UInt32(Vec<u32>),
    /// Alias of `long`
    Int64(Vec<i64>),
    /// Alias of `ulong`
    UInt64(Vec<u64>),
}

pub struct Enum<'a> {
    name: &'a str,
    values: EnumData,
}
