use super::primitives::StructFieldType;

pub struct StructField<'a> {
    name: &'a str,
    field_type: StructFieldType<'a>,
}

pub struct Struct<'a> {
    name: &'a str,
    fields: Vec<StructField<'a>>,
}