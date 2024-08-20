use super::{attributes::Attribute, primitives::Type};

pub struct TableField<'a> {
    name: &'a str,
    field_type: Type<'a>,
    default: Option<&'a str>,
    attributes: Vec<Attribute<'a>>,
}

pub struct Table<'a> {
    fields: Vec<TableField<'a>>,
}