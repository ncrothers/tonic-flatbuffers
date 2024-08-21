use super::{attributes::Attribute, primitives::TypeIdent};

pub struct TableField<'a> {
    name: &'a str,
    field_type: TypeIdent<'a>,
    default: Option<&'a str>,
    attributes: Vec<Attribute<'a>>,
}

pub struct Table<'a> {
    fields: Vec<TableField<'a>>,
}
