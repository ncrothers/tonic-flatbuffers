pub struct UnionValue<'a> {
    name: &'a str,
    alias: Option<&'a str>,
}

pub struct Union<'a> {
    name: &'a str,
    values: Vec<UnionValue<'a>>,
}