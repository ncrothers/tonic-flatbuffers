use proc_macro2::TokenStream;

use crate::parse::{flatbuffers::r#struct::Struct, parser::ParsedTypes};

use super::generator::r#struct::generate_struct;

pub trait FlatbuffersGenerator {
    /// Creates a [`proc_macro2::TokenStream`] for the struct
    fn generate_struct(&self, item: &Struct, parsed_types: &ParsedTypes) -> TokenStream {
        generate_struct(item, parsed_types)
    }
}
