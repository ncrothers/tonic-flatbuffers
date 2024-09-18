use proc_macro2::TokenStream;

use crate::parse::flatbuffers::r#struct::Struct;

use super::generator::r#struct::generate_struct;

pub trait FlatbuffersGenerator {
    /// Creates a [`proc_macro2::TokenStream`] for the struct
    fn generate_struct(&self, item: &Struct) -> TokenStream {
        generate_struct(item)
    }
}
