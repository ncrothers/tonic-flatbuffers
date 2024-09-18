use convert_case::Case;
use proc_macro2::TokenStream;
use quote::quote;

use crate::{generate::generator::utils::into_valid_ident, parse::{flatbuffers::r#struct::Struct, utils::ByteSize}};

pub fn generate_struct(item: &Struct) -> TokenStream {
    let struct_size = item.size();
    let item_ident = into_valid_ident(item.name, Case::Pascal);
    // Generate the debug implementation
    let debug_impl = debug(&item_ident, item);
    
    debug_impl
}

fn debug(ident: &syn::Ident, item: &Struct) -> TokenStream {
    let field_names = item.fields.iter().map(|field| into_valid_ident(field.name, Case::Snake));
    let field_names_str = field_names.clone().map(|field| field.to_string());
    let ident_str = ident.to_string();

    quote! {
        impl ::core::fmt::Debug for #ident {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
              f.debug_struct(#ident_str)
                #(.field(#field_names_str, &self.#field_names()))*
                .finish()
            }
          }
    }
}