use convert_case::Case;
use proc_macro2::{Span, TokenStream};
use quote::quote;

use crate::{generate::generator::utils::deconflict_name, parse::{flatbuffers::r#enum::Enum, parser::ParsedTypes}};

pub fn generate_enum(item: &Enum, _parsed_types: &ParsedTypes) -> TokenStream {
    let name_str = deconflict_name(item.name, None);
    let name = syn::parse_str::<syn::Type>(&name_str).expect("Invalid enum name");
    let base_type = item.data_type();

    let is_bitflags = item.is_bitflags();
    
    let (enum_def, base_value) = if is_bitflags {
        (enum_bitflags(item), quote! { self.bits() })
    } else {
        (enum_normal(item), quote! { self.0 })
    };

    let follow_impl = follow(&name, &base_type, is_bitflags);
    let endian_scalar_impl = endian_scalar(&name, &base_type, &base_value, is_bitflags);

    quote! {
        #enum_def

        #follow_impl

        #endian_scalar_impl

        impl ::flatbuffers::Push for #name {
            type Output = #name;

            #[inline]
            unsafe fn push(&self, dst: &mut [u8], _written_len: usize) {
                ::flatbuffers::emplace_scalar::<#base_type>(dst, #base_value);
            }
        }

        impl ::flatbuffers::Verifiable for #name {
            #[inline]
            fn run_verifier(v: &mut ::flatbuffers::Verifier, pos: usize) -> ::core::result::Result<(), ::flatbuffers::InvalidFlatBuffer> {
                use ::flatbuffers::Verifiable;
                <#base_type>::run_verifier(v, pos)
            }
        }

        impl ::flatbuffers::SimpleToVerifyInSlice for #name {}

        // TODO: Handle unions? Need to check if unions are handled the same way as enums
    }
}

/// Generates the bitflags-specific version of the enum
fn enum_bitflags(item: &Enum) -> TokenStream {
    let name_str = deconflict_name(item.name, None);
    let name = syn::parse_str::<syn::Type>(&name_str).expect("Invalid enum name");
    let mod_name = syn::Ident::new(&format!("bitflags_{}_{}", item.namespace.raw.replace(".", "_"), name_str), Span::call_site());
    let base_type = item.data_type();

    let mut idx = 0;
    let variants = item.variants.iter().map(|var| {
        let var_idx = var.idx.unwrap_or(idx);
        // Update the index
        idx = var_idx + 1;

        let var_value = 1i128 << var_idx;
        let var_ident = syn::Ident::new(var.name, Span::call_site());

        quote! {
            const #var_ident = #var_value;
        }
    });

    quote! {
        #[allow(non_upper_case_globals)]
        mod #mod_name {
            ::flatbuffers::bitflags::bitflags! {
                #[derive(Default)]
                pub enum #name: #base_type {
                    #(#variants)*
                }
            }
        }
        pub use #mod_name::#name;
    }
}

/// Generates the normal version of the enum
fn enum_normal(item: &Enum) -> TokenStream {
    let name_str = deconflict_name(item.name, Some(Case::Pascal));
    let name = syn::parse_str::<syn::Type>(&name_str).expect("Invalid enum name");
    let base_type = item.data_type();

    let mut idx = 0;
    let mut min_idx = i128::MAX;
    let mut max_idx = i128::MIN;

    let variants = item.variants.iter().map(|var| {
        let var_idx = var.idx.unwrap_or(idx);

        // Update the min/max
        if var_idx < min_idx {
            min_idx = var_idx;
        }
        if var_idx > max_idx {
            max_idx = var_idx;
        }

        // Update the index
        idx = var_idx + 1;

        let var_value = var_idx;
        let var_ident = syn::Ident::new(&deconflict_name(var.name, None), Span::call_site());

        quote! {
            const #var_ident: Self = Self(#var_value);
        }
    });

    let var_idents = item.variants.iter().map(|var| {
        syn::Ident::new(&deconflict_name(var.name, None), Span::call_site())
    });

    let var_idents_str = item.variants.iter().map(|var| {
        deconflict_name(var.name, None)
    });

    /// Testing
    let values = var_idents.clone();

    quote! {
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
        #[repr(transparent)]
        pub struct #name(pub #base_type);

        impl #name {
            #(#variants)*

            pub const ENUM_MIN: #base_type = #min_idx;
            pub const ENUM_MAX: #base_type = #max_idx;
            pub const ENUM_VALUES: &'static [Self] = &[
                #(Self::#values),*
            ];

            #[doc = "Returns the variant's name or None if unknown"]
            pub fn variant_name(self) -> Option<&'static str> {
                match self {
                    #(Self::#var_idents => Some(#var_idents_str),)*
                    _ => None,
                }
            }
        }

        impl ::core::fmt::Debug for #name {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                if let Some(name) = self.variant_name() {
                    f.write_str(name)
                } else {
                    f.write_fmt(format_args!("<UNKNOWN {:?}>", self.0))
                }
            }
        }
    }
}

fn follow(name: &syn::Type, base_type: &syn::Type, is_bitflags: bool) -> TokenStream {
    let return_value = if is_bitflags {
        quote! {
            #[doc = "Safety:"]
            #[doc = "This is safe because we know bitflags is implemented with a repr transparent uint of the correct size."]
            #[doc = "from_bits_unchecked will be replaced by an equivalent but safe from_bits_retain in bitflags 2.0"]
            #[doc = "https://github.com/bitflags/bitflags/issues/262"]
            Self::from_bits_unchecked(b)
        }
    } else {
        quote! {
            Self(b)
        }
    };
    
    quote! {
        // Since there's no way to generate normal (non-documentation) comments, there are comments
        // that need to be included in the code to match the C++ flatbuffers compiler, but
        // doc comments used outside of an item will generate a warning. This suppresses the warning.
        #[allow(unused_doc_comments)]
        impl<'a> ::flatbuffers::Follow<'a> for #name {
            type Inner = Self;

            #[inline]
            unsafe fn follow(buf: &'a [u8], loc: usize) -> Self::Inner {
                let b = ::flatbuffers::read_scalar_at::<#base_type>(buf, loc);
                #return_value
            }
        }
    }
}

fn endian_scalar(name: &syn::Type, base_type: &syn::Type, base_value: &TokenStream, is_bitflags: bool) -> TokenStream {
    let return_value = if is_bitflags {
        quote! {
            #[doc = "Safety:"]
            #[doc = "This is safe because we know bitflags is implemented with a repr transparent uint of the correct size."]
            #[doc = "from_bits_unchecked will be replaced by an equivalent but safe from_bits_retain in bitflags 2.0"]
            #[doc = "https://github.com/bitflags/bitflags/issues/262"]
            unsafe { Self::from_bits_unchecked(b) }
        }
    } else {
        quote! {
            Self(b)
        }
    };

    quote! {
        impl ::flatbuffers::EndianScalar for #name {
            type Scalar = #base_type;

            #[inline]
            fn to_little_endian(self) -> #base_type {
                #base_value.to_le()
            }

            #[inline]
            #[allow(clippy::wrong_self_convention)]
            fn from_little_endian(v: #base_type) -> Self {
                let b = <#base_type>::from_le(v);
                #return_value
            }
        }
    }
}