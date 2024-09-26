use convert_case::Case;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::{
    generate::generator::utils::{deconflict_name, into_valid_ident, into_valid_type},
    parse::{
        flatbuffers::{
            item::Item,
            primitives::{ArrayItemType, StructFieldType},
            r#struct::Struct,
        },
        parser::ParsedTypes,
    },
};

use super::utils::named_type_to_rust_name;

pub fn generate_struct(item: &Struct, parsed_types: &ParsedTypes) -> TokenStream {
    let struct_size = item.byte_size;

    println!("Struct [{}] byte size: {struct_size}", item.name);

    let struct_name_str = deconflict_name(item.name, Case::Pascal);
    let struct_name = syn::Type::Path(syn::parse_str(&struct_name_str).unwrap());

    // Generate the debug implementation
    let impls = vec![
        quote! {
            #[repr(transparent)]
            #[derive(Clone, Copy, PartialEq)]
            pub struct #struct_name([u8; #struct_size]);
        },
        debug(&struct_name, &struct_name_str, item),
        follow(&struct_name),
        verifiable(&struct_name),
        main_impl(&struct_name, &item, parsed_types),
    ];

    impls.into_iter().fold(TokenStream::new(), |agg, tokens| {
        quote! {
            #agg
            #tokens
        }
    })
}

fn debug(struct_name: &syn::Type, struct_name_str: &str, item: &Struct) -> TokenStream {
    let field_names = item
        .fields
        .iter()
        .map(|field| into_valid_ident(field.name, Case::Snake));
    let field_names_str = field_names.clone().map(|field| field.to_string());

    quote! {
        impl ::core::fmt::Debug for #struct_name {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
              f.debug_struct(#struct_name_str)
                #(.field(#field_names_str, &self.#field_names()))*
                .finish()
            }
          }
    }
}

fn follow(struct_name: &syn::Type) -> TokenStream {
    quote! {
        impl ::flatbuffers::SimpleToVerifyInSlice for #struct_name {}

        impl<'a> ::flatbuffers::Follow<'a> for #struct_name {
            type Inner = &'a #struct_name;

            #[inline]
            unsafe fn follow(buf: &'a [u8], loc: usize) -> Self::Inner {
                <&'a #struct_name>::follow(buf, loc)
            }
        }

        impl<'a> ::flatbuffers::Follow<'a> for &'a #struct_name {
            type Inner = &'a #struct_name;

            #[inline]
            unsafe fn follow(buf: &'a [u8], loc: usize) -> Self::Inner {
                ::flatbuffers::follow_cast_ref::<#struct_name>(buf, loc)
            }
        }

        impl ::flatbuffers::Push for #struct_name {
            type Output = #struct_name;

            #[inline]
            unsafe fn push(&self, dst: &mut [u8], _written_len: usize) {
                let src = ::core::slice::from_raw_parts(self as *const #struct_name as *const u8, Self::size());
                dst.copy_from_slice(src);
            }
        }
    }
}

fn verifiable(struct_name: &syn::Type) -> TokenStream {
    quote! {
        impl ::flatbuffers::Verifiable for #struct_name {
            #[inline]
            fn run_verifier(v: &mut ::flatbuffers::Verifier, pos: usize) -> ::core::result::Result<(), ::flatbuffers::InvalidFlatbuffer> {
                v.in_buffer::<Self>(pos)
            }
        }
    }
}

fn main_impl(struct_name: &syn::Type, struct_: &Struct, parsed_types: &ParsedTypes) -> TokenStream {
    let field_names = struct_
        .fields
        .iter()
        .map(|field| into_valid_ident(field.name, Case::Snake));

    let field_names_set = field_names.clone().map(|name| format_ident!("set_{name}"));

    let field_names_c = field_names.clone();

    let field_types = struct_.fields.iter().map(|field| match &field.field_type {
        StructFieldType::Array(arr) => {
            let typename = match &arr.item_type {
                ArrayItemType::Named(named) => named_type_to_rust_name(named, &struct_.namespace),
                ArrayItemType::Scalar(scalar) => scalar.to_rust_type().to_owned(),
            };
            syn::parse_str(&format!("&[{}; {}]", typename, arr.length)).unwrap()
        }
        StructFieldType::Named(named) => into_valid_type(&format!(
            "&{}",
            named_type_to_rust_name(named, &struct_.namespace)
        )),
        StructFieldType::Scalar(scalar) => syn::parse_str(scalar.to_rust_type()).unwrap(),
    });

    let field_accessors = struct_
        .fields
        .iter()
        .zip(field_names.clone())
        .zip(field_names_set.clone())
        .map(|((field, name), name_set)| {
            let (get_return_type, get_self, get_body, set_type, set_body) = match &field.field_type {
                StructFieldType::Array(arr) => {
                    let typename = match &arr.item_type {
                        ArrayItemType::Named(named) => named_type_to_rust_name(named, &struct_.namespace),
                        ArrayItemType::Scalar(scalar) => {
                            scalar.to_rust_type().to_owned()
                        }
                    };
                    let offset = field.offset as usize;

                    (
                        syn::parse_str(&format!("::flatbuffers::Array<'a, {}, {}>", typename, arr.length)).unwrap(),
                        into_valid_type("&'a self"),
                        quote! {
                            unsafe { ::flatbuffers::Array::follow(&self.0, #offset) }
                        },
                        syn::parse_str(&format!("&[{}; {}]", typename, arr.length)).unwrap(),
                        quote! {
                            unsafe {
                                ::flatbuffers::emplace_scalar_array(&mut self.0, #offset, value);
                            }
                        }
                    )
                }
                StructFieldType::Named(named) => {
                    let type_name_str = named_type_to_rust_name(named, &struct_.namespace);
                    let type_name = into_valid_type(&type_name_str);
                    let offset = field.offset as usize;
                    let field_item = parsed_types.resolve_named(named).expect("name should resolve");
                    let size = if let Item::Struct(struct_) = &*field_item.borrow() {
                        struct_.byte_size
                    } else {
                        unreachable!("only structs can be used in structs")  
                    };
                    (
                        into_valid_type(&format!("&{type_name_str}")),
                        into_valid_type("&self"),
                        quote! {
                            unsafe { &*(self.0[#offset..].as_ptr() as *const #type_name) }
                        },
                        syn::parse_str(&format!("&{}", type_name_str)).unwrap(),
                        quote! {
                            self.0[#offset..#offset + #size].copy_from_slice(&value.0)
                        }
                    )
                }
                StructFieldType::Scalar(scalar) => {
                    let offset = field.offset as usize;
                    let type_name: syn::Type = syn::parse_str(scalar.to_rust_type()).unwrap();
                    (
                        type_name.clone(),
                        into_valid_type("&self"),
                        quote! {
                            let mut mem = core::mem::MaybeUninit::<<#type_name as EndianScalar>::Scalar>::uninit();
                            
                            ::flatbuffers::EndianScalar::from_little_endian(unsafe {
                                ::core::ptr::copy_nonoverlapping(
                                  self.0[#offset..].as_ptr(),
                                  mem.as_mut_ptr() as *mut u8,
                                  core::mem::size_of::<<#type_name as EndianScalar>::Scalar>(),
                                );
                                mem.assume_init()
                              })
                        },
                        type_name.clone(),
                        quote! {
                            let value_le = value.to_little_endian();
                            unsafe {
                                core::ptr::copy_nonoverlapping(
                                  &value_le as *const _ as *const u8,
                                  self.0[#offset..].as_mut_ptr(),
                                  core::mem::size_of::<<#type_name as EndianScalar>::Scalar>(),
                                );
                              }
                        }
                    )
                }
            };
            
            let getter = quote! {
                pub fn #name(#get_self) -> #get_return_type {
                    #get_body
                }
            };

            let setter = quote! {
                pub fn #name_set(&mut self, value: #set_type) {
                    #set_body
                }
            };

            quote! {
                #[doc = " Safety:"]
                #[doc = " Created from a valid Table for this object"]
                #[doc = " Which contains a valid value in this slot"]
                #getter

                #[doc = " Safety:"]
                #[doc = " Created from a valid Table for this object"]
                #[doc = " Which contains a valid value in this slot"]
                #setter
            }
        });

    let byte_size = struct_.byte_size;

    quote! {
        impl<'a> #struct_name {
            #[allow(clippy::too_many_arguments)]
            pub fn new(
                #(#field_names: #field_types,)*
            ) -> Self {
                let mut s = Self([0; #byte_size]);
                #(s.#field_names_set(#field_names_c);)*
                s
            }

            #(#field_accessors)*
        }
    }
}
