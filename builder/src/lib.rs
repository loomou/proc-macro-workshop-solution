use proc_macro::TokenStream;
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{parse_macro_input, DataStruct, DeriveInput, Error, Field, FieldsNamed, Result, Type};
use syn::Data::Struct;
use syn::Fields::Named;
use syn::punctuated::Punctuated;
use syn::token::Comma;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let derive_input: DeriveInput = parse_macro_input!(input);

    match expand(&derive_input) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into()
    }
}

fn expand(derive_input: &DeriveInput) -> Result<TokenStream2> {
    let ident = &derive_input.ident;
    let builder_name_literal = format_ident!("{}Builder", ident.to_string());

    let derive_input_fields = get_derive_input_fields(derive_input)?;
    let derive_input_fields_name_and_type = get_derive_input_fields_name_and_type(derive_input_fields);
    let (builder_struct_fields, builder_struct_init_fields) =
        generate_builder_struct_fields_and_init_fields(derive_input_fields_name_and_type);

    Ok(
        quote! {
            struct #builder_name_literal {
                #(#builder_struct_fields),*
            }
            impl #ident {
                pub fn builder() -> #builder_name_literal {
                    #builder_name_literal {
                        #(#builder_struct_init_fields),*
                    }
                }
            }
        }
    )
}

fn get_derive_input_fields(
    derive_input: &DeriveInput
) -> Result<&Punctuated<Field, Comma>> {
    match derive_input.data {
        Struct(
            DataStruct {
                fields: Named(
                    FieldsNamed {
                        ref named, ..
                    }
                ), ..
            }
        ) => Ok(named),
        _ => Err(Error::new_spanned(derive_input, "Builder can only be derived for structs"))
    }
}

fn get_derive_input_fields_name_and_type(
    derive_input_fields: &Punctuated<Field, Comma>
) -> Vec<(&Option<Ident>, &Type)> {
    derive_input_fields.iter().map(|field| {
        get_name_and_type(field)
    }).collect()
}

fn get_name_and_type(field: &Field) -> (&Option<Ident>, &Type) {
    (&field.ident, &field.ty)
}

fn generate_builder_struct_fields_and_init_fields(
    field_name_and_type: Vec<(&Option<Ident>, &Type)>
) -> (Vec<TokenStream2>, Vec<TokenStream2>) {
    let mut builder_struct_fields = Vec::new();
    let mut builder_struct_init_fields = Vec::new();

    for (field_name, field_type) in field_name_and_type {
        builder_struct_fields.push(
            quote! { #field_name: Option<#field_type> },
        );
        builder_struct_init_fields.push(
            quote! {  #field_name: None },
        );
    }

    (builder_struct_fields, builder_struct_init_fields)
}