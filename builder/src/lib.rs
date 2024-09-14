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
    let builder_name_literal = format_ident!("{}Builder", ident);

    let derive_input_fields = get_derive_input_fields(derive_input)?;
    let derive_input_fields_name_and_type = get_derive_input_fields_name_and_type(derive_input_fields);
    let (builder_struct_fields, builder_struct_init_fields) =
        generate_builder_struct_fields_and_init_fields(&derive_input_fields_name_and_type);
    let builder_setter_methods =
        generate_builder_setter_methods(&derive_input_fields_name_and_type);
    let builder_build_method =
        generate_builder_build_method(&derive_input_fields_name_and_type, ident);

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
            impl #builder_name_literal {
                #(#builder_setter_methods)*
                #builder_build_method
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
    field_name_and_type: &Vec<(&Option<Ident>, &Type)>
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

fn generate_builder_setter_methods(
    field_name_and_type: &Vec<(&Option<Ident>, &Type)>
) -> Vec<TokenStream2> {
    field_name_and_type.iter().map(|name_and_type| {
        let (name, ty) = name_and_type;
        quote! {
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    }).collect()
}

fn generate_builder_build_method(
    field_name_and_type: &Vec<(&Option<Ident>, &Type)>,
    ident: &Ident,
) -> TokenStream2 {
    let mut check_fields = Vec::new();
    let mut build_fields = Vec::new();

    for (name, _) in field_name_and_type {
        check_fields.push(
            quote! {
                if self.#name.is_none() {
                    let err_msg = format!("{} field missing", stringify!(#name));
                    return Err(err_msg.into());
                }
            }
        );
        build_fields.push(
            quote! {
                #name: self.#name.clone().unwrap(),
            }
        );
    }

    quote! {
        pub fn build(&mut self) -> std::result::Result<#ident, std::boxed::Box<dyn std::error::Error>> {
            #(#check_fields)*

            Ok(#ident {
                #(#build_fields)*
            })
        }
    }
}