use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    // Derive an AST from the input token stream.
    let input = parse_macro_input!(input as DeriveInput);
    // eprintln!("{:#?}", input); // Debug print the AST.

    // Extract the name of the struct and its derived builder name.
    let name = &input.ident;
    let builder_name = syn::Ident::new(&format!("{}Builder", name), name.span());

    // Extract the fields of the struct. Fail if the derive is not on a struct.
    let fields = match input.data {
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
            ..
        }) => named,
        _ => {
            // Return a compile error if the derive is not on a struct.
            let error = syn::Error::new_spanned(input, "expected a struct").to_compile_error();
            return quote! { #error }.into();
        }
    };

    // Get the fields, wrapped in Options, for the builder struct.
    let optional_fields = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        if is_option(ty) {
            return quote! { #name: #ty, };
        }
        quote! { #name: std::option::Option<#ty>, }
    });

    // Get the None setters for the builder struct.
    let none_fields = fields.iter().map(|field| {
        let name = &field.ident;
        quote! { #name: None, }
    });

    // Get the methods for setting the fields on the builder struct.
    let field_methods = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        let inner_ty_result = get_option_inner_type(ty);
        if inner_ty_result.is_some() {
            let inner_ty = inner_ty_result.unwrap();
            return quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            };
        }
        quote! {
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });

    // Get the fields for the final built struct.
    let built_fields = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        if is_option(ty) {
            return quote! { #name: self.#name.clone(), };
        }
        quote! { #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?, }
    });

    // Render the macro output.
    quote! {
        pub struct #builder_name {
            #(#optional_fields)*
        }

        impl #builder_name {
            #(#field_methods)*

            pub fn build(&self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#built_fields)*
                })
            }
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#none_fields)*
                }
            }
        }
    }
    .into()
}

// Identify the inner type of an Option.
fn get_option_inner_type(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(syn::TypePath { path, .. }) = ty {
        let segments = &path.segments;
        if segments.len() == 1 && segments[0].ident == "Option" {
            if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                args,
                ..
            }) = &segments[0].arguments
            {
                if args.len() == 1 {
                    if let syn::GenericArgument::Type(ty) = args.first().unwrap() {
                        return Some(ty);
                    }
                }
            }
        }
    }
    None
}

// Check if a type is an Option.
fn is_option(ty: &syn::Type) -> bool {
    let result = get_option_inner_type(&ty);
    result.is_some()
}
