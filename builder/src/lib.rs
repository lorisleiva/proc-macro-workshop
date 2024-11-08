use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder, attributes(builder))]
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
        if is_option(ty) || is_vec(ty) {
            return quote! { #name: #ty, };
        }
        quote! { #name: std::option::Option<#ty>, }
    });

    // Get the None setters for the builder struct.
    let none_fields = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        if is_vec(ty) {
            return quote! { #name: Vec::new(), };
        }
        quote! { #name: None, }
    });

    // Get the methods for setting the fields on the builder struct.
    let field_methods = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        if let Some(inner_ty) = get_option_inner_type(ty) {
            return quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            };
        }
        if is_vec(ty) {
            return quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = #name;
                    self
                }
                // TODO: Add each methods.
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
        if is_option(ty) || is_vec(ty) {
            return quote! { #name: self.#name.clone(), };
        }
        quote! { #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?, }
    });

    // Access the "builder" attributes of the "Builder" derive.
    // eprintln!("{:#?}", input); // Debug print the AST.

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
fn unwrap_inner_type<'a>(ty: &'a syn::Type, ident: &'a str) -> Option<&'a syn::Type> {
    // Get the path of the type. — e.g. `a::b::c::Option`.
    let syn::Type::Path(syn::TypePath { path, .. }) = ty else {
        return None;
    };

    // Only match single-segment paths whose ident is expected.
    let segments = &path.segments;
    if segments.len() != 1 || segments[0].ident != ident {
        return None;
    };

    // Get the generic arguments of the segment.
    let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
        args: generic_args,
        ..
    }) = &segments[0].arguments
    else {
        return None;
    };

    // Only match types with a single generic argument.
    if generic_args.len() != 1 {
        return None;
    };

    // Ensure the generic argument is also a type.
    let syn::GenericArgument::Type(ty) = generic_args.first().unwrap() else {
        return None;
    };

    // Return the inner type.
    Some(ty)
}

// Identify the inner type of an Option.
fn get_option_inner_type(ty: &syn::Type) -> Option<&syn::Type> {
    unwrap_inner_type(ty, "Option")
}

// Check if a type is an Option.
fn is_option(ty: &syn::Type) -> bool {
    let result = get_option_inner_type(&ty);
    result.is_some()
}

// Identify the inner type of an Vec.
fn get_vec_inner_type(ty: &syn::Type) -> Option<&syn::Type> {
    unwrap_inner_type(ty, "Vec")
}

// Check if a type is an Vec.
fn is_vec(ty: &syn::Type) -> bool {
    let result = get_vec_inner_type(&ty);
    result.is_some()
}
