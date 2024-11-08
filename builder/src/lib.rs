use proc_macro::TokenStream;
use proc_macro2::TokenTree;
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

        // For options, wrap the inner type in Some.
        if let Some(inner_ty) = get_option_inner_type(ty) {
            return quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            };
        }

        // For vecs, add a method to override the whole vec and a method to push values to it.
        if let Some(inner_ty) = get_vec_inner_type(ty) {
            let all_function = get_all_function(field);
            let each_function_result = get_each_function(field, inner_ty);
            return match each_function_result {
                Ok(Some((true, each_function))) => each_function,
                Ok(Some((false, each_function))) => quote! {
                    #all_function
                    #each_function
                },
                Ok(None) => all_function,
                Err(e) => e,
            };
        }

        // For other types, set the field by wrapping it in Some.
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
    if !is_single_path(path, ident) {
        return None;
    };
    let segment = &path.segments[0];

    // Get the generic arguments of the segment.
    let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
        args: generic_args,
        ..
    }) = &segment.arguments
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

fn get_each_function(
    field: &syn::Field,
    inner_ty: &syn::Type,
) -> Result<Option<(bool, proc_macro2::TokenStream)>, proc_macro2::TokenStream> {
    let name = &field.ident.clone().unwrap();

    // Get the builder attribute.
    let Some(attr) = get_builder_attribute(field) else {
        return Ok(None);
    };

    let err = syn::Error::new_spanned(&attr.meta, "expected `builder(each = \"...\")`")
        .to_compile_error();

    // Get the token tree from the attribute.
    let syn::Meta::List(syn::MetaList { tokens, .. }) = &attr.meta else {
        return Err(err);
    };

    // Check all tokens in the token tree.
    let mut tokens = tokens.clone().into_iter();
    match tokens.next() {
        Some(TokenTree::Ident(ident)) if ident == "each" => {}
        _ => return Err(err),
    }
    match tokens.next() {
        Some(TokenTree::Punct(punct)) if punct.as_char() == '=' => {}
        _ => return Err(err),
    }

    // Get the literal string from the third token.
    let Some(TokenTree::Literal(literal)) = tokens.next() else {
        return Err(err);
    };
    let syn::Lit::Str(string_literal) = syn::Lit::new(literal) else {
        return Err(err);
    };

    // Get the indent from the string literal.
    let each_name = syn::Ident::new(&string_literal.value(), string_literal.span());

    Ok(Some((
        name == &each_name,
        quote! {
            pub fn #each_name(&mut self, #each_name: #inner_ty) -> &mut Self {
                self.#name.push(#each_name);
                self
            }
        },
    )))
}

fn get_all_function(field: &syn::Field) -> proc_macro2::TokenStream {
    let name = &field.ident;
    let ty = &field.ty;

    quote! {
        pub fn #name(&mut self, #name: #ty) -> &mut Self {
            self.#name = #name;
            self
        }
    }
}

fn get_builder_attribute(f: &syn::Field) -> Option<&syn::Attribute> {
    for attr in &f.attrs {
        if let syn::Meta::List(syn::MetaList { path, .. }) = &attr.meta {
            if is_single_path(path, "builder") {
                return Some(attr);
            }
        };
    }
    None
}

fn is_single_path(path: &syn::Path, ident: &str) -> bool {
    path.segments.len() == 1 && path.segments[0].ident == ident
}
