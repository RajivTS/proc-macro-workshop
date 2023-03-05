use std::collections::HashSet;

use quote::{quote, quote_spanned};
use syn::{parse_macro_input, parse_quote, spanned::Spanned, DeriveInput, GenericParam, Generics};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input = parse_macro_input!(input as DeriveInput);
    let struct_name = &input.ident;
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
        ..
    }) = input.data
    {
        named
    } else {
        return syn::Error::new_spanned(input, "expected named struct with named fields")
            .to_compile_error()
            .into();
    };
    let mut non_phatom_generic_idents = HashSet::new();
    let mut associated_types = HashSet::new();
    for field in fields.iter() {
        if let Some(generic_type) = non_phantom_generic_field_type(&field.ty) {
            non_phatom_generic_idents.insert(generic_type);
        }
        if let Some(associated_type) = associated_type(&field.ty) {
            associated_types.insert(associated_type);
        }
    }
    add_debug_trait_bound(&mut input.generics, &non_phatom_generic_idents);
    let where_clause = debug_where_clause(&associated_types);
    let (impl_generics, ty_generics, _) = input.generics.split_for_impl();
    // let print_fields = fields.iter().map(|f| {
    //     eprintln!("Field: {:#?}\n\n", f.ty);
    //     quote!(())
    // });
    let debug_fields = fields.iter().map(|f| {
        let custom_debug = debug_attr(&f.attrs)?;
        match custom_debug {
            Some(debug_val) => {
                let name = &f.ident;
                Ok(quote_spanned!(f.span() => .field(stringify!(#name), &format_args!(#debug_val, &self.#name))))                
            },
            None => {
                let name = &f.ident;
                Ok(quote_spanned!(f.span() => .field(stringify!(#name), &self.#name)))
            }
        }
    }).collect::<syn::Result<Vec<_>>>();
    let debug_fields = match debug_fields {
        Err(e) => return e.to_compile_error().into(),
        Ok(v) => v,
    };
    let expanded = quote! {
        impl #impl_generics ::std::fmt::Debug for #struct_name #ty_generics #where_clause {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                fmt.debug_struct(stringify!(#struct_name))
                #(#debug_fields)*
                .finish()
            }
        }
        // #(#print_fields)*
    };
    proc_macro::TokenStream::from(expanded)
}

fn debug_attr(attrs: &Vec<syn::Attribute>) -> syn::Result<Option<String>> {
    for attr in attrs {
        if attr.path.is_ident("debug") {
            let meta = attr.parse_meta()?;
            let name_value = match meta {
                syn::Meta::NameValue(nv) => nv,
                _ => {
                    return Err(syn::Error::new_spanned(
                        meta,
                        "expected `#[debug = \"...\"]`",
                    ))
                }
            };
            if !name_value.path.is_ident("debug") {
                return Err(syn::Error::new_spanned(
                    name_value,
                    "expected `#[debug = \"...\"]`",
                ));
            }
            return match &name_value.lit {
                syn::Lit::Str(s) => Ok(Some(s.value())),
                lit => Err(syn::Error::new_spanned(
                    lit,
                    "expected `#[debug = \"...\"]`",
                )),
            };
        }
    }
    Ok(None)
}

fn add_debug_trait_bound(
    generics: &mut Generics,
    non_phatom_generic_idents: &HashSet<proc_macro2::Ident>,
) {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            if non_phatom_generic_idents.contains(&type_param.ident) {
                type_param.bounds.push(parse_quote!(::std::fmt::Debug))
            }
        }
    }
}

fn debug_where_clause(
    associated_types: &HashSet<(proc_macro2::Ident, proc_macro2::Ident)>,
) -> Option<syn::WhereClause> {
    let mut where_clauses = vec![];
    for (core_ty, associated_ty) in associated_types {
        where_clauses.push(format!(
            "{}::{}: Debug",
            core_ty.to_string(),
            associated_ty.to_string()
        ));
    }
    if where_clauses.len() == 0 {
        None
    } else {
        syn::parse_str(&format!("where {}", where_clauses.join(", "))).ok()
    }
}

fn non_phantom_generic_field_type(ty: &syn::Type) -> Option<proc_macro2::Ident> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { segments, .. },
        ..
    }) = ty
    {
        if let Some(syn::PathSegment {
            arguments: syn::PathArguments::None,
            ident,
        }) = segments.first()
        {
            if segments.len() == 1 {
                return Some(ident.clone());
            }
        } else if let Some(syn::PathSegment {
            ident,
            arguments:
                syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. }),
        }) = segments.first()
        {
            if ident == "PhantomData" {
                return None;
            }
            if let Some(syn::GenericArgument::Type(typ)) = args.first() {
                return non_phantom_generic_field_type(typ);
            }
        }
    }
    None
}

fn associated_type(ty: &syn::Type) -> Option<(proc_macro2::Ident, proc_macro2::Ident)> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { segments, .. },
        ..
    }) = ty
    {
        let mut segments_iter = segments.iter();
        if let Some(syn::PathSegment {
            arguments: syn::PathArguments::None,
            ident: core_type,
        }) = segments_iter.next()
        {
            if let Some(syn::PathSegment {
                arguments: syn::PathArguments::None,
                ident: associated_type,
            }) = segments_iter.next()
            {
                return Some((core_type.clone(), associated_type.clone()));
            }
        } else if let Some(syn::PathSegment {
            arguments:
                syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. }),
            ..
        }) = segments.first()
        {
            if let Some(syn::GenericArgument::Type(typ)) = args.first() {
                return associated_type(typ);
            }
        }
    }
    None
}
