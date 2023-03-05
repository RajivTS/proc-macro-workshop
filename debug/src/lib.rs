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
    let custom_debug_bound = match bound_attr(&input.attrs) {
        Ok(bounds) => bounds,
        Err(e) => return e.to_compile_error().into(),
    };
    let mut non_phatom_generic_idents = HashSet::new();
    let mut associated_types = HashSet::new();
    let mut custom_debug_bounds = HashSet::new();
    for field in fields.iter() {
        let bounds = match bound_attr(&field.attrs) {
            Ok(bounds) => bounds,
            Err(e) => return e.to_compile_error().into(),
        };
        if let Some(bounds) = bounds {
            custom_debug_bounds.insert(bounds);
        } else {
            if let Some(generic_type) = non_phantom_generic_field_type(&field.ty) {
                non_phatom_generic_idents.insert(generic_type);
            }
            if let Some(associated_type) = associated_type(&field.ty) {
                associated_types.insert(associated_type);
            }
        }
    }
    let where_clause = if let Some(bound) = custom_debug_bound {
        custom_debug_bounds.insert(bound.to_string());
        debug_where_clause(&HashSet::new(), &custom_debug_bounds)
    } else {
        add_debug_trait_bound(&mut input.generics, &non_phatom_generic_idents);
        debug_where_clause(&associated_types, &custom_debug_bounds)
    };
    let (impl_generics, ty_generics, _) = input.generics.split_for_impl();
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
    };
    proc_macro::TokenStream::from(expanded)
}

fn bound_attr(attrs: &Vec<syn::Attribute>) -> syn::Result<Option<String>> {
    for attr in attrs {
        if attr.path.is_ident("debug") {
            let meta_list = match attr.parse_meta()? {
                syn::Meta::List(meta_list) => meta_list,
                _ => return Ok(None),
            };
            if meta_list
                .path
                .segments
                .first()
                .map(|segment| segment.ident.to_string() != "debug")
                .unwrap_or(false)
            {
                return Err(syn::Error::new_spanned(
                    meta_list,
                    "expected `#[debug(bound = \"...\")]`",
                ));
            }
            for nested_meta in meta_list.nested {
                if let syn::NestedMeta::Meta(meta) = nested_meta {
                    let name_value = match meta {
                        syn::Meta::NameValue(nv) => nv,
                        _ => {
                            return Err(syn::Error::new_spanned(
                                meta,
                                "expected `#[debug(bound = \"...\")]`",
                            ))
                        }
                    };
                    if !name_value.path.is_ident("bound") {
                        return Err(syn::Error::new_spanned(
                            name_value,
                            "expected `#[debug(bound = \"...\")]`",
                        ));
                    }
                    return match &name_value.lit {
                        syn::Lit::Str(s) => Ok(Some(s.value())),
                        lit => Err(syn::Error::new_spanned(
                            lit,
                            "expected `#[debug(bound = \"...\")]`",
                        )),
                    };
                }
            }
        }
    }
    Ok(None)
}

fn debug_attr(attrs: &Vec<syn::Attribute>) -> syn::Result<Option<String>> {
    for attr in attrs {
        if attr.path.is_ident("debug") {
            let meta = attr.parse_meta()?;
            let name_value = match meta {
                syn::Meta::NameValue(nv) => nv,
                _ => return Ok(None),
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
    custom_bounds: &HashSet<String>,
) -> Option<syn::WhereClause> {
    let mut where_clauses = vec![];
    for (core_ty, associated_ty) in associated_types {
        where_clauses.push(format!(
            "{}::{}: ::std::fmt::Debug",
            core_ty.to_string(),
            associated_ty.to_string()
        ));
    }
    for custom_bound in custom_bounds {
        where_clauses.push(custom_bound.to_string());
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
