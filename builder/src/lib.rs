use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};
use syn::{
    parse_macro_input, spanned::Spanned, AngleBracketedGenericArguments, Data, DeriveInput, Field,
    Fields, FieldsNamed, GenericArgument, Ident, Path, PathArguments, PathSegment, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder_name = format!("{}Builder", name.to_string());
    let builder_name = Ident::new(builder_name.as_str(), Span::call_site());
    let builder_props = builder_props(&input.data);
    let builder_init = builder_init(&input.data);
    let builder_prop_setters = builder_prop_setters(&input.data);
    let struct_init = struct_init(&input.data);
    let expanded = quote! {
        pub struct #builder_name {
            #builder_props
        }

        impl #builder_name {
            #builder_prop_setters

            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
                    #struct_init
                })
            }
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #builder_init
                }
            }
        }
    };
    proc_macro::TokenStream::from(expanded)
}

fn builder_props(data: &Data) -> TokenStream {
    parse_struct(data, |fields| {
        let props = fields.named.iter().map(|f| {
            let name = &f.ident;
            let ty = &f.ty;
            quote_spanned! { f.span() => #name : Option<#ty> }
        });
        Ok(quote! {
            #(#props,)*
        })
    })
}

fn builder_init(data: &Data) -> TokenStream {
    parse_struct(data, |fields| {
        let props = fields
            .named
            .iter()
            .map(|f| {
                let name = &f.ident;
                if is_option(f) {
                    Ok(quote_spanned! { f.span() => #name: Some(None)})
                } else if let Some(_) = repeated_field_name(f)? {
                    Ok(quote_spanned! { f. span() => #name: Some(Vec::new()) })
                } else {
                    Ok(quote_spanned! { f.span() => #name : None })
                }
            })
            .collect::<syn::Result<Vec<_>>>()?;
        Ok(quote! {
            #(#props,)*
        })
    })
}

fn builder_prop_setters(data: &Data) -> TokenStream {
    parse_struct(data, |fields| {
        let prop_setters = fields
            .named
            .iter()
            .map(|f| {
                let name = f.ident.as_ref().ok_or_else(|| {
                    syn::Error::new_spanned(f.ident.as_ref(), "Struct field name absent")
                })?;
                let ty = &f.ty;
                if is_option(f) {
                    let ty = type_within(f);
                    Ok(quote_spanned! { f.span() =>
                        fn #name(&mut self, #name: #ty) -> &mut Self {
                            self.#name = Some(Some(#name));
                            self
                        }
                    })
                } else if let Some(repeated_field) = repeated_field_name(f)? {
                    if repeated_field.to_string() == name.to_string() {
                        Ok(quote_spanned! { f.span() =>
                            fn #name(&mut self, #name: #ty) -> &mut Self {
                                self.#name = Some(#name);
                                self
                            }
                        })
                    } else {
                        let ty = type_within(f);
                        Ok(quote_spanned! { f.span() =>
                            fn #repeated_field(&mut self, #repeated_field: #ty) -> &mut Self {
                                self.#name = self.#name.take().or(Some(Vec::new())).map(|mut lst| {
                                    lst.push(#repeated_field);
                                    lst
                                });
                                self
                            }
                        })
                    }
                } else {
                    Ok(quote_spanned! { f.span() =>
                        fn #name(&mut self, #name: #ty) -> &mut Self {
                            self.#name = Some(#name);
                            self
                        }
                    })
                }
            })
            .collect::<syn::Result<Vec<_>>>()?;
        Ok(quote! {
            #(#prop_setters)*
        })
    })
}

fn struct_init(data: &Data) -> TokenStream {
    parse_struct(data, |fields| {
        let prop_setters = fields.named.iter().map(|f| {
            let name = &f.ident;
            quote_spanned! { f.span() => #name: self.#name.take().ok_or("Missing required property")? }
        });
        Ok(quote! {
            #(#prop_setters,)*
        })
    })
}

fn parse_struct<F>(data: &Data, quote_generator: F) -> TokenStream
where
    F: FnOnce(&FieldsNamed) -> syn::Result<TokenStream>,
{
    let output = match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => quote_generator(fields),
            _ => Err(syn::Error::new_spanned(
                data.struct_token,
                "Builder macro is supported only for named structs",
            )),
        },
        Data::Enum(ref e) => Err(syn::Error::new_spanned(
            e.enum_token,
            "Builder macro is not supported for Enums",
        )),
        Data::Union(ref e) => Err(syn::Error::new_spanned(
            e.union_token,
            "Builder macro is not supported for Unions",
        )),
    };
    output.unwrap_or_else(syn::Error::into_compile_error)
}

fn is_option(f: &Field) -> bool {
    match &f.ty {
        Type::Path(TypePath {
            qself: None,
            path: Path { segments, .. },
        }) => {
            if segments.len() != 1 {
                return false;
            }
            segments
                .first()
                .map(|segment| segment.ident.to_string() == "Option")
                .unwrap_or(false)
        }
        _ => false,
    }
}

fn type_within(f: &Field) -> Option<&Type> {
    if let Type::Path(TypePath {
        qself: None,
        path: Path { segments, .. },
    }) = &f.ty
    {
        if let Some(PathSegment {
            arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }),
            ..
        }) = segments.first()
        {
            if let Some(GenericArgument::Type(ty)) = args.first() {
                return Some(ty);
            }
        }
    };
    None
}

fn repeated_field_name(f: &Field) -> syn::Result<Option<Ident>> {
    for attr in &f.attrs {
        if attr.path.is_ident("builder") {
            let meta = attr.parse_meta()?;
            let meta_list = match meta {
                syn::Meta::List(list) => list,
                _ => {
                    return Err(syn::Error::new_spanned(
                        meta,
                        "Expected a list style attribute",
                    ))
                }
            };
            let nested = match meta_list.nested.len() {
                1 => &meta_list.nested[0],
                _ => {
                    return Err(syn::Error::new_spanned(
                        meta_list.nested,
                        "Exactly one argument is supported for the builder attribute",
                    ))
                }
            };
            let name_value = match nested {
                syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) => nv,
                _ => {
                    return Err(syn::Error::new_spanned(
                        nested,
                        "Expected name-value pair argument e.g. `each = \"repeated_field\"`",
                    ))
                }
            };
            if !name_value.path.is_ident("each") {
                return Err(syn::Error::new_spanned(
                    &name_value.path,
                    "Expected attribute argument to be named `each`",
                ));
            }
            return match &name_value.lit {
                syn::Lit::Str(s) => {
                    syn::parse_str(&s.value()).map_err(|e| syn::Error::new_spanned(s, e))
                }
                lit => Err(syn::Error::new_spanned(
                    lit,
                    "Expected string literal for attribute value of `each`",
                )),
            };
        }
    }
    Ok(None)
}
