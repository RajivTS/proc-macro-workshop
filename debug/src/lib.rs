use quote::{quote, quote_spanned};
use syn::{parse_macro_input, parse_quote, spanned::Spanned, DeriveInput, GenericParam, Generics};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input = parse_macro_input!(input as DeriveInput);
    let struct_name = &input.ident;
    add_debug_trait_bound(&mut input.generics);
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
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

fn add_debug_trait_bound(generics: &mut Generics) {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(::std::fmt::Debug))
        }
    }
    // generics
}
