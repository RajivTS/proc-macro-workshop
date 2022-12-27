use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};
use syn::{
    parse_macro_input, spanned::Spanned, AngleBracketedGenericArguments, Data, DeriveInput, Field,
    Fields, FieldsNamed, Ident, Path, PathArguments, PathSegment, Type, TypePath, GenericArgument,
};

#[proc_macro_derive(Builder)]
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
        quote! {
            #(#props,)*
        }
    })
}

fn builder_init(data: &Data) -> TokenStream {
    parse_struct(data, |fields| {
        let props = fields.named.iter().map(|f| {
            let name = &f.ident;
            if is_option(f) {
                quote_spanned! { f.span() => #name: Some(None)}
            } else {
                quote_spanned! { f.span() => #name : None }
            }
        });
        quote! {
            #(#props,)*
        }
    })
}

fn builder_prop_setters(data: &Data) -> TokenStream {
    parse_struct(data, |fields| {
        let prop_setters = fields.named.iter().map(|f| {
            let name = &f.ident;
            let ty = &f.ty;
            if is_option(f) {
                let ty = type_within_option(f);
                quote_spanned! { f.span() =>
                    fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = Some(Some(#name));
                        self
                    }
                }
            } else {
                quote_spanned! { f.span() =>
                    fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
                }
            }
        });
        quote! {
            #(#prop_setters)*
        }
    })
}

fn struct_init(data: &Data) -> TokenStream {
    parse_struct(data, |fields| {
        let prop_setters = fields.named.iter().map(|f| {
            let name = &f.ident;
            quote_spanned! { f.span() => #name: self.#name.take().ok_or("Missing required property")? }
        });
        quote! {
            #(#prop_setters,)*
        }
    })
}

fn parse_struct<F>(data: &Data, quote_generator: F) -> TokenStream
where
    F: FnOnce(&FieldsNamed) -> TokenStream,
{
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => quote_generator(fields),
            _ => unimplemented!("Builder macro is supported only for named structs"),
        },
        Data::Enum(_) | Data::Union(_) => {
            unimplemented!("Builder macro is not supported for Unions or Enums")
        }
    }
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

fn type_within_option(f: &Field) -> Option<&Type> {
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
