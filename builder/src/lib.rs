use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned};
use syn::{parse_macro_input, spanned::Spanned, Data, DeriveInput, Fields, Ident, FieldsNamed};

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder_name = format!("{}Builder", name.to_string());
    let builder_name = Ident::new(builder_name.as_str(), Span::call_site());
    let struct_props = struct_props(&input.data);
    let builder_init = builder_init(&input.data);
    let struct_prop_setters = struct_prop_setters(&input.data);
    let expanded = quote! {
        pub struct #builder_name {
            #struct_props
        }

        impl #builder_name {
            #struct_prop_setters

            pub fn build(&mut self) -> Result<#name, Box<dyn Error>> {
                #name {

                }
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

fn struct_props(data: &Data) -> TokenStream {
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
            quote_spanned! { f.span() => #name : None }
        });
        quote! {
            #(#props,)*
        }
    })
}

fn struct_prop_setters(data: &Data) -> TokenStream {
    parse_struct(data, |fields| {
        let prop_setters = fields.named.iter().map(|f| {
            let name = &f.ident;
            let ty = &f.ty;
            quote_spanned! { f.span() =>
                fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        });
        quote! {
            #(#prop_setters)*
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
