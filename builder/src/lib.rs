use proc_macro2::{TokenStream, Span};
use quote::{quote, quote_spanned};
use syn::{
    parse_macro_input, Data, DeriveInput, Fields, spanned::Spanned, Ident,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder_name = format!("{}Builder", name.to_string());
    let builder_name = Ident::new(builder_name.as_str(), Span::call_site());
    let struct_props = struct_props(&input.data);
    let struct_init = struct_empty_init(&input.data);
    let struct_prop_setters = struct_prop_setters(&input.data);
    let expanded = quote! {
        pub struct #builder_name {
            #struct_props
        }

        impl #builder_name {
            #struct_prop_setters
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #struct_init
                }
            }
        }
    };
    proc_macro::TokenStream::from(expanded)
}

fn struct_props(data: &Data) -> TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let props = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    let ty = &f.ty;
                    quote_spanned! { f.span() => #name : Option<#ty> }
                });
                quote! {
                    #(#props,)*
                }
            }
            _ => unimplemented!("Builder macro is supported only for named structs"),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!("Builder macro is not supported for Unions or Enums"),
    }
}

fn struct_empty_init(data: &Data) -> TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let props = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    quote_spanned! { f.span() => #name : None }
                });
                quote! {
                    #(#props,)*
                }
            }
            _ => unimplemented!("Builder macro is supported only for named structs"),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!("Builder macro is not supported for Unions or Enums"),
    }    
}

fn struct_prop_setters(data: &Data) -> TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
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
            },
            _ => unimplemented!("Builder macro is supported only for named structs"),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!("Builder macro is not supported for Unions or Enums"),
    }
}
