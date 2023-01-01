use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::{parse_macro_input, DeriveInput, spanned::Spanned};

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = &input.ident;
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
        ..
    }) = input.data
    {
        named
    } else {
        unimplemented!()
    };
    let debug_fields = fields.iter().map(|f| {
        let name = &f.ident;
        quote_spanned!(f.span() => .field(stringify!(#name), &self.#name))
    });
    let expanded = quote! {
        impl ::std::fmt::Debug for #struct_name {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                fmt.debug_struct(stringify!(#struct_name))
                #(#debug_fields)*
                .finish()
            }
        }
    };
    proc_macro::TokenStream::from(expanded)
}
