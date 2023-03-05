use quote::ToTokens;

#[proc_macro_attribute]
pub fn sorted(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let _ = args;
    let _ = input;
    let input = syn::parse_macro_input!(input as syn::Item);
    input.to_token_stream().into()
}
