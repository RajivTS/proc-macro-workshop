use quote::ToTokens;

#[proc_macro_attribute]
pub fn sorted(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let _ = args;
    let _ = input;
    let input = syn::parse_macro_input!(input as syn::Item);
    validate_enum(input)
        .map(|v| v.into_token_stream())
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

fn validate_enum(item: syn::Item) -> syn::Result<syn::ItemEnum> {
    let attrs = match item {
        syn::Item::Const(const_item) => const_item.attrs,
        syn::Item::Enum(enum_item) => return Ok(enum_item),
        syn::Item::ExternCrate(crate_item) => crate_item.attrs,
        syn::Item::Fn(fn_item) => fn_item.attrs,
        syn::Item::ForeignMod(foreign_item) => foreign_item.attrs,
        syn::Item::Impl(impl_item) => impl_item.attrs,
        syn::Item::Macro(macro_item) => macro_item.attrs,
        syn::Item::Macro2(macro2_item) => macro2_item.attrs,
        syn::Item::Mod(mod_item) => mod_item.attrs,
        syn::Item::Static(static_item) => static_item.attrs,
        syn::Item::Struct(struct_item) => struct_item.attrs,
        syn::Item::Trait(trait_item) => trait_item.attrs,
        syn::Item::TraitAlias(trait_alias_item) => trait_alias_item.attrs,
        syn::Item::Type(type_item) => type_item.attrs,
        syn::Item::Union(union_item) => union_item.attrs,
        syn::Item::Use(use_item) => use_item.attrs,
        _ => return Err(syn::Error::new_spanned(
            item,
            "expected enum or match expression",
        )),
    };
    return Err(syn::Error::new_spanned(
        attrs.first(),
        "expected enum or match expression",
    ))
}
