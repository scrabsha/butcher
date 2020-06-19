extern crate proc_macro;

use crate::butcher_struct::ButcheredStruct;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

use syn::Error;

#[cfg(test)]
macro_rules! assert_eq_tt {
    ($left: ident, $right: ident) => {
        let left = format!("{}", $left);
        let right = format!("{}", $right);

        assert_eq!(left, right);
    };
}

mod butcher_struct;

// TODO: remove this, actually use this dead code
mod derive_butcher;

#[proc_macro]
pub fn butcher_struct(tokens: TokenStream) -> TokenStream {
    let data = parse_macro_input!(tokens as ButcheredStruct);

    // TODO: check that we are actually destructuring something
    // TODO: check that there is no repetition

    data.expand_to_code().into()
}

#[proc_macro_derive(Butcher, attributes(butcher))]
pub fn butcher_derive(tokens: TokenStream) -> TokenStream {
    let data = parse_macro_input!(tokens as DeriveInput);
    derive_butcher::ButcheredStruct::from(data)
        .map(derive_butcher::ButcheredStruct::expand_to_code)
        .unwrap_or_else(to_compile_errors)
        .into()
}

fn to_compile_errors(i: Vec<Error>) -> TokenStream2 {
    let i = i.iter().map(syn::Error::to_compile_error);

    quote! { #( #i )* }
}
