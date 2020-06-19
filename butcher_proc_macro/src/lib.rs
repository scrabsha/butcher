extern crate proc_macro;

use crate::butcher_struct::ButcheredStruct;

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

#[macro_use]
mod utils;

mod butcher_struct;
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
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}
