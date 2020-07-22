extern crate proc_macro;

use crate::butcher_struct::ButcheredStruct;

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

#[macro_use]
mod utils;

mod butcher_struct;
mod derive_butcher;

/// Allow simple destructuring.
///
/// This macro has the same utility as `MyStruct::butcher`, but allows to
/// destructure only specific fields, and drop the others.
#[proc_macro]
pub fn butcher_struct(tokens: TokenStream) -> TokenStream {
    let data = parse_macro_input!(tokens as ButcheredStruct);

    // TODO: check that we are actually destructuring something
    // TODO: check that there is no repetition

    data.expand_to_code().into()
}

/// Derives the `Butcher` trait for a structure or an enum.
///
/// Currently, enums are not supported.
#[proc_macro_derive(Butcher, attributes(butcher))]
pub fn butcher_derive(tokens: TokenStream) -> TokenStream {
    let data = parse_macro_input!(tokens as DeriveInput);
    derive_butcher::try_from(data)
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}
