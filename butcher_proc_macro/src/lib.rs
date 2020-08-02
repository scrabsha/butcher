extern crate proc_macro;

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

#[macro_use]
mod utils;

mod derive_butcher;

/// Derives the `Butcher` trait for a structure or an enum.
///
/// See the documentation included in [`butcher`] for more information.
///
/// [`butcher`]: https://docs.rs/butcher
#[proc_macro_derive(Butcher, attributes(butcher))]
pub fn butcher_derive(tokens: TokenStream) -> TokenStream {
    let data = parse_macro_input!(tokens as DeriveInput);
    derive_butcher::try_from(data)
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}
