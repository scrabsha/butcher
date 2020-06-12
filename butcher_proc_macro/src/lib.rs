extern crate proc_macro;

use crate::butcher_struct::ButcheredStruct;

use proc_macro::TokenStream;
use syn::parse_macro_input;

#[cfg(test)]
macro_rules! assert_eq_tt {
    ($left: ident, $right: ident) => {
        let left = format!("{}", $left);
        let right = format!("{}", $right);

        assert_eq!(left, right);
    };
}

mod butcher_struct;

#[proc_macro]
pub fn butcher_struct(tokens: TokenStream) -> TokenStream {
    let data = parse_macro_input!(tokens as ButcheredStruct);

    // TODO: check that we are actually destructuring something
    // TODO: check that there is no repetition

    data.expand_to_code().into()
}
