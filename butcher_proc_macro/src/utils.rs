use std::fmt::{Formatter, Result as FmtResult};

use syn::Ident;

use quote::{format_ident, IdentFragment, ToTokens, TokenStreamExt};

use proc_macro2::{Literal, TokenStream};

#[cfg(test)]
macro_rules! assert_eq_tt {
    ($left: ident, $right: ident) => {
        let left = format!("{}", $left);
        let right = format!("{}", $right);

        assert_eq!(left, right);
    };
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum FieldName {
    Named(Ident),
    Unnamed(usize),
}

impl From<Ident> for FieldName {
    fn from(i: Ident) -> FieldName {
        FieldName::Named(i)
    }
}

impl From<usize> for FieldName {
    fn from(i: usize) -> FieldName {
        FieldName::Unnamed(i)
    }
}

impl IdentFragment for FieldName {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            FieldName::Named(name) => name.fmt(f),
            FieldName::Unnamed(id) => id.fmt(f),
        }
    }
}

impl ToTokens for FieldName {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            FieldName::Named(name) => tokens.append(name.clone()),
            FieldName::Unnamed(id) => tokens.append(Literal::usize_unsuffixed(*id)),
        }
    }
}

pub(crate) fn associated_struct_name(main_struct: &Ident, field: &FieldName) -> Ident {
    format_ident!("Butcher{}{}", main_struct, field)
}

#[cfg(test)]
mod associated_struct_name {
    use super::*;

    use syn::parse_quote;

    #[test]
    fn test() {
        let main: Ident = parse_quote! { Foo };
        let field: Ident = parse_quote! { bar };

        assert_eq!(associated_struct_name(&main, &field), "ButcherFoobar");
    }
}
