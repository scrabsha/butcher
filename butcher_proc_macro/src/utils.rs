use syn::Ident;

use quote::format_ident;

#[cfg(test)]
macro_rules! assert_eq_tt {
    ($left: ident, $right: ident) => {
        let left = format!("{}", $left);
        let right = format!("{}", $right);

        assert_eq!(left, right);
    };
}

pub(crate) fn associated_struct_name(main_struct: &Ident, field: &Ident) -> Ident {
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
