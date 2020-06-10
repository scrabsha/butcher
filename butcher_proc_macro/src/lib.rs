use proc_macro::TokenStream;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    Ident, Result, Token,
};

#[proc_macro]
pub fn butcher_struct(tokens: TokenStream) -> TokenStream {
    let _ = parse_macro_input!(tokens as ButcheredStruct);

    todo!();
}

struct ButcheredStruct {
    name: Ident,
    ty: Ident,
    fields: Vec<Field>,
}

impl Parse for ButcheredStruct {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse::<Ident>()?;
        let _ = input.parse::<Token![:]>()?;
        let ty = input.parse::<Ident>()?;
        let _ = input.parse::<Token![,]>()?;

        let fields = Punctuated::<Field, Token![,]>::parse_terminated(input)?
            .into_iter()
            .collect::<Vec<_>>();

        Ok(ButcheredStruct { name, ty, fields })
    }
}

struct Field {
    name: Ident,
    state: FieldState,
}

impl Parse for Field {
    fn parse(input: ParseStream) -> Result<Self> {
        let state = input.parse::<FieldState>()?;
        let name = input.parse::<Ident>()?;

        Ok(Field { name, state })
    }
}

#[derive(Debug, PartialEq)]
enum FieldState {
    Dereferenced,
    Normal,
}

impl Parse for FieldState {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(match input.parse::<Token![*]>() {
            Ok(_) => FieldState::Dereferenced,
            Err(_) => FieldState::Normal,
        })
    }
}

#[cfg(test)]
mod butchered_struct {
    use super::*;

    use syn::parse_quote;

    #[test]
    fn parse() {
        let res: ButcheredStruct = parse_quote! {
            foo: Foo,
            a, *b, c
        };

        assert_eq!(res.name, "foo");
        assert_eq!(res.ty, "Foo");
        assert_eq!(res.fields[0].name, "a");
        assert_eq!(res.fields[0].state, FieldState::Normal);
        assert_eq!(res.fields[1].name, "b");
        assert_eq!(res.fields[1].state, FieldState::Dereferenced);
        assert_eq!(res.fields[2].name, "c");
        assert_eq!(res.fields[2].state, FieldState::Normal);
        assert_eq!(res.fields.len(), 3);
    }
}

#[cfg(test)]
mod field {
    use super::*;

    use syn::parse_quote;

    #[test]
    fn parse_normal() {
        let res: Field = parse_quote! { a };
        assert_eq!(res.name, "a");
        assert_eq!(res.state, FieldState::Normal);
    }

    #[test]
    fn parse_dereferenced() {
        let res: Field = parse_quote! { *a };
        assert_eq!(res.name, "a");
        assert_eq!(res.state, FieldState::Dereferenced);
    }
}

#[cfg(test)]
mod field_state {
    use super::*;

    use syn::parse_quote;

    #[test]
    fn parse_normal() {
        let left: FieldState = parse_quote! {};
        let right = FieldState::Normal;

        assert_eq!(left, right);
    }

    #[test]
    fn parse_dereferenced() {
        let left: FieldState = parse_quote! { * };
        let right = FieldState::Dereferenced;

        assert_eq!(left, right);
    }
}
