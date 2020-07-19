use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Ident, Result, Token,
};

use crate::utils::{self, FieldName};

pub(super) struct ButcheredStruct {
    name: Ident,
    ty: Ident,
    fields: Vec<Field>,
}

#[inline]
fn owned() -> TokenStream {
    quote! { std::borrow::Cow::Owned }
}

#[inline]
fn borrowed() -> TokenStream {
    quote! { std::borrow::Cow::Borrowed }
}

impl ButcheredStruct {
    pub(super) fn expand_to_code(&self) -> TokenStream {
        let var_name = &self.name;
        let owned_arm = self.owned_arm();
        let borrowed_arm = self.borrowed_arm();

        quote! {
            match #var_name {
                #owned_arm
                #borrowed_arm
            }
        }
    }

    fn owned_arm(&self) -> TokenStream {
        let owned = owned();
        let isp = self.inner_struct_pattern();
        let return_expr = self.owned_return_expr();

        quote! {
            #owned(#isp) => #return_expr,
        }
    }

    fn owned_return_expr(&self) -> TokenStream {
        let variables = self.fields.iter().map(|f| f.expand_owned(&self.ty));

        quote! {
            (
                #(
                    #variables
                ),*
            )
        }
    }

    fn borrowed_arm(&self) -> TokenStream {
        let borrowed = borrowed();
        let isp = self.inner_struct_pattern();
        let return_expr = self.borrowed_return_expr();

        quote! {
            #borrowed(#isp) => #return_expr,
        }
    }

    fn borrowed_return_expr(&self) -> TokenStream {
        let variables = self.fields.iter().map(|f| f.expand_borrowed(&self.ty));

        quote! {
            (
                #(
                    #variables
                ),*
            )
        }
    }

    fn inner_struct_pattern(&self) -> TokenStream {
        let variables = self.fields.iter().map(Field::expand);
        let ty = &self.ty;

        quote! {
            #ty {
                #(
                    #variables,
                )*
                ..
            }
        }
    }
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
    name: FieldName,
}

impl Field {
    fn expand(&self) -> TokenStream {
        let name = &self.name;
        quote! { #name }
    }

    fn expand_owned(&self, main_name: &Ident) -> TokenStream {
        let associated_struct_name = utils::associated_struct_name(main_name, &self.name);
        let var_name = &self.name;

        quote! { #associated_struct_name::from_owned(#var_name) }
    }

    fn expand_borrowed(&self, main_name: &Ident) -> TokenStream {
        let associated_struct_name = utils::associated_struct_name(main_name, &self.name);
        let var_name = &self.name;

        quote! { #associated_struct_name::from_borrowed(#var_name) }
    }
}

impl Parse for Field {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse::<Ident>()?.into();

        Ok(Field { name })
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
            a, b, c
        };

        assert_eq!(res.name, "foo");
        assert_eq!(res.ty, "Foo");
        assert_eq!(res.fields[0].name, "a");
        assert_eq!(res.fields[1].name, "b");
        assert_eq!(res.fields[2].name, "c");
        assert_eq!(res.fields.len(), 3);
    }

    #[test]
    fn expand_to_code() {
        let res: ButcheredStruct = parse_quote! {
            foo: Foo,
            a,
        };

        let left = res.expand_to_code();
        let right = quote! {
            match foo {
                std::borrow::Cow::Owned(Foo { a, .. }) => (
                    ButcherFooa::from_owned(a)
                ),
                std::borrow::Cow::Borrowed(Foo { a, .. }) => (
                    ButcherFooa::from_borrowed(a)
                ),
            }
        };

        assert_eq_tt!(left, right);
    }

    #[test]
    fn owned_arm() {
        let tmp: ButcheredStruct = parse_quote! {
            foo: Foo,
            a,
        };

        let left = tmp.owned_arm();
        let right = quote! {
            std::borrow::Cow::Owned(Foo { a, .. }) => (
                ButcherFooa::from_owned(a)
            ),
        };

        assert_eq_tt!(left, right);
    }

    #[test]
    fn owned_return_expr() {
        let tmp: ButcheredStruct = parse_quote! {
            foo: Foo,
            a,
        };

        let left = tmp.owned_return_expr();
        let right = quote! {
            (
                ButcherFooa::from_owned(a)
            )
        };

        assert_eq_tt!(left, right);
    }

    #[test]
    fn borrowed_arm() {
        let tmp: ButcheredStruct = parse_quote! {
            foo: Foo,
            a,
        };

        let left = tmp.borrowed_arm();
        let right = quote! {
            std::borrow::Cow::Borrowed(Foo { a, .. }) => (
                ButcherFooa::from_borrowed(a)
            ),
        };

        assert_eq_tt!(left, right);
    }

    #[test]
    fn borrowed_return_expr() {
        let tmp: ButcheredStruct = parse_quote! {
            foo: Foo,
            a,
            b,
        };

        let left = tmp.borrowed_return_expr();
        let right = quote! {
            (
                ButcherFooa::from_borrowed(a),
                ButcherFoob::from_borrowed(b)
            )
        };

        assert_eq_tt!(left, right);
    }

    #[test]
    fn inner_struct_pattern() {
        let tmp: ButcheredStruct = parse_quote! {
            foo: Foo,
            a,
            b,
            c
        };

        let left = tmp.inner_struct_pattern();
        let right = quote! { Foo { a , b , c , .. } };
        assert_eq_tt!(left, right);
    }
}

#[cfg(test)]
mod field {
    use super::*;

    use syn::parse_quote;

    #[test]
    fn parse() {
        let res: Field = parse_quote! { a };
        assert_eq!(res.name, "a");
    }

    #[test]
    fn expand_to_pattern() {
        let tmp: Field = parse_quote! { a };
        let left = tmp.expand();
        let right = quote! { a };

        assert_eq_tt!(left, right);
    }
}
