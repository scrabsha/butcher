use std::iter::{self, FromIterator};

use syn::{
    punctuated::Punctuated, AngleBracketedGenericArguments, DeriveInput, GenericArgument,
    GenericParam, Ident, LifetimeDef, Path, PathArguments, PathSegment, Token, Type, TypeParam,
    TypePath,
};

fn create_type_signature(input: &DeriveInput) -> Type {
    let name = input.ident.clone();
    let params = input.generics.params.iter().cloned();
    let lt_token = input.generics.lt_token.clone();
    let gt_token = input.generics.gt_token.clone();

    create_type_signature_from_raws(name, params, lt_token, gt_token)
}

fn create_type_signature_from_raws(
    ident: Ident,
    params: impl Iterator<Item = GenericParam>,
    lt_token: Option<Token![<]>,
    gt_token: Option<Token![>]>,
) -> Type {
    let segments = create_path_segments(ident, params, lt_token, gt_token);
    let path = Path {
        leading_colon: None,
        segments,
    };

    Type::Path(TypePath { qself: None, path })
}

fn create_path_segments(
    ident: Ident,
    params: impl Iterator<Item = GenericParam>,
    lt_token: Option<Token![<]>,
    gt_token: Option<Token![>]>,
) -> Punctuated<PathSegment, Token![::]> {
    let args = Punctuated::from_iter(arguments_from_params(params));

    let arguments = match (lt_token, gt_token) {
        (Some(lt_token), Some(gt_token)) => {
            PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                colon2_token: None,
                lt_token,
                gt_token,
                args,
            })
        },
        (None, None) => PathArguments::None,
        _ => unreachable!(),
    };

    let segment = PathSegment { ident, arguments };

    Punctuated::from_iter(iter::once(segment))
}

fn arguments_from_params(
    input: impl Iterator<Item = GenericParam>,
) -> impl Iterator<Item = GenericArgument> {
    input.flat_map(generic_param)
}

fn generic_param(generic_param: GenericParam) -> Option<GenericArgument> {
    match generic_param {
        GenericParam::Type(TypeParam { ident, .. }) => {
            let segments = Punctuated::from_iter(iter::once(PathSegment {
                ident,
                arguments: PathArguments::None,
            }));

            let path = Path {
                leading_colon: None,
                segments,
            };

            Some(GenericArgument::Type(Type::Path(TypePath {
                qself: None,
                path,
            })))
        }

        GenericParam::Lifetime(LifetimeDef { lifetime, .. }) => {
            Some(GenericArgument::Lifetime(lifetime))
        }

        GenericParam::Const(_) => None,
    }
}

#[cfg(test)]
macro_rules! test_create_type_signature {
    ($right:path, $($left:tt)*) => {
        let tmp: DeriveInput = syn::parse_quote! { $($left)* };
        let left = create_type_signature(&tmp);
        let right: Type = syn::parse_quote! { $right };
        assert_eq_tt!(left, right);
    };
}

#[cfg(test)]
mod create_type_signature {
    use super::*;

    #[test]
    fn test() {
        test_create_type_signature!(Foo<A, B>, struct Foo<A, B>;);
        test_create_type_signature!(
            Foo<'a, A, B>,
            struct Foo<'a, A: 'a, B: ToOwned>;
        );
        test_create_type_signature!(
            Vec<T>,
            struct Vec<T: Clone>;
        );
    }
}

// Note: here it is needed to break the left-right conversion because the
// matching rule $($_:tt)+ eats the whole remaining input, including $right.
#[cfg(test)]
macro_rules! test_generic_param {
    (None, $($left:tt)+ $(,)?) => {
        let tmp: GenericParam = parse_quote! { $($left)+ };
        assert!(generic_param(tmp).is_none());
    };

    ($right:tt, $($left:tt)+ $(,)?) => {
        let input: GenericParam = syn::parse_quote! { $($left)+ };
        let left = generic_param(input);
        let right: Option<GenericArgument> = Some(syn::parse_quote! { $right });
        assert_eq_tt!(left, right);
    };
}

#[cfg(test)]
mod generic_param {
    use super::*;

    use syn::parse_quote;

    #[test]
    fn handles_type() {
        test_generic_param!(T, T);
        test_generic_param!(T, T: AsRef<str> + 'a);
    }

    #[test]
    fn handles_lifetimes() {
        test_generic_param!('a, 'a);
        test_generic_param!('a, 'a: 'b);
    }

    #[test]
    fn does_not_hangle_consts() {
        test_generic_param!(None, const LENGTH: usize);
    }
}
