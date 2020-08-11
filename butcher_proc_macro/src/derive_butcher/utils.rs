use std::{
    collections::HashSet,
    iter::{self, FromIterator},
};

use syn::{
    punctuated::Punctuated, AngleBracketedGenericArguments, DeriveInput, GenericArgument,
    GenericParam, Ident, LifetimeDef, Path, PathArguments, PathSegment, Token, Type, TypeParam,
    TypePath,
};

use proc_macro2::TokenStream;

pub(super) trait ReplaceSelf {
    fn replace(&mut self, rep: &Type);
}

macro_rules! impl_replace_self_struct {
    (
        $rep:ident,
        $(
            $ty:ident {
                $( $name:ident => $fun:expr ),* $(,)?
            } $(,)?
        ),*
    ) => {
        $(
            impl_replace_self_struct!(
                @single_type
                $rep,
                $ty {
                    $( $name => $fun ),*
                });
        )*
    };

    (
        @single_type
        $rep:ident,
        $ty:ident {
            $( $name:ident => $fun:expr ),* $(,)?
        } $(,)?
    ) => {
        impl ReplaceSelf for syn::$ty {
            #[allow(unused_variables)]
            fn replace(&mut self, $rep: &Type) {
                let syn::$ty { $( $name, )* .. } = self;
                $( $fun; )*
            }
        }
    }
}

macro_rules! impl_replace_self_enum {
    (
        $self_kw:ident,
        $rep:ident,
        $(
            $ty:ident {
                $(
                    $pat:pat $( if $cond:expr )? => {
                        $( $fun:expr ),* $(,)?
                    }
                ),* $(,)?
            }
        ),* $(,)?
    ) => {
        $(
            impl_replace_self_enum!(
                @single_type
                $self_kw,
                $rep,
                $ty {
                    $(
                        $pat $( if $cond )? => {
                            $( $fun ),*
                        }
                    ),*
                }
            );
        )*
    };
    (
        @single_type
        $self_kw:ident,
        $rep:ident,
        $ty:ident {
            $(
                $pat:pat $( if $cond:expr )? => {
                    $( $fun:expr ),* $(,)?
                }
            ),* $(,)?
        }
    ) => {
        impl ReplaceSelf for syn::$ty {
            fn replace(&mut $self_kw, $rep: &Type) {
                use syn::$ty::*;
                match $self_kw {
                    $(
                        $pat $( if $cond)? => {
                            $( $fun; )*
                        },
                    )*
                }
            }
        }
    };
}

impl_replace_self_struct! {
    rep,

    TypeArray {
        elem => elem.as_mut().replace(rep),
    },

    TypeBareFn {
        inputs => inputs.replace(rep),
        output => output.replace(rep),
    },

    BareFnArg {
        ty => ty.replace(rep),
    },

    TypeGroup {
        elem => elem.replace(rep),
    },

    TypeImplTrait {
        bounds => bounds.replace(rep),
    },

    TraitBound {
        path => path.replace(rep),
    },

    Path {
        segments => segments.replace(rep),
    },

    PathSegment {
        arguments => arguments.replace(rep),
    },

    TypeSlice {
        elem => elem.replace(rep),
    },

    TypeParen {
        elem => elem.as_mut().replace(rep),
    },

    TypePath {
        qself => if let Some(qself) = qself { qself.replace(rep) },
        path => path.replace(rep),
    },

    QSelf {
        ty => ty.as_mut().replace(rep),
    },

    TypePtr {
        elem => elem.replace(rep),
    },

    TypeReference {
        elem => elem.replace(rep),
    },

    TypeTraitObject {
        bounds => bounds.replace(rep),
    },

    TypeTuple {
        elems => elems.replace(rep),
    },

    Binding {
        ty => ty.replace(rep),
    },

    Constraint {
        bounds => bounds.replace(rep),
    },

    AngleBracketedGenericArguments {
        args => args.replace(rep),
    },

    ParenthesizedGenericArguments {
        inputs => inputs.replace(rep),
        output => output.replace(rep),
    },

    TypeInfer {},
    TypeMacro {},
    TypeNever {},
}

impl_replace_self_enum! {
    self,
    rep,

    Type {
        Array(v) => { v.replace(rep) },
        BareFn(bf) => { bf.replace(rep) },
        Group(g) => { g.replace(rep) },
        ImplTrait(it) => { it.replace(rep) },
        Infer(i) => { i.replace(rep) },
        Macro(m) => { m.replace(rep) },
        // The case in which we have to replace Self with something.
        Path(TypePath { path, .. }) if path.is_ident("Self") => {
            *self = rep.clone(),
        },
        // Other non-Self path.
        Path(p) => { p.replace(rep) },
        Ptr(p) => { p.replace(rep) },
        Reference(r) => { r.replace(rep) },
        Slice(s) => { s.replace(rep) },
        TraitObject(to) => { to.replace(rep) },
        Tuple(t) => { t.replace(rep) },
        Verbatim(v) => { v.replace(rep) },
        _ => { unimplemented!() },
    },

    ReturnType {
        Type(_,ty) => {
            ty.replace(rep),
        },
        _ => {},
    },

    TypeParamBound {
        Trait(tb) => { tb.replace(rep) },
        _ => {},
    },

    PathArguments {
        None => {},
        AngleBracketed(args) => { args.replace(rep) },
        Parenthesized(args) => { args.replace(rep) },
    },

    GenericArgument {
        Lifetime(_) => {},
        Const(_) => {},
        Type(t) => { t.replace(rep) },
        Binding(b) => { b.replace(rep) },
        Constraint(c) => { c.replace(rep) },
    }
}

impl ReplaceSelf for TokenStream {
    fn replace(&mut self, _rep: &Type) {}
}

impl<T: ReplaceSelf, U> ReplaceSelf for Punctuated<T, U> {
    fn replace(&mut self, rep: &Type) {
        self.iter_mut().for_each(|t| t.replace(rep));
    }
}

pub(super) fn create_type_signature(input: &DeriveInput) -> Type {
    let name = input.ident.clone();
    let params = input.generics.params.iter().cloned();
    let lt_token = input.generics.lt_token;
    let gt_token = input.generics.gt_token;

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
        }
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

trait FindGenerics {
    fn find_generics(&self, generics: &HashSet<Ident>) -> Vec<Ident>;
}

macro_rules! impl_find_generics_struct {
    (
        @single_type
        $generics:ident,
        $ty:ident { $($var:ident),* $(,)? } => { $( $fun:expr ),* $(,)? } $(,)?
    ) => {
        impl FindGenerics for syn::$ty {
            fn find_generics(&self, $generics: &HashSet<Ident>) -> Vec<Ident> {
                let syn::$ty { $($var,)* .. } = self;
                std::iter::empty()
                    $( .chain($fun) )*
                    .collect::<Vec<_>>()
            }
        }
    };

    (
        $generics:ident,
        $(
            $ty:ident { $($var:ident),* $(,)? } => { $( $fun:expr ),* $(,)? }
        ),* $(,)?
    ) => {
        $(
            impl_find_generics_struct!(
                @single_type
                $generics,
                $ty { $($var),* } => { $($fun),* },
            );
        )*
    }
}

macro_rules! impl_find_generics_enum {
    (
        @single_type
        $generics:ident,
        $ty:ident { $(
                $pat:pat => { $( $fun:expr ),* $(,)? }
        ),* $(,)? } $(,)?
    ) => {
        impl FindGenerics for syn::$ty {
            fn find_generics(&self, $generics: &HashSet<Ident>) -> Vec<Ident> {
                use syn::$ty::*;

                match self {
                    $(
                        $pat => {
                            std::iter::empty()
                                $( .chain($fun) )*
                                .collect::<Vec<_>>()
                        },
                    )*
                }
            }
        }
    };

    (
        $generics:ident,
        $(
            $ty:ident { $(
                    $pat:pat => { $( $fun:expr ),* $(,)? }
            ),* $(,)? }
        ),* $(,)?
    ) => {
        $(
            impl_find_generics_enum!(
                @single_type
                $generics,
                $ty { $($pat => { $($fun),* }),* },
            );
        )*
    };
}

impl_find_generics_struct! {
    gs,
    TypeArray { elem } => { elem.as_ref().find_generics(gs) },
    TypeBareFn { inputs, output } => { inputs.find_generics(gs), output.find_generics(gs) },
    BareFnArg { ty } => { ty.find_generics(gs) },
    TypeGroup { elem } => { elem.as_ref().find_generics(gs) },
    TypeImplTrait { bounds } => { bounds.find_generics(gs) },
    TraitBound { path } => { path.find_generics(gs) },
    Path { segments } => { segments.find_generics(gs) },
    PathSegment { ident, arguments } => {
        ident.find_generics(gs),
        arguments.find_generics(gs),
    },
    AngleBracketedGenericArguments { args } => { args.find_generics(gs) },
    Binding { ident, ty } => {
        ident.find_generics(gs),
        ty.find_generics(gs),
    },
    Constraint { ident, bounds } => {
        ident.find_generics(gs),
        bounds.find_generics(gs),
    },
    ParenthesizedGenericArguments { inputs, output } => {
        inputs.find_generics(gs),
        output.find_generics(gs),
    },
    TypeParen { elem } => { elem.find_generics(gs) },
    TypePath { qself, path } => {
        qself.find_generics(gs),
        path.find_generics(gs),
    },
    QSelf { ty } => { ty.find_generics(gs) },
    TypePtr { elem } => { elem.find_generics(gs) },
    TypeReference { elem } => { elem.find_generics(gs) },
    TypeSlice { elem } => { elem.find_generics(gs) },
    TypeTraitObject { bounds } => { bounds.find_generics(gs) },
    TypeTuple { elems } => { elems.find_generics(gs) },
}

impl_find_generics_enum! {
    gs,
    Type {
        Array(ta) => { ta.find_generics(gs) },
        BareFn(tbf) => { tbf.find_generics(gs) },
        Group(tg) => { tg.find_generics(gs) },
        ImplTrait(tit) => { tit.find_generics(gs) },
        Infer(_) => {},
        Macro(_) => {},
        Never(_) => {},
        Paren(tp) => { tp.find_generics(gs) },
        Path(tp) => { tp.find_generics(gs) },
        Ptr(tptr) => { tptr.find_generics(gs) },
        Reference(tr) => { tr.find_generics(gs) },
        Slice(ts) => { ts.find_generics(gs) },
        TraitObject(tto) => { tto.find_generics(gs) },
        Tuple(tt) => { tt.find_generics(gs) },
        Verbatim(_) => {},
        _ => {},
    },
    ReturnType {
        Default => {},
        Type(_, t) => { t.as_ref().find_generics(gs) },
    },
    TypeParamBound {
        Trait(t) => { t.find_generics(gs) },
        Lifetime(_) => {},
    },
    PathArguments {
        None => {},
        AngleBracketed(ab) => { ab.find_generics(gs) },
        Parenthesized(p) => { p.find_generics(gs) },
    },
    GenericArgument {
        Lifetime(_) => {},
        Type(t) => { t.find_generics(gs) },
        Binding(b) => { b.find_generics(gs) },
        Constraint(c) => { c.find_generics(gs) },
        Const(_) => {},
    },
}

impl<T: FindGenerics, U> FindGenerics for Punctuated<T, U> {
    fn find_generics(&self, gs: &HashSet<Ident>) -> Vec<Ident> {
        self.iter().flat_map(|t| t.find_generics(gs)).collect()
    }
}

impl<T: FindGenerics> FindGenerics for Option<T> {
    fn find_generics(&self, gs: &HashSet<Ident>) -> Vec<Ident> {
        self.iter().flat_map(|t| t.find_generics(gs)).collect()
    }
}

impl FindGenerics for Ident {
    fn find_generics(&self, gs: &HashSet<Ident>) -> Vec<Ident> {
        if gs.contains(self) {
            vec![self.clone()]
        } else {
            Vec::new()
        }
    }
}

#[cfg(test)]
macro_rules! test_replace_self {
    ($rep:ty, $left:ty, $right:ty) => {
        let mut left: Type = syn::parse_quote! { $left };
        let rep: Type = syn::parse_quote! { $rep };
        left.replace(&rep);
        let right: Type = syn::parse_quote! { $right };
        assert_eq_tt!(left, right);
    };
}

#[cfg(test)]
mod replace_self {
    use super::*;

    // Note: the tests here deliberately do not test everything. The goal here
    // is to ensure it works in the most simple situation.
    //
    // Anyway, most of the ReplaceSelf implementation is macro-generated, so
    // it should not contain much errors.

    #[test]
    fn simple() {
        test_replace_self! { Foo, Self, Foo };
        test_replace_self! { Bar<Foo>, Self, Bar<Foo> };
    }

    #[test]
    fn generic() {
        test_replace_self! { Foo, Vec<Self>, Vec<Foo> };
        test_replace_self! { Foo, Vec<Vec<Self>>, Vec<Vec<Foo>> };
    }

    #[test]
    fn impl_trait() {
        test_replace_self! { Foo, impl AsRef<Self>, impl AsRef<Foo> };
    }

    #[test]
    fn tuple() {
        test_replace_self! { Foo, (Self, usize), (Foo, usize) };
    }
}

#[cfg(test)]
macro_rules! test_create_type_signature {
    ($left:item, $right:path) => {
        let tmp: DeriveInput = syn::parse_quote! { $left };
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
        test_create_type_signature!(struct Foo<A, B>;, Foo<A, B>);
        test_create_type_signature!(struct Foo<'a, A: 'a, B: ToOwned>;, Foo<'a, A, B>);
        test_create_type_signature!(struct Vec<T: Clone>;, Vec<T>);
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

macro_rules! test_find_generics {
    (@make_ident $i:ident) => {
        {
            let tmp: syn::Ident = syn::parse_quote! { $i };
            tmp
        }
    };
    ( { $($gen:ident),* $(,)? }, $ty:ty => [ $($found_gen:ident),* $(,)? ] $(,)? ) => {
        let gs = {
            #[allow(unused_mut)]
            let mut tmp = HashSet::new();
            $(
                let id = test_find_generics!(@make_ident $gen);
                tmp.insert(id);
            )*
            tmp
        };

        let ty: Type = syn::parse_quote! { $ty };

        let left = ty.find_generics(&gs);

        let right: Vec<syn::Ident> = {
            #[allow(unused_mut)]
            let mut tmp = Vec::new();
            $(
                let id = test_find_generics!(@make_ident $found_gen);
                tmp.push(id);
            )*
            tmp
        };

        assert_eq!(left, right);
    };
}

#[cfg(test)]
mod find_generics {
    use super::*;

    #[test]
    fn simple() {
        test_find_generics! { {}, Foo => [] };

        test_find_generics! { { A, B, C, D }, A => [A] };

        test_find_generics! { { A }, A => [A] };
    }

    #[test]
    fn in_path() {
        test_find_generics! { { A, B, C }, Vec<A, B> => [A, B] };

        test_find_generics! { { T, E, F }, Result<T, E> => [T, E] };
    }

    #[test]
    fn in_impl_trait() {
        test_find_generics! { { A, B }, impl Iterator<Item = (A, B)> => [A, B] };

        test_find_generics! { { A, B, C, D }, impl AsRef<A> => [A] };
    }

    #[test]
    fn in_fn() {
        test_find_generics! { { A, B, C }, fn(A) -> C => [A, C] };

        test_find_generics! { { A, B, C }, fn() -> B => [B] };
    }
}
