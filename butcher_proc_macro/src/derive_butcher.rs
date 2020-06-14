use std::{
    collections::HashSet,
    error::Error,
    fmt::{self, Display},
};

use syn::{
    AngleBracketedGenericArguments, Attribute, Data, DeriveInput, Fields, GenericArgument,
    GenericParam, Ident, Meta, MetaList, NestedMeta, PathArguments, QSelf, ReturnType, Type,
    TypeArray, TypeBareFn, TypeGroup, TypeParen, TypePath, TypePtr, TypeReference, TypeSlice,
    TypeTuple, Visibility,
};

use proc_macro2::TokenStream;

#[derive(Debug, PartialEq)]
pub enum DeriveError {
    FoundUnion,
    FoundUnitStruct,
    // TODO: remove this, handle enums
    FoundEnum,
    // TODO: remove this, handle tupled struct
    FoundTupledStruct,
    MultipleButcheringMethod,
    FoundImplTrait,
    FoundMacroAsType,
    FoundTraitObject,
    MethodFoundLitteral,
    NestedMethod,
    UnknownMethod,
}

impl Display for DeriveError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use DeriveError::*;
        let to_display = match self {
            FoundUnion => format_args!("Butcher does not support unions."),
            FoundUnitStruct => format_args!("Butchering is useless for unit structs."),
            FoundEnum => format_args!(
                "Butcher currently does not support enums. This is planned for next release."
            ),
            FoundTupledStruct => format_args!("Butcher does not currently support tupled structs."),
            MultipleButcheringMethod => {
                format_args!("Multiple butchering method provided. Choose one.")
            }
            FoundImplTrait => format_args!("Butcher does not support impl Trait."),
            FoundMacroAsType => format_args!("Butcher does not support macro as type."),
            FoundTraitObject => format_args!("Butcher does not support trait objects."),
            MethodFoundLitteral => {
                format_args!("Butcher does not support litteral as butchering method.")
            }
            NestedMethod => format_args!("Butcher does not support nested methods."),
            UnknownMethod => format_args!("Unknown butchering method."),
        };

        write!(f, "{}", to_display)
    }
}

type Errors = Vec<syn::Error>;

impl Error for DeriveError {}

pub(super) struct ButcheredStruct {
    name: Ident,
    fields: Vec<Field>,
}

impl ButcheredStruct {
    pub(super) fn from(input: DeriveInput) -> Result<ButcheredStruct, Errors> {
        let name = input.ident;
        let data = match input.data {
            Data::Struct(d) => Ok(d),
            Data::Enum(de) => Err((DeriveError::FoundEnum, de.enum_token.span)),
            Data::Union(du) => Err((DeriveError::FoundUnion, du.union_token.span)),
        }
        .map_err(|(e, s)| vec![syn::Error::new(s, e)])?;

        let fields = match data.fields {
            Fields::Named(fields) => Ok(fields.named),
            Fields::Unnamed(fu) => Err((DeriveError::FoundTupledStruct, fu.paren_token.span)),
            Fields::Unit => Err((DeriveError::FoundUnitStruct, name.span())),
        }
        .map_err(|(e, s)| vec![syn::Error::new(s, e)])?;

        let generics = input
            .generics
            .params
            .into_iter()
            .filter_map(|g| match g {
                GenericParam::Type(g) => Some(g.ident),
                GenericParam::Lifetime(_) | GenericParam::Const(_) => None,
            })
            .collect::<HashSet<_>>();

        let (fields, errors) = fields.into_iter().map(|f| Field::from(f, &generics)).fold(
            (Vec::new(), Vec::new()),
            |(mut oks, mut errs), res| match res {
                Ok(v) => {
                    oks.push(v);
                    (oks, errs)
                }
                Err(e) => {
                    errs.extend(e);
                    (oks, errs)
                }
            },
        );

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(ButcheredStruct { name, fields })
    }

    pub(super) fn expand_to_code(self) -> TokenStream {
        todo!();
    }
}

struct Field {
    name: Ident,
    method: ButcheringMethod,
    vis: Visibility,
    ty: Type,
    associated_generics: Vec<Ident>,
}

impl Field {
    fn from(input: syn::Field, generics: &HashSet<Ident>) -> Result<Field, Errors> {
        let methods = input
            .attrs
            .iter()
            .map(ButcheringMethod::try_from)
            .flatten()
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| vec![e])?;

        let (method, errs) = match methods.as_slice() {
            [method] => (*method, Vec::new()),
            [] => (ButcheringMethod::Regular, Vec::new()),
            [.., last] => (
                *last,
                vec![syn::Error::new_spanned(
                    &input,
                    DeriveError::MultipleButcheringMethod,
                )],
            ),
        };

        if !errs.is_empty() {
            return Err(errs);
        }

        let vis = input.vis;

        let name = input
            .ident
            .expect("Fields of named struct should have a name");

        let ty = input.ty;

        let associated_generics = find_generics_in_type(&ty, generics).map_err(|e| vec![e])?;

        Ok(Field {
            vis,
            method,
            name,
            ty,
            associated_generics,
        })
    }
}

fn find_generics_in_type(ty: &Type, generics: &HashSet<Ident>) -> Result<Vec<Ident>, syn::Error> {
    match ty {
        Type::Array(TypeArray { elem, .. })
        | Type::Group(TypeGroup { elem, .. })
        | Type::Paren(TypeParen { elem, .. })
        | Type::Ptr(TypePtr { elem, .. })
        | Type::Reference(TypeReference { elem, .. })
        | Type::Slice(TypeSlice { elem, .. }) => find_generics_in_type(elem.as_ref(), generics),

        Type::Tuple(TypeTuple { elems, .. }) => {
            elems.into_iter().try_fold(Vec::new(), |acc, ty| {
                let found = find_generics_in_type(ty, generics);
                extend_discovered_generics(acc, found)
            })
        }

        Type::BareFn(TypeBareFn { inputs, output, .. }) => {
            let mut found_generics = inputs.into_iter().try_fold(Vec::new(), |acc, arg| {
                let found = find_generics_in_type(&arg.ty, generics);
                extend_discovered_generics(acc, found)
            })?;

            if let ReturnType::Type(_, ty) = output {
                found_generics.extend(find_generics_in_type(ty.as_ref(), generics)?);
            }

            Ok(found_generics)
        }

        Type::Path(TypePath { path, qself }) => {
            let mut found_generics = path
                .segments
                .iter()
                .filter_map(|s| match &s.arguments {
                    PathArguments::None | PathArguments::Parenthesized(_) => None,
                    PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        args, ..
                    }) => Some(args),
                })
                .flatten()
                .filter_map(|arg| match arg {
                    GenericArgument::Type(t) => Some(find_generics_in_type(t, generics)),
                    _ => None,
                })
                .try_fold(Vec::new(), |acc, rslt| {
                    extend_discovered_generics(acc, rslt)
                })?;

            match path.get_ident() {
                Some(id) if generics.contains(id) => found_generics.push(id.clone()),
                _ => {}
            }

            if let Some(QSelf { ty, .. }) = qself {
                found_generics.extend(find_generics_in_type(ty.as_ref(), generics)?);
            }

            Ok(found_generics)
        }

        Type::ImplTrait(tit) => Err(syn::Error::new_spanned(tit, DeriveError::FoundImplTrait)),

        Type::Macro(m) => Err(syn::Error::new_spanned(m, DeriveError::FoundMacroAsType)),

        Type::TraitObject(to) => Err(syn::Error::new_spanned(to, DeriveError::FoundTraitObject)),

        // For the next three arms, the compiler is going to raise an error
        // anyway.
        Type::Infer(_) => Ok(Vec::new()),
        Type::Never(_) => Ok(Vec::new()),
        Type::Verbatim(_) => Ok(Vec::new()),

        _ => panic!("Unknown type met"),
    }
}

fn extend_discovered_generics(
    mut discovered: Vec<Ident>,
    to_add: Result<Vec<Ident>, syn::Error>,
) -> Result<Vec<Ident>, syn::Error> {
    let to_add = to_add?;
    discovered.extend(to_add);
    Ok(discovered)
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum ButcheringMethod {
    Copy,
    Dereferenced,
    Regular,
    Referenced,
}

impl ButcheringMethod {
    fn try_from(input: &Attribute) -> Option<Result<ButcheringMethod, syn::Error>> {
        let meta = input.parse_meta().ok()?;

        let methods = match &meta {
            Meta::Path(_) | Meta::NameValue(_) => return None,
            Meta::List(MetaList { path, nested, .. }) if path.is_ident("butcher") => nested,
            Meta::List(_) => return None,
        };

        if methods.len() != 1 {
            return Some(Err(syn::Error::new_spanned(
                meta,
                DeriveError::MultipleButcheringMethod,
            )));
        }

        let method = &methods[0];

        let method = match method {
            NestedMeta::Lit(_) => {
                return Some(Err(syn::Error::new_spanned(
                    method,
                    DeriveError::MethodFoundLitteral,
                )))
            }
            NestedMeta::Meta(Meta::Path(p)) => p.get_ident().map(ToString::to_string),
            NestedMeta::Meta(_) => {
                return Some(Err(syn::Error::new_spanned(
                    method,
                    DeriveError::NestedMethod,
                )))
            }
        };

        Some(Ok(match method.as_deref() {
            Some("as_ref") => ButcheringMethod::Referenced,
            Some("deref") => ButcheringMethod::Dereferenced,
            Some("regular") => ButcheringMethod::Regular,
            Some("copy") => ButcheringMethod::Copy,
            _ => {
                return Some(Err(syn::Error::new_spanned(
                    method,
                    DeriveError::UnknownMethod,
                )))
            }
        }))
    }
}

#[cfg(test)]
mod butchered_struct {
    use super::*;

    use syn::DeriveInput;

    use quote::quote;
    use syn::parse_quote;

    #[test]
    fn serialization() {
        let input: DeriveInput = parse_quote! {
            #[derive(Butcher)]
            struct Foo<'a, T> {
                #[butcher(copy)]
                pub a: &'a str,
                #[butcher(as_ref)]
                pub(super) b: String,
                c: T,
                #[butcher(deref)]
                pub(crate) d: Box<T>,
                #[butcher(regular)]
                e: (),
                f: (),
            }
        };

        let bs = ButcheredStruct::from(input).unwrap();
        assert_eq!(bs.name, "Foo");

        assert_eq!(bs.fields[0].name, "a");
        assert_eq!(bs.fields[0].method, ButcheringMethod::Copy);
        assert!(bs.fields[4].associated_generics.is_empty());

        assert_eq!(bs.fields[1].name, "b");
        assert_eq!(bs.fields[1].method, ButcheringMethod::Referenced);
        assert!(bs.fields[4].associated_generics.is_empty());

        assert_eq!(bs.fields[2].name, "c");
        assert_eq!(bs.fields[2].method, ButcheringMethod::Regular);
        assert_eq!(bs.fields[2].associated_generics, &["T"]);

        assert_eq!(bs.fields[3].name, "d");
        assert_eq!(bs.fields[3].method, ButcheringMethod::Dereferenced);
        assert_eq!(bs.fields[3].associated_generics, &["T"]);

        assert_eq!(bs.fields[4].name, "e");
        assert_eq!(bs.fields[4].method, ButcheringMethod::Regular);
        assert!(bs.fields[4].associated_generics.is_empty());

        assert_eq!(bs.fields[5].name, "f");
        assert_eq!(bs.fields[5].method, ButcheringMethod::Regular);
        assert!(bs.fields[5].associated_generics.is_empty());

        let tmp = &bs.fields[0].ty;
        let left = quote! { #tmp };
        let right = quote! { &'a str };
        assert_eq_tt!(left, right);

        let tmp = &bs.fields[1].ty;
        let left = quote! { #tmp };
        let right = quote! { String };
        assert_eq_tt!(left, right);

        let tmp = &bs.fields[2].ty;
        let left = quote! { #tmp };
        let right = quote! { T };
        assert_eq_tt!(left, right);

        let tmp = &bs.fields[3].ty;
        let left = quote! { #tmp };
        let right = quote! { Box<T> };
        assert_eq_tt!(left, right);

        let tmp = &bs.fields[4].ty;
        let left = quote! { #tmp };
        let right = quote! { () };
        assert_eq_tt!(left, right);

        let tmp = &bs.fields[5].ty;
        let left = quote! { #tmp };
        let right = quote! { () };
        assert_eq_tt!(left, right);

        let tmp = &bs.fields[0].vis;
        let left = quote! { #tmp };
        let right = quote! { pub };
        assert_eq_tt!(left, right);

        let tmp = &bs.fields[1].vis;
        let left = quote! { #tmp };
        let right = quote! { pub(super) };
        assert_eq_tt!(left, right);

        let tmp = &bs.fields[2].vis;
        let left = quote! { #tmp };
        let right = quote! {};
        assert_eq_tt!(left, right);

        let tmp = &bs.fields[3].vis;
        let left = quote! { #tmp };
        let right = quote! { pub(crate) };
        assert_eq_tt!(left, right);

        let tmp = &bs.fields[4].vis;
        let left = quote! { #tmp };
        let right = quote! {};
        assert_eq_tt!(left, right);

        let tmp = &bs.fields[5].vis;
        let left = quote! { #tmp };
        let right = quote! {};
        assert_eq_tt!(left, right);
    }
}
