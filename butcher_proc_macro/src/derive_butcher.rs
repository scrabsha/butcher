use std::{
    collections::HashSet,
    error::Error,
    fmt::{self, Display},
};

use syn::{
    parse::{Parse, ParseStream},
    AngleBracketedGenericArguments, Attribute, Data, DeriveInput, Fields, GenericArgument,
    GenericParam, Ident, Lifetime, PathArguments, QSelf, Result as SynResult, ReturnType, Token,
    Type, TypeArray, TypeBareFn, TypeGroup, TypeParen, TypePath, TypePtr, TypeReference, TypeSlice,
    TypeTuple, Visibility,
};

use quote::{quote, ToTokens};

use proc_macro2::TokenStream;

use crate::utils::{self, FieldName};

#[derive(Debug, PartialEq)]
pub enum DeriveError {
    FoundUnion,
    FoundUnitStruct,
    // TODO: remove this, handle enums
    FoundEnum,
    MultipleButcheringMethod,
    FoundImplTrait,
    FoundMacroAsType,
    FoundTraitObject,
    UnknownMethod,
}

impl Display for DeriveError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DeriveError::FoundUnion => "Butcher does not support unions",
            DeriveError::FoundUnitStruct => "Butchering is useless for unit structs",
            DeriveError::FoundEnum => {
                "Butcher currently does not support enums. This is planned for next release"
            }
            DeriveError::MultipleButcheringMethod => {
                "Multiple butchering method provided. Choose one!"
            }
            DeriveError::FoundImplTrait => "Butcher does not support impl Trait",
            DeriveError::FoundMacroAsType => "Butcher does not support macro as type",
            DeriveError::FoundTraitObject => "Butcher does not support trait objects",
            DeriveError::UnknownMethod => "Unknown butchering method",
        }
        .fmt(f)
    }
}

impl Error for DeriveError {}

#[inline]
fn cow() -> TokenStream {
    quote! { std::borrow::Cow }
}

#[inline]
fn borrowed() -> TokenStream {
    let cow = cow();
    quote! { #cow::Borrowed }
}

#[inline]
fn owned() -> TokenStream {
    let cow = cow();
    quote! { #cow::Owned }
}

#[inline]
fn phantom() -> TokenStream {
    quote! { std::marker::PhantomData }
}

#[inline]
fn butcher_field() -> TokenStream {
    quote! { ButcherField }
}

pub(super) struct ButcheredStruct {
    name: Ident,
    fields: Vec<Field>,
}

impl ButcheredStruct {
    pub(super) fn from(input: DeriveInput) -> Result<ButcheredStruct, syn::Error> {
        let name = input.ident;
        let data = match input.data {
            Data::Struct(d) => Ok(d),
            Data::Enum(de) => Err((DeriveError::FoundEnum, de.enum_token.span)),
            Data::Union(du) => Err((DeriveError::FoundUnion, du.union_token.span)),
        }
        .map_err(|(e, s)| syn::Error::new(s, e))?;

        let fields = match data.fields {
            Fields::Named(fields) => Ok(fields.named),
            Fields::Unnamed(fields) => Ok(fields.unnamed),
            Fields::Unit => Err((DeriveError::FoundUnitStruct, name.span())),
        }
        .map_err(|(e, s)| syn::Error::new(s, e))?;

        let mut generic_types = HashSet::new();
        let mut lifetimes = HashSet::new();

        input.generics.params.into_iter().for_each(|g| match g {
            GenericParam::Type(t) => {
                generic_types.insert(t.ident);
            }
            GenericParam::Lifetime(lt) => {
                lifetimes.insert(lt.lifetime);
            }
            GenericParam::Const(_) => {}
        });

        let fields = fields
            .into_iter()
            .enumerate()
            .map(|(id, f)| Field::from(f, &generic_types, &lifetimes, id))
            .fold(Ok(Vec::new()), |acc, res| match (acc, res) {
                (Ok(mut main), Ok(v)) => {
                    main.push(v);
                    Ok(main)
                }
                (Ok(_), Err(e)) => Err(e),
                (Err(mut main_err), Err(e)) => {
                    main_err.combine(e);
                    Err(main_err)
                }
                (tmp @ Err(_), Ok(_)) => tmp,
            })?;

        Ok(ButcheredStruct { name, fields })
    }

    pub(super) fn expand_to_code(self) -> TokenStream {
        let fields_expansion = self.fields.iter().map(|f| f.expand_to_code(&self.name));

        quote! { #( #fields_expansion )* }
    }
}

struct Field {
    name: FieldName,
    method: ButcheringMethod,
    vis: Visibility,
    ty: Type,
    associated_generics: Vec<Ident>,
    associated_lifetimes: Vec<Lifetime>,
    additional_traits: TokenStream,
}

impl Field {
    fn from(
        input: syn::Field,
        generic_types: &HashSet<Ident>,
        lifetimes: &HashSet<Lifetime>,
        id: usize,
    ) -> Result<Field, syn::Error> {
        let FieldMetadata(method, additional_traits) = parse_meta_attrs(input.attrs.as_slice())?;

        let vis = input.vis;

        let name = input
            .ident
            .map(FieldName::from)
            .unwrap_or_else(|| FieldName::Unnamed(id));

        let ty = input.ty;

        let mut associated_generics = find_generics_in_type(&ty, generic_types)?;
        let mut associated_lifetimes = find_lifetimes_in_type(&ty, lifetimes)?;

        associated_generics.sort_unstable();
        associated_lifetimes.sort_unstable();

        associated_generics.dedup();
        associated_lifetimes.dedup();

        Ok(Field {
            vis,
            method,
            name,
            ty,
            associated_generics,
            associated_lifetimes,
            additional_traits,
        })
    }

    fn expand_to_code(&self, main_struct_name: &Ident) -> TokenStream {
        let associated_struct = self.associated_struct_declaration(main_struct_name);
        let associated_trait = self.butcher_field_implementation(main_struct_name);

        quote! {
            #associated_struct
            #associated_trait
        }
    }

    fn associated_struct_declaration(&self, main_struct_name: &Ident) -> TokenStream {
        let vis = &self.vis;
        let struct_with_generics = self.associated_struct_with_generics(main_struct_name);

        let types_in_phantom = self.associated_generics.iter();
        let lifetimes_in_phantom = self.associated_lifetimes_in_phantom();

        let phantom = phantom();

        quote! {
            #[allow(non_camel_case_types)]
            #vis struct #struct_with_generics
                (
                    #phantom< ( #( #types_in_phantom, )* ) > ,
                    #phantom< ( #( #lifetimes_in_phantom, )* ) > ,
                );
        }
    }

    fn associated_struct_with_generics(&self, main_struct_name: &Ident) -> TokenStream {
        let struct_name = self.associated_struct_name(main_struct_name);

        let lifetimes_declaration = self.associated_lifetimes.as_slice();
        let generics_declaration = self.associated_generics.as_slice();

        quote! { #struct_name < #( #lifetimes_declaration, )* #( #generics_declaration, )* > }
    }

    fn associated_struct_name(&self, main_struct_name: &Ident) -> Ident {
        utils::associated_struct_name(main_struct_name, &self.name)
    }

    fn associated_lifetimes_in_phantom(&self) -> impl Iterator<Item = impl ToTokens> + '_ {
        self.associated_lifetimes
            .iter()
            .map(|lt| quote! { & #lt () })
    }

    fn butcher_field_implementation(&self, main_struct_name: &Ident) -> TokenStream {
        let butcher_field = butcher_field();
        let struct_with_generics = self.associated_struct_with_generics(main_struct_name);
        let lt = quote! { 'cow };

        let generic_types = self.associated_generics.as_slice();
        let lifetimes = self.associated_lifetimes.as_slice();

        let where_clause = self.where_clause_trait(&lt);

        let input_type = self.input_type();
        let output_type = self.output_type(&lt);

        let borrowed_function = self.method.expand_borrowed_function(&lt);
        let owned_function = self.method.expand_owned_function();

        quote! {
            impl
                <#lt, #( #lifetimes, )* #( #generic_types ),*>
                #butcher_field<#lt> for #struct_with_generics
                #where_clause
            {
                #input_type
                #output_type

                #borrowed_function
                #owned_function
            }
        }
    }

    fn where_clause_trait(&self, lt: &TokenStream) -> TokenStream {
        let required_by_method = self.method.required_traits_for(&self.ty);

        let bounds_for_generic_types = self.associated_generics.iter().map(|t| quote! { #t: #lt });
        let bounds_for_lifetimes = self.associated_lifetimes.iter().map(|l| quote! { #l: #lt });
        let generics = bounds_for_generic_types.chain(bounds_for_lifetimes);

        let user_provided_bounds = &self.additional_traits;

        quote! {
            where
                #required_by_method,
                #( #generics, )*
                #user_provided_bounds
        }
    }

    fn input_type(&self) -> TokenStream {
        let ty = &self.ty;
        quote! { type Input = #ty; }
    }

    fn output_type(&self, lt: &TokenStream) -> TokenStream {
        self.method.output_type_for(&self.ty, lt)
    }
}

fn parse_meta_attrs(input: &[Attribute]) -> Result<FieldMetadata, syn::Error> {
    let methods = input
        .iter()
        .map(parse_meta_attr)
        .flatten()
        .collect::<Result<Vec<_>, _>>()?;

    match methods.as_slice() {
        [(_, metadata)] => Ok(metadata.clone()),
        [] => Ok(FieldMetadata(ButcheringMethod::Regular, TokenStream::new())),
        [.., (last, _)] => Err(syn::Error::new_spanned(
            last,
            DeriveError::MultipleButcheringMethod,
        )),
    }
}

fn parse_meta_attr(attr: &Attribute) -> Option<Result<(&Attribute, FieldMetadata), syn::Error>> {
    if !attr.path.is_ident("butcher") {
        return None;
    }

    Some(attr.parse_args::<FieldMetadata>().map(|md| (attr, md)))
}

#[derive(Clone, Debug)]
struct FieldMetadata(ButcheringMethod, TokenStream);

impl Parse for FieldMetadata {
    fn parse(input: ParseStream) -> SynResult<Self> {
        let method = input.parse::<ButcheringMethod>()?;

        let traits = if input.is_empty() {
            TokenStream::new()
        } else {
            let _ = input.parse::<Token![,]>()?;
            input.parse::<TokenStream>()?
        };

        Ok(FieldMetadata(method, traits))
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

        Type::Tuple(TypeTuple { elems, .. }) => elems
            .into_iter()
            .map(|ty| find_generics_in_type(ty, generics))
            .try_fold(Vec::new(), extend_discovered),

        Type::BareFn(TypeBareFn { inputs, output, .. }) => {
            let mut found_generics = inputs
                .into_iter()
                .map(|arg| find_generics_in_type(&arg.ty, generics))
                .try_fold(Vec::new(), extend_discovered)?;

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
                    GenericArgument::Type(t) => Some(t),
                    _ => None,
                })
                .map(|t| find_generics_in_type(t, generics))
                .try_fold(Vec::new(), extend_discovered)?;

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

fn find_lifetimes_in_type(ty: &Type, lts: &HashSet<Lifetime>) -> Result<Vec<Lifetime>, syn::Error> {
    match ty {
        Type::Array(TypeArray { elem, .. })
        | Type::Group(TypeGroup { elem, .. })
        | Type::Paren(TypeParen { elem, .. })
        | Type::Ptr(TypePtr { elem, .. })
        | Type::Slice(TypeSlice { elem, .. }) => find_lifetimes_in_type(elem.as_ref(), lts),

        Type::Reference(TypeReference { lifetime, elem, .. }) => {
            let mut found_lifetimes = match lifetime {
                Some(lt) if lts.contains(lt) => vec![lt.clone()],
                Some(_) | None => Vec::new(),
            };

            found_lifetimes.extend(find_lifetimes_in_type(elem.as_ref(), lts)?);

            Ok(found_lifetimes)
        }

        Type::Tuple(TypeTuple { elems, .. }) => elems
            .into_iter()
            .map(|ty| find_lifetimes_in_type(ty, lts))
            .try_fold(Vec::new(), extend_discovered),

        Type::BareFn(TypeBareFn { inputs, output, .. }) => {
            let mut found_lifetimes = inputs
                .into_iter()
                .map(|arg| find_lifetimes_in_type(&arg.ty, lts))
                .try_fold(Vec::new(), extend_discovered)?;

            if let ReturnType::Type(_, ty) = output {
                found_lifetimes.extend(find_lifetimes_in_type(ty.as_ref(), lts)?);
            }

            Ok(found_lifetimes)
        }

        Type::Path(TypePath { path, qself }) => {
            let mut found_lifetimes = path
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
                    GenericArgument::Lifetime(lt) if lts.contains(lt) => Some(Ok(vec![lt.clone()])),
                    GenericArgument::Type(ty) => Some(find_lifetimes_in_type(ty, lts)),
                    _ => None,
                })
                .try_fold(Vec::new(), extend_discovered)?;

            if let Some(QSelf { ty, .. }) = qself {
                found_lifetimes.extend(find_lifetimes_in_type(ty.as_ref(), lts)?);
            }

            Ok(found_lifetimes)
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

fn extend_discovered<T>(
    mut discovered: Vec<T>,
    to_add: Result<Vec<T>, syn::Error>,
) -> Result<Vec<T>, syn::Error> {
    let to_add = to_add?;
    discovered.extend(to_add);
    Ok(discovered)
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum ButcheringMethod {
    Copy,
    Flatten,
    Regular,
    Unbox,
}

impl ButcheringMethod {
    fn required_traits_for(&self, ty: &Type) -> TokenStream {
        match self {
            ButcheringMethod::Copy => quote! { #ty: Clone },
            ButcheringMethod::Flatten => {
                quote! { #ty: Into<<<#ty as std::ops::Deref>::Target as ToOwned>::Owned> }
            }
            ButcheringMethod::Regular => quote! { #ty: Clone },
            ButcheringMethod::Unbox => quote! { <#ty as std::ops::Deref>::Target: Clone },
        }
    }

    fn output_type_for(&self, ty: &Type, lt: &TokenStream) -> TokenStream {
        let ty = match self {
            ButcheringMethod::Copy => quote! { #ty },
            ButcheringMethod::Flatten | ButcheringMethod::Unbox => {
                let cow = cow();
                quote! { #cow < #lt , <Self::Input as std::ops::Deref>::Target > }
            }
            ButcheringMethod::Regular => {
                let cow = cow();
                quote! { #cow < #lt , #ty > }
            }
        };

        quote! { type Output = #ty; }
    }

    fn expand_borrowed_function(&self, lt: impl ToTokens) -> TokenStream {
        let var_name = quote! { b };

        let borrowed = borrowed();

        let function_body = match self {
            ButcheringMethod::Copy => quote! { #var_name.clone() },
            ButcheringMethod::Flatten => quote! { #borrowed(std::ops::Deref::deref(#var_name)) },
            ButcheringMethod::Regular => quote! { #borrowed(#var_name) },
            ButcheringMethod::Unbox => quote! { #borrowed(std::ops::Deref::deref(#var_name)) },
        };

        quote! {
            fn from_borrowed(#var_name: & #lt Self::Input) -> Self::Output {
                #function_body
            }
        }
    }

    fn expand_owned_function(&self) -> TokenStream {
        let var_name = quote! { o };

        let owned = owned();

        let function_body = match self {
            ButcheringMethod::Copy => quote! { #var_name },
            ButcheringMethod::Flatten => quote! { #owned(#var_name.into()) },
            ButcheringMethod::Regular => quote! { #owned(#var_name) },
            ButcheringMethod::Unbox => quote! { #owned(*#var_name) },
        };

        quote! {
            fn from_owned(#var_name: Self::Input) -> Self::Output {
                #function_body
            }
        }
    }
}

impl Parse for ButcheringMethod {
    fn parse(input: ParseStream) -> SynResult<Self> {
        let i = input.parse::<Ident>()?;

        if i == "copy" {
            Ok(ButcheringMethod::Copy)
        } else if i == "flatten" {
            Ok(ButcheringMethod::Flatten)
        } else if i == "regular" {
            Ok(ButcheringMethod::Regular)
        } else if i == "unbox" {
            Ok(ButcheringMethod::Unbox)
        } else {
            Err(syn::Error::new_spanned(i, DeriveError::UnknownMethod))
        }
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
                #[butcher(flatten)]
                pub(super) b: String,
                c: T,
                #[butcher(unbox)]
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
        assert_eq!(bs.fields[1].method, ButcheringMethod::Flatten);
        assert!(bs.fields[4].associated_generics.is_empty());

        assert_eq!(bs.fields[2].name, "c");
        assert_eq!(bs.fields[2].method, ButcheringMethod::Regular);
        assert_eq!(bs.fields[2].associated_generics, &["T"]);

        assert_eq!(bs.fields[3].name, "d");
        assert_eq!(bs.fields[3].method, ButcheringMethod::Unbox);
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
        let right = TokenStream::new();
        assert_eq_tt!(left, right);

        let tmp = &bs.fields[3].vis;
        let left = quote! { #tmp };
        let right = quote! { pub(crate) };
        assert_eq_tt!(left, right);

        let tmp = &bs.fields[4].vis;
        let left = quote! { #tmp };
        let right = TokenStream::new();
        assert_eq_tt!(left, right);

        let tmp = &bs.fields[5].vis;
        let left = quote! { #tmp };
        let right = TokenStream::new();
        assert_eq_tt!(left, right);
    }
}

#[cfg(test)]
mod field {
    use super::*;

    use syn::parse_quote;

    #[test]
    fn associated_struct_declaration() {
        let s: DeriveInput = parse_quote! {
            #[derive(Butcher)]
            struct Foo {
                a: usize,
            }
        };
        let bs = ButcheredStruct::from(s).unwrap();
        let left = bs.fields[0].associated_struct_declaration(&bs.name);
        let right = quote! {
            #[allow(non_camel_case_types)]
            struct ButcherFooa<>(
                std::marker::PhantomData<()>,
                std::marker::PhantomData<()>,
            );
        };
        assert_eq_tt!(left, right);

        let s: DeriveInput = parse_quote! {
            #[derive(Butcher)]
            struct Foo<T> {
                a: T,
            }
        };
        let bs = ButcheredStruct::from(s).unwrap();
        let left = bs.fields[0].associated_struct_declaration(&bs.name);
        let right = quote! {
            #[allow(non_camel_case_types)]
            struct ButcherFooa<T,>(
                std::marker::PhantomData<(T,)>,
                std::marker::PhantomData<()>,
            );
        };
        assert_eq_tt!(left, right);

        let s: DeriveInput = parse_quote! {
            #[derive(Butcher)]
            struct Foo<'a> {
                a: A<'a>,
            }
        };
        let bs = ButcheredStruct::from(s).unwrap();
        let left = bs.fields[0].associated_struct_declaration(&bs.name);
        let right = quote! {
            #[allow(non_camel_case_types)]
            struct ButcherFooa<'a,>(
                std::marker::PhantomData<()>,
                std::marker::PhantomData<(&'a (),)>,
            );
        };
        assert_eq_tt!(left, right);

        let s: DeriveInput = parse_quote! {
            #[derive(Butcher)]
            struct Foo<'a, T,> {
                a: &'a T,
            }
        };
        let bs = ButcheredStruct::from(s).unwrap();
        let left = bs.fields[0].associated_struct_declaration(&bs.name);
        let right = quote! {
            #[allow(non_camel_case_types)]
            struct ButcherFooa<'a, T,>(
                std::marker::PhantomData<(T,)>,
                std::marker::PhantomData<(&'a (),)>,
            );
        };
        assert_eq_tt!(left, right);
    }

    #[test]
    fn butcher_field_implementation_regular() {
        let s: DeriveInput = parse_quote! {
            #[derive(Butcher)]
            struct Foo {
                a: usize,
            }
        };
        let bs = ButcheredStruct::from(s).unwrap();
        let left = bs.fields[0].butcher_field_implementation(&bs.name);
        let right = quote! {
            impl<'cow,> ButcherField<'cow> for ButcherFooa<>
            where
                usize: Clone,
            {
                type Input = usize;
                type Output = std::borrow::Cow<'cow, usize>;

                fn from_borrowed(b: &'cow Self::Input) -> Self::Output {
                    std::borrow::Cow::Borrowed(b)
                }

                fn from_owned(o: Self::Input) -> Self::Output {
                    std::borrow::Cow::Owned(o)
                }
            }
        };
        assert_eq_tt!(left, right);

        let s: DeriveInput = parse_quote! {
            #[derive(Butcher)]
            struct Foo<'a> {
                a: &'a usize,
            }
        };
        let bs = ButcheredStruct::from(s).unwrap();
        let left = bs.fields[0].butcher_field_implementation(&bs.name);
        let right = quote! {
            impl<'cow, 'a,> ButcherField<'cow> for ButcherFooa<'a,>
            where
            &'a usize: Clone,
                'a: 'cow,
            {
                type Input = &'a usize;
                type Output = std::borrow::Cow<'cow, &'a usize>;

                fn from_borrowed(b: &'cow Self::Input) -> Self::Output {
                    std::borrow::Cow::Borrowed(b)
                }

                fn from_owned(o: Self::Input) -> Self::Output {
                    std::borrow::Cow::Owned(o)
                }
            }
        };
        assert_eq_tt!(left, right);

        let s: DeriveInput = parse_quote! {
            #[derive(Butcher)]
            struct Foo<T> {
                a: T,
            }
        };
        let bs = ButcheredStruct::from(s).unwrap();
        let left = bs.fields[0].butcher_field_implementation(&bs.name);
        let right = quote! {
            impl<'cow, T> ButcherField<'cow> for ButcherFooa<T,>
            where
                T: Clone,
                T: 'cow,
            {
                type Input = T;
                type Output = std::borrow::Cow<'cow, T>;

                fn from_borrowed(b: &'cow Self::Input) -> Self::Output {
                    std::borrow::Cow::Borrowed(b)
                }

                fn from_owned(o: Self::Input) -> Self::Output {
                    std::borrow::Cow::Owned(o)
                }
            }
        };
        assert_eq_tt!(left, right);
    }

    #[test]
    fn butcher_field_implementation_copy() {
        let s: DeriveInput = parse_quote! {
            #[derive(Butcher)]
            struct Foo {
                #[butcher(copy)]
                a: usize,
            }
        };
        let bs = ButcheredStruct::from(s).unwrap();
        let left = bs.fields[0].butcher_field_implementation(&bs.name);
        let right = quote! {
            impl<'cow,> ButcherField<'cow> for ButcherFooa<>
            where
                usize: Clone,
            {
                type Input = usize;
                type Output = usize;

                fn from_borrowed(b: &'cow Self::Input) -> Self::Output {
                    b.clone()
                }

                fn from_owned(o: Self::Input) -> Self::Output {
                    o
                }
            }
        };
        assert_eq_tt!(left, right);

        let s: DeriveInput = parse_quote! {
            #[derive(Butcher)]
            struct Foo<'a> {
                #[butcher(copy)]
                a: &'a usize,
            }
        };
        let bs = ButcheredStruct::from(s).unwrap();
        let left = bs.fields[0].butcher_field_implementation(&bs.name);
        let right = quote! {
            impl<'cow, 'a,> ButcherField<'cow> for ButcherFooa<'a,>
            where
                &'a usize: Clone,
                'a: 'cow,
            {
                type Input = &'a usize;
                type Output = &'a usize;

                fn from_borrowed(b: &'cow Self::Input) -> Self::Output {
                    b.clone()
                }

                fn from_owned(o: Self::Input) -> Self::Output {
                    o
                }
            }
        };
        assert_eq_tt!(left, right);

        let s: DeriveInput = parse_quote! {
            #[derive(Butcher)]
            struct Foo<T> {
                #[butcher(copy)]
                a: T,
            }
        };
        let bs = ButcheredStruct::from(s).unwrap();
        let left = bs.fields[0].butcher_field_implementation(&bs.name);
        let right = quote! {
            impl<'cow, T> ButcherField<'cow> for ButcherFooa<T,>
            where
                T: Clone,
                T: 'cow,
            {
                type Input = T;
                type Output = T;

                fn from_borrowed(b: &'cow Self::Input) -> Self::Output {
                    b.clone()
                }

                fn from_owned(o: Self::Input) -> Self::Output {
                    o
                }
            }
        };
        assert_eq_tt!(left, right);
    }

    #[test]
    fn butcher_field_implementation_dereferenced() {
        let s: DeriveInput = parse_quote! {
            #[derive(Butcher)]
            struct Foo {
                #[butcher(unbox)]
                a: Box<usize>,
            }
        };
        let bs = ButcheredStruct::from(s).unwrap();
        let left = bs.fields[0].butcher_field_implementation(&bs.name);
        let right = quote! {
            impl<'cow,> ButcherField<'cow> for ButcherFooa<>
            where
                <Box<usize> as std::ops::Deref>::Target: Clone,
            {
                type Input = Box<usize>;
                type Output = std::borrow::Cow<'cow, <Self::Input as std::ops::Deref>::Target>;

                fn from_borrowed(b: &'cow Self::Input) -> Self::Output {
                    std::borrow::Cow::Borrowed(std::ops::Deref::deref(b))
                }

                fn from_owned(o: Self::Input) -> Self::Output {
                    std::borrow::Cow::Owned(*o)
                }
            }
        };
        assert_eq_tt!(left, right);

        let s: DeriveInput = parse_quote! {
            #[derive(Butcher)]
            struct Foo<'a> {
                #[butcher(unbox)]
                a: Box<&'a usize>,
            }
        };
        let bs = ButcheredStruct::from(s).unwrap();
        assert_eq!(bs.fields[0].associated_lifetimes.len(), 1);
        let left = bs.fields[0].butcher_field_implementation(&bs.name);
        let right = quote! {
            impl<'cow, 'a,> ButcherField<'cow> for ButcherFooa<'a,>
            where
                <Box<&'a usize> as std::ops::Deref>::Target: Clone,
                'a: 'cow,
            {
                type Input = Box<&'a usize>;
                type Output = std::borrow::Cow<'cow, <Self::Input as std::ops::Deref>::Target>;

                fn from_borrowed(b: &'cow Self::Input) -> Self::Output {
                    std::borrow::Cow::Borrowed(std::ops::Deref::deref(b))
                }

                fn from_owned(o: Self::Input) -> Self::Output {
                    std::borrow::Cow::Owned(*o)
                }
            }
        };
        assert_eq_tt!(left, right);

        let s: DeriveInput = parse_quote! {
            #[derive(Butcher)]
            struct Foo<T> {
                #[butcher(unbox)]
                a: Box<T>,
            }
        };
        let bs = ButcheredStruct::from(s).unwrap();
        let left = bs.fields[0].butcher_field_implementation(&bs.name);
        let right = quote! {
            impl<'cow, T> ButcherField<'cow> for ButcherFooa<T,>
            where
                <Box<T> as std::ops::Deref>::Target: Clone,
                T: 'cow,
            {
                type Input = Box<T>;
                type Output = std::borrow::Cow<'cow, <Self::Input as std::ops::Deref>::Target>;

                fn from_borrowed(b: &'cow Self::Input) -> Self::Output {
                    std::borrow::Cow::Borrowed(std::ops::Deref::deref(b))
                }

                fn from_owned(o: Self::Input) -> Self::Output {
                    std::borrow::Cow::Owned(*o)
                }
            }
        };
        assert_eq_tt!(left, right);
    }
}
