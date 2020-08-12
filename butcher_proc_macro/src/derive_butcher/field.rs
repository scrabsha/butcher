use std::{collections::HashSet, iter};

use syn::{
    parse::{Parse, ParseStream},
    AngleBracketedGenericArguments, Attribute, GenericArgument, Ident, Lifetime, PathArguments,
    QSelf, Result as SynResult, ReturnType, Token, Type, TypeArray, TypeBareFn, TypeGroup,
    TypeParen, TypePath, TypePtr, TypeReference, TypeSlice, TypeTuple, Visibility,
};

use quote::{quote, ToTokens};

use proc_macro2::TokenStream;

use crate::{
    derive_butcher::{
        utils::{FindGenerics, ReplaceSelf},
        DeriveError,
    },
    utils::{self, FieldName},
};

fn cow() -> TokenStream {
    quote! { std::borrow::Cow }
}

fn butcher_field() -> TokenStream {
    quote! { butcher::methods::ButcherField }
}

fn phantom() -> TokenStream {
    quote! { std::marker::PhantomData }
}

pub(super) struct Field {
    pub name: FieldName,
    pub method: ButcheringMethod,
    pub vis: Visibility,
    pub ty: Type,
    pub associated_generics: Vec<Ident>,
    pub associated_lifetimes: Vec<Lifetime>,
    additional_traits: Option<TokenStream>,
}

impl Field {
    pub(super) fn from(
        input: syn::Field,
        generic_types: &HashSet<Ident>,
        lifetimes: &HashSet<Lifetime>,
        id: usize,
        main_struct_type: &Type,
    ) -> Result<Field, syn::Error> {
        let FieldMetadata(method, additional_traits) = parse_meta_attrs(input.attrs.as_slice())?;

        let vis = input.vis;

        let name = input
            .ident
            .map(FieldName::from)
            .unwrap_or_else(|| FieldName::Unnamed(id));

        let mut ty = input.ty;
        // Remove Self from ty, because the type referenced by Self changes
        // between contexts.
        ty.replace(main_struct_type);

        let mut associated_generics = ty.find_generics(generic_types);
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

    pub(super) fn expand_to_code(&self, main_struct_name: &Ident, lt: &TokenStream) -> TokenStream {
        let associated_struct = self.associated_struct_declaration(main_struct_name);
        let associated_trait = self.butcher_field_implementation(main_struct_name, lt);

        quote! {
            #associated_struct
            #associated_trait
        }
    }

    pub(super) fn associated_struct_declaration(&self, main_struct_name: &Ident) -> TokenStream {
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

    pub(super) fn associated_struct_with_generics(&self, main_struct_name: &Ident) -> TokenStream {
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

    pub(super) fn butcher_field_implementation(
        &self,
        main_struct_name: &Ident,
        lt: &TokenStream,
    ) -> TokenStream {
        let butcher_field = butcher_field();
        let struct_with_generics = self.associated_struct_with_generics(main_struct_name);

        let generic_types = self.associated_generics.as_slice();
        let lifetimes = self.associated_lifetimes.as_slice();

        let where_clause = self.where_clause_trait(&lt);

        let input_type = &self.ty;

        let method = self.method.associated_method_name();

        quote! {
            impl
                <#lt, #( #lifetimes, )* #( #generic_types ),*>
                #butcher_field<#lt, #input_type> for #struct_with_generics
                #where_clause
            {
                type Method = #method;
            }
        }
    }

    pub(super) fn where_clause_items<'a>(
        &'a self,
        lt: &'a TokenStream,
    ) -> impl Iterator<Item = TokenStream> + 'a {
        let required_by_method = self.method.required_traits_for(&self.ty, lt);

        let bounds_for_generic_types = self
            .associated_generics
            .iter()
            .map(move |t| quote! { #t: #lt });
        let bounds_for_lifetimes = self
            .associated_lifetimes
            .iter()
            .map(move |l| quote! { #l: #lt });

        iter::once(required_by_method)
            .chain(bounds_for_generic_types)
            .chain(bounds_for_lifetimes)
            .chain(self.additional_traits.clone())
    }

    fn where_clause_trait(&self, lt: &TokenStream) -> TokenStream {
        let items = self.where_clause_items(lt);

        quote! {
            where
                #( #items ),*
        }
    }

    fn output_type_unwrapped(&self, lt: &TokenStream) -> TokenStream {
        self.method.output_type_unwrapped(&self.ty, lt)
    }

    pub(super) fn associated_main_struct_data(
        &self,
        lt: &TokenStream,
    ) -> (&FieldName, TokenStream, &Visibility) {
        (&self.name, self.output_type_unwrapped(lt), &self.vis)
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
        [] => Ok(FieldMetadata(ButcheringMethod::Regular, None)),
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
struct FieldMetadata(ButcheringMethod, Option<TokenStream>);

impl Parse for FieldMetadata {
    fn parse(input: ParseStream) -> SynResult<Self> {
        let method = input.parse::<ButcheringMethod>()?;

        let traits = if input.is_empty() {
            None
        } else {
            let _ = input.parse::<Token![,]>()?;
            Some(input.parse::<TokenStream>()?)
        };

        Ok(FieldMetadata(method, traits))
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
pub(super) enum ButcheringMethod {
    Copy,
    AsDeref,
    Rebutcher,
    Regular,
    Unbox,
}

impl ButcheringMethod {
    fn required_traits_for(&self, ty: &Type, lt: &TokenStream) -> TokenStream {
        match self {
            ButcheringMethod::Copy => quote! { #ty: Clone },
            ButcheringMethod::AsDeref => {
                quote! { #ty: Into<<<#ty as std::ops::Deref>::Target as ToOwned>::Owned> }
            }
            ButcheringMethod::Rebutcher => {
                quote! {
                    #ty : Butcher< #lt > + ToOwned<Owned = #ty > + #lt,
                    <#ty as butcher::Butcher< #lt >>:: Output: Clone
                }
            }
            ButcheringMethod::Regular => quote! { #ty: Clone },
            ButcheringMethod::Unbox => quote! { <#ty as std::ops::Deref>::Target: Clone },
        }
    }

    fn output_type_unwrapped(&self, ty: &Type, lt: &TokenStream) -> TokenStream {
        match self {
            ButcheringMethod::Copy => quote! { #ty },
            ButcheringMethod::AsDeref | ButcheringMethod::Unbox => {
                let cow = cow();
                quote! { #cow < #lt , <#ty as std::ops::Deref>::Target > }
            }
            ButcheringMethod::Rebutcher => {
                quote! { < #ty as Butcher< #lt >>::Output }
            }
            ButcheringMethod::Regular => {
                let cow = cow();
                quote! { #cow < #lt , #ty > }
            }
        }
    }

    fn associated_method_name(self) -> TokenStream {
        match self {
            ButcheringMethod::Copy => quote! { butcher::methods::Copy },
            ButcheringMethod::AsDeref => quote! { butcher::methods::AsDeref },
            ButcheringMethod::Rebutcher => quote! { butcher::methods::Rebutcher },
            ButcheringMethod::Regular => quote! { butcher::methods::Regular },
            ButcheringMethod::Unbox => quote! { butcher::methods::Unbox },
        }
    }
}

impl Parse for ButcheringMethod {
    fn parse(input: ParseStream) -> SynResult<Self> {
        let i = input.parse::<Ident>()?;

        if i == "copy" {
            Ok(ButcheringMethod::Copy)
        } else if i == "as_deref" {
            Ok(ButcheringMethod::AsDeref)
        } else if i == "rebutcher" {
            Ok(ButcheringMethod::Rebutcher)
        } else if i == "regular" {
            Ok(ButcheringMethod::Regular)
        } else if i == "unbox" {
            Ok(ButcheringMethod::Unbox)
        } else {
            Err(syn::Error::new_spanned(i, DeriveError::UnknownMethod))
        }
    }
}
