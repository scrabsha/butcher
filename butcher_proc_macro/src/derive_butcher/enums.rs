use std::{collections::HashSet, iter};

use syn::{
    punctuated::Punctuated, Data, DeriveInput, Fields, GenericParam, Ident, Lifetime, LifetimeDef,
    TypeParam, Variant as SVariant, Visibility, WhereClause,
};

use proc_macro2::TokenStream;

use quote::quote;

use super::{field::Field, structs::StructKind};

use crate::utils;

pub(super) struct ButcheredEnum {
    name: Ident,
    vis: Visibility,
    variants: Vec<Variant>,
    generics_for_butchered: Vec<GenericParam>,
    where_clause_for_butchered: Option<WhereClause>,
}

impl ButcheredEnum {
    pub(super) fn from(input: DeriveInput) -> Result<ButcheredEnum, syn::Error> {
        let name = input.ident;
        let vis = input.vis;

        let generics_for_butchered = input.generics.params.iter().cloned().collect::<Vec<_>>();

        let where_clause_for_butchered = input.generics.where_clause;

        let data = match input.data {
            Data::Enum(d) => d,
            // This should have been filtered previously
            Data::Struct(_) | Data::Union(_) => unreachable!(),
        };

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

        let variants = data
            .variants
            .into_iter()
            .map(|v| Variant::from(v, &generic_types, &lifetimes))
            .fold(Ok(Vec::new()), |acc, res| match (acc, res) {
                (Ok(mut xs), Ok(x)) => {
                    xs.push(x);
                    Ok(xs)
                }
                (Ok(_), Err(e)) => Err(e),
                (Err(mut main_err), Err(e)) => {
                    main_err.combine(e);
                    Err(main_err)
                }
                (tmp @ Err(_), Ok(_)) => tmp,
            })?;

        Ok(ButcheredEnum {
            name,
            vis,
            variants,
            generics_for_butchered,
            where_clause_for_butchered,
        })
    }

    pub(super) fn expand_to_code(self) -> TokenStream {
        let lt = quote! { 'cow };
        let enum_declaration = self.expand_enum_declaration(&lt);
        let butcher_implementation = todo!();

        quote! {
            #enum_declaration
            // #butcher_implementation
        }
    }

    fn expand_enum_declaration(&self, lt: &TokenStream) -> TokenStream {
        let vis = &self.vis;
        let where_clause = &self.where_clause_for_butchered;
        let name_with_generics = self.enum_name_with_generics_declaration(lt);

        let variants = self.variants.iter().map(|v| v.expand_in_enum(lt));

        quote! {
            #vis enum #name_with_generics
            #where_clause
            {
                #( #variants ),*
            }
        }
    }

    fn enum_name_with_generics_declaration(&self, lt: &TokenStream) -> TokenStream {
        let name = utils::global_associated_struct_name(&self.name);
        let generics = self.generics_for_butchered.iter().map(|g| quote! { #g });

        quote! { #name < #lt #( , #generics )* > }
    }

    fn enum_name_with_generics(&self, lt: &TokenStream) -> TokenStream {
        let name = utils::global_associated_struct_name(&self.name);

        let generics = self.generics_for_butchered.iter().map(|g| match g {
            GenericParam::Type(TypeParam { ident, .. }) => quote! { #ident },
            GenericParam::Lifetime(LifetimeDef { lifetime, .. }) => quote! { #lifetime },
            // TODO: find how this should be done.
            GenericParam::Const(_) => unimplemented!(),
        });

        quote! { #name < #lt #( , #generics )* > }
    }
}

struct Variant {
    name: Ident,
    // TODO: discuss with syn author about visibility of variants
    // Link in the references:
    // https://doc.rust-lang.org/reference/items/enumerations.html
    // vis: Visibility,
    kind: VariantKind,
    fields: Vec<Field>,
}

impl Variant {
    fn from(
        v: SVariant,
        generic_types: &HashSet<Ident>,
        lifetimes: &HashSet<Lifetime>,
    ) -> Result<Variant, syn::Error> {
        let name = v.ident;

        let (fields, kind) = match v.fields {
            Fields::Named(fs) => (punctuated_to_vector(fs.named), VariantKind::Named),
            Fields::Unnamed(fs) => (punctuated_to_vector(fs.unnamed), VariantKind::Unnamed),
            Fields::Unit => (Vec::new(), VariantKind::Unit),
        };

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

        Ok(Variant { name, kind, fields })
    }

    fn expand_in_enum(&self, lt: &TokenStream) -> TokenStream {
        let name = &self.name;
        let fields = self
            .fields
            .iter()
            .map(|f| f.associated_main_struct_data(lt))
            .map(|(name, ty)| (name.expand_main_struct_field(), ty))
            .map(|(name, ty)| quote! { #name #ty });

        match self.kind {
            VariantKind::Unit => quote! { #name },
            VariantKind::Named => quote! {
                #name {
                    #( #fields ),*
                }
            },
            VariantKind::Unnamed => quote! {
                #name ( #( #fields ),* )
            },
        }
    }
}

fn punctuated_to_vector<T, U>(punc: Punctuated<T, U>) -> Vec<T> {
    punc.into_iter().collect()
}

enum VariantKind {
    Named,
    Unnamed,
    Unit,
}
