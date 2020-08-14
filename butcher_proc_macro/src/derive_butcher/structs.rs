use std::{collections::HashSet, iter};

use syn::{
    Data, DeriveInput, Fields, GenericParam, Ident, LifetimeDef, TypeParam, Visibility, WhereClause,
};

use quote::quote;

use proc_macro2::TokenStream;

use crate::utils;

use super::{field::Field, utils::create_type_signature, DeriveError};

pub(super) struct ButcheredStruct {
    name: Ident,
    fields: Vec<Field>,
    vis: Visibility,
    generics_for_butchered: Vec<GenericParam>,
    where_clause_for_butchered: Option<WhereClause>,
    kind: StructKind,
}

impl ButcheredStruct {
    pub(super) fn from(input: DeriveInput) -> Result<ButcheredStruct, syn::Error> {
        let self_type_signature = create_type_signature(&input);

        let name = input.ident;
        let vis = input.vis;

        let generics_for_butchered = input.generics.params.iter().cloned().collect::<Vec<_>>();

        let where_clause_for_butchered = input.generics.where_clause;

        let data = match input.data {
            Data::Struct(d) => d,
            // This should have been filtered previously
            Data::Enum(_) | Data::Union(_) => unreachable!(),
        };

        let (fields, kind) = match data.fields {
            Fields::Named(fields) => Ok((fields.named, StructKind::Named)),
            Fields::Unnamed(fields) => Ok((fields.unnamed, StructKind::Tupled)),
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
                lifetimes.insert(lt.lifetime.ident);
            }
            GenericParam::Const(_) => {}
        });

        let fields = fields
            .into_iter()
            .enumerate()
            .map(|(id, f)| Field::from(f, &generic_types, &lifetimes, id, &self_type_signature))
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

        Ok(ButcheredStruct {
            name,
            fields,
            vis,
            generics_for_butchered,
            where_clause_for_butchered,
            kind,
        })
    }

    pub(crate) fn expand_to_code(self) -> TokenStream {
        let lt = quote! { 'cow };
        let fields_expansion = self
            .fields
            .iter()
            .map(|f| f.expand_to_code(&self.name, &lt));

        let butchered_struct = self.expand_butchered_struct(&lt);
        let butchered_struct_trait = self.expand_butchered_struct_trait(&lt);

        quote! {
            #( #fields_expansion )*

            #butchered_struct_trait
            #butchered_struct
        }
    }

    fn expand_butchered_struct(&self, lt: &TokenStream) -> TokenStream {
        let vis = &self.vis;
        let name = utils::global_associated_struct_name(&self.name);

        let generics = self.generics_for_butchered.iter().map(|g| quote! { #g });
        let generics = iter::once(quote! { #lt }).chain(generics);

        let rest = self.fields_with_where_clause(lt);

        // TODO: Add handling for where clause provided in initial struct declaration
        quote! {
            #[derive(Clone)]
            #vis struct #name < #( #generics ),* >
            #rest
        }
    }

    fn fields_with_where_clause(&self, lt: &TokenStream) -> TokenStream {
        let fields = self.expand_fields(lt);
        let where_clause = self.expand_where_clause(lt);

        match self.kind {
            StructKind::Named => quote! {
                    #where_clause
                    #fields
            },
            StructKind::Tupled => quote! {
                #fields
                #where_clause;
            },
        }
    }

    fn expand_where_clause(&self, lt: &TokenStream) -> TokenStream {
        let where_clause_items = self.fields.iter().flat_map(|f| f.where_clause_items(lt));
        quote! {
            where
                #( #where_clause_items ),*
        }
    }

    fn expand_fields(&self, lt: &TokenStream) -> TokenStream {
        let fields = self
            .fields
            .iter()
            .map(|f| f.associated_main_struct_data(lt))
            .map(|(name, ty, vis)| (name.expand_main_struct_field(), ty, vis))
            .map(|(name, ty, vis)| quote! { #vis #name #ty });

        match self.kind {
            StructKind::Named => {
                quote! {
                    {
                        #(
                            #fields,
                        )*
                    }
                }
            }
            StructKind::Tupled => {
                quote! {
                    (
                        #(
                            #fields,
                        )*
                    )
                }
            }
        }
    }

    fn expand_butchered_struct_trait(&self, lt: &TokenStream) -> TokenStream {
        let generics_declaration = iter::once(lt.clone()).chain(self.generics_declaration(lt));

        let name = &self.name;
        let generics_usage = self.generics_usage();
        let where_clause = &self.where_clause_for_butchered;
        let output_type = utils::global_associated_struct_name(&self.name);
        let generics_for_output = iter::once(lt.clone()).chain(generics_usage.clone());

        let borrowed_arm = self.borrowed_match_arm(lt);
        let owned_arm = self.owned_match_arm(lt);

        let destructured = self.destructure_butchered_struct();
        let own_each_field = self.own_each_field(lt);
        let initial_struct = self.recreate_initial_struct();

        quote! {
            impl< #( #generics_declaration ),* >
                butcher::Butcher<#lt> for
                #name< #( #generics_usage ),* >
            #where_clause
            {
                type Output = #output_type < #( #generics_for_output ),* >;

                fn butcher(this: std::borrow::Cow<#lt, Self>) -> Self::Output {
                    match this {
                        #borrowed_arm,
                        #owned_arm,
                    }
                }

                fn unbutcher(this: Self::Output) -> Self {
                    let #destructured = this;
                    #own_each_field;
                    #initial_struct
                }
            }
        }
    }

    fn generics_declaration<'a>(
        &'a self,
        lt: &'a TokenStream,
    ) -> impl Iterator<Item = TokenStream> + 'a {
        self.generics_for_butchered
            .iter()
            .map(move |param| match param {
                GenericParam::Type(tp) => quote! { #tp: #lt + Clone },
                GenericParam::Lifetime(ld) => quote! { #ld: #lt },
                GenericParam::Const(cp) => quote! { #cp },
            })
    }

    fn generics_usage(&self) -> impl Iterator<Item = TokenStream> + Clone + '_ {
        self.generics_for_butchered.iter().map(|param| match param {
            GenericParam::Type(TypeParam { ident, .. }) => quote! { #ident },
            GenericParam::Lifetime(LifetimeDef { lifetime, .. }) => quote! { #lifetime },
            GenericParam::Const(_) => todo!(),
        })
    }

    fn borrowed_match_arm(&self, lt: &TokenStream) -> TokenStream {
        let pattern = self.borrowed_pattern();
        let return_expr = self.borrowed_return_expr(lt);

        quote! {
            #pattern => #return_expr
        }
    }

    fn borrowed_pattern(&self) -> TokenStream {
        let fields = self.fields_pattern();

        quote! {
            std::borrow::Cow::Borrowed( #fields )
        }
    }

    fn borrowed_return_expr(&self, lt: &TokenStream) -> TokenStream {
        let return_type_name = utils::global_associated_struct_name(&self.name);
        let fields = self
            .fields
            .iter()
            .map(|f| f.name.expand_as_pattern_identifier());
        let fields_2 = fields.clone();

        let associated_structs = self
            .fields
            .iter()
            .map(|f| f.associated_struct_with_generics(&self.name));

        let associated_struct_types = self.fields.iter().map(|f| &f.ty);

        match self.kind {
            StructKind::Named => {
                quote! {
                    #return_type_name {
                        #( #fields: <#associated_structs as butcher::methods::ButcherField<#lt, #associated_struct_types>>::from_borrowed( #fields_2 ) ),*
                    }
                }
            }

            StructKind::Tupled => quote! {
                #return_type_name(
                    #( <#associated_structs as butcher::methods::ButcherField<#lt, #associated_struct_types>>::from_borrowed( #fields_2 ) ),*
                )
            },
        }
    }

    fn owned_match_arm(&self, lt: &TokenStream) -> TokenStream {
        let pattern = self.owned_pattern();
        let return_expr = self.owned_return_expr(lt);

        quote! {
            #pattern => #return_expr
        }
    }

    fn owned_pattern(&self) -> TokenStream {
        let fields = self.fields_pattern();

        quote! {
            std::borrow::Cow::Owned( #fields )
        }
    }

    fn owned_return_expr(&self, lt: &TokenStream) -> TokenStream {
        let return_type_name = utils::global_associated_struct_name(&self.name);
        let fields = self
            .fields
            .iter()
            .map(|f| f.name.expand_as_pattern_identifier());
        let fields_2 = fields.clone();

        let associated_structs = self
            .fields
            .iter()
            // .map(|f| utils::associated_struct_name(&self.name, &f.name));
            .map(|f| f.associated_struct_with_generics(&self.name));

        let associated_struct_types = self.fields.iter().map(|f| &f.ty);

        match self.kind {
            StructKind::Named => {
                quote! {
                    #return_type_name {
                        #( #fields: <#associated_structs as butcher::methods::ButcherField<#lt, #associated_struct_types>>::from_owned( #fields_2 ) ),*
                    }
                }
            }

            StructKind::Tupled => quote! {
                #return_type_name(
                    #( <#associated_structs as butcher::methods::ButcherField<#lt, #associated_struct_types>>::from_owned( #fields ) ),*
                )
            },
        }
    }

    fn fields_pattern(&self) -> TokenStream {
        let name = &self.name;
        let fields = self
            .fields
            .iter()
            .map(|f| f.name.expand_as_pattern_identifier());
        match self.kind {
            StructKind::Named => quote! {
                #name { #( #fields ),* }
            },
            StructKind::Tupled => quote! {
                #name ( #( #fields ),* )
            },
        }
    }

    fn destructure_butchered_struct(&self) -> TokenStream {
        let type_name = utils::global_associated_struct_name(&self.name);
        let fields = self
            .fields
            .iter()
            .map(|f| f.name.expand_as_pattern_identifier());

        let destructure_expression = match self.kind {
            StructKind::Named => quote! {
                {
                    #( #fields ),*
                }
            },
            StructKind::Tupled => quote! {
                (
                    #( #fields ),*
                )
            },
        };

        quote! {
            #type_name #destructure_expression
        }
    }

    fn own_each_field(&self, lt: &TokenStream) -> TokenStream {
        let names = self
            .fields
            .iter()
            .map(|f| f.name.expand_as_pattern_identifier());

        let names2 = names.clone();

        let associated_structs = self
            .fields
            .iter()
            // .map(|f| utils::associated_struct_name(&self.name, &f.name));
            .map(|f| f.associated_struct_with_generics(&self.name));

        let associated_struct_types = self.fields.iter().map(|f| &f.ty);

        quote! {
            let ( #( #names ),* ) =
                ( #(
                    < #associated_structs as butcher::methods::ButcherField< #lt, #associated_struct_types >>::unbutcher( #names2 )
                ),* );
        }
    }

    fn recreate_initial_struct(&self) -> TokenStream {
        let type_name = &self.name;

        let fields = self
            .fields
            .iter()
            .map(|f| f.name.expand_as_pattern_identifier());

        match self.kind {
            StructKind::Named => quote! {
                #type_name {
                    #( #fields ),*
                }
            },
            StructKind::Tupled => quote! {
                #type_name( #( #fields ),* )
            },
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) enum StructKind {
    Named,
    Tupled,
}
