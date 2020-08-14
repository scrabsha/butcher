use std::collections::HashSet;

use syn::{
    punctuated::Punctuated, Data, DeriveInput, Fields, GenericParam, Ident, LifetimeDef,
    Type, TypeParam, Variant as SVariant, Visibility, WhereClause,
};

use proc_macro2::TokenStream;

use quote::{format_ident, quote};

use super::field::Field;

use crate::utils;

use super::utils::create_type_signature;

pub(super) struct ButcheredEnum {
    name: Ident,
    vis: Visibility,
    variants: Vec<Variant>,
    generics_for_butchered: Vec<GenericParam>,
    where_clause_for_butchered: Option<WhereClause>,
}

impl ButcheredEnum {
    pub(super) fn from(input: DeriveInput) -> Result<ButcheredEnum, syn::Error> {
        let self_type_signature = create_type_signature(&input);

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
                lifetimes.insert(lt.lifetime.ident);
            }
            GenericParam::Const(_) => {}
        });

        let variants = data
            .variants
            .into_iter()
            .map(|v| Variant::from(v, &generic_types, &lifetimes, &self_type_signature))
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
        let butcher_fields_implementation = self.expand_fields(&lt);
        let butcher_implementation = self.expand_butcher_implementation(&lt);

        quote! {
            #enum_declaration
            #butcher_fields_implementation
            #butcher_implementation
        }
    }

    fn expand_enum_declaration(&self, lt: &TokenStream) -> TokenStream {
        let vis = &self.vis;
        let name = self.enum_name();
        let generics = self.generics_declaration(lt);
        let where_items = self
            .provided_where_clause_items()
            .chain(self.required_where_clause_items(lt));

        let variants = self.variants.iter().map(|v| v.expand_in_enum(lt));

        quote! {
            #[derive(Clone)]
            #vis enum #name #generics
            where
                #(
                    #where_items
                ),*
            {
                #( #variants ),*
            }
        }
    }

    fn generics_declaration(&self, lt: &TokenStream) -> TokenStream {
        let generics = self.generics_for_butchered.iter().map(|g| quote! { #g });

        quote! { < #lt #( , #generics )* > }
    }

    fn generics(&self, lt: &TokenStream) -> TokenStream {
        let generics = self.generics_for_butchered.iter().map(|g| match g {
            GenericParam::Type(TypeParam { ident, .. }) => quote! { #ident },
            GenericParam::Lifetime(LifetimeDef { lifetime, .. }) => quote! { #lifetime },
            // TODO: find how this should be done.
            GenericParam::Const(_) => unimplemented!(),
        });

        quote! { < #lt #( , #generics )* > }
    }

    fn initial_generics(&self) -> TokenStream {
        let generics = self.generics_for_butchered.iter().map(|g| match g {
            GenericParam::Type(TypeParam { ident, .. }) => quote! { #ident },
            GenericParam::Lifetime(LifetimeDef { lifetime, .. }) => quote! { #lifetime },
            // TODO: find how this should be done.
            GenericParam::Const(_) => unimplemented!(),
        });

        quote! { < #( #generics ),* > }
    }

    fn enum_name(&self) -> Ident {
        utils::global_associated_struct_name(&self.name)
    }

    fn expand_fields(&self, lt: &TokenStream) -> TokenStream {
        let expanded_variants = self
            .variants
            .iter()
            .map(|v| v.expand_fields(lt, &self.name));

        quote! {
            #( #expanded_variants )*
        }
    }

    fn expand_butcher_implementation(&self, lt: &TokenStream) -> TokenStream {
        let generic_declaration = self.generics_declaration(lt);
        let name = &self.name;
        let initial_generics = self.initial_generics();
        let enum_name = self.enum_name();
        let generics = self.generics(lt);

        let new_enum_name = format_ident!("Butchered{}", self.name);

        let owned_arms = self
            .variants
            .iter()
            .map(|v| v.owned_arm(&new_enum_name, name, lt));
        let borrowed_arms = self
            .variants
            .iter()
            .map(|v| v.borrowed_arm(&new_enum_name, name, lt));

        let generics_items = self
            .provided_where_clause_items()
            .chain(self.required_where_clause_items(lt));

        let unbutcher_match_arms = self
            .variants
            .iter()
            .map(|v| v.unbutcher_match_arm(&self.name, lt));

        quote! {
            impl #generic_declaration
                butcher::Butcher< #lt >
                for #name #initial_generics
            where
                #( #generics_items ),*
            {
                type Output = #enum_name #generics;

                fn butcher(this: std::borrow::Cow<#lt, Self>) -> Self::Output {
                    match this {
                        std::borrow::Cow::Owned(this) => match this {
                            #( #owned_arms, )*
                        },
                        std::borrow::Cow::Borrowed(this) => match this {
                            #( #borrowed_arms, )*
                        },
                    }
                }

                fn unbutcher(this: Self::Output) -> Self {
                    match this {
                        #( #unbutcher_match_arms ),*
                    }
                }
            }
        }
    }

    fn provided_where_clause_items(&self) -> impl Iterator<Item = TokenStream> + '_ {
        self.where_clause_for_butchered
            .iter()
            .map(|predicate| quote! { #predicate })
    }

    fn required_where_clause_items<'a>(
        &'a self,
        lt: &'a TokenStream,
    ) -> impl Iterator<Item = TokenStream> + 'a {
        self.generics_for_butchered
            .iter()
            .flat_map(move |generic| match generic {
                GenericParam::Type(TypeParam { ident, .. }) => Some(quote! { #ident: Clone + #lt }),
                GenericParam::Lifetime(LifetimeDef { lifetime, .. }) => {
                    Some(quote! { #lifetime: #lt })
                }
                GenericParam::Const(_) => None,
            })
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
        lifetimes: &HashSet<Ident>,
        enum_type_signature: &Type,
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
            .map(|(id, f)| Field::from(f, &generic_types, &lifetimes, id, enum_type_signature))
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
            .map(|(name, ty, vis)| (name.expand_main_struct_field(), ty, vis))
            .map(|(name, ty, vis)| quote! { #vis #name #ty });

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

    fn expand_fields(&self, lt: &TokenStream, main_name: &Ident) -> TokenStream {
        let name = format_ident!("{}{}", main_name, self.name);

        let expanded_fields = self.fields.iter().map(|f| f.expand_to_code(&name, &lt));

        quote! {
            #( #expanded_fields )*
        }
    }

    fn pattern(&self, main_enum_name: &Ident) -> TokenStream {
        let variant_name = &self.name;
        let fields = self
            .fields
            .iter()
            .map(|f| f.name.expand_as_pattern_identifier());

        match self.kind {
            VariantKind::Unit => {
                quote! { #main_enum_name :: #variant_name }
            }
            VariantKind::Named => {
                quote! {
                    #main_enum_name :: #variant_name { #( #fields ),* }
                }
            }
            VariantKind::Unnamed => {
                quote! {
                    #main_enum_name :: #variant_name ( #( #fields ),* )
                }
            }
        }
    }

    fn owned_arm(
        &self,
        main_enum_name: &Ident,
        initial_enum_name: &Ident,
        lt: &TokenStream,
    ) -> TokenStream {
        let pattern = self.pattern(initial_enum_name);
        let return_expr = self.owned_return_expr(main_enum_name, initial_enum_name, lt);

        quote! { #pattern => #return_expr }
    }

    fn borrowed_arm(
        &self,
        main_enum_name: &Ident,
        initial_enum_name: &Ident,
        lt: &TokenStream,
    ) -> TokenStream {
        let pattern = self.pattern(initial_enum_name);
        let return_expr = self.borrowed_return_expr(main_enum_name, initial_enum_name, lt);

        quote! { #pattern => #return_expr }
    }

    fn owned_return_expr(
        &self,
        main_enum_name: &Ident,
        initial_enum_name: &Ident,
        lt: &TokenStream,
    ) -> TokenStream {
        let method = quote! { from_owned };
        self.return_expr(main_enum_name, initial_enum_name, method, lt)
    }

    fn borrowed_return_expr(
        &self,
        main_enum_name: &Ident,
        initial_enum_name: &Ident,
        lt: &TokenStream,
    ) -> TokenStream {
        let method = quote! { from_borrowed };
        self.return_expr(main_enum_name, initial_enum_name, method, lt)
    }

    fn return_expr(
        &self,
        main_enum_name: &Ident,
        initial_enum_name: &Ident,
        method: TokenStream,
        lt: &TokenStream,
    ) -> TokenStream {
        let variant = &self.name;
        let fields = self
            .fields
            .iter()
            .map(|f| f.name.expand_as_pattern_identifier());
        let fields_2 = fields.clone();

        let name = format_ident!("{}{}", initial_enum_name, variant);

        let associated_struct = self
            .fields
            .iter()
            .map(|f| f.associated_struct_with_generics(&name));

        let associated_struct_types = self.fields.iter().map(|f| &f.ty);

        match self.kind {
            VariantKind::Unit => quote! {
                #main_enum_name :: #variant
            },
            VariantKind::Named => quote! {
                #main_enum_name :: #variant {
                    #(
                        #fields: < #associated_struct as butcher::methods::ButcherField<#lt, #associated_struct_types>>:: #method ( #fields_2)
                    ),*
                }
            },
            VariantKind::Unnamed => quote! {
                #main_enum_name :: #variant (
                    #(
                        < #associated_struct as butcher::methods::ButcherField<#lt, #associated_struct_types>>:: #method ( #fields )
                    ),*
                )
            },
        }
    }

    fn unbutcher_match_arm(&self, enum_name: &Ident, lt: &TokenStream) -> TokenStream {
        let butchered_enum_name = utils::global_associated_struct_name(enum_name);
        let pattern = self.unbutcher_match_arm_pattern(&butchered_enum_name);
        let own_each_field = self.own_each_field(enum_name, lt);
        let initial_struct = self.recreate_initial_struct(enum_name);

        quote! {
            #pattern => {
                #own_each_field;
                #initial_struct
            }
        }
    }

    fn unbutcher_match_arm_pattern(&self, butchered_enum_name: &Ident) -> TokenStream {
        let variant_name = &self.name;
        let fields = self
            .fields
            .iter()
            .map(|f| f.name.expand_as_pattern_identifier());

        let bindings = match self.kind {
            VariantKind::Named => quote! {
                { #( #fields ),* }
            },
            VariantKind::Unnamed => quote! {
                ( #( #fields ),* )
            },
            VariantKind::Unit => quote! {},
        };

        quote! {
            #butchered_enum_name :: #variant_name #bindings
        }
    }

    fn own_each_field(&self, enum_name: &Ident, lt: &TokenStream) -> TokenStream {
        let names = self
            .fields
            .iter()
            .map(|f| f.name.expand_as_pattern_identifier());
        let names2 = names.clone();

        let name = format_ident!("{}{}", enum_name, &self.name);

        let associated_structs = self
            .fields
            .iter()
            .map(|f| f.associated_struct_with_generics(&name));

        let associated_struct_types = self.fields.iter().map(|f| &f.ty);

        quote! {
            let ( #( #names ),* ) =
                ( #(
                    < #associated_structs as butcher::methods::ButcherField< #lt, #associated_struct_types >>::unbutcher( #names2 )
                ),* )
        }
    }

    fn recreate_initial_struct(&self, enum_name: &Ident) -> TokenStream {
        let variant_name = &self.name;

        let names = self
            .fields
            .iter()
            .map(|f| f.name.expand_as_pattern_identifier());

        match self.kind {
            VariantKind::Named => quote! {
                #enum_name :: #variant_name { #( #names ),* }
            },
            VariantKind::Unnamed => quote! {
                #enum_name :: #variant_name ( #( #names ),* )
            },
            VariantKind::Unit => quote! {
                #enum_name :: #variant_name
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
