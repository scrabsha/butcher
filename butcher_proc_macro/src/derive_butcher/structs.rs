use std::{collections::HashSet, iter};

use syn::{
    Data, DeriveInput, Fields, GenericParam, Ident, LifetimeDef, TypeParam, Visibility, WhereClause,
};

use quote::quote;

use proc_macro2::TokenStream;

use crate::utils::{self};

use super::{field::Field, DeriveError};

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

        let a = quote! {
            #( #fields_expansion )*

            #butchered_struct_trait
            #butchered_struct
        };

        if self.name == "List" {
            println!("{}", a);
        }

        a
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

        let borrowed_arm = self.borrowed_match_arm();
        let owned_arm = self.owned_match_arm();

        quote! {
            impl< #( #generics_declaration ),* >
                butcher::Butcher<#lt> for
                #name< #( #generics_usage ),* >
            #where_clause
            {
                type Output = #output_type < #( #generics_for_output ),* >;

                fn butcher(this: std::borrow::Cow<'cow, Self>) -> Self::Output {
                    match this {
                        #borrowed_arm,
                        #owned_arm,
                    }
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

    fn borrowed_match_arm(&self) -> TokenStream {
        let pattern = self.borrowed_pattern();
        let return_expr = self.borrowed_return_expr();

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

    fn borrowed_return_expr(&self) -> TokenStream {
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

        match self.kind {
            StructKind::Named => {
                quote! {
                    #return_type_name {
                        #( #fields: <#associated_structs as butcher::ButcherField>::from_borrowed( #fields_2 ) ),*
                    }
                }
            }

            StructKind::Tupled => quote! {
                #return_type_name(
                    #( <#associated_structs as butcher::ButcherField>::from_borrowed( #fields_2 ) ),*
                )
            },
        }
    }

    fn owned_match_arm(&self) -> TokenStream {
        let pattern = self.owned_pattern();
        let return_expr = self.owned_return_expr();

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

    fn owned_return_expr(&self) -> TokenStream {
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

        match self.kind {
            StructKind::Named => {
                quote! {
                    #return_type_name {
                        #( #fields: <#associated_structs as butcher::ButcherField>::from_owned( #fields_2 ) ),*
                    }
                }
            }

            StructKind::Tupled => quote! {
                #return_type_name(
                    #( <#associated_structs as butcher::ButcherField>::from_owned( #fields ) ),*
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
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) enum StructKind {
    Named,
    Tupled,
}

#[cfg(test)]
mod butchered_struct {
    use super::*;

    use syn::DeriveInput;

    use quote::quote;
    use syn::parse_quote;

    use crate::derive_butcher::field::ButcheringMethod;

    #[test]
    fn serialization_named_struct() {
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

    #[test]
    fn serialization_tupled_struct() {
        let input: DeriveInput = parse_quote! {
            #[derive(Butcher)]
            struct Foo(
                #[butcher(flatten)]
                Box<str>,
                #[butcher(copy)]
                usize,
            );
        };

        let bs = ButcheredStruct::from(input).unwrap();

        assert_eq!(bs.name, "Foo");

        assert_eq!(&bs.fields[0].name, 0);
        assert_eq!(&bs.fields[1].name, 1);

        let tmp = &bs.fields[0].ty;
        let left = quote! { #tmp };
        let right = quote! { Box<str> };
        assert_eq_tt!(left, right);

        let tmp = &bs.fields[1].ty;
        let left = quote! { #tmp };
        let right = quote! { usize };
        assert_eq_tt!(left, right);

        assert_eq!(bs.fields[0].method, ButcheringMethod::Flatten);
        assert_eq!(bs.fields[1].method, ButcheringMethod::Copy);
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
        let lt = quote! { 'cow };

        let s: DeriveInput = parse_quote! {
            #[derive(Butcher)]
            struct Foo {
                a: usize,
            }
        };
        let bs = ButcheredStruct::from(s).unwrap();
        let left = bs.fields[0].butcher_field_implementation(&bs.name, &lt);
        let right = quote! {
            impl<'cow,> butcher::ButcherField<'cow> for ButcherFooa<>
            where
                usize: Clone
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
        let left = bs.fields[0].butcher_field_implementation(&bs.name, &lt);
        let right = quote! {
            impl<'cow, 'a,> butcher::ButcherField<'cow> for ButcherFooa<'a,>
            where
                &'a usize: Clone,
                'a: 'cow
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
        let left = bs.fields[0].butcher_field_implementation(&bs.name, &lt);
        let right = quote! {
            impl<'cow, T> butcher::ButcherField<'cow> for ButcherFooa<T,>
            where
                T: Clone,
                T: 'cow
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
        let lt = quote! { 'cow };
        let s: DeriveInput = parse_quote! {
            #[derive(Butcher)]
            struct Foo {
                #[butcher(copy)]
                a: usize,
            }
        };
        let bs = ButcheredStruct::from(s).unwrap();
        let left = bs.fields[0].butcher_field_implementation(&bs.name, &lt);
        let right = quote! {
            impl<'cow,> butcher::ButcherField<'cow> for ButcherFooa<>
            where
                usize: Clone
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
        let left = bs.fields[0].butcher_field_implementation(&bs.name, &lt);
        let right = quote! {
            impl<'cow, 'a,> butcher::ButcherField<'cow> for ButcherFooa<'a,>
            where
                &'a usize: Clone,
                'a: 'cow
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
        let left = bs.fields[0].butcher_field_implementation(&bs.name, &lt);
        let right = quote! {
            impl<'cow, T> butcher::ButcherField<'cow> for ButcherFooa<T,>
            where
                T: Clone,
                T: 'cow
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
        let lt = quote! { 'cow };

        let s: DeriveInput = parse_quote! {
            #[derive(Butcher)]
            struct Foo {
                #[butcher(unbox)]
                a: Box<usize>,
            }
        };
        let bs = ButcheredStruct::from(s).unwrap();
        let left = bs.fields[0].butcher_field_implementation(&bs.name, &lt);
        let right = quote! {
            impl<'cow,> butcher::ButcherField<'cow> for ButcherFooa<>
            where
                <Box<usize> as std::ops::Deref>::Target: Clone
            {
                type Input = Box<usize>;
                type Output = std::borrow::Cow<'cow, <Box<usize> as std::ops::Deref>::Target>;

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
        let left = bs.fields[0].butcher_field_implementation(&bs.name, &lt);
        let right = quote! {
            impl<'cow, 'a,> butcher::ButcherField<'cow> for ButcherFooa<'a,>
            where
                <Box<&'a usize> as std::ops::Deref>::Target: Clone,
                'a: 'cow
            {
                type Input = Box<&'a usize>;
                type Output = std::borrow::Cow<'cow, <Box<&'a usize> as std::ops::Deref>::Target>;

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
        let left = bs.fields[0].butcher_field_implementation(&bs.name, &lt);
        let right = quote! {
            impl<'cow, T> butcher::ButcherField<'cow> for ButcherFooa<T,>
            where
                <Box<T> as std::ops::Deref>::Target: Clone,
                T: 'cow
            {
                type Input = Box<T>;
                type Output = std::borrow::Cow<'cow, <Box<T> as std::ops::Deref>::Target>;

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
