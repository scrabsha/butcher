use std::collections::HashSet;

use syn::{
    AngleBracketedGenericArguments, Attribute, Data, DeriveInput, Fields, GenericArgument,
    GenericParam, Ident, Meta, MetaList, NestedMeta, PathArguments, QSelf, ReturnType, Type,
    TypeArray, TypeBareFn, TypeGroup, TypeParen, TypePath, TypePtr, TypeReference, TypeSlice,
    TypeTuple, Visibility,
};

pub(super) struct ButcheredStruct {
    name: Ident,
    fields: Vec<Field>,
}

impl ButcheredStruct {
    pub(super) fn from(input: DeriveInput) -> ButcheredStruct {
        let name = input.ident;
        let data = match input.data {
            Data::Struct(d) => d,
            Data::Enum(_) => unimplemented!(),
            Data::Union(_) => panic!("butcher does not support unions."),
        };

        let fields = match data.fields {
            Fields::Named(fields) => fields.named,
            Fields::Unnamed(_) => unimplemented!(),
            Fields::Unit => panic!("butchering an unit struct is useless."),
        };

        let generics = input
            .generics
            .params
            .into_iter()
            .filter_map(|g| match g {
                GenericParam::Type(g) => Some(g.ident),
                GenericParam::Lifetime(_) | GenericParam::Const(_) => None,
            })
            .collect::<HashSet<_>>();

        let fields = fields
            .into_iter()
            .map(|f| Field::from(f, &generics))
            .collect::<Vec<_>>();

        ButcheredStruct { name, fields }
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
    fn from(input: syn::Field, generics: &HashSet<Ident>) -> Field {
        let vis = input.vis;

        let methods = input
            .attrs
            .into_iter()
            .map(ButcheringMethod::try_from)
            .flatten()
            .collect::<Vec<_>>();

        let method = match methods.as_slice() {
            [method] => *method,
            [] => ButcheringMethod::Regular,
            _ => panic!("Multiple butchering method provided."),
        };

        let name = input
            .ident
            .expect("Fields of named struct should have a name");

        let ty = input.ty;

        let associated_generics = find_generics_in_type(&ty, generics);

        Field {
            vis,
            method,
            name,
            ty,
            associated_generics,
        }
    }
}

fn find_generics_in_type(ty: &Type, generics: &HashSet<Ident>) -> Vec<Ident> {
    match ty {
        Type::Array(TypeArray { elem, .. })
        | Type::Group(TypeGroup { elem, .. })
        | Type::Paren(TypeParen { elem, .. })
        | Type::Ptr(TypePtr { elem, .. })
        | Type::Reference(TypeReference { elem, .. })
        | Type::Slice(TypeSlice { elem, .. }) => find_generics_in_type(elem.as_ref(), generics),

        Type::Tuple(TypeTuple { elems, .. }) => elems
            .into_iter()
            .flat_map(|ty| find_generics_in_type(ty, generics))
            .collect(),

        Type::BareFn(TypeBareFn { inputs, output, .. }) => {
            let mut found_generics = inputs
                .into_iter()
                .map(|arg| arg.ty.clone())
                .flat_map(|ty| find_generics_in_type(&ty, generics))
                .collect::<Vec<_>>();

            if let ReturnType::Type(_, ty) = output {
                found_generics.extend(find_generics_in_type(ty.as_ref(), generics));
            }

            found_generics
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
                .flatten()
                .collect::<Vec<_>>();

            match path.get_ident() {
                Some(id) if generics.contains(id) => found_generics.push(id.clone()),
                _ => {}
            }

            if let Some(QSelf { ty, .. }) = qself {
                found_generics.extend(find_generics_in_type(ty.as_ref(), generics));
            }

            found_generics
        }

        Type::ImplTrait(_) => panic!("impl trait is not currently supported in butcher"),
        Type::Macro(_) => panic!("macros as type is not currently supported in butcher"),
        Type::TraitObject(_) => panic!("Trait objects are not currencly support in butcher"),

        // The compiler is going to raise an error anyway.
        Type::Infer(_) => Vec::new(),
        // The compiler is going to raise an error anyway.
        Type::Never(_) => Vec::new(),
        // The compiler is going to raise an error anyway.
        Type::Verbatim(_) => Vec::new(),

        _ => panic!("Unknown type met"),
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum ButcheringMethod {
    Copy,
    Dereferenced,
    Regular,
    Referenced,
}

impl ButcheringMethod {
    fn try_from(input: Attribute) -> Option<ButcheringMethod> {
        let meta = input.parse_meta().ok()?;

        let methods = match meta {
            Meta::Path(_) => None,
            Meta::List(MetaList { path, nested, .. }) if path.is_ident("butcher") => Some(nested),
            Meta::List(_) => None,
            Meta::NameValue(_) => None,
        }?;

        if methods.len() != 1 {
            panic!("Expected exactly one butchering method");
        }

        let method = &methods[0];

        let method = match method {
            NestedMeta::Lit(_) => panic!("Butchering methods must not be literals"),
            NestedMeta::Meta(Meta::Path(p)) => p.get_ident().map(ToString::to_string),
            NestedMeta::Meta(_) => panic!("Buthering methods must not be name-value items"),
        };

        match method.as_deref() {
            Some("as_ref") => Some(ButcheringMethod::Referenced),
            Some("deref") => Some(ButcheringMethod::Dereferenced),
            Some("regular") => Some(ButcheringMethod::Regular),
            Some("copy") => Some(ButcheringMethod::Copy),
            _ => panic!("Unknown butchering method."),
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
                a: &'a str,
                #[butcher(as_ref)]
                b: String,
                c: T,
                #[butcher(deref)]
                d: Box<T>,
                #[butcher(regular)]
                e: (),
                f: (),
            }
        };

        let bs = ButcheredStruct::from(input);
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
    }
}
