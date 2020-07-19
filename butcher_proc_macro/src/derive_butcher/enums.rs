use syn::{Data, DeriveInput, GenericParam, Ident, Visibility, WhereClause};

use proc_macro2::TokenStream;

use super::{field::Field, structs::StructKind};

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

        todo!();
    }

    pub(super) fn expand_to_code(self) -> TokenStream {
        todo!();
    }
}

struct Variant {
    name: Ident,
    vis: Visibility,
    kind: StructKind,
    fields: Vec<Field>,
}
