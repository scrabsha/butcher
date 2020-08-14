use std::{
    error::Error,
    fmt::{self, Display},
};

use syn::{Data, DataUnion, DeriveInput};

use proc_macro2::TokenStream;

mod enums;
mod field;
mod structs;
mod utils;

use enums::ButcheredEnum;
use structs::ButcheredStruct;

#[derive(Debug, PartialEq)]
pub enum DeriveError {
    FoundUnion,
    FoundUnitStruct,
    MultipleButcheringMethod,
    UnknownMethod,
}

impl Display for DeriveError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DeriveError::FoundUnion => "Butcher does not support unions",
            DeriveError::FoundUnitStruct => "Butchering is useless for unit structs",
            DeriveError::MultipleButcheringMethod => {
                "Multiple butchering method provided. Choose one!"
            }
            DeriveError::UnknownMethod => "Unknown butchering method",
        }
        .fmt(f)
    }
}

impl Error for DeriveError {}

pub(super) fn try_from(i: DeriveInput) -> Result<TokenStream, syn::Error> {
    let res = match i.data {
        Data::Struct(_) => ButcheredStruct::from(i)?.expand_to_code(),
        Data::Enum(_) => ButcheredEnum::from(i)?.expand_to_code(),
        Data::Union(DataUnion { union_token, .. }) => {
            return Err(syn::Error::new_spanned(
                union_token,
                DeriveError::FoundUnion,
            ))
        }
    };

    Ok(res)
}
