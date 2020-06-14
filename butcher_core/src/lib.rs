use std::borrow::Cow;

pub trait ButcherField<'a> {
    type Input;
    type Output: ToOwned + Sized + 'a;

    fn from_borrowed(b: &'a Self::Input) -> Self::Output;

    fn from_owned(o: Self::Input) -> Self::Output;
}

pub trait Butcher: ToOwned {
    type Output;

    fn butcher(this: Cow<Self>) -> Self::Output;
}
