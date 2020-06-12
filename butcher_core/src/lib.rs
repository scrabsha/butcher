use std::borrow::Cow;

pub trait ButcherField: ToOwned {
    type Output: ToOwned + Sized;

    fn from_borrowed(b: &Self) -> Cow<Self::Output>;

    fn from_owned(o: Self) -> Cow<'static, Self::Output>;
}

pub trait Butcher: ToOwned {
    type Output;

    fn butcher(this: Cow<Self>) -> Self::Output;
}
