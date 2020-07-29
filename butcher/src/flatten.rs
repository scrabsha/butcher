//! Allows to flatten some data wrapped in a `Cow`.
//!
//! For instance, this allows to convert a `Cow<String>` to a `Cow<str>`, which
//! is easier to deal with.

use std::borrow::Cow;
use std::ops::Deref;

// TODO: add doc
fn flatten_cow<'cow, T>(input: Cow<'cow, T>) -> Cow<'cow, <T as Deref>::Target>
where
    T: Deref + ToOwned,
    <T as Deref>::Target: ToOwned,
    <<T as Deref>::Target as ToOwned>::Owned: From<<T as ToOwned>::Owned>,
{
    match input {
        Cow::Borrowed(input) => Cow::Borrowed(input.deref()),
        Cow::Owned(input) => Cow::Owned(input.into()),
    }
}

// TODO: add doc
pub trait FlattenCow<'cow, T>
where
    T: Deref,
    <T as Deref>::Target: ToOwned,
{
    fn flatten(self) -> Cow<'cow, <T as Deref>::Target>;
}

impl<'cow, T> FlattenCow<'cow, T> for Cow<'cow, T>
where
    T: Deref + ToOwned,
    <T as Deref>::Target: ToOwned,
    <<T as Deref>::Target as ToOwned>::Owned: From<<T as ToOwned>::Owned>,
{
    fn flatten(self) -> Cow<'cow, <T as Deref>::Target> {
        flatten_cow(self)
    }
}

#[cfg(test)]
mod flatten_fn {
    use super::*;

    #[test]
    fn flatten_cow_string_owned() {
        let input: Cow<String> = Cow::Owned(String::from("foo"));
        let output: Cow<str> = flatten_cow(input);

        assert!(matches!(output, Cow::Owned(_)));
        assert_eq!(output, "foo");
    }

    #[test]
    fn flatten_cow_string_borrowed() {
        let tmp = String::from("bar");
        let input: Cow<String> = Cow::Borrowed(&tmp);
        let output: Cow<str> = flatten_cow(input);

        assert!(matches!(output, Cow::Borrowed(_)));
        assert_eq!(output, "bar");
    }
}
