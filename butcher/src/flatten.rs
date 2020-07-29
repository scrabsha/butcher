//! Allows to flatten some data wrapped in a `Cow`.
//!
//! For instance, this allows to convert a `Cow<String>` to a `Cow<str>`, which
//! is easier to deal with.

use std::borrow::Cow;
use std::ops::Deref;

fn flatten_cow<T>(input: Cow<T>) -> Cow<<T as Deref>::Target>
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

/// Allows to flatten some data wrapped in a `Cow`.
///
/// The best use case for this is to transform a `Cow<String>` into a
/// `Cow<str>`. This also works for the boxed unsized type, as long as they
/// implement the traits as required below.
///
/// # Example
///
/// The following example shows the flattening of `Cow<String>` and
/// `Cow<PathBuf>`:
///
/// ```rust
/// use std::{
///     borrow::Cow,
///     path::{Path, PathBuf},
/// };
///
/// use butcher::flatten::FlattenCow;
///
/// // The type is annotated so that it is easier to understand what's happening
/// let a: Cow<String> = Cow::Owned(String::from("Grace Hopper"));
/// let a_flattened: Cow<str> = a.flatten();
/// assert_eq!(a_flattened, "Grace Hopper");
///
/// let b: Cow<PathBuf> = Cow::Owned(PathBuf::from("/path/to/foo"));
/// let b_flattened: Cow<Path> = b.flatten();
/// assert_eq!(b_flattened, Path::new("/path/to/foo"));
/// ```
///
/// # Traits requirements
///
/// In order to call `flatten` on a `Cow<T>`, the following requirements must be
/// satisified:
///   - `T` must implement [`ToOwned`], which is required to build the initial
///   `Cow<T>`,
///   - `T` must implement [`Deref`], and its `Target` must also implement
///   [`ToOwned`], which is required to build the output
///   `Cow<<T as Deref>::Target>`, and when the borrowed case is met,
///   - `T` must be convertible into the `Owned` type associated to the `Target`
///   dereferenced type, which is required when the owned case is met.
///
/// [`ToOwned`]: https://doc.rust-lang.org/std/borrow/trait.ToOwned.html
/// [`Deref`]: https://doc.rust-lang.org/std/ops/trait.Deref.html
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

    #[test]
    fn flatten_cow_box_str_owned() {
        let input: Cow<Box<str>> = Cow::Owned(Box::from("foo"));
        let output: Cow<str> = flatten_cow(input);

        assert!(matches!(output, Cow::Owned(_)));
        assert_eq!(output, "foo");
    }

    #[test]
    fn flatten_cow_box_str_borrowed() {
        let tmp = Box::from("bar");
        let input: Cow<Box<str>> = Cow::Borrowed(&tmp);
        let output: Cow<str> = flatten_cow(input);

        assert!(matches!(output, Cow::Borrowed(_)));
        assert_eq!(output, "bar");
    }
}
