//! A trait that allows to unnest `Cow<Cow<T>>` to `Cow<T>`.

use std::borrow::Cow;
use std::ops::Deref;

fn unnest_cow<'a, 'b, T>(this: Cow<'a, Cow<'b, T>>) -> Cow<'a, T>
where
    'b: 'a,
    T: ToOwned + ?Sized + 'b,
{
    match this {
        Cow::Owned(this) => this,
        Cow::Borrowed(this) => Cow::Borrowed(this.deref()),
    }
}

/// Allows to unnest a `Cow`.
///
/// This trait is automatically implemented for each `Cow<Cow<T>>`, and provide
/// the `unnest` method. This method allows to deal easily with return type
/// created by `#[derive(Butcher)]` when it is called on objects with `Cow` in
/// one of their fields.
///
/// See for example the following struct:
///
/// ```rust
/// use butcher::Butcher;
/// use std::borrow::Cow;
///
/// #[derive(Butcher, Clone)]
/// struct Foo<'a> {
///     bar: Cow<'a, str>,
/// }
/// ```
///
/// Here, the fields `bar` of `ButcheredFoo` would have type
/// `Cow<'cow<Cow<'a str>>`. This trait allows us to easily get back a
/// `Cow<'cow str>`:
///
/// ```rust
/// # use butcher::Butcher;
/// # use std::borrow::Cow;
/// #
/// # #[derive(Butcher, Clone)]
/// # struct Foo<'a> {
/// #     bar: Cow<'a, str>,
/// # }
/// #
/// use butcher::unnest::UnnestCow;
///
/// let input_foo = Cow::Owned(Foo {
///     bar: Cow::Borrowed("hello, world!"),
/// });
///
/// // Here, bar has type Cow<'cow, Cow<'a, str>>
/// let ButcheredFoo { bar } = Foo::butcher(input_foo);
///
/// // The following piece of code transforms it into Cow<'cow, str>
/// let bar = bar.unnest();
/// ```
///
/// The outputed `Cow` will be and `Owned` variant only if the input `Cow` is
/// `Owned` on its two levels (eg: it matches `Cow::Owned(Cow::Owned(_))`).
pub trait UnnestCow<'a, T: ToOwned + ?Sized + 'a> {
    fn unnest(self) -> Cow<'a, T>;
}

impl<'a, 'b: 'a, T: ToOwned + ?Sized + 'a> UnnestCow<'a, T> for Cow<'b, Cow<'a, T>> {
    /// Flattens the `Cow`.
    fn unnest(self) -> Cow<'a, T> {
        unnest_cow(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn is_owned<T: ToOwned>(input: Cow<T>) -> bool {
        matches!(input, Cow::Owned(_))
    }

    #[test]
    fn owned_owned() {
        let input: Cow<Cow<usize>> = Cow::Owned(Cow::Owned(42));
        let tmp = input.unnest();

        assert_eq!(tmp, Cow::Owned(42));
        assert!(is_owned(tmp));
    }

    #[test]
    fn owned_borrowed() {
        let input: Cow<Cow<usize>> = Cow::Owned(Cow::Borrowed(&42));
        let tmp = input.unnest();

        assert_eq!(tmp, Cow::Owned(42));
        assert!(!is_owned(tmp));
    }

    #[test]
    fn borrowed_owned() {
        let input: Cow<Cow<usize>> = Cow::Borrowed(&Cow::Owned(42));
        let tmp = input.unnest();

        assert_eq!(tmp, Cow::Owned(42));
        assert!(!is_owned(tmp));
    }

    #[test]
    fn borrowed_borrowed() {
        let input: Cow<Cow<usize>> = Cow::Borrowed(&Cow::Borrowed(&42));
        let tmp = input.unnest();

        assert_eq!(tmp, Cow::Owned(42));
        assert!(!is_owned(tmp));
    }
}
