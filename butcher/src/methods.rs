//! Different ways to butcher a field.

use std::borrow::{Borrow, Cow};
use std::ops::Deref;

/// Allow to unify the behavior of the different butchering methods.
///
/// `T` is the input type, which can be either owned or borrowed for `'cow`. The
/// `from_owned` and `from_borrowed` take either an owned or a borrowed `T`, and
/// produce a given output type.
pub trait ButcheringMethod<'cow, T>
where
    T: 'cow,
{
    /// The output type.
    type Output: 'cow;

    /// Create an output with an owned input.
    fn from_owned(i: T) -> Self::Output;

    /// Creates an output with a borrowed input.
    fn from_borrowed(i: &'cow T) -> Self::Output;
}

/// The regular method.
///
/// This method will transform a type `T` into a `Cow<T>`. It requires that `T`
/// is `Clone`, but won't clone it.
pub struct Regular;

impl<'cow, T> ButcheringMethod<'cow, T> for Regular
where
    T: Clone + 'cow,
{
    type Output = Cow<'cow, T>;

    /// Create an `Owned` variant, containing `T`.
    fn from_owned(i: T) -> Self::Output {
        Cow::Owned(i)
    }

    /// Create a `Borrowed` variant, containing a reference to `T`.
    fn from_borrowed(i: &'cow T) -> Self::Output {
        Cow::Borrowed(i)
    }
}

/// The flatten method.
///
/// This method will transform a type `T` which implements `Deref` into a
/// `Cow<<T as Deref>::Target>`. This allows users not to have to deal with
/// for instance `Cow<String>`, and instead automatically using `Cow<str>`.
///
/// It requires `T` to implement `Deref` and `Borrow<<T as Deref>::Target>`,
/// `<T as Deref>::Target` to implement `ToOwned`, and there must be
/// `<<T as Deref>::Target as ToOwned>::Owned = T`.
pub struct Flatten;

impl<'cow, T> ButcheringMethod<'cow, T> for Flatten
where
    T: Deref + Borrow<<T as Deref>::Target> + 'cow,
    <T as Deref>::Target: ToOwned + 'cow,
    T: Into<<<T as Deref>::Target as ToOwned>::Owned>,
{
    type Output = Cow<'cow, <T as Deref>::Target>;

    /// Create an `Owned` variant, containing `T`.
    fn from_owned(i: T) -> Self::Output {
        Cow::Owned(i.into())
    }

    /// Create a `Borrowed` variant, containing a reference to `T`.
    fn from_borrowed(i: &'cow T) -> Self::Output {
        Cow::Borrowed(i)
    }
}

/// The unbox method.
///
/// This method allows to get rid of `Box` which where used in the fields in
/// order to create recursive types. Most of the time, `T` is a `Box<U>`.
pub struct Unbox;

impl<'cow, T> ButcheringMethod<'cow, Box<T>> for Unbox
where
    T: Clone + 'cow,
{
    type Output = Cow<'cow, T>;

    /// Create an `Owned` variant, using the conversion requirements described
    /// previously.
    fn from_owned(i: Box<T>) -> Self::Output {
        Cow::Owned(*i)
    }

    /// Create a `Borrowed` variant, using the `Deref` trait.
    fn from_borrowed(i: &'cow Box<T>) -> Self::Output {
        Cow::Borrowed(Deref::deref(i))
    }
}

/// The copy method.
///
/// **Note**: this is not related to the `Copy` trait, but it effectively copies
/// some data.
///
/// This method does not output any `Cow` at all. Instead, it always copies the
/// data provided as input, using the `Clone` trait.
pub struct Copy;

impl<'cow, T> ButcheringMethod<'cow, T> for Copy
where
    T: Clone + 'cow,
{
    type Output = T;

    /// Move the data.
    ///
    /// This may be reduced to a no-op.
    fn from_owned(i: T) -> Self::Output {
        i
    }

    /// `Clone` the input data.
    fn from_borrowed(i: &'cow T) -> Self::Output {
        i.clone()
    }
}

/// Allow to butcher a specific field of a `struct` or `enum`.
///
/// Implementors just have to specify a correct butchering method. The rest is
/// automatically implemented.
pub trait ButcherField<'cow, T>
where
    T: 'cow,
{
    /// The method which will be used.
    type Method: ButcheringMethod<'cow, T>;

    fn from_owned(i: T) -> <Self::Method as ButcheringMethod<'cow, T>>::Output {
        <Self::Method as ButcheringMethod<'cow, T>>::from_owned(i)
    }

    fn from_borrowed(i: &'cow T) -> <Self::Method as ButcheringMethod<'cow, T>>::Output {
        <Self::Method as ButcheringMethod<'cow, T>>::from_borrowed(i)
    }
}
