//! An iterator over data wrapped in `Cow`.
//!
//! See the documentation for [`CowIter`] for more.
//!
//! [`CowIter`]: enum.CowIter.html

use std::borrow::Cow;
use std::marker::PhantomData;

/// An iterator over data wrapped in `Cow`.
///
/// This allows to create iterators from data wrapped in `Cow` easily. The
/// value returned at each iteration will be either `Owned` or `Borrowed`,
/// depending on the initial object variant.
///
/// # Example
///
/// In this example, we use the borrowed variant:
///
/// ```
/// use std::borrow::Cow;
/// use butcher::iterator::CowIter;
///
/// // Notice that the generic type is annotated
/// let data: Cow<[_]> = Cow::Owned(vec![0, 1, 1, 3]);
/// let mut input = CowIter::from_cow(data);
///
/// assert_eq!(input.next(), Some(Cow::Owned(0)));
/// assert_eq!(input.next(), Some(Cow::Owned(1)));
/// assert_eq!(input.next(), Some(Cow::Owned(1)));
/// assert_eq!(input.next(), Some(Cow::Owned(3)));
/// assert_eq!(input.next(), None)
/// ```
///
/// Here, we use the owned variant:
///
/// ```
/// use std::borrow::Cow;
/// use butcher::iterator::CowIter;
///
/// let data = Cow::Borrowed(&[0, 1, 1, 3] as &[_]);
/// let mut input = CowIter::from_cow(data);
///
/// assert_eq!(input.next(), Some(Cow::Borrowed(&0)));
/// assert_eq!(input.next(), Some(Cow::Borrowed(&1)));
/// assert_eq!(input.next(), Some(Cow::Borrowed(&1)));
/// assert_eq!(input.next(), Some(Cow::Borrowed(&3)));
/// assert_eq!(input.next(), None)
/// ```
///
/// # Generics
///
/// This enum uses a lot of generics in order to work. Users should not care
/// about them. Most of the use cases may not require to manually specify the
/// generic types.
///
/// The only annotation which *may* be required is `Input` when
/// explicitely dealing with the `Owned` variant. Fortunately, this can be
/// circumvented by specifying the generic type of the associated `Cow`
/// somewhere. This can be specified when the `Cow` is created, or in the
/// function signature.
pub enum CowIter<'a, I, Input, Iterr1, Iterr2>
where
    I: 'a + ToOwned,
    Iterr1: Iterator<Item = &'a I>,
    Iterr2: Iterator<Item = <I as ToOwned>::Owned>,
    Input: 'a + ToOwned + ?Sized,
    &'a Input: IntoIterator<Item = &'a I, IntoIter = Iterr1> + ToOwned,
    <Input as ToOwned>::Owned: IntoIterator<Item = <I as ToOwned>::Owned, IntoIter = Iterr2>,
{
    Borrowed(Iterr1, PhantomData<Input>),
    Owned(Iterr2, PhantomData<Input>),
}

impl<'a, I, Input, Iterr1, Iterr2> CowIter<'a, I, Input, Iterr1, Iterr2>
where
    I: 'a + ToOwned,
    Iterr1: Iterator<Item = &'a I>,
    Iterr2: Iterator<Item = <I as ToOwned>::Owned>,
    Input: 'a + ToOwned + ?Sized,
    &'a Input: IntoIterator<Item = &'a I, IntoIter = Iterr1>,
    <Input as ToOwned>::Owned: IntoIterator<Item = <I as ToOwned>::Owned, IntoIter = Iterr2>,
{
    /// Creates a `CowIter` from a `Cow` containing an owned or borrowed object
    /// which can be iterated over.
    // TODO: write a test for this function
    pub fn from_cow(i: Cow<'a, Input>) -> CowIter<I, Input, Iterr1, Iterr2> {
        match i {
            Cow::Owned(i) => CowIter::Owned(i.into_iter(), PhantomData),
            Cow::Borrowed(i) => {
                let i: &'a Input = i;
                CowIter::Borrowed(i.into_iter(), PhantomData)
            }
        }
    }
}

impl<'a, I, Input, Iterr1, Iterr2> Iterator for CowIter<'a, I, Input, Iterr1, Iterr2>
where
    I: 'a + ToOwned,
    Iterr1: Iterator<Item = &'a I>,
    Iterr2: Iterator<Item = <I as ToOwned>::Owned>,
    Input: 'a + ToOwned + ?Sized,
    &'a Input: IntoIterator<Item = &'a I, IntoIter = Iterr1> + ToOwned,
    <Input as ToOwned>::Owned: IntoIterator<Item = <I as ToOwned>::Owned, IntoIter = Iterr2>,
{
    type Item = Cow<'a, I>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            CowIter::Borrowed(it, _) => it.next().map(Cow::Borrowed),
            CowIter::Owned(it, _) => it.next().map(Cow::Owned),
        }
    }
}

#[cfg(test)]
mod cow_iter {
    use super::*;

    #[test]
    fn iterator_owned() {
        let numbers: Cow<[_]> = Cow::Owned(vec![0, 1, 1, 2, 3, 5]);
        let mut iter = CowIter::from_cow(numbers);

        assert_eq!(iter.next(), Some(Cow::Owned(0)));
        assert_eq!(iter.next(), Some(Cow::Owned(1)));
        assert_eq!(iter.next(), Some(Cow::Owned(1)));
        assert_eq!(iter.next(), Some(Cow::Owned(2)));
        assert_eq!(iter.next(), Some(Cow::Owned(3)));
        assert_eq!(iter.next(), Some(Cow::Owned(5)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn iterator_borrowed() {
        let numbers: Cow<[_]> = Cow::Borrowed(&[0, 1, 1, 2, 3, 5] as &[_]);
        let mut iter = CowIter::from_cow(numbers);

        assert_eq!(iter.next(), Some(Cow::Borrowed(&0)));
        assert_eq!(iter.next(), Some(Cow::Borrowed(&1)));
        assert_eq!(iter.next(), Some(Cow::Borrowed(&1)));
        assert_eq!(iter.next(), Some(Cow::Borrowed(&2)));
        assert_eq!(iter.next(), Some(Cow::Borrowed(&3)));
        assert_eq!(iter.next(), Some(Cow::Borrowed(&5)));
        assert_eq!(iter.next(), None);
    }
}
