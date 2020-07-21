//! # Butcher
//!
//! An easy way to interact with `Cow`ed structs and enums.
//!
//! This crate proposes some simple solutions to the most common patterns I met
//! while working with `Cow`s in Rust. It currently fixes two patterns:
//! destructuring and iteration related to objects wrapped in `Cow`.
//!
//! ## Destructuring
//!
//! The `Butcher` trait can be used when it is necessary to destruture something
//! wrapped in a `Cow`. Below is a simple example:
//!
//! ```rust
//! use std::borrow::Cow;
//! use butcher::Butcher;
//!
//! #[derive(Butcher, Clone)]
//! struct MyNumberList {
//!     val: u32,
//!     next: Option<Box<MyNumberList>>,
//! }
//!
//! fn destructure_list_elem(i: Cow<MyNumberList>) -> (Cow<u32>, Cow<Option<Box<MyNumberList>>>) {
//!     let ButcheredMyNumberList { val, next } = Butcher::butcher(i);
//!
//!     (val, next)
//! }
//! ```
//!
//! See the documentation for [`Butcher`] procedural macro for more.
//!
//! [`Butcher`]: deriving_butcher_struct/index.html
//!
//! ## Iteration
//!
//! Here is a demonstration of how to iterate over an object wrapped in a `Cow`:
//!
//! ```rust
//! use std::borrow::Cow;
//! use butcher::iterator::{CowIter, IntoCowIterator};
//!
//! fn print_numbers(elems: Cow<[u32]>) {
//!     let mut iter = elems.into_cow_iter();
//!
//!     for element in iter {
//!         // The type of element is Cow<u32>
//!         println!("{:?}", element);
//!     }
//! }
//! ```
//!
//! See the documentation of [`CowIter`] for more information.
//!
//! [`CowIter`]: iterator/enum.CowIter.html

pub mod deriving_butcher_enum;
pub mod deriving_butcher_struct;
pub mod iterator;

pub use butcher_proc_macro::*;

use std::borrow::Cow;

pub trait ButcherField<'a> {
    type Input;
    type Output: ToOwned + Sized + 'a;

    fn from_borrowed(b: &'a Self::Input) -> Self::Output;

    fn from_owned(o: Self::Input) -> Self::Output;
}

pub trait Butcher<'cow>: ToOwned + 'cow {
    type Output: 'cow;

    fn butcher(this: Cow<'cow, Self>) -> Self::Output;
}
