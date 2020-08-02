//! # Butcher
//!
//! An easy way to interact with `Cow`ed structs and enums.
//!
//! This crate provides the following functionalities for data wrapped in `Cow`:
//!   - [destructuring/pattern matching over structs and enums](#destructuringpattern-matching),
//!   - [iterating over collections, or any type that implements `IntoIterator`](#iteration),
//!   - [unnesting `Cow`](#unnesting),
//!   - [flattening `Cow`](#flattening).
//!
//! ## Destructuring/pattern matching
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
//! This also allows pattern matching, as demonstrated in the following example:
//!
//! ```rust
//! use butcher::Butcher;
//! use std::borrow::Cow;
//!
//! #[derive(Butcher, Clone)]
//! enum WebEvent {
//!     PageLoad,
//!     KeyPress(char),
//!     Paste(String),
//!     // or c-like structures.
//!     Click { x: i64, y: i64 },
//! }
//!
//! fn print_action(i: Cow<WebEvent>) {
//!     match WebEvent::butcher(i) {
//!         ButcheredWebEvent::PageLoad => { /* ... */ },
//!         ButcheredWebEvent::KeyPress(key) => { /* ... */ },
//!         ButcheredWebEvent::Paste(pasted) => { /* ... */ },
//!         ButcheredWebEvent::Click { x, y } => { /* ... */ },
//!     }
//! }
//! ```
//!
//! The `Butcher` procedural macro can be derived for [structs][butcher-struct]
//! and for [enums][butcher-enum].
//!
//! [butcher-struct]: deriving_butcher_struct/index.html
//! [butcher-enum]: deriving_butcher_enum/index.html
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
//!
//! ## Unnesting
//!
//! In some situations, the `Butcher` proc macro can generate tricky fields,
//! such as nested `Cow`. The [`UnnestCow`] trait aims to remove such
//! flattening.
//!
//! [`UnnestCow`]: unnest/trait.UnnestCow.html
//!
//! ## Flattening
//!
//! The [`FlattenCow`] trait allows to transform a given `Cow<T>` into a
//! `Cow<<T as Deref>::Target>`. This can be usefull when it is needed to
//! transform a `Cow<String>` into `Cow<str>`.
//!
//! [`FlattenCow`]: flatten/trait.FlattenCow.html

pub mod deriving_butcher_enum;
pub mod deriving_butcher_struct;
pub mod flatten;
pub mod iterator;
pub mod methods;
pub mod unnest;

pub use butcher_proc_macro::*;

use std::borrow::Cow;

pub trait Butcher<'cow>: ToOwned + 'cow {
    type Output: 'cow;

    fn butcher(this: Cow<'cow, Self>) -> Self::Output;

    fn unbutcher(this: Self::Output) -> Self;
}
