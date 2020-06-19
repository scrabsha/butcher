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
//! The [`butcher_struct`] macro allows to destructurate any struct. See the
//! following example for a quick explanation of its usage.
//!
//! It should be noted that the type returned for some field may not be optimal.
//! For example, a `Cow<String>` can be replaced by a `Cow<str>`. This is
//! currently not supported by `butcher_struct`
//!
//! [`butcher_struct`]: macro.butcher_struct.html
//!
//! ```rust
//! use std::borrow::Cow;
//! use butcher::butcher_struct;
//!
//! #[derive(Clone)]
//! struct Book {
//!     title: String,
//!     id: usize,
//!     author: Box<str>,
//!     // An useless field
//!     is_opened: bool,
//! }
//!
//! fn destructure_book(b: Cow<Book>) -> (Cow<usize>, Cow<String>, Cow<Box<str>>) {
//!     butcher_struct!(b: Book, id, title, author)
//! }
//! ```
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

pub mod iterator;

// This is blocked by issue #54727. See it on github:
// https://github.com/rust-lang/rust/issues/54727
//
// As such, we have to switch to a hand-made declarative macro.
// pub use butcher_proc_macro::butcher_struct;

#[macro_export]
macro_rules! butcher_struct {
    {
        $var_name: ident : $ty: ident,
        $( $field: ident ),+ $(,)?
    } => {{
        use std::borrow::Cow;
        match $var_name {
            Cow::Owned($ty {
                $(
                    $field,
                )+
                ..
            }) => (
                $(
                    Cow::Owned($field),
                )+
            ),
            Cow::Borrowed($ty {
                $(
                    $field,
                )+
                ..
            }) => (
                $(
                    Cow::Borrowed($field),
                )+
            ),
        }
    }};

    {
        $var_name: ident : $ty: ident $( , )?
    } => {
        compile_error!("This non-distructuring is equivalent of a Drop. Use this function instead.");
    }
}

pub use butcher_proc_macro::*;

pub use butcher_core::ButcherField;

#[cfg(test)]
mod derive_butcher {
    use super::*;

    mod r#struct {
        use super::*;

        #[allow(dead_code)]
        #[derive(Butcher)]
        struct Foo<'a, T> {
            #[butcher(copy)]
            first: usize,
            #[butcher(deref, T: Clone)]
            second: Box<T>,
            #[butcher(copy)]
            third: &'a usize,
            #[butcher(copy, T: Clone)]
            fourth: T,
            #[butcher(regular, T: Clone)]
            fifth: T,
            #[butcher(as_ref)]
            sixth: String,
            #[butcher(copy, T: 'a)]
            seventh: &'a T,
            #[butcher(as_ref, T: Clone)]
            eighth: Vec<T>,
        }
    }
}
