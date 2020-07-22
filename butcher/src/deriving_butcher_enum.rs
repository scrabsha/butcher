//! # Quick introduction to butchering on enums
//!
//! **Note**: this page does not cover how the butchering methods work, but
//! just shows some precise examples on how to use butcher on enums. In order
//! to learn how to work with butcher, it is advised to read the
//! [Quick introduction to butchering on structs][qibs]
//!
//! [qibs]: ../deriving_butcher_struct/index.html
//!
//! ## On a simple enum
//!
//! ```rust
//! use butcher::Butcher;
//!
//! #[derive(Clone, Butcher)]
//! enum Coin {
//!     Euro {
//!         value: usize,
//!     },
//!     Pound(f32),
//!     Quoted(String),
//!     None,
//! }
//! ```
//!
//! Want to add some butchering method? Just as simple as you expect:
//!
//! ```rust
//! use butcher::Butcher;
//!
//! #[derive(Clone, Butcher)]
//! enum Coin {
//!     Euro {
//!         #[butcher(copy)]
//!         value: usize,
//!     },
//!     Pound(f32),
//!     Quoted(
//!         #[butcher(flatten)]
//!         String,
//!     ),
//!     None,
//! }
//! ```
//!
//! Generics? Say no more:
//!
//! ```rust
//! use butcher::Butcher;
//!
//! #[derive(Clone, Butcher)]
//! enum Foo<T> {
//!     Bar(T),
//! }
//! ```
