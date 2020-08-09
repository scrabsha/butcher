//! # Quick introduction to butchering on structs
//!
//! It is sometimes needed to destructure an object, or to pattern-match over
//! an enum. This can lead to a lot of code duplication when such objects are
//! wrapped in [`Cow`]. The `Butcher` derive macro aims to generate such
//! boilerplate automatically.
//!
//! We'll use a simple example of a struct with the following declaration:
//!
//! ```rust
//! use butcher::Butcher;
//!
//! #[derive(Butcher, Clone)]
//! struct Client {
//!     name: String,
//!     age: u8,
//! }
//! ```
//!
//! Destructing `Client`, when it is not wrapped in a [`Cow`], is very easy:
//!
//! ```rust
//! # #[derive(Butcher, Clone)]
//! # struct Client {
//! #     name: String,
//! #     age: u8,
//! # }
//! use butcher::Butcher;
//!
//! let c = Client {
//!     name: "Grace Hopper".to_string(),
//!     age: 85,
//! };
//!
//! let Client { name, age } = c;
//!
//! assert_eq!(name, "Grace Hopper");
//! assert_eq!(age, 85);
//! ```
//!
//! But once your `Client` is wrapped in a [`Cow`], it becomes harder:
//!
//! ```rust
//! #
//! # #[derive(Butcher, Clone)]
//! # struct Client {
//! #     name: String,
//! #     age: u8,
//! # }
//! use butcher::Butcher;
//! use std::borrow::Cow;
//!
//! let c_in_cow: Cow<Client> = Cow::Owned(Client {
//!     name: "Alan Turing".to_string(),
//!     age: 41,
//! });
//!
//! let (name, age) = match c_in_cow {
//!     Cow::Owned(Client { name, age }) => (Cow::Owned(name), Cow::Owned(age)),
//!     Cow::Borrowed(Client { name, age }) => (Cow::Borrowed(name), Cow::Borrowed(age)),
//! };
//!
//! assert_eq!(name, Cow::Borrowed("Alan Turing"));
//! assert_eq!(age, Cow::Borrowed(&41_u8));
//! ```
//!
//! Let's see how `butcher` can help up:
//!
//! ```rust
//! #
//! # #[derive(Butcher, Clone)]
//! # struct Client {
//! #     name: String,
//! #     age: u8,
//! # }
//! use butcher::Butcher;
//! use std::borrow::Cow;
//!
//! let c_in_cow: Cow<Client> = Cow::Owned(Client {
//!     name: "Alan Turing".to_string(),
//!     age: 41,
//! });
//!
//! let ButcheredClient { name, age } = Client::butcher(c_in_cow);
//!
//! assert_eq!(name, Cow::Borrowed("Alan Turing"));
//! assert_eq!(age, Cow::Borrowed(&41_u8));
//! ```
//!
//! No more boilerplate involved. Neat!
//!
//! If the compilation fails because some traits are required, don't panic,
//! continue reading this page, the last section will solve your problems.
//!
//! # Configuration options
//!
//! The `Butcher` procedural macro has been designed to allow special tricks,
//! so that destructuring is more intuitive. Each struct field can be
//! destructured with a destructuring method. They are all described in the next
//! paragraphs.
//!
//! You can use them like so:
//!
//! ```rust
//! use std::net::Ipv4Addr;
//! use butcher::Butcher;
//!
//! #[derive(Butcher, Clone)]
//! struct Foo {
//!     #[butcher(regular)]
//!     a: Ipv4Addr,
//!     #[butcher(copy)]
//!     b: usize,
//!     #[butcher(as_deref)]
//!     c: String,
//!     #[butcher(unbox)]
//!     d: Box<Ipv4Addr>,
//!     # // The following field is hadded so that it is possible to track any
//!     # // regression for https://github.com/scileo/butcher/issues/7
//!     # cons: Box<Self>,
//! }
//! ```
//!
//! ## Regular
//!
//! This method is used by default. If a field has type `T`, then the
//! corresponding butchered field will have type `Cow<T>`.
//!
//! See the documentation for [`Regular`] for more information.
//!
//! ## Copy
//!
//! This method will always copy the data (using the [`Clone`] trait), instead of
//! returning a [`Cow`]. This can be used for type whose size is small, such as
//! integers.
//!
//! In the previous example, the field `age` of `Client` may be marked as
//! `copy`.
//!
//! See the documentation for [`Copy`] for more information.
//!
//! ## As Deref
//!
//! This method is used for situations when data can be represented both with
//! its borrowed and its owned form. For instance, `[T]` is borrowed while
//! `Vec<T>` is owned. Here, using `as_deref` on a field whose type is `Vec<T>`
//! will convert it into `Cow<[T]>`.
//!
//! See the documentation for [`AsDeref`] for more information.
//!
//! ## Unbox
//!
//! An usage of [`Box`] on sized types is to create recursive types. This
//! butchering method will allow one to automatically get the data from the
//! [`Box`].
//!
//! See the documentation for [`Unbox`] for more information.
//!
//! ## Rebutcher
//!
//! Sometimes it is necessary to butcher again a field of a butchered struct.
//! This is what rebutcher does. This can be helpfull when it is necessay to
//! destructure/pattern match on a struct contained in another struct.
//!
//! See the documentation for [`Rebutcher`] for more information.
//!
//! ## Retrieving the initial input type
//!
//! The `unbutcher` allows to retrieve the initial data, in its owned form.
//! It will move the associated butchered struct, and return the initial struct.
//! Every borrowed data will be cloned if necessary.
//!
//! This is particularly usefull when a catch-all match arm is needed.
//!
//! ```rust
//! # #[derive(Butcher, Clone)]
//! # struct Client {
//! #     #[butcher(as_deref)]
//! #     name: String,
//! #     #[butcher(copy)]
//! #     age: u8,
//! # }
//! use butcher::Butcher;
//! use std::borrow::Cow;
//!
//! let c_in_cow: Cow<Client> = Cow::Owned(Client {
//!     name: "Alan Turing".to_string(),
//!     age: 41,
//! });
//!
//! match Client::butcher(c_in_cow) {
//!     ButcheredClient { name, age } if name.as_ref() == "Abdul Alhazred" => {
//!         println!("Necronomicon author detected!")
//!     },
//!     ButcheredClient { name, age } if age == 255 => {
//!         println!("This person will soon reborn")
//!     },
//!     other => {
//!         // other has type ButcheredClient. Let's convert it to Client:
//!         let other = Client::unbutcher(other);
//!         println!("Hello {}", other.name);
//!     },
//! }
//! ```
//!
//! ## Fixing triggered compilation errors
//!
//! While this proc macro generally generates code that compile on the first
//! try, it may become tricky when generics are involved. The next section will
//! show how to fix most errors.
//!
//! Most of the errors raised when using the macro are trait bound-related. For
//! instance, the following example does not compile:
//!
//! ```compile_fail
//! use butcher::Butcher;
//!
//! #[derive(Butcher, Clone)]
//! struct Foo<T> {
//!     #[butcher(as_deref)]
//!     elem: Vec<T>,
//! }
//! ```
//!
//! It gives us the following error:
//!
//! ```none
//! error[E0277]: the trait bound `[T]: std::borrow::ToOwned` is not satisfied
//!  --> src/deriving_butcher_struct.rs:167:10
//!   |
//! 6 | #[derive(Butcher, Clone)]
//!   |          ^^^^^^^ the trait `std::borrow::ToOwned` is not implemented for `[T]`
//!   |
//!   = note: this error originates in a derive macro (in Nightly builds, run with -Z macro-backtrace for more info)
//! ```
//!
//! So here it is necessary to indicate that `T` must be [`Clone`]. It can be
//! specified right after the butchering method:
//!
//! ```rust
//! use butcher::Butcher;
//!
//! #[derive(Butcher, Clone)]
//! struct Foo<T> {
//!     #[butcher(as_deref, T: Clone)]
//!     elem: Vec<T>,
//! }
//! ```
//!
//! If multiple trait bounds must be specified, then they have to be separated
//! by commas, using regular trait bounds syntax (eg: with the syntax of
//! [`WhereClauseItem`] defined in the reference).
//!
//! [`Cow`]: https://doc.rust-lang.org/std/borrow/enum.Cow.html
//! [`Clone`]: https://doc.rust-lang.org/std/clone/trait.Clone.html
//! [`Box`]: https://doc.rust-lang.org/std/boxed/struct.Box.html
//! [`WhereClauseItem`]: https://doc.rust-lang.org/reference/items/generics.html#where-clauses
//! [`AsDeref`]: ../methods/struct.AsDeref.html
//! [`Copy`]: ../methods/struct.Copy.html
//! [`Rebutcher`]: ../methods/struct.Rebutcher.html
//! [`Regular`]: ../methods/struct.Regular.html
//! [`Unbox`]: ../methods/struct.Unbox.html
