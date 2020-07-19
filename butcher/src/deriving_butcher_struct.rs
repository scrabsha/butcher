//! # Quick introduction to butchering
//!
//! It is sometimes needed to destructure an object, or to pattern-match over
//! an enum. This can lead to a lot of code duplication when such objects are
//! wrapped in `Cow`. This macro aims to generate the boilerplate automatically.
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
//! Destructing `Client`, when it is not wrapped in a `Cow`, is very easy:
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
//! But once your `Client` is wrapped in a `Cow`, it becomes harder:
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
//! so that destructing is more intuitive. Each struct field can be destructed
//! with a destructing method. They are all described in the next four
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
//!     #[butcher(flatten)]
//!     c: String,
//!     #[butcher(unbox)]
//!     d: Box<Ipv4Addr>,
//! }
//! ```
//!
//! ## Regular
//!
//! This method is used by default. If a field has type `T`, then the
//! corresponding butchered field will have type `T` too.
//!
//! ## Copy
//!
//! This method will always copy the data (using the `Clone` trait), instead of
//! returning a `Cow`. This can be used for type whose size is small, such as
//! integers.
//!
//! In the previous example, the field `age` of `Client` may be marked as
//! `copy`.
//!
//! ## Flatten
//!
//! This method can be used when dealing with a type which also has an non-owned
//! variant. The corresponding butchered field will have the type
//! `<T as Deref>::Target`. For the case of a `String`, it would return a simple
//! `Cow<str`, which is better to work on. The same goes for `PathBuf`,
//! `OsString`, `CString`, and so on.
//!
//! In the previous example, the field `name` of `Client` may be marked as
//! `Flatten`.
//!
//! In order to use such butchering method to your own data structures, you
//! must implement the `Deref` and the dereferenced type must implement
//! `ToOwned` so that `<<T as Deref>::Target as ToOwned>::Owned == T`.
//!
//! ## Unbox
//!
//! An usage of `Box` on sized types is to create recursive types. This
//! butchering method will allow one to automatically get the data from the box.
//!
//! ## Fixing triggered compilation errors
//!
//! While this proc macro generally generates code that compile on the first
//! try, it may become tricky when generics are involved. The next section will
//! show how to fix most errors.
//!
//! ### A reference to a generic type
//!
//! The following example does not compile:
//!
//! ```compile_fail
//! use butcher::Butcher;
//!
//! #[derive(Butcher, Clone)]
//! struct Foo<'a, T> {
//!     elem: &'a T,
//! }
//! ```
//!
//! It gives us the following error:
//!
//! ```none
//! error[E0309]: the parameter type `T` may not live long enough
//!  --> src/deriving_butcher_struct.rs:170:10
//!   |
//! 6 | #[derive(Butcher, Clone)]
//!   |          ^^^^^^^ ...so that the reference type `&'a T` does not outlive the data it points at
//! 7 | struct Foo<'a, T> {
//! 8 |     elem: &'a T,
//!   |               - help: consider adding an explicit lifetime bound...: `T: 'a`
//!   |
//!   = note: this error originates in a derive macro (in Nightly builds, run with -Z macro-backtrace for more info)
//! ```
//!
//! In order to fix it, it is needed to specify the lifetime bound, so that
//! the procedural macro can specify it too. It can be defined like so:
//!
//! ```rust
//! use butcher::Butcher;
//!
//! #[derive(Butcher, Clone)]
//! struct Foo<'a, T> {
//!     #[butcher(regular, T: 'a)]
//!     elem: &'a T,
//! }
//! ```
