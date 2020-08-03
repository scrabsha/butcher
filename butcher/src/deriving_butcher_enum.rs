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
//!         #[butcher(as_deref)]
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
//!
//! Unbutchering can be used to create simple catch-all match arm:
//!
//! ```rust
//! # fn handle_special_event(_: LogEvent) {}
//! use butcher::Butcher;
//! use std::borrow::Cow;
//!
//! #[derive(Debug, Clone, Butcher)]
//! enum LogEvent {
//!     Info,
//!     Dbg(&'static str),
//!     Warning(
//!         #[butcher(as_deref)]
//!         String,
//!     ),
//! }
//!
//! let event: Cow<LogEvent> = Cow::Owned(LogEvent::Dbg("Heyo"));
//!
//! match LogEvent::butcher(event) {
//!     ButcheredLogEvent::Info => {},
//!     ButcheredLogEvent::Warning(w) => println!("Warning emitted: {}", w),
//!     // A catch-all match arm
//!     other => {
//!         // We get back the original data
//!         let e = LogEvent::unbutcher(other);
//!
//!         handle_special_event(e);
//!     }
//! }
//! ```
