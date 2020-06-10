## Butcher

[![Build Status][actions-badge]][actions-url]
[![Latest Version][version-badge]][version-url]
[![Rust Documentation][docs-badge]][docs-url]

[actions-badge]: https://github.com/scileo/butcher/workflows/Continuous%20integration/badge.svg
[actions-url]: https://github.com/scileo/butcher/actions?query=workflow%3A%22Continuous+integration%22
[version-badge]: https://img.shields.io/crates/v/butcher.svg
[version-url]: https://crates.io/crates/butcher
[docs-badge]: https://img.shields.io/badge/docs-latest-blue.svg
[docs-url]: https://docs.rs/butcher

An easy way to interact with `Cow`ed structs and enums.

## Disclaimer

This crate is still in early steps of developpments. It should not be used in
production.

## Concept

This crate aims to allow allow simple destructuring (for `struct`s), pattern
matching (for `enum`s and `struct`s) and iteration (for `enum`s and `struct`s
that implement it).

### Destructuring

TODO

### Pattern matching

See [this gist](https://gist.github.com/5bb57b8bf4bfc08758d9cb557e1fdbfe).

### Iteration

This crate provide a `CowIter` type, which allows to write `Cow` fiendly
iterators. See this example:

```rust
use std::borrow::Cow;
use butcher::iterator::CowIter;

fn print_numbers(elems: Cow<[u32]>) {
    let mut iter = CowIter::from(elems);

    for element in iter {
        // The type of element is Cow<u32>
        println!("{:?}", element);
    }
}
```

#### License

<sup>
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
</sub>

