# 0.5.1 (August 03 2020)

This release fixes a bug introduced with the rebutcher butchering method,
which produced unparseable code.

This release only affects `butcher_proc_macro`.

# 0.5.0 (August 03 2020)

Major traits have been renamed so that they follow more closely the rust `std`
API. This includes:
  - the `flatten` butchering method has been renamed `as_deref`,
  - the `FlattenCow` trait has been renamed `AsDerefCow`,
  - the `UnnestCow` trait has been renamed `FlattenCow`.

The `unbutcher` function has been added. It allows to easily set-up a catch-all
match arm, by allowing the convertion `ButcheredFoo -> Foo`.

A proper Minimal Supported Rust Version policy has been set-up.

The `butcher_struct` proc macro has been removed, mainly because the Butcher
derive macro, combined with the Butcher::butcher function was way more usefull.

The `rebutcher` butchering method has been implemented. It allows to butcher
a specific field of a struct or enum when the struct or enum itself is
butchered.

# About Prior Releases Changelog

The previous releases of `butcher` where a bit chaotic. As such, there is no
well-maintained changelog.
