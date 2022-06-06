This directory contains a nested Alire crate configured for running GNATprove.
The purpose of this nested project is to avoid a dependency on `gnatprove` in the
top-level `alire.toml`.

The recommended workflow is to have a nested crate (this crate) that depends on
libkeccak and on gnatprove, that is used for the verification. This nested crate
needs not to be explicitly published, but can be distributed normally with the
main library sources.
See here: https://github.com/alire-project/alire/blob/master/doc/catalog-format-spec.md#using-pins-for-crate-testing