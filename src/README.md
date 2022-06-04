The libkeccak sources are organised into the following directories:
 * `common` contains sources that are common to all architectures.
 * `generic` contains instantiations for "generic" architectures.
 * `x86_64` contains instantiations and definitions specific to x86-64
   architectures (i.e. SSE2 and AVX2 definitions).

The `common` directory is always included in the build.
The `LIBKECCAK_ARCH` option selects between the `generic` and `x86_64` directories.