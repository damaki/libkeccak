[![Build Status](https://travis-ci.com/damaki/libkeccak.svg?branch=master)](https://travis-ci.com/damaki/libkeccak)

# Libkeccak

This project implements the Keccak family of sponge functions and related
constructions using the SPARK 2014 programming language, with static proof
of type safety and good performance.

libkeccak supports the following cryptographic permutations:
* The Keccak-p permutation for state sizes of 25, 50, 100, 200, 400, 800, and 1600 bits (see [1] and [2]).
* The Gimli permutation [7]
* The Ascon permutation [8]

libkeccak implements the following generic constructions which can
be instantiated using the above permutations and with various parameters:
* The Sponge construction
* The Duplex construction
* The MonkeyDuplex construction
* The MonkeyWrap construction
* Hash functions based on the Sponge construction
* eXtendable Output Functions (XOF) based on the Sponge construction
* cSHAKE, KMAC, TupleHash, and ParallelHash as specified in NIST SP 800-185 [4]
* KangarooTwelve as specified by the Keccak team [5]

libkeccak also provides concrete implementations of the above constructions,
as specified in [1,4,5,6,7]:

* Hash functions:
  * SHA-3 (224, 256, 384, and 512 bits)
  * Keccak (224, 256, 384, and 512 bits)
  * Gimli-Hash
  * Ascon-Hash
* XOFs:
  * SHAKE128 and SHAKE256
  * RawSHAKE128 and RawSHAKE256
  * Ascon-XOF
* cSHAKE:
  * cSHAKE128 and cSHAKE256
* KMAC:
  * KMAC128 and KMAC256
* TupleHash:
  * TupleHash128 and TupleHash256
* Parallel Hashes:
  * KangarooTwelve
  * MarsupilamiFourteen (256-bit security variant of KangarooTwelve)
  * ParallelHash128 and ParallelHash256
* Authenticated encryption:
  * Ketje (Jr, Sr, Minor, and Major variants)

Note that the difference between a hash function an a XOF function is that a
hash function has a fixed output length (for example, 256 bits), whereas the
XOFs have arbitrary output length.

The library's algorithms are implemented using Ada's powerful generics. This
allows for extensive customization and re-use of the various algorithms. The
generic Sponge, XOF, and Hash packages can be instantiated for other permutation
functions (other than just the Keccak permutation). This also permits use of
this library based on platforms with hardware accelerated implementations of
the Keccak permutation.

# Example

Here's an example of calculating the SHA3-256 hash of a byte array (array of
type ``Interfaces.Unsigned_8``):

```Ada
with Keccak.Types;
with SHA3;

function Compute_Hash(Data : in Keccak.Types.Byte_Array)
   return SHA3.SHA3_256.Digest_Type
is
   Ctx    : SHA3.SHA3_256.Context;
   Digest : SHA3.SHA3_256.Digest_Type;

begin
   SHA3.SHA3_256.Init(Ctx);
   SHA3.SHA3_256.Update(Ctx, Data);
   SHA3.SHA3_256.Final(Ctx, Digest);

   return Digest;
end Compute_Hash;
```

# License

Libkeccak is licensed under the 3-clause BSD license.

# Building

Assuming you've cloned this repository and have Alire installed:

```sh
alr build
```

libkeccak can be built with SIMD optimisations, if your platform supports them,
by setting the following GPR variables:

| Variable | Values | Default |
| -------- | ------ | ------- |
| LIBKECCAK_ARCH | `generic`, `x86_64` | `generic` |
| LIBKECCAK_SIMD | `none`, `SSE2`, `AVX2` |

>:warning: `SSE2` and `AVX2` are only available on `x86_64` architectures.

Enabling `SSE2` will use SSE2 instructions to speed up parallel algorithms
such as KangarooTwelve and ParallelHash. Using `SIMD=AVX2` will also enable the
AVX2 instruction set (in addition to SSE2).
To disable SSE2 and AVX2 on x86_64, set `SIMD=none`.

>:warning: `AVX2` is not guaranteed to work on Windows since GCC does not ensure 32-byte
stack alignment. See [GCC Bug #54412](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=54412)

# Benchmarks

The following performance measurements were taken on an AMD Ryzen 7 5800X on Windows 10.
The code was compiled using gnat 11.2.0-4 with the following configuration:
* `LIBKECCAK_ARCH=x86_64`
* `LIBKECCAK_SIMD=AVX2`

The measurements shown are the output of the benchmark program.

```
Message size: 524288 bytes
Performing 200 measurements for each test

Gimli: 379 cycles
Gimli Hash: 23.9 cycles/byte
Ascon (12 rounds): 113 cycles
Ascon (8 rounds): 75 cycles
Ascon (6 rounds): 74 cycles
Ascon-Hash: 17.4 cycles/byte
KangarooTwelve (Absorbing): 1.7 cycles/byte
KangarooTwelve (Squeezing): 3.0 cycles/byte
MarsupilamiFourteen (Absorbing): 2.2 cycles/byte
MarsupilamiFourteen (Squeezing): 4.0 cycles/byte
ParallelHash128 (Absorbing): 2.5 cycles/byte
ParallelHash128 (Squeezing): 5.0 cycles/byte
ParallelHash256 (Absorbing): 2.9 cycles/byte
ParallelHash256 (Squeezing): 6.2 cycles/byte
SHA3-224: 6.1 cycles/byte
SHA3-256: 6.4 cycles/byte
SHA3-384: 8.3 cycles/byte
SHA3-512: 11.8 cycles/byte
Keccak-224: 6.1 cycles/byte
Keccak-256: 6.4 cycles/byte
Keccak-384: 8.3 cycles/byte
Keccak-512: 11.8 cycles/byte
SHAKE128 (Absorbing): 5.3 cycles/byte
SHAKE128 (Squeezing): 5.0 cycles/byte
SHAKE256 (Absorbing): 6.4 cycles/byte
SHAKE256 (Squeezing): 6.1 cycles/byte
RawSHAKE128 (Absorbing): 5.4 cycles/byte
RawSHAKE128 (Squeezing): 5.0 cycles/byte
RawSHAKE256 (Absorbing): 6.5 cycles/byte
RawSHAKE256 (Squeezing): 6.2 cycles/byte
Duplex r1152c448: 987 cycles
Duplex r1088c512: 986 cycles
Duplex r832c768: 949 cycles
Duplex r576c1024: 911 cycles
Keccak-p[1600,24]: 796 cycles
Keccak-p[1600,24]×2: 1100 cycles
Keccak-p[1600,24]×4: 1100 cycles
Keccak-p[1600,24]×8: 2203 cycles
Keccak-p[1600,12]: 416 cycles
Keccak-p[1600,12]×2: 531 cycles
Keccak-p[1600,12]×4: 568 cycles
Keccak-p[1600,12]×8: 1139 cycles
Keccak-p[800,22]: 720 cycles
Keccak-p[400,20]: 683 cycles
Keccak-p[200,18]: 682 cycles
Keccak-p[100,16]: 873 cycles
Keccak-p[50,14]: 759 cycles
Keccak-p[25,12]: 416 cycles
Ketje Jr (AAD): 38.5 cycles/byte
Ketje Jr (Encrypt): 45.0 cycles/byte
Ketje Jr (Decrypt): 45.0 cycles/byte
Ketje Jr (Tag): 44.5 cycles/byte
Ketje Sr (AAD): 21.7 cycles/byte
Ketje Sr (Encrypt): 27.6 cycles/byte
Ketje Sr (Decrypt): 27.5 cycles/byte
Ketje Sr (Tag): 23.6 cycles/byte
Ketje Minor (AAD): 5.1 cycles/byte
Ketje Minor (Encrypt): 8.8 cycles/byte
Ketje Minor (Decrypt): 8.8 cycles/byte
Ketje Minor (Tag): 6.7 cycles/byte
Ketje Major (AAD): 2.7 cycles/byte
Ketje Major (Encrypt): 4.4 cycles/byte
Ketje Major (Decrypt): 4.4 cycles/byte
Ketje Major (Tag): 3.2 cycles/byte
```

# Proofs and Testing

GNATprove is used to prove, via sound static analysis, that the implementation
is free of various errors such as:
 * use of uninitialised variables;
 * integer overflows;
 * division by zero;
 * value out-of-range;
 * out-of-bounds array accesses;
 * non-terminating loops.

This achieves the silver level of assurance (absence of run-time errors)
described in [9].

The proofs do not extend to functional correctness, i.e. the proofs do not
show that the SHA-3 implementation produces the correct results.
Conventional testing is used to provide assurance of the correctness of the
algorithms. The tests consists of Known Answer Tests (KAT) and unit tests.

The KATs comprise the bulk of the tests and they provide assurance that the
algorithms are implemented correctly.

The unit tests aim to cover the cases that are not covered by the KATs, such
as boundary conditions and testing multi-part hashing operations in various
combinations.

# References

* [1] NIST FIPS PUB 202 - SHA-3 Standard: Permutation-Based Hash and Extendable
output Functions. August 2015 http://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.202.pdf
* [2] The Keccak Reference Version 3.0. January 2011
http://keccak.noekeon.org/Keccak-reference-3.0.pdf
* [3] Cryptographic Sponge Functions Version 0.1. January 2011
http://sponge.noekeon.org/CSF-0.1.pdf
* [4] NIST SP 800-185 - SHA-3 Derived Functions: cSHAKE, KMAC, TupleHash, and ParallelHash. December 2016
http://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-185.pdf
* [5] KangarooTwelve: fast hashing based on Keccak-p
http://keccak.noekeon.org/kangarootwelve.html
* [6] CAESAR submission: Ketje v2
https://keccak.team/files/Ketjev2-doc2.0.pdf
* [7] Gimli: a cross-platform permutation
https://gimli.cr.yp.to/index.html
* [8] Ascon
https://ascon.iaik.tugraz.at/index.html
* [9] Implementation Guidance for the Adoption of SPARK
https://www.adacore.com/books/implementation-guidance-spark