[![Build Status](https://travis-ci.com/damaki/libkeccak.svg?branch=master)](https://travis-ci.com/damaki/libkeccak)

# Libkeccak

This project implements the Keccak family of sponge functions and related
constructions using the SPARK 2014 programming language.

libkeccak implements the following constructions:

* The Keccak-p permutation for state sizes of 25, 50, 100, 200, 400, 800, and 1600 bits (see [1] and [2]).
* The Gimli permutation [7]
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
* XOFs:
  * SHAKE128 and SHAKE256
  * RawSHAKE128 and RawSHAKE256
* cSHAKE:
  * CSHAKE128 and CSHAKE256
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

Building libkeccak requires an Ada 2012 compiler which understands SPARK 2014
annotations, such as GCC or AdaCore's GNAT GPL toolset. To run the proofs,
the SPARK (Community or Pro) 2019 toolset is required.

To build libkeccak based on the autodetected CPU architecture, change to the
repository directory and type:
<pre><code>make build</code></pre>

You can specify a specific architecture by setting the ARCH and SIMD arguments.
For example:
<pre><code>make build ARCH=x86_64 SIMD=SSE2</code></pre>

Currently, the following ARCH and SIMD values are supported:

| ARCH    | Valid SIMD values   |
| ------- | ------------------- |
| generic | none                |
| x86_64  | none, SSE2, or AVX2 |

`ARCH=generic` should be used for any architecture which does not appear in the
above table.

Enabling `SIMD=SSE2` will use SSE2 instructions to speed up parallel algorithms
such as KangarooTwelve and ParallelHash. Using `SIMD=AVX2` will also enable the
AVX2 instruction set (in addition to SSE2).
To disable SSE2 and AVX2 on x86_64, set `SIMD=none`.

To install libkeccak to ``<destination>`` type:
<pre><code>make install &lt;destination&gt;</code></pre>

To run GNATprove to generate the proofs type:
<pre><code>make proof</code></pre>

_Note: SPARK Discovery 2017 is not distributed with the CVC4 or Z3 provers,_
_which are required to prove libkeccak. You can install them by following_
_the instructions [here](http://docs.adacore.com/spark2014-docs/html/ug/en/appendix/alternative_provers.html#installed-with-spark-discovery)_

To run the tests type:
<pre><code>make test</code></pre>

To run the benchmark type:
<pre><code>make benchmark</code></pre>

# Benchmarks

The following performance measurements were taken on an Intel Core i7-2630QM
2.0 GHz (@2.6 GHz with turbo boost) "Sandy Bridge" on 64-bit Linux. The code
was compiled using GNAT GPL 2017 (20170515) with `ARCH=x86_64 SIMD=SSE2`.
The measurements shown are the output of the benchmark program.

```
Message size: 524288 bytes
Performing 200 measurements for each test

KangarooTwelve (Absorbing): 3.4 cycles/byte
KangarooTwelve (Squeezing): 5.3 cycles/byte
SHA3-224: 9.3 cycles/byte
SHA3-256: 9.8 cycles/byte
SHA3-384: 12.6 cycles/byte
SHA3-512: 17.8 cycles/byte
Keccak-224: 9.3 cycles/byte
Keccak-256: 9.8 cycles/byte
Keccak-384: 13.0 cycles/byte
Keccak-512: 18.4 cycles/byte
SHAKE128 (Absorbing): 8.3 cycles/byte
SHAKE128 (Squeezing): 8.9 cycles/byte
SHAKE256 (Absorbing): 10.1 cycles/byte
SHAKE256 (Squeezing): 10.3 cycles/byte
RawSHAKE128 (Absorbing): 8.0 cycles/byte
RawSHAKE128 (Squeezing): 8.6 cycles/byte
RawSHAKE256 (Absorbing): 9.8 cycles/byte
RawSHAKE256 (Squeezing): 10.3 cycles/byte
Duplex r1152c448: 1604 cycles
Duplex r1088c512: 1588 cycles
Duplex r832c768: 1522 cycles
Duplex r576c1024: 1434 cycles
Keccak-p\[1600,24\]: 1190 cycles
Keccak-p\[1600,24\]×2: 1572 cycles
Keccak-p\[1600,24\]×4: 3090 cycles
Keccak-p\[1600,24\]×8: 6388 cycles
Keccak-p\[1600,12\]: 620 cycles
Keccak-p\[1600,12\]×2: 808 cycles
Keccak-p\[1600,12\]×2: 1623 cycles
Keccak-p\[1600,12\]×2: 3202 cycles
Keccak-p\[800,22\]: 1098 cycles
Keccak-p\[400,20\]: 1030 cycles
Keccak-p\[200,18\]: 898 cycles
Keccak-p\[100,16\]: 1096 cycles
Keccak-p\[50,14\]: 976 cycles
Keccak-p\[25,12\]: 464 cycles
```

# Formal Verification

SPARK 2014 and GNATprove are used to provide proof that the implementation is
free of errors such as: integer overflows, buffer overruns, use of
uninitialized variables, and that all loops terminate. Nothing is provided
currently to prove that the library correctly implements the algorithms
according to the specifications. However, there are tests to provide assurances
of the correctness of the algorithms with the Known Answer Tests. It is intended
at some point to add proof that the algorithms correctly implement the specification.

# Testing

Correctness of the algorithms is demonstrated using a combination of unit
testing and Known Answer Tests (KAT). The Known Answer Tests comprise the bulk
of the tests and they provide assurance that the algorithms are implemented
correctly.

The unit tests aim to cover the cases that are not covered by the KAT, such
as boundary conditions. As the project moves forwards I will experiment with
replacing tests with proof.

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