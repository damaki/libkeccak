# Libkeccak

This project implements the Keccak family of sponge functions and related
constructions using the SPARK 2014 programming language.

libkeccak implements the following constructions:

* The Keccak-p permutation for state sizes of 25, 50, 100, 200, 400, 800, and 1600 bits (see [1] and [2]).
* The Sponge construction
* The Duplex construction
* Hash functions based on the Sponge construction
* eXtendable Output Functions (XOF) based on the Sponge construction
* cSHAKE, KMAC, and TupleHash as specified in NIST SP 800-185 [4]

libkeccak also provides concrete implementations for the hash functions and
XOFs described in NIST FIPS 202 (see [1]):

* Hash functions:
  * SHA3-224
  * SHA3-256
  * SHA3-384
  * SHA3-512
* XOFs:
  * SHAKE128
  * SHAKE256
  * RawSHAKE128
  * RawSHAKE256

Hash function configurations are also provided for the hash functions defined by
the Keccak team which were submitted in the SHA-3 competition:

* Keccak-224
* Keccak-256
* Keccak-384
* Keccak-512

These hash functions differ from the final SHA-3 hash functions only in that the
SHA-3 functions append two additional bits to each message, whereas the Keccak
hash functions do not.

The library's algorithms are implemented using Ada's powerful generics. This
allows for extensive customization and re-use of the various algorithms. You
can define your own configurations of hash functions. For example, you can 
create a hash function using a reduced-round Keccak permutation, or you can
re-use the sponge construction based on a custom permutation algorithm.

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

Building libkeccak requires GNAT 2015 for the GNAT toolset, and and SPARK 2015
for GNATprove.

To build libkeccak, change to the source directory and type:
<pre><code>make build</code></pre>

To install libkeccak to ``<destination>`` type:
<pre><code>make install &lt;destination&gt;</code></pre>

To run GNATprove to generate the proofs type:
<pre><code>make proof</code></pre>

To run the tests type:
<pre><code>make test</code></pre>

To run the benchmark type:
<pre><code>make benchmark</code></pre>

# Benchmarks

The following performance measurements were taken on an Intel Core i7-2630QM
2.0 GHz (@2.6 GHz with turbo boost) on 64-bit Linux. The code was compiled using
GNAT GPL 2017 (20170515).

| Algorithm               | Performance |  Cycles Estimate  |
| ----------------------- | ----------- | ----------------- |
| SHA3-224                | 161 MiB/s   | 15.37 cycles/byte |
| SHA3-256                | 154 MiB/s   | 16.06 cycles/byte |
| SHA3-384                | 121 MiB/s   | 20.57 cycles/byte |
| SHA3-512                | 85 MiB/s    | 29.31 cycles/byte |
| Keccak-224              | 162 MiB/s   | 15.25 cycles/byte |
| Keccak-256              | 154 MiB/s   | 16.08 cycles/byte |
| Keccak-384              | 120 MiB/s   | 20.62 cycles/byte |
| Keccak-512              | 85 MiB/s    | 29.13 cycles/byte |
| SHAKE128 (Absorbing)    | 186 MiB/s   | 13.27 cycles/byte |
| SHAKE128 (Squeezing)    | 180 MiB/s   | 13.76 cycles/byte |
| SHAKE256 (Absorbing)    | 154 MiB/s   | 16.07 cycles/byte |
| SHAKE256 (Squeezing)    | 150 MiB/s   | 16.54 cycles/byte |
| RawSHAKE128 (Absorbing) | 187 MiB/s   | 13.24 cycles/byte |
| RawSHAKE128 (Squeezing) | 180 MiB/s   | 13.75 cycles/byte |
| RawSHAKE256 (Absorbing) | 154 MiB/s   | 16.08 cycles/byte |
| RawSHAKE256 (Squeezing) | 150 MiB/s   | 16.55 cycles/byte |
| Duplex (c448)           | 141 MiB/s   | 17.60 cycles/byte |
| Duplex (c512)           | 134 MiB/s   | 18.56 cycles/byte |
| Duplex (c768)           | 106 MiB/s   | 23.40 cycles/byte |
| Duplex (c1024)          | 77 MiB/s    | 32.34 cycles/byte |
| Keccak-f\[1600\]        | 765 ns/call | 1989 cycles/call  |
| Keccak-f\[800\]         | 680 ns/call | 1768 cycles/call  |
| Keccak-f\[400\]         | 649 ns/call | 1688 cycles/call  |
| Keccak-f\[200\]         | 587 ns/call | 1527 cycles/call  |
| Keccak-f\[100\]         | 717 ns/call | 1864 cycles/call  |
| Keccak-f\[50\]          | 590 ns/call | 1535 cycles/call  |
| Keccak-f\[25\]          | 359 ns/call | 933 cycles/call   |

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
