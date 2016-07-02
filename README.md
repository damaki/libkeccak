# Libkeccak

This project implements the Keccak family of sponge functions and related
constructions using the SPARK 2014 programming language.

libkeccak implements the following constructions:

* The Keccak-p permutation for state sizes of 25, 50, 100, 200, 400, 800, and 1600 bits (see [1] and [2]).
* The Sponge construction
* The Duplex construction
* Hash functions based on the Sponge construction
* eXtendable Output Functions (XOF) based on the Sponge construction

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
GNAT GPL 2015 (20150428-49).

| Algorithm               | Performance |  Cycles Estimate  |
| ----------------------- | ----------- | ----------------- |
| SHA3-224                | 158 MiB/s   | 15.69 cycles/byte |
| SHA3-256                | 151 MiB/s   | 16.42 cycles/byte |
| SHA3-384                | 118 MiB/s   | 21.01 cycles/byte |
| SHA3-512                | 83 MiB/s    | 29.87 cycles/byte |
| Keccak-224              | 158 MiB/s   | 15.69 cycles/byte |
| Keccak-256              | 151 MiB/s   | 16.42 cycles/byte |
| Keccak-384              | 118 MiB/s   | 21.01 cycles/byte |
| Keccak-512              | 83 MiB/s    | 29.87 cycles/byte |
| SHAKE128 (Absorbing)    | 183 MiB/s   | 13.54 cycles/byte |
| SHAKE128 (Squeezing)    | 174 MiB/s   | 14.25 cycles/byte |
| SHAKE256 (Absorbing)    | 151 MiB/s   | 16.42 cycles/byte |
| SHAKE256 (Squeezing)    | 145 MiB/s   | 17.10 cycles/byte |
| RawSHAKE128 (Absorbing) | 183 MiB/s   | 13.54 cycles/byte |
| RawSHAKE128 (Squeezing) | 174 MiB/s   | 14.25 cycles/byte |
| RawSHAKE256 (Absorbing) | 151 MiB/s   | 16.42 cycles/byte |
| RawSHAKE256 (Squeezing) | 145 MiB/s   | 17.10 cycles/byte |
| Duplex (c448)           | 135 MiB/s   | 18.37 cycles/byte |
| Duplex (c512)           | 130 MiB/s   | 19.07 cycles/byte |
| Duplex (c768)           | 104 MiB/s   | 23.84 cycles/byte |
| Duplex (c1024)          | 73 MiB/s    | 33.97 cycles/byte |
| Keccak-f\[1600\]        | 783 ns/call | 2035 cycles/call  |
| Keccak-f\[800\]         | 687 ns/call | 1786 cycles/call  |
| Keccak-f\[400\]         | 656 ns/call | 1706 cycles/call  |
| Keccak-f\[200\]         | 586 ns/call | 1524 cycles/call  |
| Keccak-f\[100\]         | 768 ns/call | 1997 cycles/call  |
| Keccak-f\[50\]          | 671 ns/call | 1745 cycles/call  |
| Keccak-f\[25\]          | 362 ns/call | 941 cycles/call   |

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
