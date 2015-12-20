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
| SHA3-224                | 155 MiB/s   | 16.00 cycles/byte |
| SHA3-256                | 146 MiB/s   | 16.98 cycles/byte |
| SHA3-384                | 114 MiB/s   | 21.75 cycles/byte |
| SHA3-512                | 80 MiB/s    | 30.99 cycles/byte |
| Keccak-224              | 155 MiB/s   | 16.00 cycles/byte |
| Keccak-256              | 146 MiB/s   | 16.99 cycles/byte |
| Keccak-384              | 114 MiB/s   | 21.75 cycles/byte |
| Keccak-512              | 80 MiB/s    | 30.99 cycles/byte |
| SHAKE128 (Absorbing)    | 177 MiB/s   | 14.00 cycles/byte |
| SHAKE128 (Squeezing)    | 165 MiB/s   | 15.03 cycles/byte |
| SHAKE256 (Absorbing)    | 147 MiB/s   | 16.87 cycles/byte |
| SHAKE256 (Squeezing)    | 141 MiB/s   | 17.59 cycles/byte |
| RawSHAKE128 (Absorbing) | 177 MiB/s   | 14.00 cycles/byte |
| RawSHAKE128 (Squeezing) | 165 MiB/s   | 15.03 cycles/byte |
| RawSHAKE256 (Absorbing) | 147 MiB/s   | 16.87 cycles/byte |
| RawSHAKE256 (Squeezing) | 141 MiB/s   | 17.59 cycles/byte |
| Duplex (c448)           | 130 MiB/s   | 19.07 cycles/byte |
| Duplex (c512)           | 125 MiB/s   | 19.83 cycles/byte |
| Duplex (c768)           | 101 MiB/s   | 24.55 cycles/byte |
| Duplex (c1024)          | 72 MiB/s    | 34.43 cycles/byte |
| Keccak-f\[1600\]        | 808 ns/call | 2100 cycles/call  |
| Keccak-f\[800\]         | 699 ns/call | 1817 cycles/call  |
| Keccak-f\[400\]         | 663 ns/call | 1724 cycles/call  |
| Keccak-f\[200\]         | 603 ns/call | 1568 cycles/call  |
| Keccak-f\[100\]         | 766 ns/call | 1992 cycles/call  |
| Keccak-f\[50\]          | 670 ns/call | 1742 cycles/call  |
| Keccak-f\[25\]          | 373 ns/call | 970 cycles/call   |

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
correctly. Currently there are 

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
