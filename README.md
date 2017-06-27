# Libkeccak

This project implements the Keccak family of sponge functions and related
constructions using the SPARK 2014 programming language.

libkeccak implements the following generic constructions:

* The Keccak-p permutation for state sizes of 25, 50, 100, 200, 400, 800, and 1600 bits (see [1] and [2]).
* The Sponge construction
* The Duplex construction
* Hash functions based on the Sponge construction
* eXtendable Output Functions (XOF) based on the Sponge construction
* cSHAKE, KMAC, TupleHash, and ParallelHash as specified in NIST SP 800-185 [4]
* KangarooTwelve as specified by the Keccak team [5]

libkeccak also provides concrete implementations of the above constructions,
as specified in [1,4,5]:

* Hash functions:
  * SHA-3 (224, 256, 384, and 512 bits)
  * Keccak (224, 256, 384, and 512 bits)
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
  * ParallelHash128 and ParallelHash256
  
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
the SPARK (GPL, Pro, or Discovery) 2017 toolset is required.

To build libkeccak, change to the source directory and type:
<pre><code>make build</code></pre>

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
was compiled using GNAT GPL 2017 (20170515) with SSE2 enabled.

The cycles per byte are roughly estimated by dividing the clock speed
(2.6 GHz) by the number of bytes (128 MiB).

| Algorithm               | Performance |  Cycles Estimate  |
| ----------------------- | ----------- | ----------------- |
| KangarooTwelve (Absorbing) | 554 MiB/S   | 4.5 cycles/byte   |
| KangarooTwelve (Squeezing) | 317 MiB/S   | 7.8 cycles/byte   |
| SHA3-224                | 181 MiB/s   | 13.7 cycles/byte  |
| SHA3-256                | 172 MiB/s   | 14.4 cycles/byte  |
| SHA3-384                | 134 MiB/s   | 18.6 cycles/byte  |
| SHA3-512                | 94 MiB/s    | 26.4 cycles/byte  |
| Keccak-224              | 182 MiB/s   | 13.7 cycles/byte  |
| Keccak-256              | 172 MiB/s   | 14.4 cycles/byte  |
| Keccak-384              | 133 MiB/s   | 18.6 cycles/byte  |
| Keccak-512              | 94 MiB/s    | 26.4 cycles/byte  |
| SHAKE128 (Absorbing)    | 210 MiB/s   | 11.8 cycles/byte  |
| SHAKE128 (Squeezing)    | 194 MiB/s   | 12.8 cycles/byte  |
| SHAKE256 (Absorbing)    | 172 MiB/s   | 14.4 cycles/byte  |
| SHAKE256 (Squeezing)    | 162 MiB/s   | 15.3 cycles/byte  |
| RawSHAKE128 (Absorbing) | 210 MiB/s   | 11.8 cycles/byte  |
| RawSHAKE128 (Squeezing) | 194 MiB/s   | 12.8 cycles/byte  |
| RawSHAKE256 (Absorbing) | 172 MiB/s   | 14.4 cycles/byte  |
| RawSHAKE256 (Squeezing) | 162 MiB/s   | 15.3 cycles/byte  |
| Duplex (c448)           | 151 MiB/s   | 16.4 cycles/byte  |
| Duplex (c512)           | 145 MiB/s   | 17.2 cycles/byte  |
| Duplex (c768)           | 115 MiB/s   | 21.6 cycles/byte  |
| Duplex (c1024)          | 84 MiB/s    | 29.5 cycles/byte  |
| Keccak-p\[1600,24\]     | 707 ns/call | 1838 cycles/call  |
| Keccak-p\[1600,12\]x2   | 415 ns/call | 1079 cycles/call  |
| Keccak-p\[800,22\]      | 686 ns/call | 1782 cycles/call  |
| Keccak-p\[400,20\]      | 622 ns/call | 1616 cycles/call  |
| Keccak-p\[200,18\]      | 534 ns/call | 1388 cycles/call  |
| Keccak-p\[100,16\]      | 626 ns/call | 1627 cycles/call  |
| Keccak-p\[50,14\]       | 699 ns/call | 1818 cycles/call  |
| Keccak-p\[25,12\]       | 296 ns/call | 769 cycles/call   |

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
