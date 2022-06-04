# Libkeccak tests

The tests consists of three parts:
 * `benchmark` contains a program to measure the performance of libkeccak.
 * `kat` contains the Known Answer Tests (KAT) and test vectors to test that
   libkeccak correctly implements the various supported algoritms.
 * `unit_tests` contains tests to check that libkeccak produces consistent
   results in various scenarios.

## Benchmark

Assuming you have Alire installed, the benchmark can be built and run in the
default configuration with:
```sh
cd tests/benchmark
alr run
```

To measure the performance with different SIMD settings (e.g. AVX2 instructions),
pass the build configurations to `alr build`, for example:
```sh
alr build -XLIBKECCAK_ARCH=x86_64 -XLIBKECCAK_SIMD=AVX2
alr run
```

## Known Answer Tests

To build and run the KATs in the default configuration:
```sh
cd tests/kat
alr build
./run-all-tests.sh
```

Individual test suites can be run by specifying the algorithm (e.g. SHA3-256)
and the path to the test vector file:
```sh
alr run --args="SHA3-256 testvectors/SHA3/bytes/SHA3_256LongMsg.rsp"
```

The following algorithms are supported:
 * SHA3-224
 * SHA3-256
 * SHA3-384
 * SHA3-512
 * SHAKE128
 * SHAKE256
 * Keccak-224
 * Keccak-256
 * Keccak-384
 * Keccak-512
 * cSHAKE128
 * cSHAKE256
 * Duplexc574
 * Duplexc573
 * KMAC128
 * KMAC256
 * KMACXOF128
 * KMACXOF256
 * ParallelHash128
 * ParallelHash256
 * ParallelHashXOF128
 * ParallelHashXOF256
 * TupleHash128
 * TupleHash256
 * TupleHashXOF128
 * TupleHashXOF256
 * KetjeJr
 * KetjeSr
 * KetjeMinor
 * KetjeMajor
 * GimliHash
 * AsconHash
 * AsconXOF

## Unit Tests

To build and run the unit tests in the default configuration:
```sh
cd tests/unit_tests
alr run
```

>:bulb: It is recommended to run the unit tests with contracts and run-time
checks enabled:
```sh
cd tests/unit_tests
alr build -XLIBKECCAK_CONTRACTS=enabled -XLIBKECCAK_RUNTIME_CHECKS=enabled
alr run
```