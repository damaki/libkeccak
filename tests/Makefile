OUTPUT_DIR ?= $(CURDIR)/out
KECCAK_DIR ?= ../lib/share/gpr

ifneq ($(MAKECMDGOALS),clean)
ifeq ($(KECCAK_DIR),)
$(error "KECCAK_DIR not set")
endif
endif

DUMMY := $(shell mkdir -p $(OUTPUT_DIR)) \
         $(shell mkdir -p $(OUTPUT_DIR)/kat) \
         $(shell mkdir -p $(OUTPUT_DIR)/benchmark) \
         $(shell mkdir -p $(OUTPUT_DIR)/unit_tests)

all: kat benchmark unit_test

.PHONY: $(OUTPUT_DIR)/kat/kat
.PHONY: kat
kat: $(OUTPUT_DIR)/kat/kat
	$(OUTPUT_DIR)/kat/kat AsconHash kat/testvectors/Ascon/Ascon_HASH_KAT_256.txt
	$(OUTPUT_DIR)/kat/kat AsconXOF kat/testvectors/Ascon/Ascon_XOF_KAT_256.txt
	$(OUTPUT_DIR)/kat/kat GimliHash kat/testvectors/Gimli/gimli_hash.txt
	$(OUTPUT_DIR)/kat/kat KetjeMajor kat/testvectors/Ketje/Ketje_Major.txt
	$(OUTPUT_DIR)/kat/kat KetjeMinor kat/testvectors/Ketje/Ketje_Minor.txt
	$(OUTPUT_DIR)/kat/kat KetjeSr kat/testvectors/Ketje/Ketje_Sr.txt
	$(OUTPUT_DIR)/kat/kat KetjeJr kat/testvectors/Ketje/Ketje_Jr.txt
	$(OUTPUT_DIR)/kat/kat ParallelHash128 kat/testvectors/ParallelHash/ParallelHash128_samples.txt
	$(OUTPUT_DIR)/kat/kat ParallelHash256 kat/testvectors/ParallelHash/ParallelHash256_samples.txt
	$(OUTPUT_DIR)/kat/kat ParallelHashXOF128 kat/testvectors/ParallelHash/ParallelHashXOF128_samples.txt
	$(OUTPUT_DIR)/kat/kat ParallelHashXOF256 kat/testvectors/ParallelHash/ParallelHashXOF256_samples.txt
	$(OUTPUT_DIR)/kat/kat TupleHash128 kat/testvectors/TupleHash/Tuple_Hash_128_samples.txt
	$(OUTPUT_DIR)/kat/kat TupleHash256 kat/testvectors/TupleHash/Tuple_Hash_256_samples.txt
	$(OUTPUT_DIR)/kat/kat TupleHashXOF128 kat/testvectors/TupleHash/Tuple_Hash_XOF_128_samples.txt
	$(OUTPUT_DIR)/kat/kat TupleHashXOF256 kat/testvectors/TupleHash/Tuple_Hash_XOF_256_samples.txt
	$(OUTPUT_DIR)/kat/kat KMAC128 kat/testvectors/KMAC/KMAC_128_samples.txt
	$(OUTPUT_DIR)/kat/kat KMAC256 kat/testvectors/KMAC/KMAC_256_samples.txt
	$(OUTPUT_DIR)/kat/kat KMACXOF128 kat/testvectors/KMAC/KMACXOF_128_samples.txt
	$(OUTPUT_DIR)/kat/kat KMACXOF256 kat/testvectors/KMAC/KMACXOF_256_samples.txt
	$(OUTPUT_DIR)/kat/kat cSHAKE128 kat/testvectors/cSHAKE/cSHAKE_128_samples.txt
	$(OUTPUT_DIR)/kat/kat cSHAKE256 kat/testvectors/cSHAKE/cSHAKE_256_samples.txt
	$(OUTPUT_DIR)/kat/kat Duplexc574 kat/testvectors/Duplex/DuplexKAT_r1026c574.txt
	$(OUTPUT_DIR)/kat/kat Duplexc573 kat/testvectors/Duplex/DuplexKAT_r1027c573.txt
	$(OUTPUT_DIR)/kat/kat SHAKE128 kat/testvectors/SHAKE/bytes/SHAKE128ShortMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHAKE128 kat/testvectors/SHAKE/bytes/SHAKE128LongMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHAKE128 kat/testvectors/SHAKE/bits/SHAKE128ShortMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHAKE128 kat/testvectors/SHAKE/bits/SHAKE128LongMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHAKE256 kat/testvectors/SHAKE/bytes/SHAKE256ShortMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHAKE256 kat/testvectors/SHAKE/bytes/SHAKE256LongMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHAKE256 kat/testvectors/SHAKE/bits/SHAKE256ShortMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHAKE256 kat/testvectors/SHAKE/bits/SHAKE256LongMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHA3-224 kat/testvectors/SHA3/bytes/SHA3_224ShortMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHA3-224 kat/testvectors/SHA3/bytes/SHA3_224LongMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHA3-224 kat/testvectors/SHA3/bits/SHA3_224ShortMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHA3-224 kat/testvectors/SHA3/bits/SHA3_224LongMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHA3-256 kat/testvectors/SHA3/bytes/SHA3_256ShortMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHA3-256 kat/testvectors/SHA3/bytes/SHA3_256LongMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHA3-256 kat/testvectors/SHA3/bits/SHA3_256ShortMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHA3-256 kat/testvectors/SHA3/bits/SHA3_256LongMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHA3-384 kat/testvectors/SHA3/bytes/SHA3_384ShortMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHA3-384 kat/testvectors/SHA3/bytes/SHA3_384LongMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHA3-384 kat/testvectors/SHA3/bits/SHA3_384ShortMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHA3-384 kat/testvectors/SHA3/bits/SHA3_384LongMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHA3-512 kat/testvectors/SHA3/bytes/SHA3_512ShortMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHA3-512 kat/testvectors/SHA3/bytes/SHA3_512LongMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHA3-512 kat/testvectors/SHA3/bits/SHA3_512ShortMsg.rsp
	$(OUTPUT_DIR)/kat/kat SHA3-512 kat/testvectors/SHA3/bits/SHA3_512LongMsg.rsp
	$(OUTPUT_DIR)/kat/kat Keccak-224 kat/testvectors/Keccak/ShortMsgKAT_224.txt
	$(OUTPUT_DIR)/kat/kat Keccak-224 kat/testvectors/Keccak/LongMsgKAT_224.txt
	$(OUTPUT_DIR)/kat/kat Keccak-224 kat/testvectors/Keccak/ExtremelyLongMsgKAT_224.txt
	$(OUTPUT_DIR)/kat/kat Keccak-256 kat/testvectors/Keccak/ShortMsgKAT_256.txt
	$(OUTPUT_DIR)/kat/kat Keccak-256 kat/testvectors/Keccak/LongMsgKAT_256.txt
	$(OUTPUT_DIR)/kat/kat Keccak-256 kat/testvectors/Keccak/ExtremelyLongMsgKAT_256.txt
	$(OUTPUT_DIR)/kat/kat Keccak-384 kat/testvectors/Keccak/ShortMsgKAT_384.txt
	$(OUTPUT_DIR)/kat/kat Keccak-384 kat/testvectors/Keccak/LongMsgKAT_384.txt
	$(OUTPUT_DIR)/kat/kat Keccak-384 kat/testvectors/Keccak/ExtremelyLongMsgKAT_384.txt
	$(OUTPUT_DIR)/kat/kat Keccak-512 kat/testvectors/Keccak/ShortMsgKAT_512.txt
	$(OUTPUT_DIR)/kat/kat Keccak-512 kat/testvectors/Keccak/LongMsgKAT_512.txt
	$(OUTPUT_DIR)/kat/kat Keccak-512 kat/testvectors/Keccak/ExtremelyLongMsgKAT_512.txt

.PHONY: benchmark
benchmark:
	gprbuild -p -P benchmark/benchmark.gpr -XLIBKECCAK_BUILD=default

.PHONY:$(OUTPUT_DIR)/unit_tests/test_keccak
unit_test: $(KECCAK_DIR)/libkeccak.a $(OUTPUT_DIR)/unit_tests/test_keccak
	$(OUTPUT_DIR)/unit_tests/test_keccak

.PHONY: $(KECCAK_DIR)/libkeccak.a
$(KECCAK_DIR)/libkeccak.a:

coverage:
	cd $(OUTPUT_DIR)/unit_tests && gcov *.gcno

$(OUTPUT_DIR)/kat/kat:
	gprbuild -j0 -p -aP$(KECCAK_DIR) -P kat.gpr

$(OUTPUT_DIR)/kat/parallelhashxof_128_kat:
	gprbuild -aP$(KECCAK_DIR) -P parallelhashxof_128_kat.gpr

$(OUTPUT_DIR)/kat/parallelhashxof_256_kat:
	gprbuild -aP$(KECCAK_DIR) -P parallelhashxof_256_kat.gpr

$(OUTPUT_DIR)/unit_tests/test_keccak:
	gprbuild -j0 -p -P unit_tests.gpr -Xarch=$(ARCH) -Xsimd=$(SIMD)

clean:
	rm -rf $(OUTPUT_DIR)
	gprclean -P benchmark/benchmark.gpr -XLIBKECCAK_BUILD=default
