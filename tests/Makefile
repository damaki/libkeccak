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

.PHONY: $(KAT_EXECUTABLES)
.PHONY: kat
kat: $(OUTPUT_DIR)/kat/kat
	$(OUTPUT_DIR)/kat/kat ParallelHash128 kat/testvectors/ParallelHash128_samples.txt
	$(OUTPUT_DIR)/kat/kat ParallelHash256 kat/testvectors/ParallelHash256_samples.txt
	$(OUTPUT_DIR)/kat/kat ParallelHashXOF128 kat/testvectors/ParallelHashXOF128_samples.txt
	$(OUTPUT_DIR)/kat/kat ParallelHashXOF256 kat/testvectors/ParallelHashXOF256_samples.txt
	$(OUTPUT_DIR)/kat/kat TupleHash128 kat/testvectors/Tuple_Hash_128_samples.txt
	$(OUTPUT_DIR)/kat/kat TupleHash256 kat/testvectors/Tuple_Hash_256_samples.txt
	$(OUTPUT_DIR)/kat/kat TupleHashXOF128 kat/testvectors/Tuple_Hash_XOF_128_samples.txt
	$(OUTPUT_DIR)/kat/kat TupleHashXOF256 kat/testvectors/Tuple_Hash_XOF_256_samples.txt
	$(OUTPUT_DIR)/kat/kat KMAC128 kat/testvectors/KMAC_128_samples.txt
	$(OUTPUT_DIR)/kat/kat KMAC256 kat/testvectors/KMAC_256_samples.txt
	$(OUTPUT_DIR)/kat/kat KMACXOF128 kat/testvectors/KMACXOF_128_samples.txt
	$(OUTPUT_DIR)/kat/kat KMACXOF256 kat/testvectors/KMACXOF_256_samples.txt
	$(OUTPUT_DIR)/kat/kat cSHAKE128 kat/testvectors/cSHAKE_128_samples.txt
	$(OUTPUT_DIR)/kat/kat cSHAKE256 kat/testvectors/cSHAKE_256_samples.txt
	$(OUTPUT_DIR)/kat/kat Duplexc574 kat/testvectors/DuplexKAT_r1026c574.txt
	$(OUTPUT_DIR)/kat/kat Duplexc573 kat/testvectors/DuplexKAT_r1027c573.txt
	$(OUTPUT_DIR)/kat/kat Keccak-224 kat/testvectors/ShortMsgKAT_224.txt
	$(OUTPUT_DIR)/kat/kat Keccak-256 kat/testvectors/ShortMsgKAT_256.txt
	$(OUTPUT_DIR)/kat/kat Keccak-384 kat/testvectors/ShortMsgKAT_384.txt
	$(OUTPUT_DIR)/kat/kat Keccak-512 kat/testvectors/ShortMsgKAT_512.txt
	$(OUTPUT_DIR)/kat/kat SHA3-224 kat/testvectors/ShortMsgKAT_SHA3-224.txt
	$(OUTPUT_DIR)/kat/kat SHA3-256 kat/testvectors/ShortMsgKAT_SHA3-256.txt
	$(OUTPUT_DIR)/kat/kat SHA3-384 kat/testvectors/ShortMsgKAT_SHA3-384.txt
	$(OUTPUT_DIR)/kat/kat SHA3-512 kat/testvectors/ShortMsgKAT_SHA3-512.txt
	$(OUTPUT_DIR)/kat/kat Keccak-224 kat/testvectors/LongMsgKAT_224.txt
	$(OUTPUT_DIR)/kat/kat Keccak-256 kat/testvectors/LongMsgKAT_256.txt
	$(OUTPUT_DIR)/kat/kat Keccak-384 kat/testvectors/LongMsgKAT_384.txt
	$(OUTPUT_DIR)/kat/kat Keccak-512 kat/testvectors/LongMsgKAT_512.txt
	$(OUTPUT_DIR)/kat/kat Keccak-224 kat/testvectors/ExtremelyLongMsgKAT_224.txt
	$(OUTPUT_DIR)/kat/kat Keccak-256 kat/testvectors/ExtremelyLongMsgKAT_256.txt
	$(OUTPUT_DIR)/kat/kat Keccak-384 kat/testvectors/ExtremelyLongMsgKAT_384.txt
	$(OUTPUT_DIR)/kat/kat Keccak-512 kat/testvectors/ExtremelyLongMsgKAT_512.txt

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
