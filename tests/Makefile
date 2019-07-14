OUTPUT_DIR ?= $(CURDIR)/out
KECCAK_DIR ?= ../lib/share/gpr

ifneq ($(MAKECMDGOALS),clean)
ifeq ($(KECCAK_DIR),)
$(error "KECCAK_DIR not set")
endif
endif

KAT_EXECUTABLES = $(OUTPUT_DIR)/kat/sha3_224_kat \
						$(OUTPUT_DIR)/kat/sha3_256_kat \
						$(OUTPUT_DIR)/kat/sha3_384_kat \
						$(OUTPUT_DIR)/kat/sha3_512_kat \
						$(OUTPUT_DIR)/kat/keccak_224_kat \
						$(OUTPUT_DIR)/kat/keccak_256_kat \
						$(OUTPUT_DIR)/kat/keccak_384_kat \
						$(OUTPUT_DIR)/kat/keccak_512_kat \
						$(OUTPUT_DIR)/kat/keccak_duplex_kat_r1026c574 \
						$(OUTPUT_DIR)/kat/keccak_duplex_kat_r1027c573 \
						$(OUTPUT_DIR)/kat/cshake_128_kat \
						$(OUTPUT_DIR)/kat/cshake_256_kat \
						$(OUTPUT_DIR)/kat/kmac_128_kat \
						$(OUTPUT_DIR)/kat/kmac_256_kat \
						$(OUTPUT_DIR)/kat/kmacxof_128_kat \
						$(OUTPUT_DIR)/kat/kmacxof_256_kat \
						$(OUTPUT_DIR)/kat/tuple_hash_128_kat \
						$(OUTPUT_DIR)/kat/tuple_hash_256_kat \
						$(OUTPUT_DIR)/kat/tuple_hash_xof_128_kat \
						$(OUTPUT_DIR)/kat/tuple_hash_xof_256_kat \
						$(OUTPUT_DIR)/kat/parallelhash_128_kat \
						$(OUTPUT_DIR)/kat/parallelhash_256_kat

DUMMY := $(shell mkdir -p $(OUTPUT_DIR)) \
         $(shell mkdir -p $(OUTPUT_DIR)/kat) \
         $(shell mkdir -p $(OUTPUT_DIR)/benchmark) \
         $(shell mkdir -p $(OUTPUT_DIR)/unit_tests)

all: kat benchmark unit_test

.PHONY: $(KAT_EXECUTABLES)
.PHONY: kat
kat: $(KAT_EXECUTABLES)
	$(OUTPUT_DIR)/kat/parallelhash_128_kat kat/testvectors/ParallelHash128_samples.txt
	$(OUTPUT_DIR)/kat/parallelhash_256_kat kat/testvectors/ParallelHash256_samples.txt
	$(OUTPUT_DIR)/kat/tuple_hash_128_kat kat/testvectors/Tuple_Hash_128_samples.txt
	$(OUTPUT_DIR)/kat/tuple_hash_256_kat kat/testvectors/Tuple_Hash_256_samples.txt
	$(OUTPUT_DIR)/kat/tuple_hash_xof_128_kat kat/testvectors/Tuple_Hash_XOF_128_samples.txt
	$(OUTPUT_DIR)/kat/tuple_hash_xof_256_kat kat/testvectors/Tuple_Hash_XOF_256_samples.txt
	$(OUTPUT_DIR)/kat/kmac_128_kat kat/testvectors/KMAC_128_samples.txt
	$(OUTPUT_DIR)/kat/kmac_256_kat kat/testvectors/KMAC_256_samples.txt
	$(OUTPUT_DIR)/kat/kmacxof_128_kat kat/testvectors/KMACXOF_128_samples.txt
	$(OUTPUT_DIR)/kat/kmacxof_256_kat kat/testvectors/KMACXOF_256_samples.txt
	$(OUTPUT_DIR)/kat/cshake_128_kat kat/testvectors/cSHAKE_128_samples.txt
	$(OUTPUT_DIR)/kat/cshake_256_kat kat/testvectors/cSHAKE_256_samples.txt
	$(OUTPUT_DIR)/kat/keccak_duplex_kat_r1026c574 kat/testvectors/DuplexKAT_r1026c574.txt
	$(OUTPUT_DIR)/kat/keccak_duplex_kat_r1027c573 kat/testvectors/DuplexKAT_r1027c573.txt
	$(OUTPUT_DIR)/kat/keccak_224_kat kat/testvectors/ShortMsgKAT_224.txt
	$(OUTPUT_DIR)/kat/keccak_256_kat kat/testvectors/ShortMsgKAT_256.txt
	$(OUTPUT_DIR)/kat/keccak_384_kat kat/testvectors/ShortMsgKAT_384.txt
	$(OUTPUT_DIR)/kat/keccak_512_kat kat/testvectors/ShortMsgKAT_512.txt
	$(OUTPUT_DIR)/kat/sha3_224_kat kat/testvectors/ShortMsgKAT_SHA3-224.txt
	$(OUTPUT_DIR)/kat/sha3_256_kat kat/testvectors/ShortMsgKAT_SHA3-256.txt
	$(OUTPUT_DIR)/kat/sha3_384_kat kat/testvectors/ShortMsgKAT_SHA3-384.txt
	$(OUTPUT_DIR)/kat/sha3_512_kat kat/testvectors/ShortMsgKAT_SHA3-512.txt
	$(OUTPUT_DIR)/kat/keccak_224_kat kat/testvectors/LongMsgKAT_224.txt
	$(OUTPUT_DIR)/kat/keccak_256_kat kat/testvectors/LongMsgKAT_256.txt
	$(OUTPUT_DIR)/kat/keccak_384_kat kat/testvectors/LongMsgKAT_384.txt
	$(OUTPUT_DIR)/kat/keccak_512_kat kat/testvectors/LongMsgKAT_512.txt
	$(OUTPUT_DIR)/kat/keccak_224_kat kat/testvectors/ExtremelyLongMsgKAT_224.txt
	$(OUTPUT_DIR)/kat/keccak_256_kat kat/testvectors/ExtremelyLongMsgKAT_256.txt
	$(OUTPUT_DIR)/kat/keccak_384_kat kat/testvectors/ExtremelyLongMsgKAT_384.txt
	$(OUTPUT_DIR)/kat/keccak_512_kat kat/testvectors/ExtremelyLongMsgKAT_512.txt

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

$(OUTPUT_DIR)/kat/keccak_224_kat:
	gprbuild -aP$(KECCAK_DIR) -P keccak_224_kat.gpr

$(OUTPUT_DIR)/kat/keccak_256_kat:
	gprbuild -aP$(KECCAK_DIR) -P keccak_256_kat.gpr

$(OUTPUT_DIR)/kat/keccak_384_kat:
	gprbuild -aP$(KECCAK_DIR) -P keccak_384_kat.gpr

$(OUTPUT_DIR)/kat/keccak_512_kat:
	gprbuild -aP$(KECCAK_DIR) -P keccak_512_kat.gpr

$(OUTPUT_DIR)/kat/sha3_224_kat:
	gprbuild -aP$(KECCAK_DIR) -P sha3_224_kat.gpr

$(OUTPUT_DIR)/kat/sha3_256_kat:
	gprbuild -aP$(KECCAK_DIR) -P sha3_256_kat.gpr

$(OUTPUT_DIR)/kat/sha3_384_kat:
	gprbuild -aP$(KECCAK_DIR) -P sha3_384_kat.gpr

$(OUTPUT_DIR)/kat/sha3_512_kat:
	gprbuild -aP$(KECCAK_DIR) -P sha3_512_kat.gpr

$(OUTPUT_DIR)/kat/keccak_duplex_kat_r1026c574:
	gprbuild -aP$(KECCAK_DIR) -P keccak_duplex_kat_r1026c574.gpr

$(OUTPUT_DIR)/kat/keccak_duplex_kat_r1027c573:
	gprbuild -aP$(KECCAK_DIR) -P keccak_duplex_kat_r1027c573.gpr

$(OUTPUT_DIR)/kat/cshake_128_kat:
	gprbuild -aP$(KECCAK_DIR) -P cshake_128_kat.gpr

$(OUTPUT_DIR)/kat/cshake_256_kat:
	gprbuild -aP$(KECCAK_DIR) -P cshake_256_kat.gpr

$(OUTPUT_DIR)/kat/kmac_128_kat:
	gprbuild -aP$(KECCAK_DIR) -P kmac_128_kat.gpr

$(OUTPUT_DIR)/kat/kmac_256_kat:
	gprbuild -aP$(KECCAK_DIR) -P kmac_256_kat.gpr

$(OUTPUT_DIR)/kat/kmacxof_128_kat:
	gprbuild -aP$(KECCAK_DIR) -P kmacxof_128_kat.gpr

$(OUTPUT_DIR)/kat/kmacxof_256_kat:
	gprbuild -aP$(KECCAK_DIR) -P kmacxof_256_kat.gpr

$(OUTPUT_DIR)/kat/tuple_hash_128_kat:
	gprbuild -aP$(KECCAK_DIR) -P tuple_hash_128_kat.gpr

$(OUTPUT_DIR)/kat/tuple_hash_256_kat:
	gprbuild -aP$(KECCAK_DIR) -P tuple_hash_256_kat.gpr

$(OUTPUT_DIR)/kat/tuple_hash_xof_128_kat:
	gprbuild -aP$(KECCAK_DIR) -P tuple_hash_xof_128_kat.gpr

$(OUTPUT_DIR)/kat/tuple_hash_xof_256_kat:
	gprbuild -aP$(KECCAK_DIR) -P tuple_hash_xof_256_kat.gpr

$(OUTPUT_DIR)/kat/parallelhash_128_kat:
	gprbuild -aP$(KECCAK_DIR) -P parallelhash_128_kat.gpr

$(OUTPUT_DIR)/kat/parallelhash_256_kat:
	gprbuild -aP$(KECCAK_DIR) -P parallelhash_256_kat.gpr

$(OUTPUT_DIR)/unit_tests/test_keccak:
	gprbuild -P unit_tests.gpr -Xarch=$(ARCH) -Xsimd=$(SIMD)

clean:
	rm -rf $(OUTPUT_DIR)
	gprclean -P benchmark/benchmark.gpr -XLIBKECCAK_BUILD=default
