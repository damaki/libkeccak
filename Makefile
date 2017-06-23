OUTPUT_DIR   = $(CURDIR)/out
BUILD_DIR    = $(OUTPUT_DIR)/build

UNAME_M := $(shell uname -m)

ARCH ?= $(UNAME_M)

ifeq ($(ARCH),x86_64)
	# default to SSE2 on x86_64 as it's available on all modern hardware
	SIMD ?= SSE2
else
	SIMD ?= none
endif

GENERIC_BODIES = src/common/keccak-generic_cshake.adb \
                 src/common/keccak-generic_duplex.adb \
                 src/common/keccak-generic_hash.adb \
                 src/common/keccak-generic_kangarootwelve.adb \
                 src/common/keccak-generic_keccakf-bit_lanes.adb \
                 src/common/keccak-generic_keccakf-byte_lanes.adb \
                 src/common/keccak-generic_keccakf-optimized_permutation.adb \
                 src/common/keccak-generic_keccakf-reference_permutation.adb \
                 src/common/keccak-generic_keccakf.adb \
                 src/common/keccak-generic_kmac.adb \
                 src/common/keccak-generic_parallel_keccakf.adb \
                 src/common/keccak-generic_parallel_permutation_parallel_fallback.adb \
                 src/common/keccak-generic_parallel_permutation_serial_fallback.adb \
                 src/common/keccak-generic_parallel_sponge.adb \
                 src/common/keccak-generic_parallel_xof.adb \
                 src/common/keccak-generic_sponge.adb \
                 src/common/keccak-generic_tuple_hash.adb \
                 src/common/keccak-generic_xof.adb

################################################################################

all: build doc proof test

build: $(BUILD_DIR)/libkeccak.a

proof:
	gnatprove -P build/build_libkeccak.gpr -Xarch=$(ARCH) -Xsimd=$(SIMD)

doc: build
	gnatdoc -P build/build_libkeccak.gpr

$(BUILD_DIR)/libkeccak.a:
	gprbuild $(GNATMAKE_OPTS) -p -P build/build_libkeccak -Xarch=$(ARCH) -Xsimd=$(SIMD)

test: kat unit_test

kat: install_local
	$(MAKE) KECCAK_DIR=$(OUTPUT_DIR)/keccak -C tests kat

unit_test:
	$(MAKE) KECCAK_DIR=$(OUTPUT_DIR)/keccak -C tests unit_test ARCH=$(ARCH) SIMD=$(SIMD)

coverage:
	$(MAKE) KECCAK_DIR=$(OUTPUT_DIR)/keccak -C tests coverage

.PHONY: benchmark
benchmark: install_local
	$(MAKE) KECCAK_DIR=$(OUTPUT_DIR)/keccak -C tests benchmark
	
install: build
	install -d -m 755 $(DESTDIR)/adalib $(DESTDIR)/adainclude
	install -p -m 644 $(BUILD_DIR)/adalib/libkeccak.a $(BUILD_DIR)/adalib/*.ali $(DESTDIR)/adalib/
	install -p -m 644 src/common/*.ads $(DESTDIR)/adainclude/
ifeq ($(ARCH),generic)
	install -p -m 644 src/generic/*.ads $(DESTDIR)/adainclude/
endif
ifeq ($(ARCH),x86_64)
ifeq ($(SIMD),none)
	install -p -m 644 src/generic/*.ads $(DESTDIR)/adainclude/
else
	install -p -m 644 src/x86_64/SSE2/*.ads $(DESTDIR)/adainclude/
endif
endif
	install -p -m 644 $(GENERIC_BODIES) $(DESTDIR)/adainclude
	install -p -m 644 build/libkeccak.gpr $(DESTDIR)/
	
install_local: DESTDIR=$(OUTPUT_DIR)/keccak
install_local: install
	
test_clean:
	$(MAKE) -C tests clean

clean: test_clean
	rm -rf $(OUTPUT_DIR)

.PHONY: clean all build doc install_local 
