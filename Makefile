OUTPUT_DIR   = $(CURDIR)/out
BUILD_DIR    = $(OUTPUT_DIR)/build

GENERIC_BODIES = src/keccak-generic_cshake.adb \
                 src/keccak-generic_duplex.adb \
                 src/keccak-generic_hash.adb \
                 src/keccak-generic_kangarootwelve.adb \
                 src/keccak-generic_keccakf-bit_lanes.adb \
                 src/keccak-generic_keccakf-byte_lanes.adb \
                 src/keccak-generic_keccakf-optimized_permutation.adb \
                 src/keccak-generic_keccakf-reference_permutation.adb \
                 src/keccak-generic_keccakf.adb \
                 src/keccak-generic_kmac.adb \
                 src/keccak-generic_parallel_permutation_parallel_fallback.adb \
                 src/keccak-generic_parallel_permutation_serial_fallback.adb \
                 src/keccak-generic_parallel_sponge.adb \
                 src/keccak-generic_parallel_xof.adb \
                 src/keccak-generic_sponge.adb \
                 src/keccak-generic_tuple_hash.adb \
                 src/keccak-generic_xof.adb

################################################################################

all: build doc proof test

build: $(BUILD_DIR)/libkeccak.a

proof:
	gnatprove -P build/build_libkeccak.gpr

doc: build
	gnatdoc -P build/build_libkeccak.gpr

$(BUILD_DIR)/libkeccak.a:
	gprbuild $(GNATMAKE_OPTS) -p -P build/build_libkeccak

test: kat unit_test

kat: install_local
	$(MAKE) KECCAK_DIR=$(OUTPUT_DIR)/keccak -C tests kat

unit_test:
	$(MAKE) KECCAK_DIR=$(OUTPUT_DIR)/keccak -C tests unit_test

coverage:
	$(MAKE) KECCAK_DIR=$(OUTPUT_DIR)/keccak -C tests coverage

.PHONY: benchmark
benchmark: install_local
	$(MAKE) KECCAK_DIR=$(OUTPUT_DIR)/keccak -C tests benchmark
	
install: build
	install -d -m 755 $(DESTDIR)/adalib $(DESTDIR)/adainclude
	install -p -m 644 $(BUILD_DIR)/adalib/libkeccak.a $(BUILD_DIR)/adalib/*.ali $(DESTDIR)/adalib/
	install -p -m 644 src/*.ads $(DESTDIR)/adainclude/
	install -p -m 644 $(GENERIC_BODIES) $(DESTDIR)/adainclude
	install -p -m 644 build/libkeccak.gpr $(DESTDIR)/
	
install_local: DESTDIR=$(OUTPUT_DIR)/keccak
install_local: install
	
test_clean:
	$(MAKE) -C tests clean

clean: test_clean
	rm -rf $(OUTPUT_DIR)

.PHONY: clean all build doc install_local 
