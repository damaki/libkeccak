OUTPUT_DIR   = $(CURDIR)/out
BUILD_DIR    = $(OUTPUT_DIR)/build

GENERIC_BODIES = src/keccak-duplex.adb \
                 src/keccak-hash.adb \
                 src/keccak-sponge.adb \
                 src/keccak-xof.adb \
                 src/keccak-keccakf.adb \
                 src/keccak-keccakf-permutation.adb

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
