UNAME_M := $(shell uname -m)

ARCH ?= $(UNAME_M)

ifeq ($(ARCH),x86_64)
	# default to SSE2 on x86_64 as it's available on all modern hardware
	SIMD ?= SSE2
else
	SIMD ?= none
endif

################################################################################

all: build doc proof test

build: build/libkeccak.a

proof:
	gnatprove -P build/libkeccak.gpr -Xarch=$(ARCH) -Xsimd=$(SIMD)

doc: build
	gnatdoc -P build/libkeccak.gpr

build/libkeccak.a:
	gprbuild $(GNATMAKE_OPTS) -p -P build/libkeccak -Xarch=$(ARCH) -Xsimd=$(SIMD)

test: kat unit_test

kat: install_local
	$(MAKE) -C tests kat

unit_test: install_local
	$(MAKE) -C tests unit_test ARCH=$(ARCH) SIMD=$(SIMD)

coverage: install_local
	$(MAKE) -C tests coverage

.PHONY: benchmark
benchmark: install_local
	$(MAKE) -C tests benchmark
	
install: build
ifeq ($(PREFIX),)
	gprinstall -Xarch=$(ARCH) -Xsimd=$(SIMD) -f -p -P build/libkeccak.gpr
else
	gprinstall -Xarch=$(ARCH) -Xsimd=$(SIMD) -f -p -P build/libkeccak.gpr --prefix=$(PREFIX)
endif

install_local: build
	gprinstall -Xarch=$(ARCH) -Xsimd=$(SIMD) -f -p -P build/libkeccak.gpr --prefix=lib
	
test_clean:
	$(MAKE) -C tests clean

clean: test_clean
	rm -rf obj lib

.PHONY: clean all build doc install_local 
