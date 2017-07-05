UNAME_M    := $(shell uname -m)
LSCPU_AVX2 := $(shell lscpu | grep -o avx2)

SUPPORTED_ARCHS = x86_64

# If uname -m returns a model that isn't supported, then
# default to the generic architecture. Otherwise, default
# to the architecture returned by uname -m
ifeq ($(filter $(UNAME_M),$(SUPPORTED_ARCHS)),)
	ARCH ?= generic
else
	ARCH ?= $(UNAME_M)
endif

# Select default value for SIMD based on the current architecture.
ifeq ($(ARCH),x86_64)

ifeq ($(LSCPU_AVX2),avx2)
	# default to AVX2 if available
	SIMD ?= AVX2
else
	# default to SSE2 on x86_64 as it's available on all modern hardware
	SIMD ?= SSE2
endif

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
