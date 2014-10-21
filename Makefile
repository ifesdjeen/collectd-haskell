GHC=ghc
GHC_RUNTIME_LINKER_FLAG=-lHSrts-ghc7.8.3

.PHONY: cbits/wrapper.o
cbits/wrapper.o: cbits/wrapper.c dist/build/Collectd/C_stub.h
	$(GHC) -c -dynamic -fPIC wrapper.c

wrapper:
	cbits/wrapper.o
