GHC=ghc
COLLECTD_LIB=${HOME}/opt/collectd/lib/collectd

.PHONY: cbits/wrapper.o
cbits/wrapper.o: cbits/wrapper.c dist/build/Collectd/C_stub.h
	$(GHC) -c -dynamic -fPIC -I./dist/build/Collectd/ cbits/wrapper.c

wrapper:
	make cbits/wrapper.o

install: cbits/wrapper.o
	cabal configure && \
	cabal build     && \
	cp collectd_haskell.so ${COLLECTD_LIB}
