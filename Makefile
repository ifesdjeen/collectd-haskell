GHC           = ghc
COLLECTD_LIB ?= ${HOME}/opt/collectd/lib/collectd

dist/build/Collectd/C_stub.h: src/Collectd/C.hsc
	cabal build

.PHONY: cbits/wrapper.o
cbits/wrapper.o: cbits/wrapper.c dist/build/Collectd/C_stub.h
	$(GHC) -c -threaded -dynamic -fPIC -I./dist/build/Collectd/ cbits/wrapper.c

wrapper: cbits/wrapper.o

.PHONY: collectd_haskell.so
collectd_haskell.so:
	$(GHC) -threaded -I./dist/build/Collectd/ -Wall -optc-DTHREADED_RTS -shared -dynamic -fPIC -lHSrts_thr-ghc7.8.3 -no-hs-main -o collectd_haskell.so ./cbits/wrapper.c

install_plugin: cbits/wrapper.o
	cabal configure && \
	cabal build     && \
	cp test_plugin.so ${COLLECTD_LIB}

install_bindings: collectd_haskell.so
	cp collectd_haskell.so ${COLLECTD_LIB}
