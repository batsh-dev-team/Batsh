build: ocp-build.root
	ocp-build build -njobs 16
	ln -sf _obuild/batsh/batsh.asm batsh

LIBDIR=$(CAML_LD_LIBRARY_PATH)/../batsh

install: build
	ocp-build install -install-lib "$(LIBDIR)"

uninstall:
	ocp-build uninstall -install-lib "$(LIBDIR)"

test: build
	ocp-build test
	@./_obuild/test/test.asm

update: build
	node scripts/update.js | bash

ocp-build.root:
	ocp-build root

clean:
	ocp-build clean

.PHONY: build
