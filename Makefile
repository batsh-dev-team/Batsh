build: _obuild
	ocp-build build batsh
	ln -sf _obuild/batsh/batsh.asm batsh

LIBDIR=$(CAML_LD_LIBRARY_PATH)/../batsh

install: build
	ocp-build install -install-lib "$(LIBDIR)"

uninstall:
	ocp-build uninstall -install-lib "$(LIBDIR)"

test: build
	ocp-build build test
	@./_obuild/test/test.asm

update: build
	node scripts/update.js | bash

_obuild:
	ocp-build init

clean:
	ocp-build clean

distclean:
	rm -rf _obuild

.PHONY: build
