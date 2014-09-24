build: _obuild
	ocp-build build batsh
	ln -sf _obuild/batsh/batsh.asm batsh

LIBDIR=$(CAML_LD_LIBRARY_PATH)/..
BATSHDIR=$(LIBDIR)/batsh

install: build
	ocp-build install batsh-lib batsh -install-lib "$(LIBDIR)"
	# This is an unly wordaround for fixing the generated META file
	sed -i 's/ camlp4lib//g' "$(LIBDIR)/META.batsh"
	sed -i 's/ camlp4lib//g' "$(LIBDIR)/META.batsh-lib"

uninstall:
	ocp-build uninstall

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
