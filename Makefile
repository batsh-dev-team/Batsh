build: ocp-build.root
	ocp-build build -njobs 16

LIBDIR=$(CAML_LD_LIBRARY_PATH)/../batsh

install: build
	ocp-build install -install-lib "$(LIBDIR)"

uninstall:
	ocp-build uninstall -install-lib "$(LIBDIR)"

test:
	ocp-build test
	@cd tests && ../_obuild/test/test.asm

ocp-build.root:
	ocp-build root

clean:
	ocp-build clean

.PHONY: build
