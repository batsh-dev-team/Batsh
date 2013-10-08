# Generic Makefile for oasis project

# Set to setup for the release
SETUP := setup-dev

# Default rule
default: build

# Setup for the development version
setup-dev: _oasis setup.ml
	grep -v '^#' setup.ml > setup_dev.ml
	ocamlfind ocamlopt -o $@ -linkpkg -package ocamlbuild,oasis.dynrun setup_dev.ml || 	  ocamlfind ocamlc -o $@ -linkpkg -package ocamlbuild,oasis.dynrun setup_dev.ml || true
	rm -f setup_dev.*

# Setup for the release
setup: setup.ml
	ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	rm -f setup.cmx setup.cmi setup.o setup.obj setup.cmo

build: $(SETUP) setup.data
	./$(SETUP) -build $(BUILDFLAGS)

doc: $(SETUP) setup.data build
	./$(SETUP) -doc $(DOCFLAGS)

test: $(SETUP) setup.data build
	./$(SETUP) -test $(TESTFLAGS)

all: $(SETUP)
	./$(SETUP) -all $(ALLFLAGS)

install: $(SETUP) setup.data
	./$(SETUP) -install $(INSTALLFLAGS)

uninstall: $(SETUP) setup.data
	./$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: $(SETUP) setup.data
	./$(SETUP) -reinstall $(REINSTALLFLAGS)

clean: $(SETUP)
	./$(SETUP) -clean $(CLEANFLAGS)

distclean: $(SETUP)
	./$(SETUP) -distclean $(DISTCLEANFLAGS)
	rm -f $(SETUP)

configure: $(SETUP)
	./$(SETUP) -configure $(CONFIGUREFLAGS)

setup.data: $(SETUP)
	./$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: default build doc test all install uninstall reinstall clean distclean configure
