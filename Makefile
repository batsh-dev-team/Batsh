SRCFILES = src/*.ml*

OCAMLFORMAT = ocamlformat \
	--inplace \
	--field-space loose \
	--let-and sparse \
	--let-open auto \
	--type-decl sparse \
	--sequence-style terminator \
	$(SRCFILES)

OCPINDENT = ocp-indent \
	--inplace \
	$(SRCFILES)

.PHONY: all
all :
	dune build @all

.PHONY: test
test :
	dune exec ./tests/main.exe

.PHONY: run
run :
	dune exec ./src/main.exe

.PHONY: format
format :
	$(OCAMLFORMAT)
	$(OCPINDENT)

.PHONY : clean
clean:
	dune clean
