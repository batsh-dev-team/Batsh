batsh: dist/build/batsh/batsh
	ln -sf dist/build/batsh/batsh batsh

test:
	cabal build
	cabal test

dist:
	cabal configure --enable-tests

dist/build/batsh/batsh: dist
	cabal build

.PHONY: clean test

clean:
	cabal clean
	rm -f batsh
