batsh: dist/build/batsh/batsh
	ln -sf dist/build/batsh/batsh batsh

test:
	cabal build
	cabal test

dist:
	cabal configure --enable-tests

dist/build/batsh/batsh: dist src/*.hs src/BatshLex.x
	cabal build

.PHONY: clean test

clean:
	cabal clean
	rm -f batsh
