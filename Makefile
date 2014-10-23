batsh: dist/build/batsh/batsh
	ln -sf dist/build/batsh/batsh batsh

dist:
	cabal configure

dist/build/batsh/batsh: dist src/*.hs src/BatshLex.x
	cabal build

.PHONY: clean

clean:
	cabal clean
	rm -f batsh
