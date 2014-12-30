build:
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests && cabal build && cabal test
.PHONY: build

run:
	cabal run
.PHONY: run

clean:
	rm -f *.o *.hi
	rm -rf dist
.PHONY: clean
