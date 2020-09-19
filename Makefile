configure:
	cabal configure

build:
	cabal build

clean:
	cabal clean

ghci:
	cabal repl

ghcid:
	ghcid -c "cabal repl"

sdist:
	cabal sdist
