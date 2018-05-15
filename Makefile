all:
	cabal configure
	cabal build
	cp ./dist/build/jppinterpreter-exe/jppinterpreter-exe ./interpreter

clean:
	rm -rf ./dist