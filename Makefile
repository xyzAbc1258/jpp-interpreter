all:
	cabal configure
	cabal build
	cp ./dist/build/jppinterpreter-exe/jppinterpreter-exe ./Interpreter

clean:
	rm -rf ./dist