all:
	happy -gca Pargramm.y
	alex -g Lexgramm.x
	latex Docgramm.tex; dvips Docgramm.dvi -o Docgramm.ps
	ghc --make Testgramm.hs -o Testgramm
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f Docgramm.ps
distclean: clean
	-rm -f Docgramm.* Lexgramm.* Pargramm.* Layoutgramm.* Skelgramm.* Printgramm.* Testgramm.* Absgramm.* Testgramm ErrM.* SharedString.* gramm.dtd XMLgramm.* Makefile*

