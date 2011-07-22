all :
	ghc -o tcc --make Main
clean :
	rm  *.o *.hi */*.o */*.hi
