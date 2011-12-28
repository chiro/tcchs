all :
	ghc -o tcc --make Main
clean :
	rm  tcc *.o *.hi */*.o */*.hi
