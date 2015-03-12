
#include plots/Makefile

lmvn: plots
	pdflatex $@
	bibtex $@
	pdflatex $@
	pdflatex $@

plots:
	@make -C plots
