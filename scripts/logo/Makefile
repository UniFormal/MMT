all: mmtlogo.svg
mmtlogo.svg: mmtlogo.tex
	pdflatex --shell-escape mmtlogo.tex
mmtlogo.pdf: mmtlogo.tex
	pdflatex mmtlogo.tex
clean: logclean
	rm mmtlogo.svg
logclean:
	rm *.aux *.log *.pdf
