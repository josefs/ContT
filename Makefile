Paper.pdf: Paper.tex
	pdflatex Paper.tex
	pdflatex Paper.tex

Paper.tex: Paper.md Makefile Paper.bib latex.template
	pandoc Paper.md -o Paper.tex -s --tab-stop=2 --bibliography=Paper.bib --template=latex.template
