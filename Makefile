filename=presentation

pdf:
	pdflatex ${filename}
	bibtex ${filename}||true
	pdflatex ${filename}
	pdflatex ${filename}
	make clean

clean:
	git clean -Xfd
