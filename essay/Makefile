filename=essay

pdf:
	pdflatex -synctex=1 -interaction=nonstopmode ${filename}
	bibtex ${filename}||true
	pdflatex ${filename}
	pdflatex ${filename}
	make clean

clean:
	rm -f ./*.ps ./*.log ./*.aux ./*.out ./*.dvi ./*.blg ./*.brf ./*.nav ./*.snm ./*.toc
