default: doc.pdf
doc.tex:
	rst2latex doc.rst > doc.tex

doc.pdf: doc.tex
	rubber --pdf doc.tex

view: doc.pdf
	gnome-open doc.pdf

clean:
	rm -f doc.tex doc.out doc.log doc.aux

clean-all: clean
	rm -f doc.pdf

