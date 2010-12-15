# vim: ts=4 noet

default: syntatic lab.zip

lex.yy.c: lexical.l
	flex lexical.l

y.tab.c: syntatic.y
	yacc --report state syntatic.y

syntatic: lex.yy.c y.tab.c main.c yyerror.c
	gcc -ggdb y.tab.c yyerror.c main.c -lfl -o syntatic

test-syntatic: syntatic
	for teste in tests/*.mp; do \
		echo "$$teste" ; \
		./syntatic < $$teste > output/$$(basename $$teste .mp).mp ; \
	done

clean:
	rm -f syntatic lex.yy.c y.tab.c lab.zip y.output output/*.mp
	cd doc && make clean

doc/doc.pdf:
	cd doc && make doc.pdf

lab.zip: doc/doc.pdf test-syntatic
	zip lab.zip doc/doc.pdf lexical.l syntatic.y main.c yyerror.c Makefile tests/*.mp output/*.mp
