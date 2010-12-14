# vim: ts=4 noet

default: syntatic

lex.yy.c: lexical.l
	flex lexical.l

y.tab.c: syntatic.y
	yacc --report state syntatic.y

syntatic: lex.yy.c y.tab.c
	gcc -ggdb y.tab.c yyerror.c main.c -lfl -o syntatic

test-syntatic: syntatic
	for teste in tests/*.mp; do \
		echo "$$teste" ; \
		./syntatic < $$teste > output/$$(basename $$teste .mp).mp ; \
	done

clean:
	rm -f syntatic lex.yy.c y.tab.c


