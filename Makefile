# vim: ts=4 noet

default: syntatic

lex.yy.c:
	flex lexical.l

y.tab.c:
	yacc --report state syntatic.y

syntatic: lex.yy.c y.tab.c
	gcc y.tab.c yyerror.c main.c -lfl -o syntatic

test-syntatic:
	for teste in tests/*.mp; do \
		./syntatic < $teste > output/$(basename $teste .mp).mp ; \
		./syntatic < output/$(basename $teste .mp).mp > output/$(basename $teste .mp).mp.mp ; \
	done

clean:
	rm -f syntatic lex.yy.c y.tab.c


