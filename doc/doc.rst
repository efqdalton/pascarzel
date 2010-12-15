===========================
 2.o Laboratório de CES-41
===========================

:Autor: Alexandre Hideki Deguchi Martani
:Contato: amartani@gmail.com
:Data: 30 de setembro de 2010
:Professor: Fábio Carneiro Mokarzel
:Assunto: Analisador sintático

Introdução
==========

Neste laboratório, foi implementado o analisador sintático para a linguagem
de programação simplificada COMP-ITA 2010. Utilizando-se este analisador,
será futuramente construido o analisador semântico e, por fim, um interpretador
para a linguagem.

Como forma de ilustração do funcionamento do analisador sintático, o mesmo
será utilizado para a criação de um *pretty-printer*, ou seja, que imprima
o código fonte dado como entrada de forma organizada, com identações e
espaçamentos seguindo os padrões adequados.

Cabe ressaltar que as regras de
espaçamento adotados foram ligeiramente diferentes das utilizadas pelo
professor, devido à preferência do aluno.

Ferramentas utilizadas
======================

 * Gerador de analisador léxico flex 2.5.35
 * Gerador de analisador sintático bison 2.4.1 (compatível com yacc)
 * Compilador GCC 4.4.3

Resultados Obtidos
==================

Primeiramente, foi executado o analisador em um arquivo de teste que
contém somente a função principal, com instruções de atribuição, para
testar a correta interpretação das sintaxes básicas, de variáveis e
de expressões. O conteúdo do primeiro arquivo de testes é o seguinte::

	main local { x: int; } statements {
	x:=x+5*(3+x);
	y:=x[5,1,6]%y;
	if x>2.2e-3||y[2,3]<x[2] then z:=y&&x[2];
	}

Para esta entrada, a saída foi a seguinte::

	main
	local
	{
		x: int;
	}
	statements
	{
		x := x + 5 * (3 + x);
		y := x[5, 1, 6] % y;
		if x > 0.0022 || y[2, 3] < x[2] then
			z := y && x[2];
	}

Em seguida, foi executado o analisador em um arquivo de teste que
ainda contém somente a função principal, porém que contém todas as
estruturas de controle de fluxo (if, while, repeat, for), para testar
a correta análise dos mesmos. O conteúdo deste segundo arquivo de
testes é o seguinte::

	main
	local
	{
	x: int;
	y, z: int;
	}
	statements
	{
	write ( "Digite os numeros:" );
	read (x, y);
	write ( "O numero eh ", x);
	if x > 2 then x := 4; else {
	while x > 0 do x := y + 2;
	repeat x := x / 2; until x < z - 2;
	}
	for x := 2 to 10 do {
	for y := x downto 0 step 2 do
	z := x * y;
	write(z);
	}
	}

Para esta entrada, o resultado foi o seguinte::

	main
	local
	{
		x: int;
		y, z: int;
	}
	statements
	{
		write("Digite os numeros:");
		read(x, y);
		write("O numero eh ", x);
		if x > 2 then
			x := 4;
		else
			{
				while x > 0 do
					x := y + 2;
				repeat
					x := x / 2;
				until x < z - 2;
			}
		for x := 2 to 10 do
			{
				for y := x downto 0 step 2 do
					z := x * y;
				write(z);
			}
	}

Por fim, foi utilizada uma entrada que contém uma subrotina, uma
variável global e variáveis locais do tipo vetor, para testar estas
estruturas. Também foram incluidos comentários para garantir a correta
eliminação dos mesmos. A entrada utilizada foi a seguinte::

	global { a: float; }
	/* Funcao maior
	Calcula o maior valor entre os tres parametros.
	*/
	function maior : int
	parameters {
		b: float; c: float;
	} local { maior: int; }
	statements
	{
		/* Checa se o primeiro eh maior */
		if b > a then
			/* Se nao for, compara o segundo com o terceiro */
			if c > b then maior := 2; else maior := 1;
		else
			/* Se for, compara o primeiro com o terceiro */
			if c > a then
			maior := 3;
			else
					maior := 0;
		return maior;
	}

	main
	local
	{
		x: array[3] of int;
	}
	statements
	{
		write ( "Digite tres numeros:" );
		read (x[1]);
		read (x[2]);
		read (x[3]);
		write ( "O maior eh o ", maior(x, y, z), "o." );
		call maior(x[2]);
	}

Para esta entrada, a saída obtida foi a seguinte::

	global
	{
		a: float;
	}
	function maior: int
	parameters
	{
		b: float;
		c: float;
	}
	local
	{
		maior: int;
	}
	statements
	{
		if b > a then
			if c > b then
				maior := 2;
			else
				maior := 1;
		else
			if c > a then
				maior := 3;
			else
				maior := 0;
		return maior;
	}
	main
	local
	{
		x: array [3] of int;
	}
	statements
	{
		write("Digite tres numeros:");
		read(x[1]);
		read(x[2]);
		read(x[3]);
		write("O maior eh o ", maior(x, y, z), "o.");
		call maior(x[2]);
	}

Todas as saídas obtidas foram como as esperadas.

Conclusões
==========

 * O analisador sintático complementa o analisador léxico como passos iniciais
   no projeto de um compilador, identificando a maior parte das estruturas do
   programa e permitindo a obtenção de uma estrutura de dados de fácil manipulação
   posterior;
 * A construção de um analisador sintático pode ser bastante simplificada com a utilização
   de ferramentas como o `yacc`.

