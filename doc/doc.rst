===========================
 3.o Laboratório de CES-41
===========================

:Autor: Alexandre Hideki Deguchi Martani
:Contato: amartani@gmail.com
:Autor: Dalton V. T. Pinto
:Contato: efqdalton@gmail.com
:Data: 14 de dezembro de 2010
:Professor: Fábio Carneiro Mokarzel
:Assunto: Analisador semantico

Introdução
==========

Neste laboratório, foi implementado o analisador semântico para a linguagem
de programação simplificada COMP-ITA 2010. Utilizando-se este analisador,
será futuramente construido o gerador de código intermediário e, por fim,
um interpretador para a linguagem.

Ferramentas utilizadas
======================

 * Sistema operacional Linux Ubuntu 10.10
 * Gerador de analisador léxico flex 2.5.35
 * Gerador de analisador sintático bison 2.4.1 (compatível com yacc)
 * Compilador GCC 4.4.4

Resultados Obtidos
==================

Os arquivos com o código-fonte, as entradas de teste e as saídas obtidas
estão em anexo. As entradas de teste estão presentes na pasta *tests*, e as
saídas na pasta *output*. Em um sistema Linux com as ferramentas adequadas
instaladas, todos os testes podem ser executados a partir do seguinte
comando na pasta principal do projeto::

    make test-syntatic

Buscou-se criar pequenos casos de teste que exercitassem cada possível erro
de ser encontrado (entradas iniciadas em "error"), além de casos de testes
mais abrangentes com códigos semanticamente corretos.

Todas as saídas obtidas foram como as esperadas.

Conclusões
==========

 * O analisador semântico complementa os analisadores sintático e léxico como passos iniciais
   no projeto de um compilador, identificando todos os possíveis erros do código-fonte e
   obtendo uma estrutura de fácil manipulação posterior;
 * A construção de um analisador semântico pode ser bastante simplificada com a utilização
   de ferramentas como o `yacc`. Em particular, a utilização de atributos das produções foi
   fundamental nesta implementação.

