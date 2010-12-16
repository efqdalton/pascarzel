===========================
 4.o Laboratório de CES-41
===========================

:Autor: Alexandre Hideki Deguchi Martani
:Contato: amartani@gmail.com
:Autor: Dalton V. T. Pinto
:Contato: efqdalton@gmail.com
:Data: 16 de dezembro de 2010
:Professor: Fábio Carneiro Mokarzel
:Assunto: Gerador de código intermediário

Introdução
==========

Neste laboratório, foi implementado o gerador de código intermediário para a linguagem
de programação simplificada COMP-ITA 2010. Utilizando-se este gerador,
será futuramente construido
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

Buscou-se criar pequenos casos de teste que exercitassem cada regra de produção
de quadruplas, além de casos de testes mais abrangentes, com códigos maiores.

Todas as saídas obtidas foram como as esperadas.

Conclusões
==========

 * O gerador de código intermediário transforma o programa escrito na linguagem de programação
   em um código próximo ao código de máquina, de forma que a etapa final de transformação do
   mesmo se torna simples. Ao mesmo tempo, o código intermediário é genérico o suficiente
   para permitir a portabilidade entre plataformas distintas;
 * A construção de um interpretador pode ser bastante simplificada com a utilização
   de ferramentas como o `yacc`.

