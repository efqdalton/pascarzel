#include <stdio.h>
#include <stdlib.h>

int yyerror(char *msg){
  printf("\n\n/* Erro: %s */\n\n", msg);
  fprintf(stderr, "Erro: %s\n\n", msg);
  exit(65); /* Unix data format error */
}
