#include <stdio.h>
#include <stdlib.h>

int yyerror(char *msg){
  addError("/* Erro: %s */\n", msg);
  exit(65); /* Unix data format error */
}
