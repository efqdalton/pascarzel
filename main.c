#include <stdio.h>
#include <string.h>

int hasErrors = 0;
char errorMessage[100];

void addError(char *msg){
  strcpy(errorMessage, msg);
  hasErrors = 1;
}

int main(){
  yyparse();
  if(!hasErrors) fprintf(stderr, "/* Sintaticamente correto! */\n");
  return 0;
}
