#include <stdio.h>
#include <string.h>
#include <stdarg.h>

int hasErrors = 0;

void addError(char *buff, ...){
  va_list arglist;
  va_start(arglist, buff);
  vfprintf(stdout, buff, arglist);
  hasErrors = 1;       
  va_end(arglist);
}

int main(){
  yyparse();
  if(!hasErrors) fprintf(stdout, "/* Sintaticamente correto! */\n");
  return 0;
}
