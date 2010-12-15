#include <stdio.h>
#include <string.h>
#include <stdarg.h>

int hasErrors = 0;

void addError(char *buff, ...){
  va_list arglist, arglist_copy;
  va_start(arglist, buff);
  va_copy(arglist_copy, arglist);
  vfprintf(stderr, buff, arglist);
  vfprintf(stdout, buff, arglist_copy);
  hasErrors = 1;
  va_end(arglist);
  va_end(arglist_copy);
}

int main(){
  yyparse();
  if(!hasErrors) fprintf(stderr, "/* Sintaticamente correto! */\n");
  return 0;
}
