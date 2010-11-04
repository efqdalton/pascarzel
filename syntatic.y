%{

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

/* Definicao dos atributos dos atomos operadores */
#define   LT         1
#define   LE         2
#define   GT         3
#define   GE         4
#define   EQ         5
#define   NE         6
#define   MAIS       7
#define   MENOS      8
#define   MULT       9
#define   DIV        10
#define   RESTO      11
#define   OR         12
#define   AND        13
#define   NOT        14
#define   NEG        15

/* Definicao dos tipos de identificadores */
#define   IDVAR      1

/* Definicao dos tipos de variaveis */
#define   NAOVAR     0
#define   INTEIRO    1
#define   LOGICO     2
#define   REAL       3
#define   CARACTERE  4

/* Definicao de outras constantes */
#define   NCLASSHASH  23
#define   VERDADE     1
#define   FALSO       0

/* Protótipos para analisador semantico */
void declareVariable(char *);

/* Protótipos e variaveis para pretty-printer */
int identation_deep = 0;
void increaseTabSize();
void decreaseTabSize();
void printTabs();
void printWithTabs(char *buff, ...);
void printIncreasingTabs(char *buff, ...);
void printDecreasingTabs(char *buff, ...);
void printReadableChar(char);
char *translateOperator(int);

/* Strings para nomes dos tipos de identificadores */
char *nometipid[2] = {" ", "IDVAR"};

/* Strings para nomes dos tipos de variaveis */
char *nometipvar[5] = { "NAOVAR", "INTEIRO", "LOGICO", "REAL", "CARACTERE" };

/* Declaracoes para a tabela de simbolos */
typedef struct celsimb celsimb;
typedef celsimb *simbolo;
struct celsimb {
  char *cadeia;
  int  tid, tvar;
  char inic, ref;
  simbolo prox;
};

/* Declarações para lista linear de simbolos */
typedef struct elemlistsimb elemlistsimb;
typedef elemlistsimb *listasimbolo;
struct elemlistsimb {
  simbolo simb;
  elemlistsimb *prox;
};

/* Variaveis globais para a tabela de simbolos e analise semantica */
simbolo tabsimb[NCLASSHASH];
simbolo simb;
listasimbolo listsimb;
int tipocorrente;

/* Prototipos das funcoes para a tabela de simbolos e analise semantica */

void    InicTabSimb(void);

void    InicListSimb(void);
void    InsereListSimb(simbolo);
void    AnulaListSimb(void);
void    AdicTipoVar(void);

void    ImprimeTabSimb(void);
simbolo InsereSimb(char *, int);
int     hash(char *);
simbolo ProcuraSimb(char *);
void    DeclaracaoRepetida(char *s);
void    TipoInadequado(char *);
void    NaoDeclarado(char *);

%}

/* Ativa erros com mais detalhes do yacc */
%error-verbose

/* Definicao do tipo de yylval */
%union {
  char    cadeia[100];
  int     atr, valint;
  float   valreal;
  char    carac;
  simbolo simb;
}

/* Declaracao dos atributos dos tokens e dos nao-terminais */
%token    <cadeia>    ID
%token    <valint>    INTCT
%token    <carac>     CHARCT
%token    <valreal>   FLOATCT
%token    <cadeia>    STRING

%token    <atr>       OROP
%token    <atr>       ANDOP
%token    <atr>       NOTOP
%token    <atr>       RELOP
%token    <atr>       ADOP
%token    <atr>       MULTOP
%token    <atr>       NEGOP
%token    OPPAR
%token    CLPAR
%token    OPBRAK
%token    CLBRAK
%token    OPBRACE
%token    CLBRACE
%token    SCOLON
%token    COMMA
%token    COLON
%token    ASSIGN 
%token    ARRAY
%token    CALL
%token    CHAR
%token    DO
%token    ELSE
%token    FALSE
%token    FLOAT
%token    FOR
%token    FUNCTION
%token    GLOBAL
%token    IF
%token    INT
%token    LOCAL
%token    LOGIC
%token    MAIN
%token    OF
%token    PARAMETERS
%token    READ
%token    REPEAT
%token    RETURN
%token    STATEMENTS
%token    STEP
%token    THEN
%token    TO
%token    DOWNTO
%token    TRUE
%token    UNTIL
%token    VOID
%token    WHILE
%token    WRITE

%%

/* Producoes da gramatica */

Prog         : { InicTabSimb(); InicListSimb(); } GlobDecls FuncList MainDef
             ;
GlobDecls    : ;
             | GLOBAL { printIncreasingTabs("global {\n"); } OPBRACE DeclList CLBRACE { printDecreasingTabs("}\n\n"); }
             ;
DeclList     : Declaration
             | DeclList Declaration
             ;  
Declaration  : { printTabs(); } IdList { printf(" : "); } COLON Type SCOLON { printf(";\n"); }
             ;
IdList       : ID              { declareVariable($1); printf("%s", $1); }
             | IdList COMMA ID { declareVariable($3); printf(", %s", $3); }
             ;
Type         : ScalarType
             | ArrayType
             ;
ScalarType   : INT   { printf("int");   }
             | FLOAT { printf("float"); }
             | CHAR  { printf("char");  }
             | LOGIC { printf("logic"); }
             | VOID  { printf("void");  }
             ;
ArrayType    : ARRAY OPBRAK { printf("array ["); } DimList CLBRAK OF { printf("] of "); } ScalarType
             ;
DimList      : INTCT               { printf("%d", $1);}
             | DimList COMMA INTCT { printf(", %d", $3);}
             ;
FuncList     : ;
             | FuncList FuncDef
             ;
FuncDef      : { AnulaListSimb(); } FUNCTION ID COLON { printIncreasingTabs("function %s : ", $3); } ScalarType { printf("\n"); } Params LocDecls Statmts { printDecreasingTabs("\n"); }
             ;
MainDef      : { AnulaListSimb(); } MAIN { printIncreasingTabs("main \n"); } LocDecls Statmts { printDecreasingTabs("\n"); }
             ;
Params       : ;
             | PARAMETERS OPBRACE { printIncreasingTabs("parameters {\n"); } ParamList CLBRACE { printDecreasingTabs("}\n"); }
             ;
ParamList    : ParamDecl
             | ParamList ParamDecl
             ;
ParamDecl    : { printTabs(); } IdList COLON { printf(" : "); } ScalarType SCOLON { printf(";\n"); }
             ;
LocDecls     : ;
             | LOCAL OPBRACE { printIncreasingTabs("local {\n"); } DeclList CLBRACE { printDecreasingTabs("}\n"); }
             ;
Statmts      : STATEMENTS OPBRACE { printIncreasingTabs("statements {\n"); } StatList CLBRACE { printDecreasingTabs("}\n"); }
             ;
StatList     : Statement
             | StatList Statement
             ;
Statement    : CompoundStat
             | IfStat
             | WhileStat
             | RepeatStat
             | ForStat
             | ReadStat
             | WriteStat
             | AssignStat
             | CallStat
             | ReturnStat
             | SCOLON
             ;
CompoundStat : OPBRACE { printDecreasingTabs("{\n"); increaseTabSize(); } StatList CLBRACE {  decreaseTabSize(); printIncreasingTabs("}\n"); }
             ;
IfStat       : IF { printWithTabs("if "); } Expression THEN { printf(" then\n"); increaseTabSize(); } Statement { decreaseTabSize(); } ElseStat
             ;
ElseStat     : ;
             | ELSE { printIncreasingTabs("else\n"); } Statement { decreaseTabSize(); }
             ;
WhileStat    : WHILE { printIncreasingTabs("while "); } Expression DO { printf(" do\n"); } Statement { decreaseTabSize(); }
             ;
RepeatStat   : REPEAT { printIncreasingTabs("repeat "); } Statement UNTIL { printDecreasingTabs("until "); } Expression SCOLON { printf(";"); }
             ;
ForStat      : FOR { printIncreasingTabs("for "); } Variable ASSIGN { printf(" := "); } Expression Direcao Expression StepDef DO { printf(" do\n"); } Statement { decreaseTabSize(); }
             ;
Direcao      : TO     { printf(" to "); }
             | DOWNTO { printf(" downto "); }
             ;             
StepDef      : ;
             | STEP { printf(" step "); } Expression
             ;
ReadStat     : READ OPPAR { printWithTabs("read( "); } VarList CLPAR SCOLON { printf(" );\n"); }
             ;
VarList      : Variable
             | VarList COMMA { printf(", "); } Variable
             ;
WriteStat    : WRITE OPPAR { printWithTabs("write( "); } WriteList CLPAR SCOLON { printf(" );\n"); }
             ;
WriteList    : WriteElem
             | WriteList COMMA { printf(", "); } WriteElem
             ;
WriteElem    : STRING { printf("%s", $1); }
             | Expression
             ;
CallStat     : CALL { printWithTabs("call "); } FuncCall SCOLON { printf(";\n"); }
             ;
FuncCall     : ID OPPAR { printf("%s(", $1); } Arguments CLPAR { printf(")"); }
             ;
Arguments    : ;
             | ExprList
             ;             
ExprList     : Expression
             | ExprList COMMA { printf(", "); } Expression
             ;                        
ReturnStat   : RETURN SCOLON { printWithTabs("return ;\n"); }
             | RETURN { printWithTabs("return "); } Expression SCOLON { printf(";\n"); }
             ;
AssignStat   : { printTabs(); } Variable ASSIGN { printf(" := "); } Expression SCOLON { printf(";\n"); }
             ;
Expression   : AuxExpr1
             | Expression OROP { printf(" || "); } AuxExpr1
             ;
AuxExpr1     : AuxExpr2
             | AuxExpr1 ANDOP { printf(" && "); } AuxExpr2
             ;
AuxExpr2     : AuxExpr3
             | NOTOP { printf("!"); } AuxExpr3
             ;
AuxExpr3     : AuxExpr4
             | AuxExpr4 RELOP { printf(" %s ", translateOperator($2)); } AuxExpr4
             ;
AuxExpr4     : Term
             | AuxExpr4 ADOP { printf("%s", translateOperator($2)); } Term
             ;
Term         : Factor
             | Term MULTOP { printf("%s", translateOperator($2)); } Factor
             ;
Factor       : Variable
             | INTCT    { printf("%d", $1);                 }
             | FLOATCT  { printf("%e", $1);                 }
             | CHARCT   { printReadableChar($1);            }
             | TRUE     { printf("true");                   }
             | FALSE    { printf("false");                  }
             | NEGOP { printf("~"); } Factor
             | OPPAR { printf("("); } Expression CLPAR { printf(")"); }
             | FuncCall
             ;
Variable     : ID { printf("%s", $1); } Subscripts
             ;
Subscripts   : ;
             | OPBRAK { printf("["); } SubscrList CLBRAK { printf("]"); }
             ;
SubscrList   : AuxExpr4
             | SubscrList COMMA { printf(", "); } AuxExpr4
             ;

%%

/* Inclusao do analisador lexico */
#include "lex.yy.c"

/* Semantic Analisys */
void declareVariable(char *variable){
  if( ProcuraListSimb(variable) != NULL ){
    DeclaracaoRepetida(variable);
  }else{
    simb = InsereSimb(variable, IDVAR);
    InsereListSimb(simb);    
  }
}

/* Pretty-printer */
void increaseTabSize(){
  identation_deep++;  
}

void decreaseTabSize(){
  identation_deep--;
}

void printTabs(){
  int i;
  for(i = 0; i < identation_deep; i++ ) printf("  ");
}

void printWithTabs(char *buff, ...){
  va_list arglist;
  va_start(arglist, buff);
  printTabs();
  vprintf(buff, arglist);
  va_end(arglist);
}

void printIncreasingTabs(char *buff, ...){
  va_list arglist;
  va_start(arglist, buff);
  printTabs();
  vprintf(buff, arglist);
  increaseTabSize();
  va_end(arglist);
}

void printDecreasingTabs(char *buff, ...){
  va_list arglist;
  va_start(arglist, buff);
  decreaseTabSize();
  printTabs();
  vprintf(buff, arglist);
  va_end(arglist);
}

void printReadableChar(char c){
  char result[2];
  result[0] = c;
  result[1] = '\0';
  switch(c){
    case 7 :  printf("'\\a'") ; break;  case 92:  printf("'\\\\'"); break;
    case 8 :  printf("'\\b'") ; break;  case 13:  printf("'\\r'") ; break;
    case 34:  printf("'\\\"'"); break;  case 12:  printf("'\\f'") ; break;
    case 9 :  printf("'\\t'") ; break;  case 10:  printf("'\\n'") ; break;
    case 0 :  printf("'\\0'") ; break;  case 39:  printf("'\\\''"); break;
    case 11:  printf("'\\v'") ; break;
    default:  printf("'%s'", result);
  }
}

char *translateOperator(int op){
  switch(op){
    case LT:    return "<" ;  case LE:    return "<=";
    case GT:    return ">" ;  case GE:    return ">=";
    case EQ:    return "=" ;  case NE:    return "!=";
    case MAIS:  return "+" ;  case MENOS: return "-" ;
    case MULT:  return "*" ;  case DIV:   return "/" ;
    case RESTO: return "%" ;  case OR:    return "||";
    case AND:   return "&&";  case NOT:   return "!" ;
    case NEG:   return "~" ;
  }
}

/* InicTabSimb: Inicializa a tabela de simbolos */
void InicTabSimb(){
  int i;
  for(i = 0; i < NCLASSHASH; i++) tabsimb[i] = NULL;
}

/* InicListSimb: Inicializa a lista linear global de simbolos */
void InicListSimb(){
  listsimb = NULL;
}

/* InsereListSimb: Insere um simbolo na lista linear global de simbolos
   simb: simbolo a ser inserido */
void InsereListSimb(simbolo simb){
  elemlistsimb *p;
  p = listsimb;
  listsimb = malloc(sizeof(elemlistsimb));
  listsimb->simb = simb;
  listsimb->prox = p;
}

/*  AnulaListSimb: Anula a corrente lista linear de simbolos  */
void AnulaListSimb(void) {
  elemlistsimb *p;

  while(listsimb!=NULL) {
    p=listsimb;
    listsimb = listsimb->prox;
    free(p);
  }
}

/* AdicTipoVar: Coloca o tipo de variavel corrente em todos os simbolos da lista de simbolos */
void AdicTipoVar() {
  elemlistsimb *p;
  for(p=listsimb; p!=NULL; p = p->prox) p->simb->tvar = tipocorrente;
}

/*
  ProcuraSimb (cadeia): Procura cadeia na tabela de simbolos;
  Caso ela ali esteja, retorna um ponteiro para sua celula;
  Caso contrario, retorna NULL.
*/
simbolo ProcuraSimb (char *cadeia) {
  simbolo s; int i;
  i = hash (cadeia);
  for(s = tabsimb[i]; (s!=NULL) && strcmp(cadeia, s->cadeia); s = s->prox);
  return s;
}

/*
  InsereSimb (cadeia, tid): Insere cadeia na tabela de
  simbolos, com tid como tipo de identificador; Retorna um
  ponteiro para a celula inserida
*/
simbolo InsereSimb(char *cadeia, int tid) {
  int i; simbolo aux, s;
  i = hash(cadeia);
  aux = tabsimb[i];
  s = tabsimb[i] = malloc(sizeof (celsimb));
  s->cadeia = malloc((strlen(cadeia)+1)*sizeof(char));
  strcpy(s->cadeia, cadeia);
  s->tid = tid;
  s->tvar = NAOVAR;
  s->inic = FALSO; 
  s->ref = FALSO;
  s->prox = aux;
  return s;
}

/*
  hash (cadeia): funcao que determina e retorna a classe
  de cadeia na tabela de simbolos implementada por hashing
*/
int hash(char *cadeia) {
  int i, h;
  for (h = i = 0; cadeia[i]; i++) {h += cadeia[i];}
  h = h % NCLASSHASH;
  return h;
}

/* ImprimeTabSimb: Imprime todo o conteudo da tabela de simbolos  */
void ImprimeTabSimb() {
  int i; simbolo s;
  printf("\n\n   TABELA  DE  SIMBOLOS:\n\n");
  for(i = 0; i < NCLASSHASH; i++){
    if(tabsimb[i]) {
      printf("Classe %d:\n", i);
      for(s = tabsimb[i]; s!=NULL; s = s->prox){
        printf("  (%s, %s", s->cadeia,  nometipid[s->tid]);
        if(s->tid == IDVAR)
          printf(", %s, %d, %d", nometipvar[s->tvar], s->inic, s->ref);
        printf(")\n");
      }
    }
  }
}

/*  Erros semanticos  */
void DeclaracaoRepetida(char *s){
  addError("/* Declaracao Repetida: %s */\n", s);
}

void NaoDeclarado(char *s){
  addError("/* Identificador Nao Declarado: %s */\n", s);
}

void TipoInadequado(char *s){
  addError("/* Identificador de Tipo Inadequado: %s */\n", s);
}

/* FALTA VerificaInicRef (slides III, pag 73) */
