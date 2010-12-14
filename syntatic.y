%{
/* vim: ts=2 sts=2 sw=2 et
 */

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
#define   IDGLOB     2
#define   IDFUNC     3

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
#define   MAXDIMS     2

/* Protótipos e variaveis para pretty-printer */
int identation_deep = 0;
void InicProg();
void FimProg();
void InicFunc(char *id);
void FimFunc();
void InicFuncParamDecl();
void FimFuncParamDecl();
void InicDeclaration();
void FimDeclaration();
void increaseTabSize();
void decreaseTabSize();
void printTabs();
void printWithTabs(char *buff, ...);
void printIncreasingTabs(char *buff, ...);
void printDecreasingTabs(char *buff, ...);
void printReadableChar(char);
char *translateOperator(int);

/* Strings para nomes dos tipos de identificadores */
char *nometipid[] = {" ", "IDVAR", "IDGLOB", "IDFUNC"};

/* Strings para nomes dos tipos de variaveis */
char *nometipvar[5] = { "NAOVAR", "INTEIRO", "LOGICO", "REAL", "CARACTERE" };

/* Declaracoes para a tabela de simbolos */
typedef struct celsimb celsimb;
typedef celsimb *simbolo;
typedef struct elemlistsimb elemlistsimb;
typedef elemlistsimb *listasimbolo;

struct celsimb {
  char *cadeia;
  int  tid, tvar, tparam, ndims, dims[MAXDIMS+1], nparam;
  char inic, ref, array, param;
  listasimbolo listvar, listparam, listfunc;
  simbolo escopo, prox;
};

/* Declarações para lista linear de simbolos */
struct elemlistsimb {
  simbolo simb;
  elemlistsimb *prox;
};

/* Variaveis globais para a tabela de simbolos e analise semantica */
simbolo tabsimb[NCLASSHASH];
simbolo simb;
listasimbolo listsimb;
int tipocorrente;
short declparam;
simbolo escopo;
listasimbolo pontvardecl;
listasimbolo pontfunc;
listasimbolo pontparam;

/* Prototipos das funcoes para a tabela de simbolos e analise semantica */

void    InicTabSimb(void);

void    InicListSimb(listasimbolo*);
void    InsereListSimb(simbolo, listasimbolo*);
void    AnulaListSimb(listasimbolo*);
//simbolo ProcuraListSimb(char *);
void    AdicTipoVar(listasimbolo);

void    ImprimeTabSimb(void);
simbolo InsereSimb(char *cadeia, int tid, int tvar, simbolo escopo);
int     hash(char *);
simbolo ProcuraSimb(char *, simbolo);

/* Protótipos para analisador semantico */
void    declareVariable(char *);
void    validateVectorSize(int);
void    validateVariableType();
void    VariableReferenced(simbolo s);
void    VariableAssigned(simbolo s);
simbolo UsarVariavel(char *name);
void    VerificaInicRef();
int     CheckNegop(int);
int     CheckFuncCall(char *id);

/* Protótipos de errors */
void    DeclaracaoRepetida(char *s);
void    TamanhoInvalidoDeVetor(int);
void    VariavelDeTipoVoid();
void    TipoInadequado(char *);
void    NaoDeclarado(char *);
void    VariavelNaoReferenciada(simbolo s);
void    OperadorInvalidoAoMenosUnario();

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

/* Declaracao dos tipos retornados pelas producoes */
%type     <simb>      Variable
%type     <valint>    Expression
%type     <valint>    Factor
%type     <cadeia>    FuncCall

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

Prog         : { InicTabSimb(); InicProg(); } GlobDecls FuncList MainDef { FimProg(); ImprimeTabSimb(); }
             ;
GlobDecls    :
             | GLOBAL { printIncreasingTabs("global {\n"); } OPBRACE DeclList CLBRACE { printDecreasingTabs("}\n\n"); }
             ;
DeclList     : Declaration
             | DeclList Declaration
             ;
Declaration  : { printTabs(); InicDeclaration(); } IdList { printf(" : "); } COLON Type SCOLON { printf(";\n"); FimDeclaration(); }
             ;
IdList       : ID              { declareVariable($1); printf("%s", $1); }
             | IdList COMMA ID { declareVariable($3); printf(", %s", $3); }
             ;
Type         : ScalarType
             | ArrayType
             ;
ScalarType   : INT   { printf("int");   tipocorrente = INTEIRO;   }
             | FLOAT { printf("float"); tipocorrente = REAL;      }
             | CHAR  { printf("char");  tipocorrente = CARACTERE; }
             | LOGIC { printf("logic"); tipocorrente = LOGICO;    }
             | VOID  { printf("void");  tipocorrente = NAOVAR;    }
             ;
ArrayType    : ARRAY OPBRAK { printf("array ["); } DimList CLBRAK OF { printf("] of "); } ScalarType
             ;
DimList      : INTCT               { printf("%d", $1); validateVectorSize($1); }
             | DimList COMMA INTCT { printf(", %d", $3); validateVectorSize($3); }
             ;
FuncList     :
             | FuncList FuncDef
             ;
FuncDef      : { /* AnulaListSimb(); */ } FUNCTION ID COLON { printIncreasingTabs("function %s : ", $3); InicFunc($3); } ScalarType { printf("\n"); } Params LocDecls Statmts { printDecreasingTabs("\n"); FimFunc(); }
             ;
MainDef      : { /* AnulaListSimb(); */ } MAIN { printIncreasingTabs("main \n"); InicFunc("##main"); } LocDecls Statmts { printDecreasingTabs("\n"); }
             ;
Params       :
             | PARAMETERS OPBRACE { InicFuncParamDecl(); printIncreasingTabs("parameters {\n"); } ParamList CLBRACE { FimFuncParamDecl(); printDecreasingTabs("}\n"); }
             ;
ParamList    : ParamDecl
             | ParamList ParamDecl
             ;
ParamDecl    : { printTabs(); } IdList COLON { printf(" : "); } ScalarType SCOLON { printf(";\n"); }
             ;
LocDecls     : 
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
ForStat      : FOR { printIncreasingTabs("for "); } Variable ASSIGN { printf(" := "); VariableAssigned($3); } Expression Direcao Expression StepDef DO { printf(" do\n"); } Statement { decreaseTabSize(); }
             ;
Direcao      : TO     { printf(" to "); }
             | DOWNTO { printf(" downto "); }
             ;             
StepDef      : ;
             | STEP { printf(" step "); } Expression
             ;
ReadStat     : READ OPPAR { printWithTabs("read( "); } VarList CLPAR SCOLON { printf(" );\n"); }
             ;
VarList      : Variable { VariableAssigned($1); }
             | VarList COMMA { printf(", "); } Variable { VariableAssigned($4); }
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
FuncCall     : ID OPPAR { printf("%s(", $1); } Arguments CLPAR { printf(")"); strcpy($$, $1); }
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
AssignStat   : { printTabs(); } Variable { VariableAssigned($2); } ASSIGN { printf(" := "); } Expression SCOLON { printf(";\n"); }
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
Factor       : Variable { VariableReferenced($1); if($1 != NULL){ $1->ref  =  VERDADE; $$ = $1->tvar; } }
             | INTCT    { printf("%d", $1);                              $$ = INTEIRO;                  }
             | FLOATCT  { printf("%e", $1);                              $$ = REAL;                     }
             | CHARCT   { printReadableChar($1);                         $$ = CARACTERE;                }
             | TRUE     { printf("true");                                $$ = LOGICO;                   }
             | FALSE    { printf("false");                               $$ = LOGICO;                   }
             | NEGOP    { printf("~"); } Factor {                        $$ = CheckNegop($3);           }
             | OPPAR    { printf("("); } Expression CLPAR { printf(")"); $$ = $3;                       }
             | FuncCall {                                                $$ = CheckFuncCall($1);        }
             ;
Variable     : ID { printf("%s", $1); simb = UsarVariavel($1); $<simb>$ = simb; } Subscripts { $$ = $<simb>2; }
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

// Initialize global variables on program initialization
void InicProg()
{
  declparam = 0;
  escopo = simb = InsereSimb("##global", IDGLOB, NAOVAR, NULL);
  pontvardecl = simb->listvar;
  pontfunc = simb->listfunc;
}

/* To be called after end of program */
void FimProg()
{
  VerificaInicRef();
}

// Initialize a funcion
void InicFunc(char *id)
{
  escopo = simb = InsereSimb(id, IDFUNC, tipocorrente, escopo);
  pontvardecl = simb -> listvar;
  pontparam = simb -> listparam;
}

void FimFunc()
{
  escopo = simb = escopo->escopo;
  AnulaListSimb(&pontvardecl);
  AnulaListSimb(&pontparam);
}

// Start declaring function parameters
void InicFuncParamDecl()
{
  declparam = 1;
  InicDeclaration();
}

// End declaring function parameters
void FimFuncParamDecl()
{
  FimDeclaration();
  declparam = 0;
}

void InicDeclaration()
{
  InicListSimb(&listsimb);
}

void FimDeclaration()
{
  AdicTipoVar(listsimb);
  AnulaListSimb(&listsimb);
}

void declareVariable(char *variable){
  simbolo s;
  s = ProcuraSimb(variable, escopo);
  if (s != NULL && s->escopo == escopo) {
    DeclaracaoRepetida(variable);
  } else {
    simb = InsereSimb(variable, IDVAR, NAOVAR, escopo);
    InsereListSimb(simb, &listsimb);
  }
}

void validateVectorSize(int n){
  if(n <= 0) TamanhoInvalidoDeVetor(n);
}

void validateVariableType(){
  if(tipocorrente == NAOVAR) VariavelDeTipoVoid();
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
void InicListSimb(listasimbolo *listsimb){
  *listsimb = NULL;
}

/* InsereListSimb: Insere um simbolo na lista linear global de simbolos
   simb: simbolo a ser inserido */
void InsereListSimb(simbolo simb, listasimbolo *listsimb){
  elemlistsimb *p;
  p = *listsimb;
  *listsimb = malloc(sizeof(elemlistsimb));
  (*listsimb)->simb = simb;
  (*listsimb)->prox = p;
}

/*  AnulaListSimb: Anula a corrente lista linear de simbolos  */
void AnulaListSimb(listasimbolo *listsimb) {
  elemlistsimb *p;

  while(*listsimb != NULL) {
    p = *listsimb;
    *listsimb = (*listsimb)->prox;
    free(p);
  }
}

//simbolo ProcuraListSimb(char *cadeia){
//  elemlistsimb *p;
//  p = listsimb;
//
//  while(p!=NULL) {
//    if( strcmp( p->simb->cadeia, cadeia) == 0 ) return p->simb;
//    p = p->prox;
//  }
//  return NULL;
//}

/* AdicTipoVar: Coloca o tipo de variavel corrente em todos os simbolos da lista de simbolos */
void AdicTipoVar(listasimbolo listsimb) {
  elemlistsimb *p;
  validateVariableType();
  for(p=listsimb; p!=NULL; p = p->prox) p->simb->tvar = tipocorrente;
}

/*
  ProcuraSimb (cadeia): Procura cadeia na tabela de simbolos;
  Caso ela ali esteja, retorna um ponteiro para sua celula;
  Caso contrario, retorna NULL.
*/
simbolo ProcuraSimb(char *cadeia, simbolo escopo) {
  simbolo s; int i;
  i = hash (cadeia);
  while (escopo != NULL) {
    for(s = tabsimb[i]; s!=NULL; s = s->prox)
    {
      if (
          s->escopo == escopo &&
          strcmp(cadeia, s->cadeia) == 0
         )
        return s;
    }
    escopo = escopo->escopo;
  }
  return NULL;
}

/*
  InsereSimb (cadeia, tid): Insere cadeia na tabela de
  simbolos, com tid como tipo de identificador; Retorna um
  ponteiro para a celula inserida
*/
simbolo InsereSimb(char *cadeia, int tid, int tvar, simbolo escopo) {
  int i;
  simbolo aux, s;

  i = hash(cadeia);
  aux = tabsimb[i];
  s = tabsimb[i] = malloc(sizeof (celsimb));
  s->cadeia = malloc((strlen(cadeia)+1)*sizeof(char));
  strcpy(s->cadeia, cadeia);
  s->tid = tid;
  s->tvar = tvar;
  s->escopo = escopo;
  s->prox = aux;

  if (declparam) {
    // Todo parametro eh considerado referenciado e inicializado
    s->inic = s->ref = s->param = VERDADE;
    if (s->tid == IDVAR) {
      // Insere o parametro na lista de parametros do seu escopo
      InsereListSimb (s, &pontparam);
    }
    s->escopo->nparam++;
  }
  else {
    s->inic = s->ref = s->param = FALSO;
    if (s->tid == IDVAR)
      // Insere o parametro na lista de variaveis globais ou locais a uma funcao
      InsereListSimb (s, &pontvardecl);
  }

  if (tid == IDGLOB || tid == IDFUNC) {
    // Insere noh lider
    s->listvar = (elemlistsimb*) malloc(sizeof(elemlistsimb));
    s->listvar->prox = NULL;
  }

  if (tid == IDGLOB) {
    s->listfunc = (elemlistsimb*) malloc(sizeof(elemlistsimb));
    s->listfunc->prox = NULL;
  }

  if (tid == IDFUNC) {
    // Insere noh lider na lista de parametros
    s->listparam = (elemlistsimb*) malloc(sizeof(elemlistsimb));
    s->listparam->prox = NULL;
    s->nparam = 0;
    InsereListSimb(s, &pontfunc);
  }

  return s;
}

/* Executada ao utilizar uma variavel */
simbolo UsarVariavel(char *name){
  simbolo simb;

  simb = ProcuraSimb(name, escopo);
  if (simb == NULL) {
    NaoDeclarado(name);
  } else if (simb->tid != IDVAR) {
    TipoInadequado(name);
  }

  return simb;
}

/* Executada quando variavel eh referenciada em um fator */
void VariableReferenced(simbolo s) {
  if (s != NULL) {
    s->ref = VERDADE;
  }
}

void VariableAssigned(simbolo s) {
  if (s != NULL) {
    s->ref = VERDADE;
    s->inic = VERDADE;
  }
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

/* Verifica se todas as variaveis foram referenciadas */
void VerificaInicRef()
{
  int i;
  simbolo s;

  for(i = 0; i < NCLASSHASH; i++){
    if(tabsimb[i]) {
      for(s = tabsimb[i]; s!=NULL; s = s->prox){
        if (s->tid == IDVAR && s->ref == FALSO) {
          VariavelNaoReferenciada(s);
        }
      }
    }
  }
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
        if (s->escopo)
          printf(", %s", s->escopo->cadeia);
        printf(")\n");
      }
    }
  }
}

int CheckNegop(int type){
  if(type != INTEIRO && type != REAL && type != CARACTERE) OperadorInvalidoAoMenosUnario();
  if (type == REAL) return REAL;
  return INTEIRO;
}

int CheckFuncCall(char *id){
  simbolo s;
  s = ProcuraSimb(id, escopo);
}

/*  Erros semanticos  */
void DeclaracaoRepetida(char *s){
  addError("/* Declaracao Repetida: %s */\n", s);
}

void TamanhoInvalidoDeVetor(int n){
  addError("/* Tamanho Invalido De Vetor: %d */\n", n);
}

void VariavelDeTipoVoid(){
  addError("/* Variável de tipo void */\n");
}

void NaoDeclarado(char *s){
  addError("/* Identificador Nao Declarado: %s */\n", s);
}

void TipoInadequado(char *s){
  addError("/* Identificador de Tipo Inadequado: %s */\n", s);
}

void VariavelNaoReferenciada(simbolo s) {
  addError("/* Variavel nao referenciada: %s */\n", s->cadeia);
}

void OperadorInvalidoAoMenosUnario(){
  addError("/* Operando improprio para menos unario */\n");
}