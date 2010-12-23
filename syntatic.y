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
#define   CADEIA     5

/* Definicao de constantes para os operadores de quadruplas */

#define   OPOR       1
#define   OPAND      2
#define   OPLT       3
#define   OPLE       4
#define   OPGT       5
#define   OPGE       6
#define   OPEQ       7
#define   OPNE       8
#define   OPMAIS     9
#define   OPMENOS   10
#define   OPMULT    11
#define   OPDIV     12
#define   OPRESTO   13
#define   OPMENUN   14
#define   OPNOT     15
#define   OPATRIB   16
#define   OPENMOD   17
#define   NOP       18
#define   OPJUMP    19
#define   OPJF      20
#define   PARAM     21
#define   OPREAD    22
#define   OPWRITE   23
#define   OPCALL    24
#define   OPRETURN  25
#define   OPIND     26
#define   OPINDEX   27
#define   OPCONTAP  28
#define   OPATRIBP  29
#define   OPEXIT    30

/* Definicao de constantes para os tipos de operandos de quadruplas */

#define   IDLEOPND   0
#define   VAROPND    1
#define   INTOPND    2
#define   REALOPND   3
#define   CHAROPND   4
#define   LOGICOPND  5
#define   CADOPND    6
#define   ROTOPND    7
#define   FUNCOPND   8
#define   INVALOPND  9


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
char *nometipvar[] = { "NAOVAR", "INTEIRO", "LOGICO", "REAL", "CARACTERE", "CADEIA" };

/* Strings para operadores de quadruplas */

char *nomeoperquad[] = {"",
  "OR", "AND", "LT", "LE", "GT", "GE", "EQ", "NE", "MAIS",
  "MENOS", "MULT", "DIV", "RESTO", "MENUN", "NOT", "ATRIB",
  "OPENMOD", "NOP", "JUMP", "JF", "PARAM", "READ", "WRITE",
  "CALL", "RETURN", "IND", "INDEX", "CONTAPONT", "ATRIBPONT",
  "OPEXIT"
};

/* Strings para tipos de operandos de quadruplas */

char *nometipoopndquad[] = {"IDLE",
  "VAR", "INT", "REAL", "CARAC", "LOGIC", "CADEIA", "ROTULO", "FUNCAO", "INVAL"
};


/* Declaracoes para a tabela de simbolos */
typedef struct celsimb celsimb;
typedef celsimb *simbolo;
typedef struct elemlistsimb elemlistsimb;
typedef elemlistsimb *listasimbolo;

typedef union atribopnd atribopnd;
typedef struct operando operando;
typedef struct celquad celquad;
typedef celquad *quadrupla;
typedef struct celfunchead celfunchead;
typedef celfunchead *funchead;

struct celsimb {
  char *cadeia;
  int  tid, tvar, tparam, ndims, dims[MAXDIMS+1], nparam;
  char inic, ref, array, param;
  listasimbolo listvar, listparam, listfunc;
  simbolo escopo, prox;
  celfunchead *fhead;
  int   *valint;
  float *valfloat;
  char  *valchar, *vallogic;
};

/* Declarações para lista linear de simbolos */
struct elemlistsimb {
  simbolo simb;
  elemlistsimb *prox;
};

/* Tipo para tratamento de lista de argumentos para funcoes */
typedef struct elemlisttipo elemlisttipo;
typedef elemlisttipo *listtipo;
struct elemlisttipo {
  int tid;
  elemlisttipo *prox;
};

typedef struct infolistexpr infolistexpr;
struct infolistexpr {
  elemlisttipo *elem;
  int nargs;
};

void CheckArgumentos(infolistexpr largumentos, simbolo lparam);
void ConcatListTipo(listtipo l1, listtipo l2);
listtipo InicListTipo(int tid);
infolistexpr EmptyInfoList();
infolistexpr InicListExpr(int tid);
infolistexpr ConcatListExpr(infolistexpr l1, infolistexpr l2);

/* Declaracoes para a estrutura do codigo intermediario */

union atribopnd {
  simbolo simb; int valint; float valfloat;
  char valchar; char vallogic; char *valcad;
  quadrupla rotulo; funchead func;
};

struct operando {
  int tipo; atribopnd atr;
};

struct celquad {
  int num, oper;
  operando opnd1, opnd2, result;
  quadrupla prox;
};

struct celfunchead {
  simbolo funcname; funchead prox;
  quadrupla listquad;
};

typedef struct infoexpressao infoexpressao;
struct infoexpressao { int tipo;  operando opnd; };

typedef struct infovariavel infovariavel;
struct infovariavel {
  simbolo simb;
  operando opnd;
  infoexpressao pointer;
};

/* Variaveis globais para a tabela de simbolos e analise semantica */
simbolo tabsimb[NCLASSHASH];
simbolo simb;
listasimbolo listsimb;
int tipocorrente;
int arraycorrente, arraydim, arraydimcorrente[10];
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
void            declareVariable(char *);
void            validateVectorSize(int);
void            validateVariableType();
void            VariableReferenced(simbolo s);
void            VariableAssigned(simbolo s);
simbolo         UsarVariavel(char *name, int tid);
void            VerificaInicRef();
int             CheckNegop(infoexpressao);
infoexpressao   FuncFactor(simbolo simb);
infoexpressao   CheckMult(infoexpressao, int, infoexpressao);
infoexpressao   CheckAdop(infoexpressao, int, infoexpressao);
infoexpressao   CheckRelop(infoexpressao, int, infoexpressao);
infoexpressao   CheckLogop(infoexpressao, int, infoexpressao);
infoexpressao   CheckNotop(infoexpressao);
void            CheckAssign(simbolo, int);
void            CheckLogic(int);
infovariavel    CheckVariable(simbolo, int);
quadrupla       IfInic(infoexpressao);

/* Protótipos de errors */
void    DeclaracaoRepetida(char *s);
void    TamanhoInvalidoDeVetor(int);
void    VariavelDeTipoVoid();
void    TipoInadequado(char *);
void    NaoDeclarado(char *);
void    VariavelNaoReferenciada(simbolo s);
void    VariavelNaoInicializada(simbolo s);
void    OperadorInvalidoAoMenosUnario();
void    OperandoNaoAritmetico();
void    OperandoInvalidoAoResto();
void    OperandoNaoComparavel();
void    OperandosImproprioAosOperadoresLogicos();
void    OperandoNaoNegavel();
void    AtribuicaoInvalida();
void    ExpressaoDeveriaSerLogica();
void    NumeroDeArgumentosIncorreto(int expected, int actual);
void    TipoSubscritoInvalido();
void    SubscritoNaoEsperado();
void    SubscritoEsperado();
void    NumeroDeSubscritoIncompativel();

/* Variaveis globais para o codigo intermediario */
quadrupla quadcorrente, quadaux;
funchead codintermed, funccorrente;
int oper, numquadcorrente;
operando opnd1, opnd2, result, opndaux;
int numtemp;
const operando opndidle = {IDLEOPND, 0};

/* Prototipos das funcoes para o codigo intermediario */
void      InicCodIntermed (void);
void      InicCodIntermFunc (simbolo);
void      ImprimeQuadruplas (void);
quadrupla GeraQuadrupla (int, operando, operando, operando);
quadrupla GeraQuadruplaIdle ();
quadrupla GeraQuadruplaJump(quadrupla destino);
quadrupla GeraQuadruplaRead(int params);
quadrupla GeraQuadruplaWrite(int params);
quadrupla GeraQuadruplaParam(operando opnd);
simbolo   NovaTemp (int);
void      RenumQuadruplas (quadrupla, quadrupla);
void      AssignVariable(infoexpressao, infovariavel);
int       GeraQuadruplaParamRead(infovariavel, int);

infoexpressao VariableFactor  (infovariavel infovar);
infoexpressao IntFactor  (int value);
infoexpressao FloatFactor(float value);
infoexpressao CharFactor (char value);
infoexpressao CadeiaFactor (char *value);
infoexpressao BoolFactor (int value);
infoexpressao NegOpFactor(int tid, operando opnd);
infoexpressao FactorType (infoexpressao expression);

// Estruturas para Interpretador

typedef struct nohopnd nohopnd;
struct nohopnd{
  operando opnd;
  nohopnd *prox;
};
typedef nohopnd *pilhaoperando;
pilhaoperando pilhaopnd, pilhaopndaux, pilhaind;
FILE *finput;

// Protótipos para helpers de Interpretador

void EmpilharOpnd(operando, pilhaoperando *);
void DesempilharOpnd(pilhaoperando *);
operando TopoOpnd(pilhaoperando);
void InicPilhaOpnd(pilhaoperando *);
char VaziaOpnd(pilhaoperando);

// Protótipos para Interpretador

void InterpCodIntermed();
void ExecQuadWrite(quadrupla);
void ExecQuadMais(quadrupla);
void ExecQuadMenos(quadrupla);
void ExecQuadMult(quadrupla);
void ExecQuadDiv(quadrupla);
void ExecQuadResto(quadrupla);
void ExecQuadAtrib(quadrupla);
void ExecQuadLT(quadrupla);
void ExecQuadRead(quadrupla);

%}

/* Ativa erros com mais detalhes do yacc */
%error-verbose

/* Definicao do tipo de yylval */
%union {
  char      cadeia[100];
  int       atr, valint;
  float     valreal;
  char      carac;
  simbolo   simb;
  int       nsubscr, nargs, direcao;
  quadrupla quad;
  infoexpressao infoexpr;
  infovariavel infovar;
  infolistexpr infolexpr;
}

/* Declaracao dos tipos retornados pelas producoes */
%type     <infoexpr>  AuxExpr1
%type     <infoexpr>  AuxExpr2
%type     <infoexpr>  AuxExpr3
%type     <infoexpr>  AuxExpr4
%type     <infovar>   Variable
%type     <infoexpr>  Expression WriteElem Factor StepDef FuncCall
%type     <infoexpr>  Term
%type     <infolexpr> ExprList Arguments
%type     <nsubscr>   Subscripts SubscrList
%type     <direcao>   Direcao
%type     <nargs>     VarList WriteList

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

Prog         : { InicTabSimb(); InicCodIntermed(); InicProg(); } GlobDecls FuncList MainDef { FimProg(); ImprimeTabSimb(); GeraQuadrupla(OPEXIT, opndidle, opndidle, opndidle); ImprimeQuadruplas(); InterpCodIntermed(); }
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
Type         : { arraycorrente = FALSO; } ScalarType
             | { arraycorrente = VERDADE; arraydim = 0; } ArrayType {}
             ;
ScalarType   : INT   { printf("int");   tipocorrente = INTEIRO;   }
             | FLOAT { printf("float"); tipocorrente = REAL;      }
             | CHAR  { printf("char");  tipocorrente = CARACTERE; }
             | LOGIC { printf("logic"); tipocorrente = LOGICO;    }
             | VOID  { printf("void");  tipocorrente = NAOVAR;    }
             ;
ArrayType    : ARRAY OPBRAK { printf("array ["); } DimList CLBRAK OF { printf("] of "); } ScalarType
             ;
DimList      : INTCT               { printf("%d", $1);   validateVectorSize($1); }
             | DimList COMMA INTCT { printf(", %d", $3); validateVectorSize($3); }
             ;
FuncList     :
             | FuncList FuncDef
             ;
FuncDef      : { /* AnulaListSimb(); */ } FUNCTION ID COLON { printIncreasingTabs("function %s : ", $3); } ScalarType { printf("\n"); InicFunc($3); } Params LocDecls Statmts { printDecreasingTabs("\n"); FimFunc(); }
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
IfStat       : IF { printWithTabs("if "); } Expression THEN
                { printf(" then\n"); CheckLogic($3.tipo); $<quad>$ = IfInic($3); increaseTabSize(); }
                Statement
                { 
                  decreaseTabSize();
                  $<quad>$ = quadcorrente;
                  $<quad>5->result.atr.rotulo = GeraQuadruplaIdle();
                }
                ElseStat
                {
                  if($<quad>7->prox != quadcorrente){
                    quadaux              = $<quad>7->prox;
                    $<quad>7->prox       = quadaux->prox;
                    quadaux->prox        = $<quad>7->prox->prox;
                    $<quad>7->prox->prox = quadaux;
                    RenumQuadruplas($<quad>7, quadcorrente);
                  }
                }
             ;
ElseStat     :
             | ELSE
               {
                 printIncreasingTabs("else\n");
                 opndaux.tipo = ROTOPND;
                 $<quad>$ = GeraQuadrupla(OPJUMP, opndidle, opndidle, opndaux);
               }
               Statement
               {
                 decreaseTabSize();
                 $<quad>2->result.atr.rotulo = GeraQuadruplaIdle();
               }
             ;
WhileStat    : WHILE
                { printIncreasingTabs("while "); $<quad>$ = GeraQuadruplaIdle();}
                Expression DO 
                { printf(" do\n"); CheckLogic($3.tipo); $<quad>$ = IfInic($3);}
                Statement
                { decreaseTabSize(); GeraQuadruplaJump($<quad>2); $<quad>5->result.atr.rotulo = GeraQuadruplaIdle(); }
             ;
RepeatStat   : REPEAT { printIncreasingTabs("repeat "); } Statement UNTIL { printDecreasingTabs("until "); } Expression SCOLON { printf(";"); }
             ;
ForStat      : FOR { printIncreasingTabs("for "); }
               Variable { VariableAssigned($3.simb); } ASSIGN { printf(" := "); VariableAssigned($3.simb); } Expression
               Direcao Expression StepDef DO
               {
                 printf(" do\n");
                 CheckAssign($3.simb, $7.tipo);
                 AssignVariable($7, $3);
                 // GeraQuadrupla(OPATRIB, $3.opnd, opndidle, $7.opnd);
               }{
                 $<quad>$ = GeraQuadruplaIdle();
               }{
                 if($8 == 1){
                   $<quad>$ = IfInic( CheckRelop(VariableFactor($3), LE, $9) );
                 }else{
                   $<quad>$ = IfInic( CheckRelop(VariableFactor($3), GE, $9) );
                 }
               }
               Statement
               {
                 decreaseTabSize();
                 if($8 == 1){
                   GeraQuadrupla(OPMAIS, $3.opnd, $10.opnd, $3.opnd);
                 }else{
                   GeraQuadrupla(OPMENOS, $3.opnd, $10.opnd, $3.opnd);
                 }
                 GeraQuadruplaJump($<quad>13);
                 $<quad>14->result.atr.rotulo = GeraQuadruplaIdle();
               }
             ;
Direcao      : TO     { printf(" to ");     $$ = 1; }
             | DOWNTO { printf(" downto "); $$ = -1; }
             ;
StepDef      : { $$ = IntFactor(1); }
             | STEP { printf(" step "); } Expression { $$ = $3; }
             ;
ReadStat     : READ OPPAR { printWithTabs("read( "); } VarList CLPAR SCOLON { printf(" );\n"); GeraQuadruplaRead($4); }
             ;
VarList      : Variable { VariableAssigned($1.simb); $$ = GeraQuadruplaParamRead($1, 0);/*GeraQuadruplaParam($1.opnd); $$ = 1;*/ }
             | VarList COMMA { printf(", "); } Variable { VariableAssigned($4.simb); $$ = GeraQuadruplaParamRead($4, $1); /*GeraQuadruplaParam($4.opnd); $$ = $1 + 1;*/ }
             ;
WriteStat    : WRITE OPPAR { printWithTabs("write( "); } WriteList CLPAR SCOLON { printf(" );\n"); GeraQuadruplaWrite($4); }
             ;
WriteList    : WriteElem { GeraQuadruplaParam($1.opnd); $$ = 1; }
             | WriteList COMMA { printf(", "); } WriteElem { GeraQuadruplaParam($4.opnd); $$ = $1 + 1; }
             ;
WriteElem    : STRING { printf("%s", $1); $$ = CadeiaFactor($1); }
             | Expression
             ;
CallStat     : CALL { printWithTabs("call "); } FuncCall SCOLON { printf(";\n"); }
             ;
FuncCall     : ID OPPAR { $<simb>$ = UsarVariavel($1, IDFUNC); printf("%s(", $1); }
               Arguments CLPAR { CheckArgumentos($4, $<simb>3); printf(")"); $$ = FuncFactor($<simb>3); }
             ;
Arguments    : { $$ = EmptyInfoList(); }
             | ExprList
             ;
ExprList     : Expression { $$ = InicListExpr($1.tipo); GeraQuadruplaParam($1.opnd); }
             | ExprList COMMA { printf(", "); } Expression { $$ = ConcatListExpr($1, InicListExpr($4.tipo)); GeraQuadruplaParam($4.opnd); }
             ;
ReturnStat   : RETURN SCOLON { printWithTabs("return ;\n"); GeraQuadrupla(OPRETURN, opndidle, opndidle, opndidle);}
             | RETURN { printWithTabs("return "); } Expression SCOLON { printf(";\n"); GeraQuadrupla(OPRETURN, $3.opnd, opndidle, opndidle); }
             ;
AssignStat   : { printTabs(); } Variable { VariableAssigned($2.simb); } ASSIGN { printf(" := "); } Expression SCOLON { printf(";\n"); CheckAssign($2.simb, $6.tipo); AssignVariable($6, $2); /*GeraQuadrupla(OPATRIB, $2.opnd, opndidle, $6.opnd);*/ }
             ;
Expression   : AuxExpr1 { $$ = $1; }
             | Expression OROP { printf(" || "); } AuxExpr1 { $$ = CheckLogop($1, $2, $4); }
             ;
AuxExpr1     : AuxExpr2 { $$ = $1; }
             | AuxExpr1 ANDOP { printf(" && "); } AuxExpr2 { $$ = CheckLogop($1, $2, $4); }
             ;
AuxExpr2     : AuxExpr3 { $$ = $1; }
             | NOTOP { printf("!"); } AuxExpr3 { $$ = CheckNotop($3); }
             ;
AuxExpr3     : AuxExpr4 { $$ = $1; }
             | AuxExpr4 RELOP { printf(" %s ", translateOperator($2)); } AuxExpr4 { $$ = CheckRelop($1, $2, $4); }
             ;
AuxExpr4     : Term { $$ = $1;}
             | AuxExpr4 ADOP { printf("%s", translateOperator($2)); } Term { $$ = CheckAdop($1, $2, $4); }
             ;
Term         : Factor { $$ = $1; }
             | Term MULTOP { printf("%s", translateOperator($2)); } Factor { $$ = CheckMult($1, $2, $4); }
             ;
Factor       : Variable { VariableReferenced($1.simb);                   $$ = VariableFactor($1);                         }
             | INTCT    { printf("%d", $1);                              $$ = IntFactor  ($1);                            }
             | FLOATCT  { printf("%e", $1);                              $$ = FloatFactor($1);                            }
             | CHARCT   { printReadableChar($1);                         $$ = CharFactor ($1);                            }
             | TRUE     { printf("true");                                $$ = BoolFactor (VERDADE);                       }
             | FALSE    { printf("false");                               $$ = BoolFactor (FALSO);                         }
             | NEGOP    { printf("~"); } Factor {                        $$ = NegOpFactor(CheckNegop($3), $3.opnd);       }
             | OPPAR    { printf("("); } Expression CLPAR { printf(")"); $$ = FactorType ($3);                            }
             | FuncCall
             ;
Variable     : ID { printf("%s", $1); simb = UsarVariavel($1, IDVAR); $<simb>$ = simb; } Subscripts { $$ = CheckVariable($<simb>2, $3); }
             ;
Subscripts   : {$$ = 0;}
             | OPBRAK { printf("["); } SubscrList CLBRAK { printf("]"); $$ = $3; }
             ;
SubscrList   : AuxExpr4 { if($1.tipo != INTEIRO && $1.tipo != CARACTERE) TipoSubscritoInvalido(); $$ = 1; GeraQuadrupla(OPIND, $1.opnd, opndidle, opndidle); }
             | SubscrList COMMA { printf(", "); } AuxExpr4 { if($4.tipo != INTEIRO && $4.tipo != CARACTERE) TipoSubscritoInvalido(); $$ = $1 + 1; GeraQuadrupla(OPIND, $4.opnd, opndidle, opndidle); }
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

  numtemp = 0;
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

  InicCodIntermFunc(simb);
  operando opnd;
  opnd.tipo = FUNCOPND;
  opnd.atr.func = funccorrente;
  GeraQuadrupla(OPENMOD, opnd, opndidle, opndidle);
}

void FimFunc()
{
  if (quadcorrente->oper != OPRETURN) {
    GeraQuadrupla(OPRETURN, opndidle, opndidle, opndidle);
  }
  escopo = simb = escopo->escopo;
  //AnulaListSimb(&pontvardecl);
  //AnulaListSimb(&pontparam);
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
  arraydim++;
  arraydimcorrente[arraydim] = n;
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
  elemlistsimb *p = malloc(sizeof(elemlistsimb));

  p->prox = NULL;
  p->simb = simb;

  if (*listsimb == NULL) {
    *listsimb = p;
    return;
  }

  listasimbolo lsimb = *listsimb;

  while (lsimb->prox != NULL) lsimb = lsimb->prox;
  lsimb->prox = p;
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

/* Funcoes para manipulacao de lista de tipos */
void CheckArgumentos(infolistexpr infoargumentos, simbolo function)
{
  listasimbolo lparam;
  listtipo largumentos;

  if (function == NULL) return;
  if (infoargumentos.nargs != function->nparam) {
    NumeroDeArgumentosIncorreto(function->nparam, infoargumentos.nargs);
    return;
  }
  lparam = function->listparam->prox;
  largumentos = infoargumentos.elem;
  while (largumentos != NULL) {
    CheckAssign(lparam->simb, largumentos->tid);
    largumentos = largumentos->prox;
    lparam = lparam->prox;
  }

}

void ConcatListTipo(listtipo l1, listtipo l2)
{
  while (l1->prox) {
    l1 = l1->prox;
  }
  l1->prox = l2;
}

listtipo InicListTipo(int tid)
{
  listtipo l = malloc(sizeof(elemlisttipo));
  l->tid = tid;
  l->prox = NULL;
  return l;
}

infolistexpr InicListExpr(int tid)
{
  infolistexpr list;
  list.elem = InicListTipo(tid);
  list.nargs = 1;
  return list;
}

infolistexpr ConcatListExpr(infolistexpr l1, infolistexpr l2)
{
  infolistexpr l;
  l.nargs = l1.nargs + l2.nargs;
  ConcatListTipo(l1.elem, l2.elem);
  l.elem = l1.elem;

  return l;
}

/* AdicTipoVar: Coloca o tipo de variavel corrente em todos os simbolos da lista de simbolos */
void AdicTipoVar(listasimbolo listsimb){
  int i;
  elemlistsimb *p;
  validateVariableType();
  for(p=listsimb; p!=NULL; p = p->prox){
    p->simb->tvar = tipocorrente;
    if(arraycorrente){
      p->simb->array = VERDADE;
      p->simb->ndims = arraydim;
      for(i = 1; i <= arraydim; i++) p->simb->dims[i] = arraydimcorrente[i];
    }
  }
}

infolistexpr EmptyInfoList() {
  infolistexpr infolist;
  infolist.elem = NULL;
  infolist.nargs = 0;
  return infolist;
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
    s->listparam->simb = (void*) -1;
    s->nparam = 0;
    InsereListSimb(s, &pontfunc);
  }

  return s;
}

/* Executada ao utilizar uma variavel */
simbolo UsarVariavel(char *name, int tid){
  simbolo simb;

  simb = ProcuraSimb(name, escopo);
  if (simb == NULL) {
    NaoDeclarado(name);
  } else if (simb->tid != tid) {
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
        if (s->tid == IDVAR && s->inic == FALSO) {
          VariavelNaoInicializada(s);
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

int CheckNegop(infoexpressao elem){
  if(elem.tipo != INTEIRO && elem.tipo != REAL && elem.tipo != CARACTERE) OperadorInvalidoAoMenosUnario();
  if(elem.tipo == REAL) return REAL;
  return INTEIRO;
}

infoexpressao FuncFactor(simbolo simb){
  infoexpressao infoexpr;
  operando opnd1, opnd2, result;

  opnd1.tipo = FUNCOPND;
  opnd1.atr.func = simb->fhead;

  opnd2.tipo = INTOPND;
  opnd2.atr.valint = simb->nparam;

  result.tipo = VAROPND;
  result.atr.simb = NovaTemp(simb->tvar);

  GeraQuadrupla(OPCALL, opnd1, opnd2, result);

  infoexpr.tipo = simb->tvar;
  infoexpr.opnd = result;

  return infoexpr;
}

infoexpressao CheckMult(infoexpressao term, int op, infoexpressao factor){
  infoexpressao res;
  res.opnd.tipo = INVALOPND;

  switch(op) {
    case MULT:
    case DIV:
      if( (term.tipo != INTEIRO && term.tipo != REAL && term.tipo != CARACTERE) || (factor.tipo != INTEIRO && factor.tipo != REAL && factor.tipo != CARACTERE) )
        OperandoNaoAritmetico();

      if(term.tipo == REAL || factor.tipo == REAL) res.tipo = REAL;
      else res.tipo = INTEIRO;

      res.opnd.tipo = VAROPND;
      res.opnd.atr.simb = NovaTemp(res.tipo);

      switch (op) {
        case MULT:
          GeraQuadrupla(OPMULT, term.opnd, factor.opnd, res.opnd);
          break;
        case DIV:
          GeraQuadrupla(OPDIV, term.opnd, factor.opnd, res.opnd);
          break;
      }
      break;

    case RESTO:
      if(term.tipo != INTEIRO && term.tipo != CARACTERE || factor.tipo != INTEIRO && factor.tipo != CARACTERE)
        OperandoInvalidoAoResto();
      res.tipo = INTEIRO;
      res.opnd.tipo = VAROPND;
      res.opnd.atr.simb = NovaTemp(res.tipo);
      GeraQuadrupla(OPRESTO, term.opnd, factor.opnd, res.opnd);
      break;

  }
  return res;
}

infoexpressao CheckAdop(infoexpressao term, int op, infoexpressao factor){
  infoexpressao res;

  if( (term.tipo != INTEIRO && term.tipo != REAL && term.tipo != CARACTERE) || (factor.tipo != INTEIRO && factor.tipo != REAL && factor.tipo != CARACTERE) )
    OperandoNaoAritmetico();

  if(term.tipo == REAL || factor.tipo == REAL) res.tipo = REAL;
  else res.tipo = INTEIRO;

  res.opnd.tipo = VAROPND;
  res.opnd.atr.simb = NovaTemp(res.tipo);

  switch (op) {
    case MAIS:
      GeraQuadrupla(OPMAIS, term.opnd, factor.opnd, res.opnd);
      break;
    case MENOS:
      GeraQuadrupla(OPMENOS, term.opnd, factor.opnd, res.opnd);
      break;
  }

  return res;
}

infoexpressao CheckRelop(infoexpressao expr1, int op, infoexpressao expr2){
  infoexpressao res;

  res.tipo          = LOGICO;
  res.opnd.tipo     = VAROPND;
  res.opnd.atr.simb = NovaTemp(res.tipo);

  if( (op == EQ || op == NE) && expr1.tipo == LOGICO && expr2.tipo == LOGICO ){
    switch(op) {
      case EQ:
        GeraQuadrupla(OPEQ, expr1.opnd, expr2.opnd, res.opnd);
        break;
      case NE:
        GeraQuadrupla(OPNE, expr1.opnd, expr2.opnd, res.opnd);
        break;
    }
    return res;
  }

  if( (expr1.tipo != INTEIRO && expr1.tipo != REAL && expr1.tipo != CARACTERE) || (expr2.tipo != INTEIRO && expr2.tipo != REAL && expr2.tipo != CARACTERE) ){
    OperandoNaoComparavel();
    printf("/* expr1 = %d expr2 = %d */", expr1.tipo, expr2.tipo);
  }

  switch(op) {
    case EQ:
      GeraQuadrupla(OPEQ, expr1.opnd, expr2.opnd, res.opnd);
      break;
    case NE:
      GeraQuadrupla(OPNE, expr1.opnd, expr2.opnd, res.opnd);
      break;
    case GE:
      GeraQuadrupla(OPGE, expr1.opnd, expr2.opnd, res.opnd);
      break;
    case GT:
      GeraQuadrupla(OPGT, expr1.opnd, expr2.opnd, res.opnd);
      break;
    case LE:
      GeraQuadrupla(OPLE, expr1.opnd, expr2.opnd, res.opnd);
      break;
    case LT:
      GeraQuadrupla(OPLT, expr1.opnd, expr2.opnd, res.opnd);
      break;
  }

  return res;
}

infoexpressao CheckLogop(infoexpressao expr1, int op, infoexpressao expr2){
  infoexpressao res;

  if(expr1.tipo != LOGICO || expr2.tipo != LOGICO) OperandosImproprioAosOperadoresLogicos();

  res.tipo          = LOGICO;
  res.opnd.tipo     = VAROPND;
  res.opnd.atr.simb = NovaTemp(res.tipo);

  switch(op) {
    case AND:
      GeraQuadrupla(OPAND, expr1.opnd, expr2.opnd, res.opnd);
      break;
    case OR:
      GeraQuadrupla(OPOR, expr1.opnd, expr2.opnd, res.opnd);
      break;
  }

  return res;
}

infoexpressao CheckNotop(infoexpressao expr){
  infoexpressao res;

  if(expr.tipo != LOGICO) OperandoNaoNegavel();

  res.tipo          = LOGICO;
  res.opnd.tipo     = VAROPND;
  res.opnd.atr.simb = NovaTemp(res.tipo);
  GeraQuadrupla(OPNOT, expr.opnd, opndidle, res.opnd);

  return res;
}

void CheckAssign(simbolo variable, int expr_type){
  if (variable == NULL) return;
  switch(variable->tvar){
    case INTEIRO:
    case CARACTERE:
      if( expr_type != INTEIRO && expr_type != CARACTERE )
        AtribuicaoInvalida();
      break;
    case REAL:
      if( expr_type != REAL && expr_type != INTEIRO && expr_type != CARACTERE )
        AtribuicaoInvalida();
      break;
    case LOGICO:
      if( expr_type != LOGICO )
        AtribuicaoInvalida();
  }
}

void CheckLogic(int type){
  if(type != LOGICO) ExpressaoDeveriaSerLogica();
}

infovariavel CheckVariable(simbolo simb, int index){
  infovariavel infoexpr;

  infoexpr.simb = simb;
  infoexpr.opnd.tipo = INVALOPND;

  if (simb == NULL) {
    return infoexpr;
  }

  if(simb->array == FALSO){
    if(index != 0) SubscritoNaoEsperado();
    infoexpr.opnd.tipo = VAROPND;
    infoexpr.opnd.atr.simb = simb;
  }else{
    if(index == 0){
      SubscritoEsperado();
    }else{
      if(simb->ndims != index){
        NumeroDeSubscritoIncompativel();
      }else{
        infoexpr.opnd.tipo = VAROPND;
        infoexpr.opnd.atr.simb = simb;
        infoexpr.pointer.tipo          = simb->tvar;
        infoexpr.pointer.opnd.tipo     = VAROPND;
        infoexpr.pointer.opnd.atr.simb = NovaTemp(infoexpr.pointer.tipo);
        GeraQuadrupla(OPINDEX, infoexpr.opnd, IntFactor(index).opnd, infoexpr.pointer.opnd);
      }
    }
  }

  return infoexpr;
}

int GeraQuadruplaParamRead(infovariavel infovar, int stack_size){
  infoexpressao aux;

  if(infovar.simb->array == FALSO){
    GeraQuadruplaParam(infovar.opnd);
    return stack_size + 1;
  }else{
    GeraQuadruplaRead(stack_size);
    aux.tipo = infovar.simb->tvar;
    aux.opnd.tipo = VAROPND;
    aux.opnd.atr.simb = NovaTemp(infovar.pointer.tipo);
    GeraQuadruplaParam(aux.opnd);
    GeraQuadruplaRead(1);
    GeraQuadrupla(OPATRIBP, aux.opnd, opndidle, infovar.pointer.opnd);
    return 0;
  }
}

void AssignVariable(infoexpressao assigner, infovariavel assignee){
  if(assignee.simb->array == FALSO){
    GeraQuadrupla(OPATRIB, assigner.opnd, opndidle, assignee.opnd);
  }else{
    GeraQuadrupla(OPATRIBP, assigner.opnd, opndidle, assignee.pointer.opnd);
  }
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

void VariavelNaoInicializada(simbolo s) {
  addError("/* Variavel nao inicializada: %s */\n", s->cadeia);
}

void VariavelNaoReferenciada(simbolo s) {
  addError("/* Variavel nao referenciada: %s */\n", s->cadeia);
}

void OperadorInvalidoAoMenosUnario(){
  addError("/* Operando improprio para menos unario */\n");
}

void OperandoNaoAritmetico(){
  addError("/* Operando improprio para operador aritmetico */\n");
}

void OperandoInvalidoAoResto(){
  addError("/* Operando improprio para operador resto */\n");
}

void OperandoNaoComparavel(){
  addError("/* Operandos incomparáveis */\n");
}

void OperandosImproprioAosOperadoresLogicos(){
  addError("/* Operandos improprios para operadores logicos */\n");
}

void OperandoNaoNegavel(){
  addError("/* Operando não aceita negação */\n");
}

void AtribuicaoInvalida(){
  addError("/* Atribuição invalida */\n");
}

void ExpressaoDeveriaSerLogica(){
  addError("/* Expressao para IF ou WHILE deve ser lógica */\n");
}

void NumeroDeArgumentosIncorreto(int expected, int actual)
{
  addError("/* Numero de parametros incorreto. Esperado: %d, Recebido: %d */\n", expected, actual);
}

void TipoSubscritoInvalido(){
  addError("/* Tipo de subscrito invalido, deve ser inteiro ou caractere */\n");
}

void SubscritoNaoEsperado(){
  addError("/* Subcrito não esperado para variavel onde foi aplicado */\n");
}

void SubscritoEsperado(){
  addError("/* Um subcrito era esperado para variavel onde foi aplicado */\n");
}

void NumeroDeSubscritoIncompativel(){
  addError("/* Foram aplicados mais sobrescritos do que o devido */\n");
}

/* Funcoes para o codigo intermediario */
void InicCodIntermed () {
  funccorrente = codintermed = malloc(sizeof (celfunchead));
  funccorrente->prox = NULL;
}

void InicCodIntermFunc (simbolo simb) {
  funccorrente->prox     = malloc(sizeof (celfunchead));
  funccorrente           = funccorrente->prox;
  funccorrente->prox     = NULL;
  funccorrente->funcname = simb;
  funccorrente->listquad = malloc(sizeof (celquad));
  simb->fhead            = funccorrente;
  quadcorrente           = funccorrente->listquad;
  quadcorrente->prox     = NULL;
  numquadcorrente        = 0;
  quadcorrente->num      = numquadcorrente;
}

quadrupla GeraQuadrupla (int oper, operando opnd1, operando opnd2, operando result) {
  quadcorrente->prox   = malloc(sizeof (celquad));
  quadcorrente         = quadcorrente->prox;
  quadcorrente->oper   = oper;
  quadcorrente->opnd1  = opnd1;
  quadcorrente->opnd2  = opnd2;
  quadcorrente->result = result;
  quadcorrente->prox   = NULL;
  numquadcorrente++;
  quadcorrente->num    = numquadcorrente;
  return quadcorrente;
}

quadrupla GeraQuadruplaIdle () {
  return GeraQuadrupla(NOP, opndidle, opndidle, opndidle);
}

simbolo NovaTemp (int tip) {
  simbolo simb;
  int temp, i, j;
  char nometemp[10] = "##", s[10] = {0};

  numtemp ++; temp = numtemp;
  for (i = 0; temp > 0; temp /= 10, i++)
    s[i] = temp % 10 + '0';

  i --;
  for (j = 0; j <= i; j++)
    nometemp[2+i-j] = s[j];

  simb = InsereSimb (nometemp, IDVAR, tip, escopo);
   //simb->tvar = tip;
  simb->inic = simb->ref = VERDADE;
  return simb;
}

void ImprimeQuadruplas () {
  funchead p;
  quadrupla q;
  for (p = codintermed->prox; p != NULL; p = p->prox) {
    printf("\n\nQuadruplas da funcao %s:\n", p->funcname->cadeia);
    for (q = p->listquad->prox; q != NULL; q = q->prox) {
      printf("\n\t%4d) %s", q->num, nomeoperquad[q->oper]);
      printf(", (%s", nometipoopndquad[q->opnd1.tipo]);
      switch (q->opnd1.tipo) {
        case IDLEOPND: break;
        case VAROPND:   printf(", %s", q->opnd1.atr.simb->cadeia); break;
        case INTOPND:   printf(", %d", q->opnd1.atr.valint); break;
        case REALOPND:  printf(", %g", q->opnd1.atr.valfloat); break;
        case CHAROPND:  printf(", %c", q->opnd1.atr.valchar); break;
        case LOGICOPND: printf(", %d", q->opnd1.atr.vallogic); break;
        case CADOPND:   printf(", %s", q->opnd1.atr.valcad); break;
        case ROTOPND:   printf(", %d", q->opnd1.atr.rotulo->num); break;
        case FUNCOPND:  printf(", %s", q->opnd1.atr.func->funcname->cadeia);
          break;
      }
      printf(")");
      printf(", (%s", nometipoopndquad[q->opnd2.tipo]);
      switch (q->opnd2.tipo) {
        case IDLEOPND: break;
        case VAROPND:   printf(", %s", q->opnd2.atr.simb->cadeia); break;
        case INTOPND:   printf(", %d", q->opnd2.atr.valint); break;
        case REALOPND:  printf(", %g", q->opnd2.atr.valfloat); break;
        case CHAROPND:  printf(", %c", q->opnd2.atr.valchar); break;
        case LOGICOPND: printf(", %d", q->opnd2.atr.vallogic); break;
        case CADOPND:   printf(", %s", q->opnd2.atr.valcad); break;
        case ROTOPND:   printf(", %d", q->opnd2.atr.rotulo->num); break;
        case FUNCOPND:  printf(", %s", q->opnd2.atr.func->funcname->cadeia);
          break;
      }
      printf(")");
      printf(", (%s", nometipoopndquad[q->result.tipo]);
      switch (q->result.tipo) {
        case IDLEOPND: break;
        case VAROPND:   printf(", %s", q->result.atr.simb->cadeia); break;
        case INTOPND:   printf(", %d", q->result.atr.valint); break;
        case REALOPND:  printf(", %g", q->result.atr.valfloat); break;
        case CHAROPND:  printf(", %c", q->result.atr.valchar); break;
        case LOGICOPND: printf(", %d", q->result.atr.vallogic); break;
        case CADOPND:   printf(", %s", q->result.atr.valcad); break;
        case ROTOPND:   printf(", %d", q->result.atr.rotulo->num); break;
        case FUNCOPND:  printf(", %s", q->result.atr.func->funcname->cadeia);
          break;
      }
      printf(")");
    }
  }
   printf("\n");
}

void RenumQuadruplas(quadrupla quad1, quadrupla quad2){
  quadrupla q;
  int nquad;
  for(q = quad1->prox, nquad = quad1->num; q != quad2; q = q->prox) {
    nquad++;
    q->num = nquad;
  }
}

infoexpressao IntFactor(int value)
{
  infoexpressao infoexpr;

  infoexpr.tipo = INTEIRO;
  infoexpr.opnd.tipo = INTOPND;
  infoexpr.opnd.atr.valint = value;

  return infoexpr;
}

infoexpressao FloatFactor(float value)
{
  infoexpressao infoexpr;

  infoexpr.tipo = REAL;
  infoexpr.opnd.tipo = REALOPND;
  infoexpr.opnd.atr.valfloat = value;

  return infoexpr;
}

infoexpressao CharFactor (char value)
{
  infoexpressao infoexpr;

  infoexpr.tipo = CARACTERE;
  infoexpr.opnd.tipo = CHAROPND;
  infoexpr.opnd.atr.valchar = value;

  return infoexpr;
}

infoexpressao CadeiaFactor (char* value)
{
  infoexpressao infoexpr;

  infoexpr.tipo = CADEIA;
  infoexpr.opnd.tipo = CADOPND;
  infoexpr.opnd.atr.valcad = malloc(strlen(value) + 1);
  strcpy(infoexpr.opnd.atr.valcad, value);

  return infoexpr;
}

infoexpressao BoolFactor (int value)
{
  infoexpressao infoexpr;

  infoexpr.tipo = LOGICO;
  infoexpr.opnd.tipo = LOGICOPND;
  infoexpr.opnd.atr.vallogic = value;

  return infoexpr;
}

infoexpressao NegOpFactor(int tid, operando opnd){
  infoexpressao infoexpr;

  infoexpr.tipo = tid;
  infoexpr.opnd.tipo = VAROPND;
  infoexpr.opnd.atr.simb = NovaTemp(tid);
  GeraQuadrupla(OPMENUN, opnd, opndidle, infoexpr.opnd);

  return infoexpr;
}

infoexpressao FactorType(infoexpressao expression)
{
  return expression;
}

infoexpressao VariableFactor(infovariavel infovar){
  infoexpressao infoexpr;

  infoexpr.opnd = infovar.opnd;

  if(infovar.simb == NULL) return infoexpr;

  if(infovar.simb->array == FALSO){
    infoexpr.tipo = infovar.simb->tvar;
  }else{
    infoexpr.tipo = infovar.simb->tvar;
    infoexpr.opnd.tipo = VAROPND;
    infoexpr.opnd.atr.simb = NovaTemp(infoexpr.tipo);
    GeraQuadrupla(OPCONTAP, infovar.pointer.opnd, opndidle, infoexpr.opnd);
  }

  if (infovar.simb != NULL) {
    infoexpr.tipo = infovar.simb->tvar;
  }

  return infoexpr;
}

quadrupla IfInic(infoexpressao expr){
  opndaux.tipo = ROTOPND;
  return GeraQuadrupla(OPJF, expr.opnd, opndidle, opndaux);
}

quadrupla GeraQuadruplaJump(quadrupla destino) {
  operando opndaux;

  opndaux.tipo = ROTOPND;
  opndaux.atr.rotulo = destino;
  return GeraQuadrupla(OPJUMP, opndidle, opndidle, opndaux);
}

quadrupla GeraQuadruplaRead(int params){
  operando opnd1;

  if(params == 0) return NULL;

  opnd1.tipo = INTOPND;
  opnd1.atr.valint = params;

  return GeraQuadrupla(OPREAD, opnd1, opndidle, opndidle);
}

quadrupla GeraQuadruplaWrite(int params)
{
  operando opnd1;

  opnd1.tipo = INTOPND;
  opnd1.atr.valint = params;

  return GeraQuadrupla(OPWRITE, opnd1, opndidle, opndidle);
}

quadrupla GeraQuadruplaParam(operando opnd){
  return GeraQuadrupla(PARAM, opnd, opndidle, opndidle);
}

// Helpers para Interpretador

void EmpilharOpnd(operando x, pilhaoperando *P){
  nohopnd *temp;
  temp = *P;
  *P = (nohopnd *) malloc (sizeof (nohopnd));
  (*P)->opnd = x; (*P)->prox = temp;
}

void DesempilharOpnd(pilhaoperando *P){
  nohopnd *temp;
  if(!VaziaOpnd(*P)){
    temp = *P;
    *P = (*P)->prox;
    free(temp);
  }else{
    printf("\n  Delecao em pilha vazia\n");
  }
}

operando TopoOpnd(pilhaoperando P){
  if(!VaziaOpnd(P)) return P->opnd;
  else printf("\n\tTopo de pilha vazia\n");
}

void InicPilhaOpnd(pilhaoperando *P){
  *P = NULL;
}

char VaziaOpnd(pilhaoperando P){
  if(P == NULL) return 1;
  else return 0;
}

// Interpretador

void AlocaVariaveis () {
  simbolo s; int nelemaloc, i, j;
  printf ("\n    Alocando as variaveis:");
  for (i = 0; i < NCLASSHASH; i++){
    if(tabsimb[i]){
      for(s = tabsimb[i]; s != NULL; s = s->prox){
        if(s->tid == IDVAR){
          nelemaloc = 1;
          if(s->array){
            for(j = 1; j <= s->ndims; j++) nelemaloc *= s->dims[j];
          }
          switch(s->tvar){
          case INTEIRO:
            s->valint = malloc(nelemaloc * sizeof (int));     break;
          case REAL:
            s->valfloat = malloc(nelemaloc * sizeof (float)); break;
          case CARACTERE:
            s->valchar = malloc(nelemaloc * sizeof (char));   break;
          case LOGICO:
            s->vallogic = malloc(nelemaloc * sizeof (char));  break;
          }
          printf ("\n      %s: %d elemento(s) alocado(s) ", s->cadeia, nelemaloc);
        }
      }
    }
  }
}

void ExecQuadWrite(quadrupla quad){
  int i;
  operando opndaux;
  pilhaoperando pilhaopndaux;

  printf("\n  Escrevendo:\n    ");
  InicPilhaOpnd(&pilhaopndaux);
  for(i = 1; i <= quad->opnd1.atr.valint; i++) {
    EmpilharOpnd(TopoOpnd (pilhaopnd), &pilhaopndaux);
    DesempilharOpnd(&pilhaopnd);
  }
  for(i = 1; i <= quad->opnd1.atr.valint; i++){
    opndaux = TopoOpnd(pilhaopndaux);
    DesempilharOpnd(&pilhaopndaux);
    switch(opndaux.tipo){
      case INTOPND:     printf("%d", opndaux.atr.valint);             break;
      case REALOPND:    printf("%g", opndaux.atr.valfloat);           break;
      case CHAROPND:    printf("%c", opndaux.atr.valchar);            break;
      case LOGICOPND:
        if(opndaux.atr.vallogic == 1) printf("TRUE");
        else printf("FALSE");
        break;
      case CADOPND:     printf("%s", opndaux.atr.valcad);             break;
      case VAROPND:
        switch(opndaux.atr.simb->tvar){
          case INTEIRO: printf("%d", *(opndaux.atr.simb->valint));    break;
          case REAL:    printf("%g", *(opndaux.atr.simb->valfloat));  break;
          case LOGICO:
            if(*(opndaux.atr.simb->vallogic) == 1) printf("TRUE");
            else printf("FALSE");
            break;
          case CARACTERE: printf("%c", *(opndaux.atr.simb->valchar)); break;
        }
        break;
    }
  }
  printf("\n");
}

void ExecQuadMais(quadrupla quad){
  int tipo1, tipo2, valint1, valint2;
  float valfloat1, valfloat2;
  switch (quad->opnd1.tipo) {
    case INTOPND:
      tipo1 = INTOPND;  valint1 = quad->opnd1.atr.valint;  break;
    case REALOPND:
      tipo1 = REALOPND;  valfloat1 = quad->opnd1.atr.valfloat; break;
    case CHAROPND:
      tipo1 = INTOPND;  valint1 = quad->opnd1.atr.valchar;  break;
    case VAROPND:
      switch (quad->opnd1.atr.simb->tvar) {
        case INTEIRO:
          tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valint);  break;
        case REAL:
          tipo1 = REALOPND;
          valfloat1=*(quad->opnd1.atr.simb->valfloat);break;
        case CARACTERE:
          tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valchar); break;
      }
      break;
  }
  switch (quad->opnd2.tipo) {
    case INTOPND:
      tipo2 = INTOPND;  valint2 = quad->opnd2.atr.valint;  break;
    case REALOPND:
      tipo2 = REALOPND;  valfloat2 = quad->opnd2.atr.valfloat;  break;
    case CHAROPND:
      tipo2 = INTOPND;  valint2 = quad->opnd2.atr.valchar;  break;
    case VAROPND:
      switch (quad->opnd2.atr.simb->tvar) {
        case INTEIRO:
          tipo2 = INTOPND;
          valint2 = *(quad->opnd2.atr.simb->valint);  break;
        case REAL:
          tipo2 = REALOPND;
          valfloat2=*(quad->opnd2.atr.simb->valfloat);break;
        case CARACTERE:
          tipo2 = INTOPND;
          valint2=*(quad->opnd2.atr.simb->valchar);break;
      }
      break;
  }
  switch (quad->result.atr.simb->tvar) {
    case CARACTERE:
    case INTEIRO:
      *(quad->result.atr.simb->valint) = valint1 + valint2;
      break;
    case REAL:
      if (tipo1 == INTOPND && tipo2 == INTOPND)
        *(quad->result.atr.simb->valfloat) = valint1 + valint2;
      if (tipo1 == INTOPND && tipo2 == REALOPND)
        *(quad->result.atr.simb->valfloat) = valint1 + valfloat2;
      if (tipo1 == REALOPND && tipo2 == INTOPND)
        *(quad->result.atr.simb->valfloat) = valfloat1 + valint2;
      if (tipo1 == REALOPND && tipo2 == REALOPND)
        *(quad->result.atr.simb->valfloat) = valfloat1 + valfloat2;
      break;
  }
}

void ExecQuadMenos(quadrupla quad){
  int tipo1, tipo2, valint1, valint2;
  float valfloat1, valfloat2;
  switch (quad->opnd1.tipo) {
    case INTOPND:
      tipo1 = INTOPND;  valint1 = quad->opnd1.atr.valint;  break;
    case REALOPND:
      tipo1 = REALOPND;  valfloat1 = quad->opnd1.atr.valfloat; break;
    case CHAROPND:
      tipo1 = INTOPND;  valint1 = quad->opnd1.atr.valchar;  break;
    case VAROPND:
      switch (quad->opnd1.atr.simb->tvar) {
        case INTEIRO:
          tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valint);  break;
        case REAL:
          tipo1 = REALOPND;
          valfloat1=*(quad->opnd1.atr.simb->valfloat);break;
        case CARACTERE:
          tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valchar); break;
      }
      break;
  }
  switch (quad->opnd2.tipo) {
    case INTOPND:
      tipo2 = INTOPND;  valint2 = quad->opnd2.atr.valint;  break;
    case REALOPND:
      tipo2 = REALOPND;  valfloat2 = quad->opnd2.atr.valfloat;  break;
    case CHAROPND:
      tipo2 = INTOPND;  valint2 = quad->opnd2.atr.valchar;  break;
    case VAROPND:
      switch (quad->opnd2.atr.simb->tvar) {
        case INTEIRO:
          tipo2 = INTOPND;
          valint2 = *(quad->opnd2.atr.simb->valint);  break;
        case REAL:
          tipo2 = REALOPND;
          valfloat2=*(quad->opnd2.atr.simb->valfloat);break;
        case CARACTERE:
          tipo2 = INTOPND;
          valint2=*(quad->opnd2.atr.simb->valchar);break;
      }
      break;
  }
  switch (quad->result.atr.simb->tvar) {
    case CARACTERE:
    case INTEIRO:
      *(quad->result.atr.simb->valint) = valint1 - valint2;
      break;
    case REAL:
      if (tipo1 == INTOPND && tipo2 == INTOPND)
        *(quad->result.atr.simb->valfloat) = valint1 - valint2;
      if (tipo1 == INTOPND && tipo2 == REALOPND)
        *(quad->result.atr.simb->valfloat) = valint1 - valfloat2;
      if (tipo1 == REALOPND && tipo2 == INTOPND)
        *(quad->result.atr.simb->valfloat) = valfloat1 - valint2;
      if (tipo1 == REALOPND && tipo2 == REALOPND)
        *(quad->result.atr.simb->valfloat) = valfloat1 - valfloat2;
      break;
  }
}

void ExecQuadMenum(quadrupla quad){
  int tipo1, valint1;
  float valfloat1;
  switch (quad->opnd1.tipo) {
    case INTOPND:
      tipo1 = INTOPND;  valint1 = quad->opnd1.atr.valint;  break;
    case REALOPND:
      tipo1 = REALOPND;  valfloat1 = quad->opnd1.atr.valfloat; break;
    case CHAROPND:
      tipo1 = INTOPND;  valint1 = quad->opnd1.atr.valchar;  break;
    case VAROPND:
      switch (quad->opnd1.atr.simb->tvar) {
        case INTEIRO:
          tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valint);  break;
        case REAL:
          tipo1 = REALOPND;
          valfloat1=*(quad->opnd1.atr.simb->valfloat);break;
        case CARACTERE:
          tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valchar); break;
      }
      break;
  }

  switch (quad->result.atr.simb->tvar) {
    case CARACTERE:
    case INTEIRO:
      *(quad->result.atr.simb->valint) = - valint1;
      break;
    case REAL:
      if (tipo1 == INTOPND)
        *(quad->result.atr.simb->valfloat) = - valint1;
      if (tipo1 == REALOPND)
        *(quad->result.atr.simb->valfloat) = - valfloat1;
      break;
  }
}

void ExecQuadMult(quadrupla quad){
  int tipo1, tipo2, valint1, valint2;
  float valfloat1, valfloat2;
  switch (quad->opnd1.tipo) {
    case INTOPND:
      tipo1 = INTOPND;  valint1 = quad->opnd1.atr.valint;  break;
    case REALOPND:
      tipo1 = REALOPND;  valfloat1 = quad->opnd1.atr.valfloat; break;
    case CHAROPND:
      tipo1 = INTOPND;  valint1 = quad->opnd1.atr.valchar;  break;
    case VAROPND:
      switch (quad->opnd1.atr.simb->tvar) {
        case INTEIRO:
          tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valint);  break;
        case REAL:
          tipo1 = REALOPND;
          valfloat1=*(quad->opnd1.atr.simb->valfloat);break;
        case CARACTERE:
          tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valchar); break;
      }
      break;
  }
  switch (quad->opnd2.tipo) {
    case INTOPND:
      tipo2 = INTOPND;  valint2 = quad->opnd2.atr.valint;  break;
    case REALOPND:
      tipo2 = REALOPND;  valfloat2 = quad->opnd2.atr.valfloat;  break;
    case CHAROPND:
      tipo2 = INTOPND;  valint2 = quad->opnd2.atr.valchar;  break;
    case VAROPND:
      switch (quad->opnd2.atr.simb->tvar) {
        case INTEIRO:
          tipo2 = INTOPND;
          valint2 = *(quad->opnd2.atr.simb->valint);  break;
        case REAL:
          tipo2 = REALOPND;
          valfloat2=*(quad->opnd2.atr.simb->valfloat);break;
        case CARACTERE:
          tipo2 = INTOPND;
          valint2=*(quad->opnd2.atr.simb->valchar);break;
      }
      break;
  }
  switch (quad->result.atr.simb->tvar) {
    case CARACTERE:
    case INTEIRO:
      *(quad->result.atr.simb->valint) = valint1 * valint2;
      break;
    case REAL:
      if (tipo1 == INTOPND && tipo2 == INTOPND)
        *(quad->result.atr.simb->valfloat) = valint1 * valint2;
      if (tipo1 == INTOPND && tipo2 == REALOPND)
        *(quad->result.atr.simb->valfloat) = valint1 * valfloat2;
      if (tipo1 == REALOPND && tipo2 == INTOPND)
        *(quad->result.atr.simb->valfloat) = valfloat1 * valint2;
      if (tipo1 == REALOPND && tipo2 == REALOPND)
        *(quad->result.atr.simb->valfloat) = valfloat1 * valfloat2;
      break;
  }
}

void ExecQuadDiv(quadrupla quad){
  int tipo1, tipo2, valint1, valint2;
  float valfloat1, valfloat2;
  switch (quad->opnd1.tipo) {
    case INTOPND:
      tipo1 = INTOPND;  valint1 = quad->opnd1.atr.valint;  break;
    case REALOPND:
      tipo1 = REALOPND;  valfloat1 = quad->opnd1.atr.valfloat; break;
    case CHAROPND:
      tipo1 = INTOPND;  valint1 = quad->opnd1.atr.valchar;  break;
    case VAROPND:
      switch (quad->opnd1.atr.simb->tvar) {
        case INTEIRO:
          tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valint);  break;
        case REAL:
          tipo1 = REALOPND;
          valfloat1=*(quad->opnd1.atr.simb->valfloat);break;
        case CARACTERE:
          tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valchar); break;
      }
      break;
  }
  switch (quad->opnd2.tipo) {
    case INTOPND:
      tipo2 = INTOPND;  valint2 = quad->opnd2.atr.valint;  break;
    case REALOPND:
      tipo2 = REALOPND;  valfloat2 = quad->opnd2.atr.valfloat;  break;
    case CHAROPND:
      tipo2 = INTOPND;  valint2 = quad->opnd2.atr.valchar;  break;
    case VAROPND:
      switch (quad->opnd2.atr.simb->tvar) {
        case INTEIRO:
          tipo2 = INTOPND;
          valint2 = *(quad->opnd2.atr.simb->valint);  break;
        case REAL:
          tipo2 = REALOPND;
          valfloat2=*(quad->opnd2.atr.simb->valfloat);break;
        case CARACTERE:
          tipo2 = INTOPND;
          valint2=*(quad->opnd2.atr.simb->valchar);break;
      }
      break;
  }
  switch (quad->result.atr.simb->tvar) {
    case CARACTERE:
    case INTEIRO:
      *(quad->result.atr.simb->valint) = valint1 / valint2;
      break;
    case REAL:
      if (tipo1 == INTOPND && tipo2 == INTOPND)
        *(quad->result.atr.simb->valfloat) = valint1 / valint2;
      if (tipo1 == INTOPND && tipo2 == REALOPND)
        *(quad->result.atr.simb->valfloat) = valint1 / valfloat2;
      if (tipo1 == REALOPND && tipo2 == INTOPND)
        *(quad->result.atr.simb->valfloat) = valfloat1 / valint2;
      if (tipo1 == REALOPND && tipo2 == REALOPND)
        *(quad->result.atr.simb->valfloat) = valfloat1 / valfloat2;
      break;
  }
}

void ExecQuadResto(quadrupla quad){
  int tipo1, tipo2, valint1, valint2;
  switch (quad->opnd1.tipo) {
    case INTOPND:
      tipo1 = INTOPND;  valint1 = quad->opnd1.atr.valint;  break;
    case CHAROPND:
      tipo1 = INTOPND;  valint1 = quad->opnd1.atr.valchar;  break;
    case VAROPND:
      switch (quad->opnd1.atr.simb->tvar) {
        case INTEIRO:
          tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valint);  break;
        case CARACTERE:
          tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valchar); break;
      }
      break;
  }
  switch (quad->opnd2.tipo) {
    case INTOPND:
      tipo2 = INTOPND;  valint2 = quad->opnd2.atr.valint;  break;
    case CHAROPND:
      tipo2 = INTOPND;  valint2 = quad->opnd2.atr.valchar;  break;
    case VAROPND:
      switch (quad->opnd2.atr.simb->tvar) {
        case INTEIRO:
          tipo2 = INTOPND;
          valint2 = *(quad->opnd2.atr.simb->valint);  break;
        case CARACTERE:
          tipo2 = INTOPND;
          valint2=*(quad->opnd2.atr.simb->valchar);break;
      }
      break;
  }
  switch (quad->result.atr.simb->tvar) {
    case CARACTERE:
    case INTEIRO:
      *(quad->result.atr.simb->valint) = valint1 % valint2;
      break;
  }
}

void ExecQuadAtrib(quadrupla quad) {
  int tipo1, valint1;
  float valfloat1;
  char valchar1, vallogic1;
  switch(quad->opnd1.tipo) {
    case INTOPND:
      tipo1 = INTOPND;
      valint1 = quad->opnd1.atr.valint; break;
    case REALOPND:
      tipo1 = REALOPND;
      valfloat1 = quad->opnd1.atr.valfloat; break;
    case CHAROPND:
      tipo1 = CHAROPND;
      valchar1 = quad->opnd1.atr.valchar; break;
    case LOGICOPND:
      tipo1 = LOGICOPND;
      vallogic1 = quad->opnd1.atr.vallogic; break;
    case VAROPND:
      switch (quad->opnd1.atr.simb->tvar) {
        case INTEIRO:
          tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valint); break;
        case REAL:
          tipo1 = REALOPND;
          valfloat1=*(quad->opnd1.atr.simb->valfloat);break;
        case CARACTERE:
          tipo1 = CHAROPND;
          valchar1=*(quad->opnd1.atr.simb->valchar);break;
        case LOGICO:
          tipo1 = LOGICOPND;
          vallogic1 = *(quad->opnd1.atr.simb->vallogic);
          break;
      }
      break;
  }
  switch(quad->result.atr.simb->tvar) {
    case INTEIRO:
      if (tipo1 == INTOPND)  *(quad->result.atr.simb->valint) = valint1;
      if (tipo1 == CHAROPND) *(quad->result.atr.simb->valint) = valchar1;
      break;
    case CARACTERE:
      if (tipo1 == INTOPND)  *(quad->result.atr.simb->valchar) = valint1;
      if (tipo1 == CHAROPND) *(quad->result.atr.simb->valchar) = valchar1;
      break;
    case LOGICO:  *(quad->result.atr.simb->vallogic) = vallogic1; break;
    case REAL:
      if (tipo1 == INTOPND)
        *(quad->result.atr.simb->valfloat) = valint1;
      if (tipo1 == REALOPND)
        *(quad->result.atr.simb->valfloat) = valfloat1;
      if (tipo1 == CHAROPND)
        *(quad->result.atr.simb->valfloat) = valchar1;
      break;
  }
  printf("ATRIBUTED: %d", valint1);
}

void ExecQuadLT(quadrupla quad) {
  int tipo1, tipo2, valint1, valint2;
  float valfloat1, valfloat2;
  switch (quad->opnd1.tipo) {
    case INTOPND:
      tipo1 = INTOPND;  valint1 = quad->opnd1.atr.valint;  break;
    case REALOPND:
      tipo1 = REALOPND; valfloat1=quad->opnd1.atr.valfloat;break;
    case CHAROPND:
      tipo1 = INTOPND; valint1 = quad->opnd1.atr.valchar; break;
    case VAROPND:
      switch (quad->opnd1.atr.simb->tvar) {
        case INTEIRO:  tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valint);
          break;
        case REAL:  tipo1 = REALOPND;
          valfloat1 = *(quad->opnd1.atr.simb->valfloat);
          break;
        case CARACTERE:  tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valchar);
          break;
      }
      break;
  }
  switch (quad->opnd2.tipo) {
    case INTOPND:
      tipo2 = INTOPND;  valint2 = quad->opnd2.atr.valint;  break;
    case REALOPND:
      tipo2=REALOPND;valfloat2 = quad->opnd2.atr.valfloat;break;
    case CHAROPND:
      tipo2 = INTOPND;valint2 = quad->opnd2.atr.valchar; break;
    case VAROPND:
      switch (quad->opnd2.atr.simb->tvar) {
        case INTEIRO:  tipo2 = INTOPND;
          valint2 = *(quad->opnd2.atr.simb->valint);
          break;
        case REAL:  tipo2 = REALOPND;
          valfloat2 = *(quad->opnd2.atr.simb->valfloat);
          break;
        case CARACTERE:  tipo2 = INTOPND;
          valint2 = *(quad->opnd2.atr.simb->valchar);
          break;
        }
      break;
  }
  if (tipo1 == INTOPND && tipo2 == INTOPND)
    *(quad->result.atr.simb->vallogic) = valint1 < valint2;
  if (tipo1 == INTOPND && tipo2 == REALOPND)
    *(quad->result.atr.simb->vallogic) = valint1 < valfloat2;
  if (tipo1 == REALOPND && tipo2 == INTOPND)
    *(quad->result.atr.simb->vallogic) = valfloat1 < valint2;
  if (tipo1 == REALOPND && tipo2 == REALOPND)
    *(quad->result.atr.simb->vallogic) = valfloat1 < valfloat2;
}

void ExecQuadLE(quadrupla quad) {
  int tipo1, tipo2, valint1, valint2;
  float valfloat1, valfloat2;
  switch (quad->opnd1.tipo) {
    case INTOPND:
      tipo1 = INTOPND;  valint1 = quad->opnd1.atr.valint;  break;
    case REALOPND:
      tipo1 = REALOPND; valfloat1=quad->opnd1.atr.valfloat;break;
    case CHAROPND:
      tipo1 = INTOPND; valint1 = quad->opnd1.atr.valchar; break;
    case VAROPND:
      switch (quad->opnd1.atr.simb->tvar) {
        case INTEIRO:  tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valint);
          break;
        case REAL:  tipo1 = REALOPND;
          valfloat1 = *(quad->opnd1.atr.simb->valfloat);
          break;
        case CARACTERE:  tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valchar);
          break;
      }
      break;
  }
  switch (quad->opnd2.tipo) {
    case INTOPND:
      tipo2 = INTOPND;  valint2 = quad->opnd2.atr.valint;  break;
    case REALOPND:
      tipo2=REALOPND;valfloat2 = quad->opnd2.atr.valfloat;break;
    case CHAROPND:
      tipo2 = INTOPND;valint2 = quad->opnd2.atr.valchar; break;
    case VAROPND:
      switch (quad->opnd2.atr.simb->tvar) {
        case INTEIRO:  tipo2 = INTOPND;
          valint2 = *(quad->opnd2.atr.simb->valint);
          break;
        case REAL:  tipo2 = REALOPND;
          valfloat2 = *(quad->opnd2.atr.simb->valfloat);
          break;
        case CARACTERE:  tipo2 = INTOPND;
          valint2 = *(quad->opnd2.atr.simb->valchar);
          break;
        }
      break;
  }
  if (tipo1 == INTOPND && tipo2 == INTOPND)
    *(quad->result.atr.simb->vallogic) = valint1 <= valint2;
  if (tipo1 == INTOPND && tipo2 == REALOPND)
    *(quad->result.atr.simb->vallogic) = valint1 <= valfloat2;
  if (tipo1 == REALOPND && tipo2 == INTOPND)
    *(quad->result.atr.simb->vallogic) = valfloat1 <= valint2;
  if (tipo1 == REALOPND && tipo2 == REALOPND)
    *(quad->result.atr.simb->vallogic) = valfloat1 <= valfloat2;
}

void ExecQuadGT(quadrupla quad) {
  int tipo1, tipo2, valint1, valint2;
  float valfloat1, valfloat2;
  switch (quad->opnd1.tipo) {
    case INTOPND:
      tipo1 = INTOPND;  valint1 = quad->opnd1.atr.valint;  break;
    case REALOPND:
      tipo1 = REALOPND; valfloat1=quad->opnd1.atr.valfloat;break;
    case CHAROPND:
      tipo1 = INTOPND; valint1 = quad->opnd1.atr.valchar; break;
    case VAROPND:
      switch (quad->opnd1.atr.simb->tvar) {
        case INTEIRO:  tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valint);
          break;
        case REAL:  tipo1 = REALOPND;
          valfloat1 = *(quad->opnd1.atr.simb->valfloat);
          break;
        case CARACTERE:  tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valchar);
          break;
      }
      break;
  }
  switch (quad->opnd2.tipo) {
    case INTOPND:
      tipo2 = INTOPND;  valint2 = quad->opnd2.atr.valint;  break;
    case REALOPND:
      tipo2=REALOPND;valfloat2 = quad->opnd2.atr.valfloat;break;
    case CHAROPND:
      tipo2 = INTOPND;valint2 = quad->opnd2.atr.valchar; break;
    case VAROPND:
      switch (quad->opnd2.atr.simb->tvar) {
        case INTEIRO:  tipo2 = INTOPND;
          valint2 = *(quad->opnd2.atr.simb->valint);
          break;
        case REAL:  tipo2 = REALOPND;
          valfloat2 = *(quad->opnd2.atr.simb->valfloat);
          break;
        case CARACTERE:  tipo2 = INTOPND;
          valint2 = *(quad->opnd2.atr.simb->valchar);
          break;
        }
      break;
  }
  if (tipo1 == INTOPND && tipo2 == INTOPND)
    *(quad->result.atr.simb->vallogic) = valint1 > valint2;
  if (tipo1 == INTOPND && tipo2 == REALOPND)
    *(quad->result.atr.simb->vallogic) = valint1 > valfloat2;
  if (tipo1 == REALOPND && tipo2 == INTOPND)
    *(quad->result.atr.simb->vallogic) = valfloat1 > valint2;
  if (tipo1 == REALOPND && tipo2 == REALOPND)
    *(quad->result.atr.simb->vallogic) = valfloat1 > valfloat2;
}

void ExecQuadGE(quadrupla quad) {
  int tipo1, tipo2, valint1, valint2;
  float valfloat1, valfloat2;
  switch (quad->opnd1.tipo) {
    case INTOPND:
      tipo1 = INTOPND;  valint1 = quad->opnd1.atr.valint;  break;
    case REALOPND:
      tipo1 = REALOPND; valfloat1=quad->opnd1.atr.valfloat;break;
    case CHAROPND:
      tipo1 = INTOPND; valint1 = quad->opnd1.atr.valchar; break;
    case VAROPND:
      switch (quad->opnd1.atr.simb->tvar) {
        case INTEIRO:  tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valint);
          break;
        case REAL:  tipo1 = REALOPND;
          valfloat1 = *(quad->opnd1.atr.simb->valfloat);
          break;
        case CARACTERE:  tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valchar);
          break;
      }
      break;
  }
  switch (quad->opnd2.tipo) {
    case INTOPND:
      tipo2 = INTOPND;  valint2 = quad->opnd2.atr.valint;  break;
    case REALOPND:
      tipo2=REALOPND;valfloat2 = quad->opnd2.atr.valfloat;break;
    case CHAROPND:
      tipo2 = INTOPND;valint2 = quad->opnd2.atr.valchar; break;
    case VAROPND:
      switch (quad->opnd2.atr.simb->tvar) {
        case INTEIRO:  tipo2 = INTOPND;
          valint2 = *(quad->opnd2.atr.simb->valint);
          break;
        case REAL:  tipo2 = REALOPND;
          valfloat2 = *(quad->opnd2.atr.simb->valfloat);
          break;
        case CARACTERE:  tipo2 = INTOPND;
          valint2 = *(quad->opnd2.atr.simb->valchar);
          break;
        }
      break;
  }
  if (tipo1 == INTOPND && tipo2 == INTOPND)
    *(quad->result.atr.simb->vallogic) = valint1 >= valint2;
  if (tipo1 == INTOPND && tipo2 == REALOPND)
    *(quad->result.atr.simb->vallogic) = valint1 >= valfloat2;
  if (tipo1 == REALOPND && tipo2 == INTOPND)
    *(quad->result.atr.simb->vallogic) = valfloat1 >= valint2;
  if (tipo1 == REALOPND && tipo2 == REALOPND)
    *(quad->result.atr.simb->vallogic) = valfloat1 >= valfloat2;
}

void ExecQuadAnd(quadrupla quad) {
  char vallogic1, vallogic2;
  switch (quad->opnd1.tipo) {
    case LOGICOPND:
      vallogic1 = quad->opnd1.atr.vallogic;  break;
    case VAROPND:
      switch (quad->opnd1.atr.simb->tvar) {
        case LOGICO:
          vallogic1 = *(quad->opnd1.atr.simb->vallogic);
          break;
      }
      break;
  }
  switch (quad->opnd2.tipo) {
    case LOGICOPND:
      vallogic2 = quad->opnd2.atr.vallogic;  break;
    case VAROPND:
      switch (quad->opnd1.atr.simb->tvar) {
        case LOGICO:
          vallogic2 = *(quad->opnd2.atr.simb->vallogic);
          break;
      }
      break;
  }

  *(quad->result.atr.simb->vallogic) = vallogic1 && vallogic2;
}

void ExecQuadOr(quadrupla quad) {
  char vallogic1, vallogic2;
  switch (quad->opnd1.tipo) {
    case LOGICOPND:
      vallogic1 = quad->opnd1.atr.vallogic;  break;
    case VAROPND:
      switch (quad->opnd1.atr.simb->tvar) {
        case LOGICO:
          vallogic1 = *(quad->opnd1.atr.simb->vallogic);
          break;
      }
      break;
  }
  switch (quad->opnd2.tipo) {
    case LOGICOPND:
      vallogic2 = quad->opnd2.atr.vallogic;  break;
    case VAROPND:
      switch (quad->opnd1.atr.simb->tvar) {
        case LOGICO:
          vallogic2 = *(quad->opnd2.atr.simb->vallogic);
          break;
      }
      break;
  }

  *(quad->result.atr.simb->vallogic) = vallogic1 || vallogic2;
}

void ExecQuadNot(quadrupla quad) {
  char vallogic1;
  switch (quad->opnd1.tipo) {
    case LOGICOPND:
      vallogic1 = quad->opnd1.atr.vallogic;  break;
    case VAROPND:
      switch (quad->opnd1.atr.simb->tvar) {
        case LOGICO:
          vallogic1 = *(quad->opnd1.atr.simb->vallogic);
          break;
      }
      break;
  }

  *(quad->result.atr.simb->vallogic) = !vallogic1;
}

void ExecQuadEQ(quadrupla quad) {
  int tipo1, tipo2, valint1, valint2;
  float valfloat1, valfloat2;
  char vallogic1, vallogic2;
  switch (quad->opnd1.tipo) {
    case INTOPND:
      tipo1 = INTOPND;  valint1 = quad->opnd1.atr.valint;  break;
    case REALOPND:
      tipo1 = REALOPND; valfloat1=quad->opnd1.atr.valfloat;break;
    case CHAROPND:
      tipo1 = INTOPND; valint1 = quad->opnd1.atr.valchar; break;
    case LOGICOPND:
      tipo1 = LOGICOPND; vallogic1 = quad->opnd1.atr.vallogic;  break;
    case VAROPND:
      switch (quad->opnd1.atr.simb->tvar) {
        case INTEIRO:  tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valint);
          break;
        case REAL:  tipo1 = REALOPND;
          valfloat1 = *(quad->opnd1.atr.simb->valfloat);
          break;
        case CARACTERE:  tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valchar);
          break;
        case LOGICO:  tipo1 = LOGICOPND;
          vallogic1 = *(quad->opnd1.atr.simb->vallogic);
          break;
      }
      break;
  }
  switch (quad->opnd2.tipo) {
    case INTOPND:
      tipo2 = INTOPND;  valint2 = quad->opnd2.atr.valint;  break;
    case REALOPND:
      tipo2=REALOPND;valfloat2 = quad->opnd2.atr.valfloat;break;
    case CHAROPND:
      tipo2 = INTOPND; valint2 = quad->opnd2.atr.valchar; break;
    case LOGICOPND:
      tipo2 = LOGICOPND; vallogic2 = quad->opnd2.atr.vallogic;  break;
    case VAROPND:
      switch (quad->opnd2.atr.simb->tvar) {
        case INTEIRO:  tipo2 = INTOPND;
          valint2 = *(quad->opnd2.atr.simb->valint);
          break;
        case REAL:  tipo2 = REALOPND;
          valfloat2 = *(quad->opnd2.atr.simb->valfloat);
          break;
        case CARACTERE:  tipo2 = INTOPND;
          valint2 = *(quad->opnd2.atr.simb->valchar);
          break;
        case LOGICO:  tipo2 = LOGICOPND;
          vallogic2 = *(quad->opnd2.atr.simb->vallogic);
          break;
        }
      break;
  }
  if (tipo1 == INTOPND && tipo2 == INTOPND)
    *(quad->result.atr.simb->vallogic) = valint1 == valint2;
  if (tipo1 == REALOPND && tipo2 == REALOPND)
    *(quad->result.atr.simb->vallogic) = valfloat1 == valfloat2;
  if (tipo1 == LOGICOPND && tipo2 == LOGICOPND)
    *(quad->result.atr.simb->vallogic) = vallogic1 == vallogic2;
}

void ExecQuadNE(quadrupla quad) {
  int tipo1, tipo2, valint1, valint2;
  float valfloat1, valfloat2;
  char vallogic1, vallogic2;
  switch (quad->opnd1.tipo) {
    case INTOPND:
      tipo1 = INTOPND;  valint1 = quad->opnd1.atr.valint;  break;
    case REALOPND:
      tipo1 = REALOPND; valfloat1=quad->opnd1.atr.valfloat;break;
    case CHAROPND:
      tipo1 = INTOPND; valint1 = quad->opnd1.atr.valchar; break;
    case LOGICOPND:
      tipo1 = LOGICOPND; vallogic1 = quad->opnd1.atr.vallogic;  break;
    case VAROPND:
      switch (quad->opnd1.atr.simb->tvar) {
        case INTEIRO:  tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valint);
          break;
        case REAL:  tipo1 = REALOPND;
          valfloat1 = *(quad->opnd1.atr.simb->valfloat);
          break;
        case CARACTERE:  tipo1 = INTOPND;
          valint1 = *(quad->opnd1.atr.simb->valchar);
          break;
        case LOGICO:  tipo1 = LOGICOPND;
          vallogic1 = *(quad->opnd1.atr.simb->vallogic);
          break;
      }
      break;
  }
  switch (quad->opnd2.tipo) {
    case INTOPND:
      tipo2 = INTOPND;  valint2 = quad->opnd2.atr.valint;  break;
    case REALOPND:
      tipo2=REALOPND;valfloat2 = quad->opnd2.atr.valfloat;break;
    case CHAROPND:
      tipo2 = INTOPND; valint2 = quad->opnd2.atr.valchar; break;
    case LOGICOPND:
      tipo2 = LOGICOPND; vallogic2 = quad->opnd2.atr.vallogic;  break;
    case VAROPND:
      switch (quad->opnd2.atr.simb->tvar) {
        case INTEIRO:  tipo2 = INTOPND;
          valint2 = *(quad->opnd2.atr.simb->valint);
          break;
        case REAL:  tipo2 = REALOPND;
          valfloat2 = *(quad->opnd2.atr.simb->valfloat);
          break;
        case CARACTERE:  tipo2 = INTOPND;
          valint2 = *(quad->opnd2.atr.simb->valchar);
          break;
        case LOGICO:  tipo2 = LOGICOPND;
          vallogic2 = *(quad->opnd2.atr.simb->vallogic);
          break;
        }
      break;
  }
  if (tipo1 == INTOPND && tipo2 == INTOPND)
    *(quad->result.atr.simb->vallogic) = valint1 != valint2;
  if (tipo1 == REALOPND && tipo2 == REALOPND)
    *(quad->result.atr.simb->vallogic) = valfloat1 != valfloat2;
  if (tipo1 == LOGICOPND && tipo2 == LOGICOPND)
    *(quad->result.atr.simb->vallogic) = vallogic1 != vallogic2;
}

void ExecQuadIndex(quadrupla quad){
  int i, pointer_value = 0, weight = 1;
  operando opndaux;
  
  printf("\n  Indexando:\n    ");
  for(i = quad->opnd2.atr.valint-1; i > 0; i--){
    opndaux = TopoOpnd(pilhaind);
    if(opndaux.tipo == INTOPND) pointer_value += opndaux.atr.valint*weight;
    else pointer_value += (*(opndaux.atr.simb->valint))*weight;
    DesempilharOpnd(&pilhaind);
    weight *= quad->opnd1.atr.simb->dims[i];
  }
  switch(quad->opnd1.atr.simb->tvar){
    case INTEIRO:   quad->result.atr.simb->valint = quad->opnd1.atr.simb->valint + pointer_value;     break;
    case REAL:      quad->result.atr.simb->valfloat = quad->opnd1.atr.simb->valfloat + pointer_value; break;
    case LOGICO:    quad->result.atr.simb->vallogic = quad->opnd1.atr.simb->vallogic + pointer_value; break;
    case CARACTERE: quad->result.atr.simb->valchar = quad->opnd1.atr.simb->valchar + pointer_value;   break;
  }
}

void ExecQuadRead(quadrupla quad) {
  int i;  operando opndaux;
  pilhaoperando pilhaopndaux;
  
  printf("\n    Lendo: \n");
  InicPilhaOpnd(&pilhaopndaux);
  for (i = 1; i <= quad->opnd1.atr.valint; i++) {
    EmpilharOpnd(TopoOpnd (pilhaopnd), &pilhaopndaux);
    DesempilharOpnd(&pilhaopnd);
  }
  for (i = 1; i <= quad->opnd1.atr.valint; i++) {
    opndaux = TopoOpnd(pilhaopndaux);
    DesempilharOpnd (&pilhaopndaux);
        switch (opndaux.atr.simb->tvar) {
          case INTEIRO:
              fscanf(finput, "%d", opndaux.atr.simb->valint); printf("readed: %d\n", *opndaux.atr.simb->valint); break;
             case REAL:
              fscanf(finput, "%g", opndaux.atr.simb->valfloat); printf("readed: %g\n", *opndaux.atr.simb->valfloat); break;
            case LOGICO:
              fscanf(finput, "%d", opndaux.atr.simb->vallogic); printf("readed: %d\n", *opndaux.atr.simb->vallogic); break;
            case CARACTERE:
              fscanf(finput, "%c", opndaux.atr.simb->valchar); printf("readed: %c\n", *opndaux.atr.simb->valchar); break;
        }
  }
}

void InterpCodIntermed(){
  quadrupla quad, quadprox;
  funchead functions;
  char encerra, condicao;
  printf("\n\nINTERPRETADOR:\n");
  finput = fopen("input.txt", "r");
  InicPilhaOpnd(&pilhaopnd);
  InicPilhaOpnd(&pilhaind);
  encerra = FALSO;
  quad = codintermed->prox->listquad->prox;
  functions = codintermed->prox;
  while(!encerra){
    printf("\n%4d) %s", quad->num, nomeoperquad[quad->oper]);
    quadprox = quad->prox;
    switch(quad->oper){
      case OPRETURN: functions = functions->prox;
      quadprox = functions->listquad->prox; break;
      case OPEXIT:   encerra = VERDADE;                     break;
      case OPENMOD:  AlocaVariaveis();                      break;
      case OPREAD:   ExecQuadRead (quad);                   break;
      case PARAM:    EmpilharOpnd(quad->opnd1, &pilhaopnd); break;
      case OPWRITE:  ExecQuadWrite(quad);                   break;
      case OPMAIS:   ExecQuadMais(quad);                    break;
      case OPMENOS:  ExecQuadMenos(quad);                   break;
      case OPMENUN:  ExecQuadMenum(quad);                   break;
      case OPMULT:   ExecQuadMult(quad);                    break;
      case OPDIV:    ExecQuadDiv(quad);                     break;
      case OPRESTO:  ExecQuadResto(quad);                   break;
      case OPATRIB:  ExecQuadAtrib(quad);                   break;
      case OPLT:     ExecQuadLT(quad);                      break;
      case OPLE:     ExecQuadLE(quad);                      break;
      case OPGT:     ExecQuadGT(quad);                      break;
      case OPGE:     ExecQuadGE(quad);                      break;
      case OPAND:    ExecQuadAnd(quad);                     break;
      case OPOR:     ExecQuadOr(quad);                      break;
      case OPNOT:    ExecQuadNot(quad);                     break;
      case OPEQ:     ExecQuadEQ(quad);                      break;
      case OPNE:     ExecQuadNE(quad);                      break;
      case OPJUMP:   quadprox = quad->result.atr.rotulo;    break;
      case OPJF:
        if(quad->opnd1.tipo == LOGICOPND) condicao = quad->opnd1.atr.vallogic;
        if(quad->opnd1.tipo == VAROPND)   condicao = *(quad->opnd1.atr.simb->vallogic);
        if(!condicao) quadprox = quad->result.atr.rotulo;
        break;
      case OPIND:    EmpilharOpnd(quad->opnd1, &pilhaind);  break;
      case OPINDEX:  ExecQuadIndex(quad);                   break;
      case OPATRIBP:
      case OPCONTAP: ExecQuadAtrib(quad);                   break;
      // *CALL
      // *RETURN
      // NOP DONE!
    }
    if(!encerra) quad = quadprox;
  }
  fclose(finput);
  printf("\n");
}
