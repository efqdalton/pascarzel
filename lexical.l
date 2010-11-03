%{
/*#define   ID           1*/
/*#define   INTCT        2*/
/*#define   CHARCT       3*/
/*#define   FLOATCT      4*/
/*#define   STRING       5*/

/*#define   OROP         6*/
/*#define   ANDOP        7*/
/*#define   NOTOP        8*/
/*#define   RELOP        9*/
/*#define   ADOP        10*/
/*#define   MULTOP      11*/
/*#define   NEGOP       12*/

/*#define   OPPAR       13*/
/*#define   CLPAR       14*/
/*#define   OPBRAK      15*/
/*#define   CLBRAK      16*/
/*#define   OPBRACE     17*/
/*#define   CLBRACE     18*/
/*#define   SCOLON      19*/
/*#define   COMMA       20*/
/*#define   COLON       21*/
/*#define   ASSIGN      22*/

/*#define   ARRAY       23*/
/*#define   CALL        24*/
/*#define   CHAR        25*/
/*#define   DO          26*/
/*#define   ELSE        27*/
/*#define   FALSE       28*/
/*#define   FLOAT       29*/
/*#define   FOR         30*/
/*#define   FUNCTION    31*/
/*#define   GLOBAL      32*/
/*#define   IF          33*/
/*#define   INT         34*/
/*#define   LOCAL       35*/
/*#define   LOGIC       36*/
/*#define   MAIN        37*/
/*#define   OF          38*/
/*#define   PARAMETERS  39*/
/*#define   READ        40*/
/*#define   REPEAT      41*/
/*#define   RETURN      42*/
/*#define   STATEMENTS  43*/
/*#define   STEP        44*/
/*#define   THEN        45*/
/*#define   TO          46*/
/*#define   DOWNTO      47*/
/*#define   TRUE        48*/
/*#define   UNTIL       49*/
/*#define   VOID        50*/
/*#define   WHILE       51*/
/*#define   WRITE       52*/

void getString();
void getChar();
void getInt();
void getFloat();
void getId();
void getOperator();
char tratachar(char *);

%}

id          {letra}({letra}|{digito})*
intct       {digito}+
charct      \'("\\".|[^\'])?\'
floatct     {digito}+\.{digito}*([Ee](\+|\-)?{digito}+)?
string      \"(\\\"|[^\"])*\"

orop        \|\|
andop       \&\&
notop       \!
relop       ((\<)|(\<\=)|(\>)|(\>\=)|(\=)|(\!\=))
adop        (\+|\-)
multop      (\/|\*|\%)
negop       \~

oppar       \(
clpar       \)
opbrak      \[
clbrak      \]
opbrace     \{
clbrace     \}
scolon      ;
comma       ,
colon       :
assign      :=

comment     "/*"(.|{delim})*"*/"

letra       [A-Za-z]
digito      [0-9]
delim       [ \t\n\r]+

%%
{string}       { getString();     return STRING;  }
{charct}       { getChar();       return CHARCT;  }
{intct}        { getInt();        return INTCT;   }
{floatct}      { getFloat();      return FLOATCT; }
{comment}      { ;                                }
array          { return ARRAY;                    }
call           { return CALL;                     }
char           { return CHAR;                     }
do             { return DO;                       }
else           { return ELSE;                     }
false          { return FALSE;                    }
float          { return FLOAT;                    }
for            { return FOR;                      }
function       { return FUNCTION;                 }
global         { return GLOBAL;                   }
if             { return IF;                       }
int            { return INT;                      }
local          { return LOCAL;                    }
logic          { return LOGIC;                    }
main           { return MAIN;                     }
of             { return OF;                       }
parameters     { return PARAMETERS;               }
read           { return READ;                     }
repeat         { return REPEAT;                   }
return         { return RETURN;                   }
statements     { return STATEMENTS;               }
step           { return STEP;                     }
then           { return THEN;                     }
to             { return TO;                       }
downto         { return DOWNTO;                   }
true           { return TRUE;                     }
until          { return UNTIL;                    }
void           { return VOID;                     }
while          { return WHILE;                    }
write          { return WRITE;                    }
{id}           { getId();         return ID;      }
{orop}         { getOperator();   return OROP;    }
{andop}        { getOperator();   return ANDOP;   }
{notop}        { getOperator();   return NOTOP;   }
{relop}        { getOperator();   return RELOP;   }
{adop}         { getOperator();   return ADOP;    }
{multop}       { getOperator();   return MULTOP;  }
{negop}        { getOperator();   return NEGOP;   }
{oppar}        { return OPPAR;                    }
{clpar}        { return CLPAR;                    }
{opbrak}       { return OPBRAK;                   }
{clbrak}       { return CLBRAK;                   }
{opbrace}      { return OPBRACE;                  }
{clbrace}      { return CLBRACE;                  }
{scolon}       { return SCOLON;                   }
{comma}        { return COMMA;                    }
{colon}        { return COLON;                    }
{assign}       { return ASSIGN;                   }
{delim}        { ;                                }
%%

/*main(){*/
/*  int i;*/

/*  printf("                 ANÁLISE LÉXICA\n");*/
/*  printf("---------------------------------------------------\n");*/
/*  printf(" Texto                | Tipo | Atributo\n");*/
/*  printf("---------------------------------------------------\n");*/
/*  while( i = yylex() ){*/
/*    if( i > 12 ) printf(" %-20s | %02d   |\n", yytext, i);*/
/*    else         printf(" %-20s | %02d   | %-20s\n", yytext, i, yytext);*/
/*  }*/
/*  printf("\n");*/
/*}*/

void getString(){
  // TODO: Escapar caracteres especiais!
  strcpy(yylval.cadeia, yytext);
}

void getChar(){
  yylval.carac = tratachar(yytext);
}

void getInt(){
  yylval.valint = atoi(yytext);
}

void getFloat(){
  yylval.valreal = atof(yytext);
}

void getId(){
  strcpy(yylval.cadeia, yytext);
}

void getOperator(){
  switch(yytext[0]){
    case '<': 
      if(yytext[1] == '=') yylval.atr = LE;
      else yylval.atr = LT;
      break;
    case '>':
      if(yytext[1] == '=') yylval.atr = GE;
      else yylval.atr = GT;
      break;
    case '!':
      if(yytext[1] == '=') yylval.atr = NE;
      else yylval.atr = NOT;
      break;
    case '=': yylval.atr = EQ;    break;
    case '+': yylval.atr = MAIS;  break;
    case '-': yylval.atr = MENOS; break;
    case '*': yylval.atr = MULT;  break;
    case '/': yylval.atr = DIV;   break;
    case '%': yylval.atr = RESTO; break;
    case '|': yylval.atr = OR;    break;
    case '&': yylval.atr = AND;   break;
    case '~': yylval.atr = NEG;   break;
  }
}

char tratachar(char *s) {
  if(s[1] != '\\') return s[1];
  else switch(s[2]){
    case 'a':   return 7;   case '\\':  return 92;
    case 'b':   return 8;   case 'r':   return 13;
    case '\"':  return 34;  case 'f':   return 12;
    case 't':   return 9;   case 'n':   return 10;
    case '0':   return 0;   case '\'':  return 39;
    case 'v':   return 11;
    default:    return s[2];
  }
}
