%{
/*
 * $Id$
 *
 * Harbour lex rules.
 * Build 21: Spring 99
 * Usage: flex -i -8 -oyylex.c harbour.l
 * You may find flex.exe at www.harbour-project.org
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "y_tab.h"
#include "hbsetup.h"    /* main configuration file */
#include "hberrors.h"
#include "types.h"

void yyerror( char * );
static void yyunput( int, char * );
#undef yywrap        /* to implement our own yywrap() funtion to handle EOFs */
#ifdef __cplusplus
extern "C" int yywrap( void );
#else
int yywrap( void );
#endif
#undef YY_INPUT      /* to implement our own YY_INPUT function to manage PRGs without \n at the end */
extern FILE * yyin;  /* currently yacc parsed file */
int yy_lex_input( char *, int );
#define YY_INPUT( buf, result, max_size ) result = yy_lex_input( buf, max_size );

typedef struct
{
   char * szDefine;
   char * szValue;
   void * pKeys;
   void * pNext;
} _DEFINE, * PDEFINE;

PDEFINE LastDef( PDEFINE pDef );    /* searches for the latest #define */
void Define( char * szDefine );     /* add a new #define symbol */
void DefineKey( char * szKey );     /* add a new key to a #define expression */
void AddDefine( char * szDefine, char * szValue ); /* add a new define from the command line */
PDEFINE FindDef( char * szText );   /* finds a #define */

/* variables defined in harbour.y */
extern int _iQuiet;
extern int _iRestrictSymbolLength;
extern WORD _wSeqCounter;
extern WORD _wForCounter;
extern WORD _wIfCounter;
extern WORD _wWhileCounter;
extern WORD _wCaseCounter;

PDEFINE pDefs = 0;                  /* support for #defines */
int iLine = 1;
long lNumber = 0;

#define LOOKUP          0       /* scan from the begining of line */
#define OPERATOR        -1
int _iState = LOOKUP;

void yy_lex_count_lf( void )
{
  char * pTmp = yytext;
  while( (pTmp = strchr(pTmp, '\n')) )
  {
    ++iLine;
    ++pTmp;
  }
}

%}

SpaceTab      [ \t]+
InvalidNumber [0-9]+\.
Number        ([0-9]+)|([0-9]*\.[0-9]+)
HexNumber     0x[0-9A-F]+
Identifier    (([a-zA-Z])|([_a-zA-Z][_a-zA-Z0-9]+))
String        (\"(([^\"]*)|([\!]*))\")|(\'(([^\']*)|([\!]*))\')
PseudoFunc    {Identifier}"("+.*")"+

Comment1      "/*"([^\*]|[\*][^\/])*"*/"
Comment2      [\/][\/].*
Comment       ({Comment1}|{Comment2})
LineCont      (;.*\n)
Separator     {SpaceTab}|{Comment}|{LineCont}

%x            COMMENT3 DEFINE DEFINE_PARAMS DEFINE_EXPR
%x            IFDEF IFNDEF STRING1 STRING2
%x            NEXT_ BREAK_ CASE_ DO_ WHILE_ WITH_ END_

%%

("//".*)|("&&".*)   ;

^{SpaceTab}*(\*|"NOTE").*  ;
\n{SpaceTab}*(\*|"NOTE").*  ++iLine; if( ! _iQuiet ) printf( "\rline: %i", iLine );

'              BEGIN STRING1;
\"             BEGIN STRING2;

<STRING1>[^'^\n]*   GenError( ERR_STRING_TERMINATOR, yytext, NULL ); BEGIN 0;
<STRING2>[^\"^\n]*  GenError( ERR_STRING_TERMINATOR, yytext, NULL ); BEGIN 0;

<STRING1>[^']*'   { BEGIN 0; yylval.string = strdup( yytext + 1 );
                    yylval.string[ yyleng - 2 ] = 0; return LITERAL; }
<STRING2>[^\"]*\" { BEGIN 0; yylval.string = strdup( yytext + 1 );
                    yylval.string[ yyleng - 2 ] = 0; return LITERAL; }

"/*"           BEGIN COMMENT3;
<COMMENT3>"*"["*"]*"/" BEGIN 0;
<COMMENT3>[^"*/"\n]* ;
<COMMENT3>"*"     ;
<COMMENT3>[\/\"]+ ;
<COMMENT3>\n   ++iLine; if( ! _iQuiet ) printf( "\rline: %i", iLine );

"#"{SpaceTab}*"define"      BEGIN DEFINE;
<DEFINE>{Identifier}/{SpaceTab}+   Define( yytext );
<DEFINE>{Identifier}/{SpaceTab}*\n Define( yytext ); BEGIN 0;
<DEFINE>{SpaceTab}          ;
<DEFINE>{Identifier}        LastDef( pDefs )->szValue = strdup( yytext ); BEGIN 0;
<DEFINE>{PseudoFunc}        LastDef( pDefs )->szValue = strdup( yytext ); BEGIN 0;
<DEFINE>{String}            LastDef( pDefs )->szValue = strdup( yytext ); BEGIN 0;
<DEFINE>-?{Number}          LastDef( pDefs )->szValue = strdup( yytext ); BEGIN 0;
<DEFINE>{HexNumber}         LastDef( pDefs )->szValue = strdup( yytext ); BEGIN 0;
<DEFINE>"/*".*"*/"          ;
<DEFINE>{Number}{SpaceTab}*[\(]    yyerror( "Syntax error in #define" );
<DEFINE>{String}{SpaceTab}*[\(]    yyerror( "Syntax error in #define" );
<DEFINE>{Identifier}{SpaceTab}*[\(] Define( yytext ); BEGIN DEFINE_PARAMS;
<DEFINE_PARAMS>{SpaceTab}   ;
<DEFINE_PARAMS>{Identifier} DefineKey( yytext );
<DEFINE_PARAMS>[\,]         ;
<DEFINE_PARAMS>[\)]         BEGIN DEFINE_EXPR;
<DEFINE_EXPR>.*/\n          LastDef( pDefs )->szValue = strdup( yytext ); BEGIN 0;

"#"{SpaceTab}*"ifdef"          BEGIN IFDEF;
<IFDEF>{Identifier}            if( FindDef( yytext ) ) BEGIN 0;
<IFDEF>"#"{SpaceTab}*"else"\n  ++iLine; BEGIN 0;
<IFDEF>"#"{SpaceTab}*"endif"\n ++iLine; BEGIN 0;
<IFDEF>\n                      ++iLine;
<IFDEF>[\(\),\"\/\.]*          ;
<IFDEF>"#"{SpaceTab}*"ifdef"   ;
<IFDEF>"#"{SpaceTab}*"else"    BEGIN 0;
"#"{SpaceTab}*"else"           BEGIN IFDEF;
"#"{SpaceTab}*"endif"          BEGIN 0;

"#"{SpaceTab}*"ifndef"      BEGIN IFNDEF;
<IFNDEF>{Identifier}        if( ! FindDef( yytext ) ) BEGIN 0;
<IFNDEF>"#endif"\n          ++iLine; BEGIN 0;
<IFNDEF>\n                  ++iLine;

"#"{SpaceTab}*"line".* ;

{SpaceTab}     ;

\n.*           _iState=LOOKUP; yyless( 1 ); ++iLine; if( ! _iQuiet ) printf( "\rline: %i", iLine ); return '\n';
;\n            _iState=LOOKUP; ++iLine; if( ! _iQuiet ) printf( "\rline: %i", iLine );

"begin"{Separator}+"sequ"("ence"|"enc"|"en"|"e")?        return BEGINSEQ;
%{
/* ************************************************************************ */
%}
"break"        BEGIN BREAK_;
<BREAK_>{Separator}*\n { /* at the end of line */
               yy_lex_count_lf();
               --iLine;
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( _iState == LOOKUP )
               { /* it is first item in the line */
                 return BREAK;
               }
               else
               { /* there is another item in line already */
                 yylval.string = strdup("BREAK");
                 return IDENTIFIER;
               }
             }
<BREAK_>{Separator}*[\[] { /* array */
               yy_lex_count_lf();
               BEGIN 0;
               /* Clipper does not like break[] at all */
               GenError( ERR_SYNTAX, yytext, NULL );
             }
<BREAK_>{Separator}*[^_a-zA-Z\[] { /* there is no identifier after "break" */
               yy_lex_count_lf();
               BEGIN 0;
               yylval.string = strdup( "BREAK" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<BREAK_>{Separator}*. { /* an identifier follows BREAK statement */
               yy_lex_count_lf();
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               _iState =BREAK;
               return BREAK;
             }
<BREAK_>.    { BEGIN 0; unput( yytext[ yyleng-1 ] ); }
%{
/* ************************************************************************ */
%}
"case"       { BEGIN CASE_; }
<CASE_>{Separator}*[\:\=\|\$\%\*\,\/\]\)\}\^] { /* there is an operator after "case" */
               yy_lex_count_lf();
               BEGIN 0;
               yylval.string = strdup( "CASE" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<CASE_>{Separator}*[\[] { /* array */
               yy_lex_count_lf();
               BEGIN 0;
               /* Clipper does not like case[] at all */
               GenError( ERR_SYNTAX, yytext, NULL );
             }
<CASE_>{Separator}*("+="|"-="|"->") { /* operators */
               yy_lex_count_lf();
               BEGIN 0;
               yylval.string = strdup( "CASE" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<CASE_>{Separator}*(\n|.) { /* not operator */
               yy_lex_count_lf();
               BEGIN 0;
               if( yytext[ yyleng-1 ] == '\n' )
                 --iLine;
               unput( yytext[ yyleng-1 ] );
               if( _iState == LOOKUP )
               { /* it is first item in the line */
                 _iState =CASE;
                 return CASE;
               }
               else
               { /* there is another item in line already */
                 yylval.string = strdup( "CASE" );
                 return IDENTIFIER;
               }
             }
%{
/* ************************************************************************ */
%}
"do"            BEGIN DO_;
<DO_>{Separator}+"case"         { /* DO CASE statement */
                BEGIN 0;
                yy_lex_count_lf();
                _iState =DOCASE;
                return DOCASE;
              }
<DO_>{Separator}+"while" { /* DO WHILE found -move it to WHILE state */
                /* NOTE: we cannot decide here if it is DO WHILE <condition>
                 * or DO while [WITH <args>]
                 */
                yy_lex_count_lf();
                BEGIN 0;
                _iState =DO;
                yyless( yyleng-5 );
              }
<DO_>{Separator}+[_a-zA-Z]   { /* an identifier DO id WITH */
                yy_lex_count_lf();
                BEGIN 0;
                unput( yytext[ yyleng-1 ] );
                if( _iState == LOOKUP )
                { /* it is first item in the line */
                  _iState =DO;
                  return DO;
                }
                else
                { /* there is another item in line already */
                  yylval.string = strdup( "DO" );
                _iState =IDENTIFIER;
                  return IDENTIFIER;
                }
              }
<DO_>{Separator}*(.|\n)         { /* end of line or any operator */
                yy_lex_count_lf();
                BEGIN 0;
                if( yytext[ yyleng-1 ] == '\n' )
                  --iLine;
                unput( yytext[ yyleng-1 ] );
                yylval.string = strdup( "DO" );
                _iState =IDENTIFIER;
                return IDENTIFIER;
              }
%{
/* ************************************************************************ */
%}
"else"         { /* ELSE can be used in one context only */
                 if( _wIfCounter == 0 )
                   GenError( ERR_UNMATCHED_ELSE, NULL, NULL );
                 _iState =ELSE;
                 return ELSE;
               }
"elseif"       { /* ELSEIF can be used in one context only */
                 if( _wIfCounter == 0 )
                   GenError( ERR_UNMATCHED_ELSEIF, NULL, NULL );
                 _iState =ELSEIF;
                 return ELSEIF;
               }
"end"{Separator}+"sequ"("ence"|"enc"|"en"|"e")?  {
                 if( _wSeqCounter == 0 )
                   GenError( ERR_ENDIF, NULL, NULL );
                 return END;
               }
"end"          { BEGIN END_; }
<END_>{Separator}*[\[\(] { /* array, function call */
               yy_lex_count_lf();
               BEGIN 0;
               if( _iState == LOOKUP )
               { /* Clipper does not like end[] & end() at the begining of line */
                 GenError( ERR_ENDIF, NULL, NULL );
               }
               yylval.string = strdup( "END" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<END_>{Separator}*("->"|"++"|"--") { /* operators */
               yy_lex_count_lf();
               BEGIN 0;
               if( _iState == LOOKUP )
               { /* Clipper does not like end-> & end++ at the begining of line */
                 GenError( ERR_ENDIF, NULL, NULL );
               }
               yylval.string = strdup( "END" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<END_>{Separator}*[\+\-\:\=\|\$\%\*\,\/\[\]\)\}\^] { /* there is an operator after "end" */
               yy_lex_count_lf();
               BEGIN 0;
               yylval.string = strdup( "END" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<END_>{Separator}*(\n|.) { /* not operator */
               yy_lex_count_lf();
               BEGIN 0;
               if( yytext[ yyleng-1 ] == '\n' )
                 --iLine;
               unput( yytext[ yyleng-1 ] );
               if( _iState == LOOKUP )
               { /* it is first item in the line */
                 _iState =END;
                 return END;
               }
               else
               { /* there is another item in line already */
                 yylval.string = strdup( "END" );
                 return IDENTIFIER;
               }
             }
%{
/* ************************************************************************ */
%}
"endif"|"endi" { /* ENDIF can be used in one context only */
                 if( _wIfCounter == 0 )
                   GenError( ERR_ENDIF, NULL, NULL );
                 return ENDIF;
               }
"endc"("ase"|"as"|"a")? { /* ENDCASE can be used in one context only */
                 if( _wWhileCounter == 0 )
                   GenError( ERR_ENDCASE, NULL, NULL );
                 return ENDCASE;
               }
"enddo"|"endd" { /* ENDDO can be used in one context only */
                 if( _wWhileCounter == 0 )
                   GenError( ERR_ENDDO, NULL, NULL );
                 return ENDDO;
               }
"exit"/[\n\;]  return EXITLOOP;
"exit"         return EXIT;
"extern"|"external" _iState =EXTERN; return EXTERN;
"field"/[^(]   _iState =FIELD; return FIELD;
"for"          _iState = FOR; return FOR;
"func"|"funct"|"functi"|"functio"|"function"/[^(] _iState=FUNCTION; return FUNCTION;
("if"|"iif"){SpaceTab}*/[(]  _iState =IF; return IF;
"if"           _iState =IF; return IF;
"in"           return IN;
"include"      return INCLUDE;
"init"         return INIT;
"local"        _iState =LOCAL; return LOCAL;
"loop"         return LOOP;
"memvar"       _iState =MEMVAR; return MEMVAR;
%{
/* ************************************************************************ */
%}
"next"         BEGIN NEXT_;
<NEXT_>{Separator}*[\n\;] { /* at the end of line */
               BEGIN 0;
               if( _iState == LOOKUP )
               { /* it is first item in the line */
                 ++iLine;
                 if( _wForCounter == 0 )
                   GenError( ERR_NEXTFOR, NULL, NULL );
                 _iState =NEXT;
                 return NEXT;
               }
               else
               { /* there is another item in line already */
                 unput( yytext[ yyleng-1 ] );
                 yylval.string = strdup( "NEXT" );
                 return IDENTIFIER;
               }
             }
<NEXT_>{Separator}*[\[\(] { /* array, function call */
               yy_lex_count_lf();
               BEGIN 0;
               if( _iState == LOOKUP )
               { /* Clipper does not like NEXT[] & NEXT() at the begining of line */
                 GenError( ERR_NEXTFOR, NULL, NULL );
               }
               yylval.string = strdup( "NEXT" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<NEXT_>{Separator}*("->"|"++"|"--") { /* operators */
               yy_lex_count_lf();
               BEGIN 0;
               if( _iState == LOOKUP )
               { /* Clipper does not like next-> & next++ at the begining of line */
                 GenError( ERR_NEXTFOR, NULL, NULL );
               }
               yylval.string = strdup( "NEXT" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<NEXT_>{Separator}*[^_a-zA-Z] { /* there is no identifier after "next" */
               yy_lex_count_lf();
               BEGIN 0;
               yylval.string = strdup( "NEXT" );
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<NEXT_>{Separator}*. { /* an identifier follows NEXT statement */
               yy_lex_count_lf();
               BEGIN 0;
               if( _wForCounter == 0 )
                 GenError( ERR_NEXTFOR, NULL, NULL );
               unput( yytext[ yyleng-1 ] );
               _iState =NEXT;
               return NEXT;
             }
%{
/* ************************************************************************ */
%}
"nil"          return NIL;
"otherwise"    return OTHERWISE;
"parameters"   _iState =PARAMETERS; return PARAMETERS;
"private"      _iState =PRIVATE; return PRIVATE;
"proc"|"procedure" return PROCEDURE;
"public"       _iState =PUBLIC; return PUBLIC;
"qself"{SpaceTab}*[(]{SpaceTab}*[)] return SELF;
"recover"      _iState =RECOVER; return RECOVER;
"retu"|"retur"|"return"  _iState =RETURN; return RETURN;
"static"       _iState =STATIC; return STATIC;
"step"/[^(]    return STEP;
"to"           return TO;
"using"        return USING;
%{
/* ************************************************************************ */
%}
"while"        BEGIN WHILE_;
<WHILE_>{Separator}*\n      { /* end of line */
                yy_lex_count_lf();
                --iLine;
                BEGIN 0;
                unput( '\n' );
                if( _iState == DO )
                { /* we have DO while - replace it with while() */
                  unput( ')' ); unput( '(' );
                }
                yylval.string = strdup( "WHILE" );
                return IDENTIFIER;
              }
<WHILE_>{Separator}*[\[] { /* array */
               yy_lex_count_lf();
               BEGIN 0;
               /* Clipper does not like while[] at all */
               GenError( ERR_SYNTAX, yytext, NULL );
             }
<WHILE_>{Separator}*[\:\=\|\$\%\*\,\/\]\)\}\^] { /* there is an operator after "case" */
               yy_lex_count_lf();
               BEGIN 0;
               yylval.string = strdup( "WHILE" );
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<WHILE_>{Separator}*("+="|"-="|"->") { /* operators */
               yy_lex_count_lf();
               BEGIN 0;
               yylval.string = strdup( "WHILE" );
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<WHILE_>{Separator}*.      { /* identifiers and literals */
                yy_lex_count_lf();
                BEGIN 0;
                if( _iState == LOOKUP || _iState == DO )
                { /* it is first item in the line or after DO */
                  unput( yytext[ yyleng-1 ] );
                  _iState =WHILE;
                  return WHILE;
                }
                else
                { /* there is another item in line already */
                  unput( yytext[ yyleng-1 ] );
                  yylval.string = strdup( "WHILE" );
                  return IDENTIFIER;
                }
              }
%{
/* ************************************************************************ */
%}
"with"        BEGIN WITH_;
<WITH_>{Separator}*\n           { /* at the end of line */
                yy_lex_count_lf();
                --iLine;
                BEGIN 0;
                unput( '\n' );
                yylval.string = strdup( "WITH" );
                return IDENTIFIER;
              }
<WITH_>{Separator}*"with"       {
                yy_lex_count_lf();
                BEGIN 0;
                yyless( yyleng-4 );
                if( _iState == DO )
                { /* DO with */
                  _iState =IDENTIFIER;
                  yylval.string = strdup( "WITH" );
                  return IDENTIFIER;
                }
                else
                { /* DO WITH with <arg> */
                  _iState =WITH;
                  return WITH;
                }
              }
<WITH_>{Separator}*[\[] { /* array */
               yy_lex_count_lf();
               /* Clipper does not like with[] at all */
               GenError( ERR_SYNTAX, yytext, NULL );
             }
<WITH_>{Separator}*.            {
                yy_lex_count_lf();
                BEGIN 0;
                unput( yytext[ yyleng-1 ] );
                if( _iState == WHILE || _iState == DO || _iState == IDENTIFIER )
                { /* DO <ident> WITH <arg> */
                  _iState =WITH;
                  return WITH;
                }
                else
                {
                  yylval.string = strdup( "WITH" );
                  return IDENTIFIER;
                }
              }
%{
/* ************************************************************************ */
%}
"#"            _iState =NE1; return NE1;
"="            _iState =OPERATOR; return yytext[ 0 ];
"+"            _iState =OPERATOR; return yytext[ 0 ];
"-"            _iState =OPERATOR; return yytext[ 0 ];
"*"            _iState =OPERATOR; return yytext[ 0 ];
[\/]           _iState =OPERATOR; return yytext[ 0 ];
"%"            _iState =OPERATOR; return yytext[ 0 ];
"$"            _iState =OPERATOR; return yytext[ 0 ];
"<>"|"!="      _iState =NE2; return NE2;
":="           _iState =INASSIGN; return INASSIGN;
"=="           _iState =EQ; return EQ;
"++"           _iState =INC; return INC;
"--"           _iState =DEC; return DEC;
"->"           _iState =ALIAS; return ALIAS;
"<="           _iState =LE; return LE;
">="           _iState =GE; return GE;
"+="           _iState =PLUSEQ; return PLUSEQ;
"-="           _iState =MINUSEQ; return MINUSEQ;
"*="           _iState =MULTEQ; return MULTEQ;
"/="           _iState =DIVEQ; return DIVEQ;
"^="           _iState =EXPEQ; return EXPEQ;
"%="           _iState =MODEQ; return MODEQ;
"**"|"^"       _iState =POWER; return POWER;
"."[t|y]"."    return TRUEVALUE;
"."[f|n]"."    return FALSEVALUE;
".and."        return AND;
".or."         return OR;
"!"|".not."    return NOT;
"::"           unput( ':' ); unput( 'f' ); unput( 'l' ); unput( 'e' ); unput( 'S' );
[,\;\{\}\|\#\&\:\<\>\[\]\(\)\@] return yytext[ 0 ];

{InvalidNumber} GenError( ERR_NUMERIC_FORMAT, NULL, NULL );

{Number}       { yylval.dNumber = atof( yytext );
                 if( strchr( yytext, '.' ) )
                    return DOUBLE;
                 else
                 {
                    if( ( double )SHRT_MIN <= yylval.dNumber &&
                        yylval.dNumber <= ( double )SHRT_MAX )
                    {
                       yylval.iNumber = ( int ) yylval.dNumber;
                       return INTEGER;
                    }
                    else if( ( double )LONG_MIN <= yylval.dNumber &&
                             yylval.dNumber <= ( double )LONG_MAX )
                    {
                       yylval.lNumber = ( long ) yylval.dNumber;
                       return INTLONG;
                    }
                    else
                       return DOUBLE;
                 }
               }

{HexNumber}   {  sscanf( yytext, "%lxI", &lNumber );

                 if( ( double )SHRT_MIN <= lNumber &&
                        lNumber <= ( double )SHRT_MAX )
                 {
                    yylval.iNumber = lNumber;
                    return INTEGER;
                 }
                 else if( ( double )LONG_MIN <= lNumber &&
                          lNumber <= ( double )LONG_MAX )
                 {
                    yylval.lNumber = lNumber;
                    return INTLONG;
                 }
                 else
                 {
                    yylval.dNumber = lNumber;
                    return DOUBLE;
                 }
              }

{String}      yylval.string = strdup( yytext + 1 ); yylval.string[ yyleng - 2 ] = 0; return LITERAL;

{LineCont}    ++iLine;

{Identifier}               {
                              PDEFINE pDef = FindDef( yytext );
                              char * szText; int c;
                              _iState = IDENTIFIER;
                              if( pDef )
                              {
                                 c = strlen( pDef->szValue ) - 1;
                                 szText = pDef->szValue;
                                 while( c >= 0 )
                                    unput( szText[ c-- ] );
                              }
                              else
                              {
                                 if( _iRestrictSymbolLength && strlen( yytext ) > 10 )
                                 {
                                    yytext[ 10 ] = 0;
                                    yyleng = 10;
                                 }
                                 yylval.string = strupr( strdup( yytext ) );
                                 return IDENTIFIER;
                              }
                           }

%%

PDEFINE LastDef( PDEFINE pDef )
{
   while( pDef->pNext )
      pDef = (PDEFINE) pDef->pNext;

   return pDef;
}

void Define( char * szDefine )
{
   PDEFINE pDef = ( PDEFINE ) malloc( sizeof( _DEFINE ) );

   if( ! pDefs )
      pDefs = pDef;
   else
      LastDef( pDefs )->pNext = pDef;

   pDef->szDefine = strdup( szDefine );
   pDef->szValue  = 0;
   pDef->pKeys    = 0;
   pDef->pNext    = 0;
}

void DefineKey( char * szKey )
{
   PDEFINE pDef = ( PDEFINE ) malloc( sizeof( _DEFINE ) );
   PDEFINE pLast = LastDef( pDefs );

   if( pLast->pKeys )
     LastDef( (PDEFINE) pLast->pKeys )->pNext = pDef;
   else
     pLast->pKeys = pDef;

   pDef->szDefine = strdup( szKey );
   pDef->szValue  = 0;
   pDef->pKeys    = 0;
   pDef->pNext    = 0;
}

PDEFINE FindDef( char * szText )
{
   PDEFINE pDef = pDefs;

   while( pDef )
   {
      if( ! strcmp( pDef->szDefine, szText ) )
         return pDef;
      else
      {
         if( pDef->pNext )
            pDef = (PDEFINE) pDef->pNext;
         else
            return 0;
      }
   }
   return 0;
}

void AddDefine( char * szId, char * szValue ) /* adds a new define from command line */
{
   PDEFINE pDefine = FindDef( szId ); /* just incase there are several repited defines */

   if( pDefine )
   {
      if( pDefine->szValue )
      {
         free( pDefine->szValue );
         pDefine->szValue = 0;
      }
      if( szValue )
         pDefine->szValue = strdup( szValue );
   }
   else
   {
      Define( szId );
      if( szValue )
         LastDef( pDefs )->szValue = strdup( szValue );
   }
}

int yy_lex_input( char *buffer, int iBufferSize )
{
   int i;
   static int _ineedLF = 1; /* we want LF as the last character */

   if( ((i = fread(buffer, 1, iBufferSize, yyin)) == 0) && ferror(yyin) )
       YY_FATAL_ERROR( "input in flex scanner failed" );

   if( i == iBufferSize )
     _ineedLF =( buffer[i-1] != '\n' ); /* check if last character is LF */
                                        /* in case it is the last block */
   else if( i < iBufferSize && _ineedLF )
   {
     if( i == 0 )
       buffer[ i++ ] ='\n';
     else if( buffer[ i-1 ] != '\n' )
       buffer[ i++ ] ='\n';
     _ineedLF =0;              /* OK - no more LF */
   }
   return i;
}
