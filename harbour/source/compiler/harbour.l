%{
/*
 * $Id$
 */

/*
 * Harbour lex rules.
 * Build 27: Summer 99
 * Usage: flex -i -8 -oyylex.c harbour.l
 * You may find flex.exe at www.harbour-project.org
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "harboury.h"
#include "hbsetup.h"    /* main configuration file */
#include "hberrors.h"
#include "types.h"

void yyerror( char * );
char *yy_strupr( char * );
char * yy_strdup( char *p );
static void yyunput( int, char * );
#undef yywrap        /* to implement our own yywrap() funtion to handle EOFs */
#ifdef __cplusplus
extern "C" int yywrap( void );
#else
int yywrap( void );
#endif
#undef YY_INPUT      /* to implement our own YY_INPUT function to manage PRGs without \n at the end */
extern FILE * yyin;  /* currently yacc parsed file */
  /* Following three lines added for preprocessor */
extern FILE *yyppo;  /* output .ppo file */
extern int lPpo;     /* flag indicating, is ppo output needed */
extern int PreProcess( FILE*, FILE*, char *);
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
int iLine = 0;
long lNumber = 0;

#define LOOKUP          0       /* scan from the begining of line */
#define OPERATOR        -1
#define SEPARATOR       -2
static int _iState = LOOKUP;
static int _iOpenBracket = 0;

/* Support for Array Index */
int iIndexSets  = 0;
int i_INDEX_STATE = 0;

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

%{
#ifdef __WATCOMC__
/* disable warnings for unreachable code */
#pragma warning 13 9
#endif
%}

SpaceTab      [ \t]+
InvalidNumber [0-9]+\.
Number        ([0-9]+)|([0-9]*\.[0-9]+)
HexNumber     0x[0-9A-F]+
Identifier    (([a-zA-Z])|([_a-zA-Z][_a-zA-Z0-9]+))

/* TODO check if String definition can be removed - Ron Pinkas added support for [] String Delimiters see STRING1, STRING2 and STRING3*/
/*String        (\"(([^\"]*)|([\!]*))\")|(\'(([^\']*)|([\!]*))\')*/

PseudoFunc    {Identifier}"("+.*")"+

Array         {Identifier}[ \t]*"["
ExpArray      ")"[ \t]*"["
SubArray      "]"[ \t]*"["

Comment1      "/*"([^\*]|[\*][^\/])*"*/"
Comment2      [\/][\/].*
Comment       ({Comment1}|{Comment2})
Separator     {SpaceTab}|{Comment}

%x            COMMENT3 DEFINE DEFINE_PARAMS DEFINE_EXPR
%x            IFDEF IFNDEF STRING1 STRING2 STRING3
%x            NEXT_ BREAK_ CASE_ DO_ WHILE_ WITH_ END_ EXIT_ EXTERNAL_ FIELD_
%x            FOR_ FUNCTION_ IIF_ IF_ IN_ INCLUDE_ INIT_ LOCAL_ LOOP_
%x            LINECONT_
%s            INDEX

%%

("//".*)|("&&".*)   ;

^{SpaceTab}*(\*|"NOTE").*  ;
\n{SpaceTab}*(\*|"NOTE").*  ++iLine; if( ! _iQuiet ) printf( "\rline: %i", iLine );

"&"("'"|\"|\[) { printf( "\nSyntax error : '%s'\n", yytext ); exit(1); }

'              BEGIN STRING1;
\"             BEGIN STRING2;

"="[ \t]*"["                         { BEGIN STRING3; _iState =OPERATOR; return '=';      }
"+"[ \t]*"["                         { BEGIN STRING3; _iState =OPERATOR; return '+';      }
"-"[ \t]*"["                         { BEGIN STRING3; _iState =OPERATOR; return '-';      }
"*"[ \t]*"["                         { BEGIN STRING3; _iState =OPERATOR; return '*';      }
"/"[ \t]*"["                         { BEGIN STRING3; _iState =OPERATOR; return '/';      }
"%"[ \t]*"["                         { BEGIN STRING3; _iState =OPERATOR; return '%';      }
"$"[ \t]*"["                         { BEGIN STRING3; _iState =OPERATOR; return '$';      }
("<>"|"!=")[ \t]*"["                 { BEGIN STRING3; _iState =NE2;      return NE2;      }
":="[ \t]*"["                        { BEGIN STRING3; _iState =INASSIGN; return INASSIGN; }
"=="[ \t]*"["                        { BEGIN STRING3; _iState =EQ;       return EQ;       }
"<="[ \t]*"["                        { BEGIN STRING3; _iState =LE;       return LE;       }
">="[ \t]*"["                        { BEGIN STRING3; _iState =GE;       return GE;       }
"+="[ \t]*"["                        { BEGIN STRING3; _iState =PLUSEQ;   return PLUSEQ;   }
"-="[ \t]*"["                        { BEGIN STRING3; _iState =MINUSEQ;  return MINUSEQ;  }
"*="[ \t]*"["                        { BEGIN STRING3; _iState =MULTEQ;   return MULTEQ;   }
"/="[ \t]*"["                        { BEGIN STRING3; _iState =DIVEQ;    return DIVEQ;    }
"^="[ \t]*"["                        { BEGIN STRING3; _iState =EXPEQ;    return EXPEQ;    }
"%="[ \t]*"["                        { BEGIN STRING3; _iState =MODEQ;    return MODEQ;    }
("**"|"^")[ \t]*"["                  { BEGIN STRING3; _iState =POWER;    return POWER;    }
".and."[ \t]*"["                     { BEGIN STRING3;                    return AND;      }
".or."[ \t]*"["                      { BEGIN STRING3;                    return OR;       }
("!"|".not.")[ \t]*"["               { BEGIN STRING3;                    return NOT;      }
(","|"{"|"<"|">"|"(")[ \t]*"["   { BEGIN STRING3; _iState = OPERATOR; yyleng = 1; yytext[1] = 0; return yytext[ 0 ]; }

<INITIAL>\[    BEGIN STRING3;

<STRING1>[^'^\n]*   { GenError( ERR_STRING_TERMINATOR, yytext, NULL ); BEGIN 0; }
<STRING2>[^\"^\n]*  { GenError( ERR_STRING_TERMINATOR, yytext, NULL ); BEGIN 0; }
<STRING3>[^\]]*\n   { GenError( ERR_STRING_TERMINATOR, yytext, NULL ); BEGIN 0; }

<STRING1>[^']*'   { if( i_INDEX_STATE )
                       BEGIN INDEX;
                    else
                       BEGIN 0;

                    yyleng--;
                    yytext[yyleng] = 0;
                    yylval.string = yy_strdup( yytext );
                    /*printf( "\nLITERAL = %s\n", yylval.string );*/
                    return LITERAL;
                  }

<STRING2>[^\"]*\" { if( i_INDEX_STATE )
                       BEGIN INDEX;
                    else
                       BEGIN 0;

                    yyleng--;
                    yytext[yyleng] = 0;
                    yylval.string = yy_strdup( yytext );
                    /*printf( "\nLITERAL = %s\n", yylval.string );*/
                    return LITERAL;
                  }

<STRING3>[^\]]*\] { if( i_INDEX_STATE )
                       BEGIN INDEX;
                    else
                       BEGIN 0;

                    yyleng--;
                    yytext[yyleng] = 0;
                    yylval.string = yy_strdup( yytext );
                    /*printf( "\nLITERAL = %s\n", yylval.string );*/
                    return LITERAL;
                  }

<INDEX>\n    { yyerror( "Unterminated Array Index" ); exit(1); }

<INDEX>\[    { iIndexSets++; return yytext[ 0 ]; }

<INDEX>\]    {
                iIndexSets-- ;
                if( iIndexSets == 0 )
                {
                   /*printf( "\nIndex End\n" );*/

                   /* No longer in this state. */
                   i_INDEX_STATE = 0;
                   BEGIN 0;
                }
                _iState =OPERATOR;
                return yytext[ 0 ];
             }

"/*"           BEGIN COMMENT3;
<COMMENT3>"*"["*"]*"/" { if( i_INDEX_STATE )
                         {
                            BEGIN INDEX;
                            /*printf( "\nRESUMING INDEX STATE\n" );*/
                         }
                         else
                            BEGIN 0;
                       }

<COMMENT3>[^"*/"\n]* ;
<COMMENT3>"*"     ;
<COMMENT3>[\/\"]+ ;
<COMMENT3>\n   ++iLine; if( ! _iQuiet ) printf( "\rline: %i", iLine );

"#"{SpaceTab}*"define"      BEGIN DEFINE;
<DEFINE>{Identifier}/{SpaceTab}+   Define( yytext );
<DEFINE>{Identifier}/{SpaceTab}*\n Define( yytext ); BEGIN 0;
<DEFINE>{SpaceTab}          ;
<DEFINE>{Identifier}        LastDef( pDefs )->szValue = yy_strdup( yytext ); BEGIN 0;
<DEFINE>{PseudoFunc}        LastDef( pDefs )->szValue = yy_strdup( yytext ); BEGIN 0;
<DEFINE>-?{Number}          LastDef( pDefs )->szValue = yy_strdup( yytext ); BEGIN 0;
<DEFINE>{HexNumber}         LastDef( pDefs )->szValue = yy_strdup( yytext ); BEGIN 0;
<DEFINE>"/*".*"*/"          ;
<DEFINE>{Number}{SpaceTab}*[\(]    yyerror( "Syntax error in #define" );

<DEFINE>{Identifier}{SpaceTab}*[\(] Define( yytext ); BEGIN DEFINE_PARAMS;
<DEFINE_PARAMS>{SpaceTab}   ;
<DEFINE_PARAMS>{Identifier} DefineKey( yytext );
<DEFINE_PARAMS>[\,]         ;
<DEFINE_PARAMS>[\)]         BEGIN DEFINE_EXPR;
<DEFINE_EXPR>.*/\n          LastDef( pDefs )->szValue = yy_strdup( yytext ); BEGIN 0;

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
%{
/* ************************************************************************ */
%}
;              BEGIN LINECONT_;
<LINECONT_>{Separator}*\n {
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               if( ! _iQuiet ) printf( "\rline: %i", iLine );
               _iState=LINECONT_;
             }
<LINECONT_>{Separator}*("("|")") {
               GenError( ERR_INCOMPLETE_STMT, yytext, NULL );
             }
<LINECONT_>{Separator}*. {
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( _iOpenBracket == 0 && (_iState==SEPARATOR || _iState==IDENTIFIER) && i_INDEX_STATE==0 )
               {
                 _iState=LOOKUP;
                 return '\n';
               }
               else
                 _iState=LINECONT_;
             }
%{
/* ************************************************************************ */
%}
"begin"{Separator}+"sequ"("ence"|"enc"|"en"|"e")?        return BEGINSEQ;
%{
/* ************************************************************************ */
%}
"break"        BEGIN BREAK_;
<BREAK_>{Separator}*\n { /* at the end of line */
               yy_lex_count_lf();
               --iLine;
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( _iState == LOOKUP )
               { /* it is first item in the line */
                 return BREAK;
               }
               else
               { /* there is another item in line already */
                 yylval.string = yy_strdup("BREAK");
                 return IDENTIFIER;
               }
             }
<BREAK_>{Separator}*[\[] { /* array */
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               /* Clipper does not like break[] at all */
               GenError( ERR_SYNTAX, yytext, NULL );
             }
<BREAK_>{Separator}*;.*\n      yy_lex_count_lf(); /* ignore any text after ';' */
<BREAK_>{Separator}*[^_a-zA-Z\[] { /* there is no identifier after "break" */
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = yy_strdup( "BREAK" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<BREAK_>{Separator}*. { /* an identifier follows BREAK statement */
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( _iState == LOOKUP )
               {
                 _iState =BREAK;
                 return BREAK;
               }
               else
               {
                 yylval.string = yy_strdup( "BREAK" );
                 _iState =IDENTIFIER;
                 return IDENTIFIER;
               }
             }
<BREAK_>.    { if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0; unput( yytext[ yyleng-1 ] ); }
%{
/* ************************************************************************ */
%}
"case"         BEGIN CASE_;
<CASE_>{Separator}*[\:\=\|\$\%\*\,\/\]\)\}\^] { /* there is an operator after "case" */
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = yy_strdup( "CASE" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<CASE_>{Separator}*[\[] { /* array */
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               /* Clipper does not like case[] at all */
               GenError( ERR_SYNTAX, yytext, NULL );
             }
<CASE_>{Separator}*("+="|"-="|"->") { /* operators */
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = yy_strdup( "CASE" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<CASE_>{Separator}*;.*\n      yy_lex_count_lf(); /* ignore any text after ';' */
<CASE_>{Separator}*(\n|.) { /* not operator */
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
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
                 yylval.string = yy_strdup( "CASE" );
                 _iState =IDENTIFIER;
                 return IDENTIFIER;
               }
             }
%{
/* ************************************************************************ */
%}
"do"            BEGIN DO_;
<DO_>{Separator}+"case"         { /* DO CASE statement */
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                yy_lex_count_lf();
                _iState =DOCASE;
                return DOCASE;
              }
<DO_>{Separator}+"while" { /* DO WHILE found -move it to WHILE state */
                /* NOTE: we cannot decide here if it is DO WHILE <condition>
                 * or DO while [WITH <args>]
                 */
                yy_lex_count_lf();
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                _iState =DO;
                yyless( yyleng-5 );
              }
<DO_>{Separator}+[_a-zA-Z]   { /* an identifier DO id WITH */
                yy_lex_count_lf();
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                unput( yytext[ yyleng-1 ] );
                if( _iState == LOOKUP )
                { /* it is first item in the line */
                  _iState =DO;
                  return DO;
                }
                else
                { /* there is another item in line already */
                  yylval.string = yy_strdup( "DO" );
                  _iState =IDENTIFIER;
                  return IDENTIFIER;
                }
              }
<DO_>{Separator}*;.*\n      yy_lex_count_lf(); /* ignore any text after ';' */
<DO_>{Separator}*(.|\n)         { /* end of line or any operator */
                yy_lex_count_lf();
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                if( yytext[ yyleng-1 ] == '\n' )
                  --iLine;
                unput( yytext[ yyleng-1 ] );
                yylval.string = yy_strdup( "DO" );
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
%{
/* ************************************************************************ */
%}
"end"          { BEGIN END_; }
<END_>{Separator}*[\[\(] { /* array, function call */
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               if( _iState == LOOKUP )
               { /* Clipper does not like end[] & end() at the begining of line */
                 GenError( ERR_ENDIF, NULL, NULL );
               }
               yylval.string = yy_strdup( "END" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<END_>{Separator}*("->"|"++"|"--") { /* operators */
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               if( _iState == LOOKUP )
               { /* Clipper does not like end-> & end++ at the begining of line */
                 GenError( ERR_ENDIF, NULL, NULL );
               }
               yylval.string = yy_strdup( "END" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<END_>{Separator}*[\+\-\:\=\|\$\%\*\,\/\[\]\)\}\^] { /* there is an operator after "end" */
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = yy_strdup( "END" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<END_>{Separator}*;.*\n      yy_lex_count_lf(); /* ignore any text after ';' */
<END_>{Separator}*(\n|.) { /* not operator */
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
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
                 yylval.string = yy_strdup( "END" );
                 _iState =IDENTIFIER;
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
                 if( _wCaseCounter == 0 )
                   GenError( ERR_ENDCASE, NULL, NULL );
                 return ENDCASE;
               }
"enddo"|"endd" { /* ENDDO can be used in one context only */
                 if( _wWhileCounter == 0 )
                   GenError( ERR_ENDDO, NULL, NULL );
                 return ENDDO;
               }
%{
/* ************************************************************************ */
%}
"exit"         { BEGIN EXIT_; }
<EXIT_>{Separator}*[\n]    { /* EXIT last item in the line */
                 yy_lex_count_lf();
                 --iLine;
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( _iState == LOOKUP )
                 { /* it is first item in the line */
                   if( _wForCounter == 0 && _wWhileCounter == 0 )
                     GenError( ERR_UNMATCHED_EXIT, "EXIT", NULL );
                   _iState =EXITLOOP;
                   return EXITLOOP;
                 }
                 else
                 { /* there is another item in line already */
                   yylval.string = yy_strdup( "EXIT" );
                   _iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<EXIT_>{Separator}+[fFpP] { /* FUNCTION or PROCEDURE after EXIT */
                 yy_lex_count_lf();
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( _iState == LOOKUP )
                 { /* it is first item in the line */
                   _iState =EXIT;
                   return EXIT;
                 }
                 else
                 { /* there is another item in line already */
                   yylval.string = yy_strdup( "EXIT" );
                   _iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<EXIT_>{Separator}*;.*\n      yy_lex_count_lf(); /* ignore any text after ';' */
<EXIT_>{Separator}*. { /* any character (not identifier) after EXIT */
                 yy_lex_count_lf();
                 unput( yytext[ yyleng-1 ] );
                 yylval.string = yy_strdup( "EXIT" );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 _iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"exte"|"exter"|"extern"|"externa"|"external" { BEGIN EXTERNAL_;
                   yylval.string = yy_strupr( yy_strdup( yytext ) );
                 }
<EXTERNAL_>{Separator}+[_a-zA-Z] { /* an identifier after the EXTERNAL */
                 yy_lex_count_lf();
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( _iState == LOOKUP )
                 {
                   free( yylval.string );
                   _iState =EXTERN;
                   return EXTERN;
                 }
                 else
                 {
                   _iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<EXTERNAL_>{Separator}*;.*\n      yy_lex_count_lf(); /* ignore any text after ';' */
<EXTERNAL_>{Separator}*[^_a-zA-Z]   {
                 yy_lex_count_lf();
                 if( yytext[ yyleng-1 ] == '\n' )
                   --iLine;
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 _iState =IDENTIFIER;
                 return IDENTIFIER;
               }
<EXTERNAL_>.    { if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0; unput( yytext[ yyleng-1 ] ); }
%{
/* ************************************************************************ */
%}
"fiel"|"field"   { BEGIN FIELD_;
                   yylval.string = yy_strupr( yy_strdup( yytext ) );
                 }
<FIELD_>{Separator}+[_a-zA-Z] { /* an identifier after the FIELD */
                 yy_lex_count_lf();
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( _iState == LOOKUP )
                 {
                   free( yylval.string );
                   _iState =FIELD;
                   return FIELD;
                 }
                 else
                 {
                   _iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<FIELD_>{Separator}*;.*\n      yy_lex_count_lf(); /* ignore any text after ';' */
<FIELD_>{Separator}*[^_a-zA-Z]   {
                 yy_lex_count_lf();
                 if( yytext[ yyleng-1 ] == '\n' )
                   --iLine;
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 _iState =IDENTIFIER;
                 return IDENTIFIER;
               }
<FIELD_>.     { if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0; unput( yytext[ yyleng-1 ] ); }
%{
/* ************************************************************************ */
%}
"for"          { BEGIN FOR_; }
<FOR_>{Separator}+[_a-zA-Z] { /* an identifier after the FOR */
                 yy_lex_count_lf();
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( _iState == LOOKUP )
                 {
                   _iState =FOR;
                   return FOR;
                 }
                 else
                 { /* for example: DO for WITH variable */
                   yylval.string = yy_strdup( "FOR" );
                   _iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<FOR_>{Separator}*[\(] { /* function call */
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               if( _iState == LOOKUP )
               { /* Clipper does not like FOR() at the begining of line */
                 GenError( ERR_SYNTAX, yytext, NULL );
               }
               yylval.string = yy_strdup( "FOR" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<FOR_>{Separator}*;.*\n      yy_lex_count_lf(); /* ignore any text after ';' */
<FOR_>{Separator}*[^_a-zA-Z] { /* there is no identifier after "FOR" */
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               if( yytext[ yyleng-1 ] == '\n' )
                 --iLine;
               yylval.string = yy_strdup( "FOR" );
               unput( yytext[ yyleng-1 ] );
               _iState =IDENTIFIER;
               return IDENTIFIER;
             }
%{
/* ************************************************************************ */
%}
"func"|"funct"|"functi"|"functio"|"function" { BEGIN FUNCTION_; }
<FUNCTION_>{Separator}+[_a-zA-Z] {
               yy_lex_count_lf();
               BEGIN 0; /* we can don't care about INDEX_STATE here */
               unput( yytext[ yyleng-1 ] );
               _iState=FUNCTION;
               return FUNCTION;
             }
<FUNCTION_>{Separator}*;.*\n      yy_lex_count_lf(); /* ignore any text after ';' */
<FUNCTION_>{Separator}*[^_a-zA-Z] { /* Clipper needs FUNCTION in one context only */
               GenError( ERR_SYNTAX, ((yytext[ yyleng-1 ]=='\n')?"FUNCTION":yytext), NULL );
             }
%{
/* ************************************************************************ */
%}
"iif"          {
                 if( _iState == FUNCTION || _iState == PROCEDURE )
                   GenError( ERR_SYNTAX, "IIF", NULL );
                 else
                   BEGIN IIF_;
               /* Note: In Clipper:
                  IIF( expression )
                  ENDIF
                 is not a valid statement -this is why we have to separate
                 IF and IIF
               */
            }
<IIF_>{Separator}*"(" {
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               _iState=IIF;
               return IIF;
             }
<IIF_>{Separator}*;.*\n      yy_lex_count_lf(); /* ignore any text after ';' */
<IIF_>{Separator}*[^\(] {
               GenError( ERR_SYNTAX, ((yytext[ yyleng-1 ]=='\n')?"IIF":yytext), NULL );
             }
%{
/* ************************************************************************ */
%}
"if"           {
                 if( _iState == FUNCTION || _iState == PROCEDURE )
                   GenError( ERR_SYNTAX, "IF", NULL );
                 else
                   BEGIN IF_;
              }
<IF_>{Separator}*"(" {
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( _iState == LOOKUP )
                 _iState =IF;
               else
                 _iState =IIF;
               return _iState;
             }
<IF_>{Separator}*[\)\[\]\/\^\*\%\=\$\@] {
               GenError( ERR_SYNTAX2, yytext, "IF" );
             }
<IF_>{Separator}*"->" {
               GenError( ERR_SYNTAX2, yytext, "IF" );
             }
<IF_>{Separator}*[\n] {
               GenError( ERR_SYNTAX, "IF", NULL );
             }
<IF_>{Separator}*("++"|"--")/[\n] {
               GenError( ERR_SYNTAX2, yytext, "IF" );
             }
<IF_>{Separator}*;.*\n      yy_lex_count_lf(); /* ignore any text after ';' */
<IF_>{Separator}*. {
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               _iState =IF;
               return IF;
             }
%{
/* ************************************************************************ */
%}
"in"           BEGIN IN_;
<IN_>{Separator}+[_a-zA-Z]       {
                 yy_lex_count_lf();
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( _iState == IDENTIFIER )
                   return IN;
                 else
                 {
                   yylval.string =yy_strdup( "IN" );
                   _iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
             }
<IN_>{Separator}*\n     {
               yy_lex_count_lf();
               --iLine;
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               yylval.string =yy_strdup( "IN" );
               _iState =IDENTIFIER;
               return IDENTIFIER;
             }
<IN_>{Separator}*[0-9]      {
                GenError( ERR_SYNTAX, yytext, NULL );
             }
<IN_>{Separator}*;.*\n      yy_lex_count_lf(); /* ignore any text after ';' */
<IN_>{Separator}*.      {
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               yylval.string =yy_strdup( "IN" );
               _iState =IDENTIFIER;
               return IDENTIFIER;
             }
%{
/* ************************************************************************ */
%}
"include"      BEGIN INCLUDE_;
<INCLUDE_>{Separator}+\" { /* only "" are allowed for filename */
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               _iState =INCLUDE;
               return INCLUDE;
             }
<INCLUDE_>{Separator}*;.*\n      yy_lex_count_lf(); /* ignore any text after ';' */
<INCLUDE_>{Separator}*[^\"] {
               yy_lex_count_lf();
               if( yytext[ yyleng-1 ] == '\n' )
                 --iLine;
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               yylval.string =yy_strdup( "INCLUDE" );
               _iState =IDENTIFIER;
               return IDENTIFIER;
             }
%{
/* ************************************************************************ */
%}
"init"         BEGIN INIT_;
<INIT_>{Separator}+[fFpP] { /* FUNCTION or PROCEDURE after INIT */
                 yy_lex_count_lf();
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( _iState == LOOKUP )
                 { /* it is first item in the line */
                   _iState =INIT;
                   return INIT;
                 }
                 else
                 { /* there is another item in line already */
                   yylval.string = yy_strdup( "INIT" );
                   _iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<INIT_>{Separator}*;.*\n      yy_lex_count_lf(); /* ignore any text after ';' */
<INIT_>{Separator}*[^fFpP] { /* any character (not identifier) after EXIT */
                 yy_lex_count_lf();
                 if( yytext[ yyleng-1 ] == '\n' )
                   --iLine;
                 unput( yytext[ yyleng-1 ] );
                 yylval.string = yy_strdup( "INIT" );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 _iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"local"        BEGIN LOCAL_;
<LOCAL_>{Separator}+[_a-zA-Z]   { /* an identifier after LOCAL */
                 yy_lex_count_lf();
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( _iState == LOOKUP )
                 { /* it is first item in the line */
                   _iState =LOCAL;
                   return LOCAL;
                 }
                 else
                 { /* there is another item in line already */
                   yylval.string = yy_strdup( "LOCAL" );
                   _iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<LOCAL_>{Separator}*;.*\n      yy_lex_count_lf(); /* ignore any text after ';' */
<LOCAL_>{Separator}*[^a-zA-Z] { /* any character (not identifier) after LOCAL */
                 yy_lex_count_lf();
                 if( yytext[ yyleng-1 ] == '\n' )
                   --iLine;
                 unput( yytext[ yyleng-1 ] );
                 yylval.string = yy_strdup( "LOCAL" );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 _iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"loop"         BEGIN LOOP_;
<LOOP_>{Separator}*\n   { /* at the end of the line */
                 yy_lex_count_lf();
                 --iLine;
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( _iState == LOOKUP )
                 { /* it is first item in the line */
                   if( _wWhileCounter == 0 && _wForCounter == 0 )
                     GenError( ERR_UNMATCHED_EXIT, "LOOP", NULL );
                   _iState =LOOP;
                   return LOOP;
                 }
                 else
                 { /* there is another item in line already */
                   yylval.string = yy_strdup( "LOOP" );
                   _iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<LOOP_>{Separator}*. { /* any character (not LF) after LOOP */
                 yy_lex_count_lf();
                 unput( yytext[ yyleng-1 ] );
                 yylval.string = yy_strdup( "LOOP" );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 _iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"memvar"       _iState =MEMVAR; return MEMVAR;
%{
/* ************************************************************************ */
%}
"next"         BEGIN NEXT_;
<NEXT_>{Separator}*[\n\;] { /* at the end of line */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               if( yytext[ yyleng-1 ] == '\n' )
                 --iLine;
               unput( yytext[ yyleng-1 ] );
               if( _iState == LOOKUP )
               { /* it is first item in the line */
                 if( _wForCounter == 0 )
                   GenError( ERR_NEXTFOR, NULL, NULL );
                 _iState =NEXT;
                 return NEXT;
               }
               else
               { /* there is another item in line already */
                 unput( yytext[ yyleng-1 ] );
                 yylval.string = yy_strdup( "NEXT" );
                 _iState =IDENTIFIER;
                 return IDENTIFIER;
               }
             }
<NEXT_>{Separator}*[\[\(] { /* array, function call */
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               if( _iState == LOOKUP )
               { /* Clipper does not like NEXT[] & NEXT() at the begining of line */
                 GenError( ERR_NEXTFOR, NULL, NULL );
               }
               yylval.string = yy_strdup( "NEXT" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<NEXT_>{Separator}*("->"|"++"|"--") { /* operators */
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               if( _iState == LOOKUP )
               { /* Clipper does not like next-> & next++ at the begining of line */
                 GenError( ERR_NEXTFOR, NULL, NULL );
               }
               yylval.string = yy_strdup( "NEXT" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<NEXT_>{Separator}*[^_a-zA-Z] { /* there is no identifier after "next" */
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = yy_strdup( "NEXT" );
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<NEXT_>{Separator}*. { /* an identifier follows NEXT statement */
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( _iState == LOOKUP )
               {
                 if( _wForCounter == 0 )
                   GenError( ERR_NEXTFOR, NULL, NULL );
                 _iState =NEXT;
                 return NEXT;
               }
               else
               {
                 yylval.string = yy_strdup( "NEXT" );
                 _iState =IDENTIFIER;
                 return IDENTIFIER;
               }
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
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                unput( '\n' );
                if( _iState == DO )
                { /* we have DO while - replace it with while() */
                  unput( ')' ); unput( '(' );
                }
                yylval.string = yy_strdup( "WHILE" );
                return IDENTIFIER;
              }
<WHILE_>{Separator}*[\[] { /* array */
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               /* Clipper does not like while[] at all */
               GenError( ERR_SYNTAX, yytext, NULL );
             }
<WHILE_>{Separator}*[\:\=\|\$\%\*\,\/\]\)\}\^] { /* there is an operator after "case" */
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = yy_strdup( "WHILE" );
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<WHILE_>{Separator}*("+="|"-="|"->") { /* operators */
               yy_lex_count_lf();
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = yy_strdup( "WHILE" );
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<WHILE_>{Separator}*;.*\n      yy_lex_count_lf(); /* ignore any text after ';' */
<WHILE_>{Separator}*.      { /* identifiers and literals */
                yy_lex_count_lf();
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                unput( yytext[ yyleng-1 ] );
                if( _iState == LOOKUP || _iState == DO )
                { /* it is first item in the line or after DO or FIELD */
                  _iState =WHILE;
                  return WHILE;
                }
                else
                { /* there is another item in line already */
                  yylval.string = yy_strdup( "WHILE" );
                  _iState =IDENTIFIER;
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
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                unput( '\n' );
                yylval.string = yy_strdup( "WITH" );
                return IDENTIFIER;
              }
<WITH_>{Separator}*"with"       {
                yy_lex_count_lf();
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                yyless( yyleng-4 );
                if( _iState == DO )
                { /* DO with */
                  _iState =IDENTIFIER;
                  yylval.string = yy_strdup( "WITH" );
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
<WITH_>{Separator}*;.*\n      yy_lex_count_lf(); /* ignore any text after ';' */
<WITH_>{Separator}*.            {
                yy_lex_count_lf();
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                unput( yytext[ yyleng-1 ] );
                if( _iState == WHILE || _iState == DO || _iState == IDENTIFIER )
                { /* DO <ident> WITH <arg> */
                  _iState =WITH;
                  return WITH;
                }
                else
                {
                  yylval.string = yy_strdup( "WITH" );
                  _iState =IDENTIFIER;
                  return IDENTIFIER;
                }
              }

"as numeric"    { return AS_NUMERIC; }
"as character"  { return AS_CHARACTER; }
"as logical"    { return AS_LOGICAL; }
"as date"       { return AS_DATE; }
"as array"      { return AS_ARRAY; }
"as object"     { return AS_OBJECT; }

%{
/* ************************************************************************ */
%}
"#"            _iState =OPERATOR; return NE1;
"="            _iState =OPERATOR; return yytext[ 0 ];
"+"            _iState =OPERATOR; return yytext[ 0 ];
"-"            _iState =OPERATOR; return yytext[ 0 ];
"*"            _iState =OPERATOR; return yytext[ 0 ];
[\/]           _iState =OPERATOR; return yytext[ 0 ];
"%"            _iState =OPERATOR; return yytext[ 0 ];
"$"            _iState =OPERATOR; return yytext[ 0 ];
"<>"|"!="      _iState =OPERATOR; return NE2;
":="           _iState =OPERATOR; return INASSIGN;
"=="           _iState =OPERATOR; return EQ;
"++"           _iState =OPERATOR; return INC;
"--"           _iState =OPERATOR; return DEC;
"->"           _iState =OPERATOR; return ALIAS;
"<="           _iState =OPERATOR; return LE;
">="           _iState =OPERATOR; return GE;
"+="           _iState =OPERATOR; return PLUSEQ;
"-="           _iState =OPERATOR; return MINUSEQ;
"*="           _iState =OPERATOR; return MULTEQ;
"/="           _iState =OPERATOR; return DIVEQ;
"^="           _iState =OPERATOR; return EXPEQ;
"%="           _iState =OPERATOR; return MODEQ;
"**"|"^"       _iState =OPERATOR; return POWER;
"."[t|y]"."    _iState =SEPARATOR; return TRUEVALUE;
"."[f|n]"."    _iState =SEPARATOR; return FALSEVALUE;
".and."        _iState =OPERATOR; return AND;
".or."         _iState =OPERATOR; return OR;
"!"|".not."    _iState =OPERATOR; return NOT;
"::"           unput( ':' ); unput( 'f' ); unput( 'l' ); unput( 'e' ); unput( 'S' );
[,\{\}\|\#\&\:\<\>\[\]\@] _iState =OPERATOR; return yytext[ 0 ];
[\(]           ++_iOpenBracket; _iState =SEPARATOR; return yytext[ 0 ];
[\)]           --_iOpenBracket; _iState =SEPARATOR; return yytext[ 0 ];

{InvalidNumber} GenError( ERR_NUMERIC_FORMAT, NULL, NULL );

{Number}       { char * ptr;
                 yylval.dNum.dNumber = atof( yytext );
                 ptr = strchr( yytext, '.' );
                 if( ptr )
                 {
                    yylval.dNum.bDec = strlen( ptr + 1 );
                    return DOUBLE;
                 }
                 else
                 {
                    if( ( double )SHRT_MIN <= yylval.dNum.dNumber &&
                        yylval.dNum.dNumber <= ( double )SHRT_MAX )
                    {
                       yylval.iNumber = ( int ) yylval.dNum.dNumber;
                       return INTEGER;
                    }
                    else if( ( double )LONG_MIN <= yylval.dNum.dNumber &&
                             yylval.dNum.dNumber <= ( double )LONG_MAX )
                    {
                       yylval.lNumber = ( long ) yylval.dNum.dNumber;
                       return INTLONG;
                    }
                    else
                    {
                       yylval.dNum.bDec = 0;
                       return DOUBLE;
                    }
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
                    yylval.dNum.dNumber = lNumber;
                    yylval.dNum.bDec = 0;
                    return DOUBLE;
                 }
              }

{Array}     {
               if( ! i_INDEX_STATE )
               {
                  BEGIN INDEX;
                  i_INDEX_STATE = 1;
               }

               unput( '[' );
               yyleng--;

               /* Remove optional white space between Identifier and Index */
               while( yytext[ yyleng - 1 ] < 48 )
                  yyleng--;

               yytext[yyleng] = 0;

               {
                  PDEFINE pDef = FindDef( yytext );
                  char * szText; int c;

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
                     yylval.string = yy_strupr( yy_strdup( yytext ) );
                     /*printf( "\nIdentifier = '%s'\n", yy_strupr( yy_strdup( yytext ) ) );*/
                     _iState = IDENTIFIER;
                     return IDENTIFIER;
                  }
               }
            }

{ExpArray}  {
               /* Must be recursive */
               if( ! i_INDEX_STATE )
               {
                  BEGIN INDEX;
                  i_INDEX_STATE = 1;
               }

               unput( '[' );
/*
               yyleng = 1;
               yytext[1] = 0;
               yylval.string = yy_strdup( ")" );
*/
               _iState = OPERATOR;
               --_iOpenBracket;
               return ')';
            }

{SubArray}  {
               /* Must be recursive */
               if( i_INDEX_STATE )
               {
                  BEGIN INDEX;
                  i_INDEX_STATE = 1;
               }

               iIndexSets--;

               unput( '[' );
/*
               yyleng = 1;
               yytext[1] = 0;
               yylval.string = yy_strdup( "]" );
*/
               _iState = OPERATOR;
               return ']';
            }

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
                                 yylval.string = yy_strupr( yy_strdup( yytext ) );
                                 return IDENTIFIER;
                              }
                           }

%{
#ifdef __WATCOMC__
/* enable warnings for unreachable code */
#pragma warning 13 1
#endif
%}

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

   pDef->szDefine = yy_strdup( szDefine );
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

   pDef->szDefine = yy_strdup( szKey );
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

/*  Removed for preprocessor needs
void AddDefine( char * szId, char * szValue )
{
   PDEFINE pDefine = FindDef( szId );

   if( pDefine )
   {
      if( pDefine->szValue )
      {
         free( pDefine->szValue );
         pDefine->szValue = 0;
      }
      if( szValue )
         pDefine->szValue = yy_strdup( szValue );
   }
   else
   {
      Define( szId );
      if( szValue )
         LastDef( pDefs )->szValue = yy_strdup( szValue );
   }
}

int yy_lex_input( char *buffer, int iBufferSize )
{
   int i;
   static int _ineedLF = 1;

   if( ((i = fread(buffer, 1, iBufferSize, yyin)) == 0) && ferror(yyin) )
       YY_FATAL_ERROR( "input in flex scanner failed" );

   if( i == iBufferSize )
     _ineedLF =( buffer[i-1] != '\n' );

   else if( i < iBufferSize && _ineedLF )
   {
     if( i == 0 )
       buffer[ i++ ] ='\n';
     else if( buffer[ i-1 ] != '\n' )
       buffer[ i++ ] ='\n';
     _ineedLF =0;
   }
   return i;
}
*/

int yy_lex_input( char *buffer, int iBufferSize )
{
   return PreProcess( yyin, (lPpo)? yyppo:NULL, buffer );
}
