%{
/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler LEX rules
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/* Compile using: flex -i -8 -oyylex.c harbour.l */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "harboury.h"
#include "compiler.h"
#include "hbsetup.h"    /* main configuration file */
#include "hberrors.h"
#include "hbdefs.h"

/* Functions defined in harbour.y */
char * yy_strupr( char * p );
char * yy_strdup( char * p );
extern void     hb_xfree( void * pMem );    /* frees memory */

/* YACC functions */
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

/* Following three lines added for preprocessor */
extern FILE *yyppo;  /* output .ppo file */
extern BOOL _bPPO;   /* flag indicating, is ppo output needed */
extern int PreProcess( FILE*, FILE*, char *);
int yy_lex_input( char *, int );
#define YY_INPUT( buf, result, max_size ) result = yy_lex_input( buf, max_size );

/* variables defined in harbour.y */
extern int _bRestrictSymbolLength;
extern WORD _wSeqCounter;
extern WORD _wForCounter;
extern WORD _wIfCounter;
extern WORD _wWhileCounter;
extern WORD _wCaseCounter;

int iLine = 1;
long lNumber = 0;

#define LOOKUP          0       /* scan from the begining of line */
#define OPERATOR        -1
#define SEPARATOR       -2
int _iState = LOOKUP;
static int _iOpenBracket = 0;

/* Support for Array Index */
int iIndexSets  = 0;
int i_INDEX_STATE = 0;

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

Separator     {SpaceTab}

%x            STRING1 STRING2 STRING3
%x            NEXT_ BREAK_ CASE_ DO_ WHILE_ WITH_ END_ EXIT_ EXTERNAL_ FIELD_
%x            FOR_ FUNCTION_ IIF_ IF_ IN_ INIT_ LOCAL_ LOOP_ DECLARE_
%x            MEMVAR_ PARAM_ PRIVATE_ PUBLIC_
%s            INDEX

%%


"&"("'"|\"|\[) { GenError( _szCErrors, 'E', ERR_SYNTAX, yytext, NULL ); }

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

<STRING1>[^'^\n]*   { GenError( _szCErrors, 'E', ERR_STRING_TERMINATOR, yytext, NULL ); BEGIN 0; }
<STRING2>[^\"^\n]*  { GenError( _szCErrors, 'E', ERR_STRING_TERMINATOR, yytext, NULL ); BEGIN 0; }
<STRING3>[^\]]*\n   { GenError( _szCErrors, 'E', ERR_STRING_TERMINATOR, yytext, NULL ); BEGIN 0; }

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

<INDEX>\n    { GenError( _szCErrors, 'E', ERR_UNTERM_ARRAY_INDEX, NULL, NULL ); }

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

{SpaceTab}     ;

\n.*         {
                _iState = LOOKUP;
                yyless( 1 );
                ++iLine;
#if 0
                if( ! _bQuiet )
                {
                   printf( "\r%i", iLine );
                }
#endif
                if( ! _bQuiet && ( iLine % 100 ) == 0 )
                {
                   printf( "\r%i", iLine );
                   fflush( stdout );
                }
                return '\n';
             }
%{
/* ************************************************************************ */
%}
;              _iState = LOOKUP; if( ! i_INDEX_STATE ) return ';';
%{
/* ************************************************************************ */
%}
"begin"{Separator}+"sequ"("ence"|"enc"|"en"|"e")?        return BEGINSEQ;
%{
/* ************************************************************************ */
%}
"break"        { if( _iState == LOOKUP )
                    BEGIN BREAK_;
                 else
                 {
                    yylval.string = yy_strupr( yy_strdup( yytext ) );
                    _iState =IDENTIFIER;
                    return IDENTIFIER;
                 }
               }
<BREAK_>{Separator}*[\n;] { /* at the end of line */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               return BREAK;
             }
%{
/* NOTE: Clipper does not like break[] in any context
 *       There are no resons to limit this use in Harbour.
 */
/*
<BREAK_>{Separator}*[\[] {
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               GenError( _szCErrors, 'E', ERR_SYNTAX, yytext, NULL );
             }
*/
%}
<BREAK_>{Separator}*(":="|"+="|"-="|"->"|"*="|"/="|"^="|"==") { /* operators */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = yy_strdup( "BREAK" );
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               _iState =IDENTIFIER;
               return IDENTIFIER;
             }
<BREAK_>{Separator}*("++"|"--") { /* operators */
               /* NOTE: It is not possible to distinguish between
                * break++ and break ++i
                * For this reason we are allowing the BREAK statement only
                */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = yy_strdup( "BREAK" );
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               _iState =BREAK;
               return BREAK;
             }
<BREAK_>{Separator}*[\=\(] { /* operators = ( */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = yy_strdup( "BREAK" );
               unput( yytext[ yyleng-1 ] );
               _iState =IDENTIFIER;
               return IDENTIFIER;
             }
<BREAK_>{Separator}*. { /* all other cases */
               /* NOTE: This state includes break&var
                */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               _iState =BREAK;
               return BREAK;
             }
%{
/* ************************************************************************ */
%}
"case"         BEGIN CASE_;
<CASE_>{Separator}*[\:\=\|\$\%\*\,\/\]\)\}\^] { /* there is an operator after "case" */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = yy_strdup( "CASE" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<CASE_>{Separator}*[\[] { /* array */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               /* Clipper does not like case[] at all */
               GenError( _szCErrors, 'E', ERR_SYNTAX, yytext, NULL );
             }
<CASE_>{Separator}*("+="|"-="|"->") { /* operators */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = yy_strdup( "CASE" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<CASE_>{Separator}*("::") { /* send operators */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               _iState =CASE;
               return CASE;
             }
<CASE_>{Separator}*(\n|.) { /* not operator */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
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
"decl"("are"|"ar"|"a")? { BEGIN PRIVATE_;
                  yylval.string = yy_strupr( yy_strdup( yytext ) );
                }
%{
/* ************************************************************************ */
%}
"do"            BEGIN DO_;
<DO_>{Separator}+"case"         { /* DO CASE statement */
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                _iState =DOCASE;
                return DOCASE;
              }
<DO_>{Separator}+"while" { /* DO WHILE found -move it to WHILE state */
                /* NOTE: we cannot decide here if it is DO WHILE <condition>
                 * or DO while [WITH <args>]
                 */
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                _iState =DO;
                yyless( yyleng-5 );
              }
<DO_>{Separator}+[_a-zA-Z]   { /* an identifier DO id WITH */
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
<DO_>{Separator}*(.|\n)         { /* end of line or any operator */
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
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
                   GenError( _szCErrors, 'E', ERR_UNMATCHED_ELSE, NULL, NULL );
                 _iState =ELSE;
                 return ELSE;
               }
"elseif"       { /* ELSEIF can be used in one context only */
                 if( _wIfCounter == 0 )
                   GenError( _szCErrors, 'E', ERR_UNMATCHED_ELSEIF, NULL, NULL );
                 _iState =ELSEIF;
                 return ELSEIF;
               }
"end"{Separator}+"sequ"("ence"|"enc"|"en"|"e")?  {
                 if( _wSeqCounter == 0 )
                   GenError( _szCErrors, 'E', ERR_ENDIF, NULL, NULL );
                 return END;
               }
%{
/* ************************************************************************ */
%}

"endif"|"endi" { /* ENDIF can be used in one context only */
                 if( _wIfCounter == 0 )
                   GenError( _szCErrors, 'E', ERR_ENDIF, NULL, NULL );
                 return ENDIF;
               }
"endc"("ase"|"as"|"a")? { /* ENDCASE can be used in one context only */
                 if( _wCaseCounter == 0 )
                   GenError( _szCErrors, 'E', ERR_ENDCASE, NULL, NULL );
                 return ENDCASE;
               }
"enddo"|"endd" { /* ENDDO can be used in one context only */
                 if( _wWhileCounter == 0 )
                   GenError( _szCErrors, 'E', ERR_ENDDO, NULL, NULL );
                 return ENDDO;
               }
%{
/* ************************************************************************ */
%}
"end"          { BEGIN END_; }
<END_>{Separator}*[\[\(] { /* array, function call */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               if( _iState == LOOKUP )
               { /* Clipper does not like end[] & end() at the begining of line */
                 GenError( _szCErrors, 'E', ERR_ENDIF, NULL, NULL );
               }
               yylval.string = yy_strdup( "END" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<END_>{Separator}*("->"|"++"|"--") { /* operators */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               if( _iState == LOOKUP )
               { /* Clipper does not like end-> & end++ at the begining of line */
                 GenError( _szCErrors, 'E', ERR_ENDIF, NULL, NULL );
               }
               yylval.string = yy_strdup( "END" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<END_>{Separator}*[\+\-\:\=\|\$\%\*\,\/\[\]\)\}\^] { /* there is an operator after "end" */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = yy_strdup( "END" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<END_>{Separator}*(.|\n) { /* not operator */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
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

"exit"         { BEGIN EXIT_; }
<EXIT_>{Separator}*[\n;]    { /* EXIT last item in the line */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( _iState == LOOKUP )
                 { /* it is first item in the line */
                   if( _wForCounter == 0 && _wWhileCounter == 0 )
                     GenError( _szCErrors, 'E', ERR_UNMATCHED_EXIT, "EXIT", NULL );
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
<EXIT_>{Separator}*. { /* any character (not identifier) after EXIT */
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
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( _iState == LOOKUP )
                 {
                   hb_xfree( (void *) yylval.string );
                   _iState =EXTERN;
                   return EXTERN;
                 }
                 else
                 {
                   _iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<EXTERNAL_>{Separator}*[^_a-zA-Z]   {
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
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( _iState == LOOKUP )
                 {
                   hb_xfree( (void *) yylval.string );
                   _iState =FIELD;
                   return FIELD;
                 }
                 else
                 {
                   _iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<FIELD_>{Separator}*[^_a-zA-Z]   {
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
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               if( _iState == LOOKUP )
               { /* Clipper does not like FOR() at the begining of line */
                 GenError( _szCErrors, 'E', ERR_SYNTAX, yytext, NULL );
               }
               yylval.string = yy_strdup( "FOR" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<FOR_>{Separator}*[^_a-zA-Z] { /* there is no identifier after "FOR" */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
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
               BEGIN 0; /* we can don't care about INDEX_STATE here */
               unput( yytext[ yyleng-1 ] );
               _iState=FUNCTION;
               return FUNCTION;
             }
<FUNCTION_>{Separator}*[^_a-zA-Z] { /* Clipper needs FUNCTION in one context only */
               GenError( _szCErrors, 'E', ERR_SYNTAX, ((yytext[ yyleng-1 ]=='\n')?"FUNCTION":yytext), NULL );
             }
%{
/* ************************************************************************ */
%}
"iif"          {
                 if( _iState == FUNCTION || _iState == PROCEDURE )
                   GenError( _szCErrors, 'E', ERR_SYNTAX, "IIF", NULL );
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
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               _iState=IIF;
               return IIF;
             }
<IIF_>{Separator}*[^\(] {
               GenError( _szCErrors, 'E', ERR_SYNTAX, ((yytext[ yyleng-1 ]=='\n')?"IIF":yytext), NULL );
             }
%{
/* ************************************************************************ */
%}
"if"           {
                 if( _iState == FUNCTION || _iState == PROCEDURE )
                   GenError( _szCErrors, 'E', ERR_SYNTAX, "IF", NULL );
                 else
                   BEGIN IF_;
              }
<IF_>{Separator}*"(" {
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( _iState == LOOKUP )
                 _iState =IF;
               else
                 _iState =IIF;
               return _iState;
             }
<IF_>{Separator}*[\)\[\]\/\^\*\%\=\$\@] {
               GenError( _szCErrors, 'E', ERR_SYNTAX2, yytext, "IF" );
             }
<IF_>{Separator}*"->" {
               GenError( _szCErrors, 'E', ERR_SYNTAX2, yytext, "IF" );
             }
<IF_>{Separator}*[\n] {
               GenError( _szCErrors, 'E', ERR_SYNTAX, "IF", NULL );
             }
<IF_>{Separator}*("++"|"--")/[\n] {
               GenError( _szCErrors, 'E', ERR_SYNTAX2, yytext, "IF" );
             }
<IF_>{Separator}*. {
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
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               yylval.string =yy_strdup( "IN" );
               _iState =IDENTIFIER;
               return IDENTIFIER;
             }
<IN_>{Separator}*[0-9]      {
                GenError( _szCErrors, 'E', ERR_SYNTAX, yytext, NULL );
             }
<IN_>{Separator}*.      {
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               yylval.string =yy_strdup( "IN" );
               _iState =IDENTIFIER;
               return IDENTIFIER;
             }
%{
/* ************************************************************************ */
%}
"init"         BEGIN INIT_;
<INIT_>{Separator}+[fFpP] { /* FUNCTION or PROCEDURE after INIT */
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
<INIT_>{Separator}*[^fFpP] { /* any character (not identifier) after EXIT */
                 unput( yytext[ yyleng-1 ] );
                 yylval.string = yy_strdup( "INIT" );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 _iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"#"{Separator}*"line"  return LINE;
"loca"|"local"  { BEGIN LOCAL_;
                  yylval.string = yy_strupr( yy_strdup( yytext ) );
                }
<LOCAL_>{Separator}+[_a-zA-Z]   { /* an identifier after LOCAL */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( _iState == LOOKUP )
                 { /* it is first item in the line */
                   hb_xfree( (void *) yylval.string );
                   _iState =LOCAL;
                   return LOCAL;
                 }
                 else
                 { /* there is another item in line already */
                   _iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<LOCAL_>{Separator}*[^a-zA-Z] { /* any character (not identifier) after LOCAL */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 _iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"loop"         BEGIN LOOP_;
<LOOP_>{Separator}*[\n;]   { /* at the end of the line */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( _iState == LOOKUP )
                 { /* it is first item in the line */
                   if( _wWhileCounter == 0 && _wForCounter == 0 )
                     GenError( _szCErrors, 'E', ERR_UNMATCHED_EXIT, "LOOP", NULL );
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
                 unput( yytext[ yyleng-1 ] );
                 yylval.string = yy_strdup( "LOOP" );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 _iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"memv"|"memva"|"memvar" { BEGIN MEMVAR_;
                  yylval.string = yy_strupr( yy_strdup( yytext ) );
                }
<MEMVAR_>{Separator}+[_a-zA-Z]   { /* an identifier after MEMVAR */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( _iState == LOOKUP )
                 { /* it is the first item in the line */
                   hb_xfree( (void *) yylval.string );
                   _iState =MEMVAR;
                   return MEMVAR;
                 }
                 else
                 { /* there is another item in line already */
                   _iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<MEMVAR_>{Separator}*[^a-zA-Z] { /* any character (not identifier) after MEMVAR */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 _iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"next"         BEGIN NEXT_;
<NEXT_>{Separator}*[\n\;] { /* at the end of line */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( _iState == LOOKUP )
               { /* it is first item in the line */
                 if( _wForCounter == 0 )
                   GenError( _szCErrors, 'E', ERR_NEXTFOR, NULL, NULL );
                 _iState =NEXT;
                 return NEXT;
               }
               else
               { /* there is another item in line already */
                 yylval.string = yy_strdup( "NEXT" );
                 _iState =IDENTIFIER;
                 return IDENTIFIER;
               }
             }
<NEXT_>{Separator}*[\[\(] { /* array, function call */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               if( _iState == LOOKUP )
               { /* Clipper does not like NEXT[] & NEXT() at the begining of line */
                 GenError( _szCErrors, 'E', ERR_NEXTFOR, NULL, NULL );
               }
               yylval.string = yy_strdup( "NEXT" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<NEXT_>{Separator}*("->"|"++"|"--") { /* operators */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               if( _iState == LOOKUP )
               { /* Clipper does not like next-> & next++ at the begining of line */
                 GenError( _szCErrors, 'E', ERR_NEXTFOR, NULL, NULL );
               }
               yylval.string = yy_strdup( "NEXT" );
               _iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<NEXT_>{Separator}*[^_a-zA-Z] { /* there is no identifier after "next" */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = yy_strdup( "NEXT" );
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<NEXT_>{Separator}*. { /* an identifier follows NEXT statement */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( _iState == LOOKUP )
               {
                 if( _wForCounter == 0 )
                   GenError( _szCErrors, 'E', ERR_NEXTFOR, NULL, NULL );
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
"nil"          _iState =LITERAL; return NIL;
"otherwise"    return OTHERWISE;
%{
/* ************************************************************************ */
%}
"para"|"param"|"parame"|"paramet"|"paramete"|"parameter"|"parameters"   {
                  BEGIN PARAM_;
                  yylval.string = yy_strupr( yy_strdup( yytext ) );
                }
<PARAM_>{Separator}+[_a-zA-Z]   { /* an identifier after PARAMETERS */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( _iState == LOOKUP )
                 { /* it is the first item in the line */
                   hb_xfree( (void *) yylval.string );
                   _iState =PARAMETERS;
                   return PARAMETERS;
                 }
                 else
                 { /* there is another item in line already */
                   _iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<PARAM_>{Separator}*[^a-zA-Z] { /* any character (not identifier) after PARAMETERS */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 _iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"priv"("ate"|"at"|"a")? { BEGIN PRIVATE_;
                  yylval.string = yy_strupr( yy_strdup( yytext ) );
                }
<PRIVATE_>{Separator}+[_a-zA-Z] { /* an Identifier after PRIVATE */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( _iState == LOOKUP )
                 { /* it is first item in the line */
                   hb_xfree( (void *) yylval.string );
                   _iState =PRIVATE;
                   return PRIVATE;
                 }
                 else
                 { /* there is another item in line already */
                   _iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<PRIVATE_>{Separator}*[^a-zA-Z] { /* any character (not identifier) after PRIVATE */
                /* TODO: add support for macro operator */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 _iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"proc"|"procedure" return PROCEDURE;
%{
/* ************************************************************************ */
%}
"publ"("ic"|"i")? { BEGIN PUBLIC_;
                  yylval.string = yy_strupr( yy_strdup( yytext ) );
                }
<PUBLIC_>{Separator}+[_a-zA-Z]   { /* an identifier after PUBLIC */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( _iState == LOOKUP )
                 { /* it is first item in the line */
                   hb_xfree( (void *) yylval.string );
                   _iState =PUBLIC;
                   return PUBLIC;
                 }
                 else
                 { /* there is another item in line already */
                   _iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<PUBLIC_>{Separator}*[^a-zA-Z] { /* any character (not identifier) after PUBLIC */
                /* TODO: add support for macro operator */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 _iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
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
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               /* Clipper does not like while[] at all */
               GenError( _szCErrors, 'E', ERR_SYNTAX, yytext, NULL );
             }
<WHILE_>{Separator}*[\:\=\|\$\%\*\,\/\]\)\}\^] { /* there is an operator after "case" */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = yy_strdup( "WHILE" );
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<WHILE_>{Separator}*("+="|"-="|"->") { /* operators */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = yy_strdup( "WHILE" );
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<WHILE_>{Separator}*("::") { /* send operators */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               _iState =WHILE;
               return WHILE;
             }
<WHILE_>{Separator}*.      { /* identifiers and literals */
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
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                unput( '\n' );
                yylval.string = yy_strdup( "WITH" );
                return IDENTIFIER;
              }
<WITH_>{Separator}*"with"       {
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
               /* Clipper does not like with[] at all */
               GenError( _szCErrors, 'E', ERR_SYNTAX, yytext, NULL );
             }
<WITH_>{Separator}*.            {
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

"as numeric"       { return AS_NUMERIC; }
"as character"     { return AS_CHARACTER; }
"as logical"       { return AS_LOGICAL; }
"as date"          { return AS_DATE; }
"as array"         { return AS_ARRAY; }
"as block"         { return AS_BLOCK; }
"as object"        { return AS_OBJECT; }
"declare function" { return DECLARE_FUN; }

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

{InvalidNumber} GenError( _szCErrors, 'E', ERR_NUMERIC_FORMAT, NULL, NULL );

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
                  if( _bRestrictSymbolLength && strlen( yytext ) > 10 )
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

{ExpArray}  {
               /* Must be recursive */
               if( ! i_INDEX_STATE )
               {
                  BEGIN INDEX;
                  i_INDEX_STATE = 1;
               }

               unput( '[' );
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
               _iState = OPERATOR;
               return ']';
            }

{Identifier}               {
                              if( _bRestrictSymbolLength && strlen( yytext ) > 10 )
                              {
                                 yytext[ 10 ] = 0;
                                 yyleng = 10;
                              }
                              yylval.string = yy_strupr( yy_strdup( yytext ) );
                              _iState =IDENTIFIER;
                              return IDENTIFIER;
                           }

%{
#ifdef __WATCOMC__
/* enable warnings for unreachable code */
#pragma warning 13 1
#endif
%}

%%

int yy_lex_input( char *buffer, int iBufferSize )
{
   HB_SYMBOL_UNUSED( iBufferSize );

   return PreProcess( yyin, _bPPO ? yyppo : NULL, buffer );
}
