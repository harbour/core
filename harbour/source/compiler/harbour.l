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
#include "hbcomp.h"
#include "harboury.h"
#include "hbsetup.h"    /* main configuration file */
#include "hberrors.h"
#include "hbdefs.h"

/* helper functions */
static int yy_ConvertNumber( char * szBuffer );

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

/* Following two lines added for preprocessor */
int yy_lex_input( char *, int );
#define YY_INPUT( buf, result, max_size ) result = yy_lex_input( buf, max_size );


#define LOOKUP          0       /* scan from the begining of line */
#define OPERATOR        -1
#define SEPARATOR       -2
static int hb_comp_iState = LOOKUP;
static int _iOpenBracket = 0;

/* Support for Array Index */
static int iIndexSets  = 0;
static int i_INDEX_STATE = 0;

%}

%{
#ifdef __WATCOMC__
/* disable warnings for unreachable code */
#pragma warning 13 9
#endif
%}

SpaceTab      [ \t]+
Number        ([0-9]+)|([0-9]*\.[0-9]+)
InvalidNumber [0-9]+\.
HexNumber     0x[0-9A-F]+
Identifier    (([a-zA-Z])|([_a-zA-Z][_a-zA-Z0-9]+))

MacroVar      \&{Identifier}[\.]?
MacroEnd      \&{Identifier}\.({Identifier})|([0-9]+)
MacroId       ({Identifier}\&(({Identifier}[\.]?)|({Identifier}\.({Identifier})|([0-9]+))))
MacroTxt      ({MacroVar}|{MacroEnd}|{MacroId})+

TrueValue     "."[t|y]"."
FalseValue    "."[f|n]"."

Array         {Identifier}[ \t]*"["
MacroVarArray {MacroVar}[ \t]*"["
MacroTxtArray ({MacroEnd}|{MacroId}|{MacroTxt})[ \t]*"["
AtArray       "}"[ \t]*"["
ExpArray      ")"[ \t]*"["
SubArray      "]"[ \t]*"["

Separator     {SpaceTab}

%x            STRING1 STRING2 STRING3
%x            NEXT_ BREAK_ CASE_ DO_ WHILE_ WITH_ END_ EXIT_ EXTERNAL_ FIELD_
%x            FOR_ FUNCTION_ IIF_ IF_ IN_ INIT_ LOCAL_ LOOP_ DECLARE_
%x            MEMVAR_ PARAM_ PRIVATE_ PUBLIC_ STATIC_ RETURN_ RECOVER_
%x            INVALIDNUM_ OTHERWISE_ PROCEDURE_
%s            INDEX

%%


"&"("'"|\"|\[) { hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, yytext, NULL ); }

'              BEGIN STRING1;
\"             BEGIN STRING2;

"="[ \t]*"["                         { BEGIN STRING3; hb_comp_iState =OPERATOR; return '=';      }
"+"[ \t]*"["                         { BEGIN STRING3; hb_comp_iState =OPERATOR; return '+';      }
"-"[ \t]*"["                         { BEGIN STRING3; hb_comp_iState =OPERATOR; return '-';      }
"*"[ \t]*"["                         { BEGIN STRING3; hb_comp_iState =OPERATOR; return '*';      }
"/"[ \t]*"["                         { BEGIN STRING3; hb_comp_iState =OPERATOR; return '/';      }
"%"[ \t]*"["                         { BEGIN STRING3; hb_comp_iState =OPERATOR; return '%';      }
"$"[ \t]*"["                         { BEGIN STRING3; hb_comp_iState =OPERATOR; return '$';      }
("<>"|"!=")[ \t]*"["                 { BEGIN STRING3; hb_comp_iState =NE2;      return NE2;      }
":="[ \t]*"["                        { BEGIN STRING3; hb_comp_iState =INASSIGN; return INASSIGN; }
"=="[ \t]*"["                        { BEGIN STRING3; hb_comp_iState =EQ;       return EQ;       }
"<="[ \t]*"["                        { BEGIN STRING3; hb_comp_iState =LE;       return LE;       }
">="[ \t]*"["                        { BEGIN STRING3; hb_comp_iState =GE;       return GE;       }
"+="[ \t]*"["                        { BEGIN STRING3; hb_comp_iState =PLUSEQ;   return PLUSEQ;   }
"-="[ \t]*"["                        { BEGIN STRING3; hb_comp_iState =MINUSEQ;  return MINUSEQ;  }
"*="[ \t]*"["                        { BEGIN STRING3; hb_comp_iState =MULTEQ;   return MULTEQ;   }
"/="[ \t]*"["                        { BEGIN STRING3; hb_comp_iState =DIVEQ;    return DIVEQ;    }
"^="[ \t]*"["                        { BEGIN STRING3; hb_comp_iState =EXPEQ;    return EXPEQ;    }
"%="[ \t]*"["                        { BEGIN STRING3; hb_comp_iState =MODEQ;    return MODEQ;    }
("**"|"^")[ \t]*"["                  { BEGIN STRING3; hb_comp_iState =POWER;    return POWER;    }
".and."[ \t]*"["                     { BEGIN STRING3;                    return AND;      }
".or."[ \t]*"["                      { BEGIN STRING3;                    return OR;       }
("!"|".not.")[ \t]*"["               { BEGIN STRING3;                    return NOT;      }
(","|"{"|"<"|">"|"(")[ \t]*"["       { BEGIN STRING3; hb_comp_iState = OPERATOR; yyleng = 1; yytext[1] = 0; return yytext[ 0 ]; }
("retu"|"retur"|"return")[ \t]*"["   { BEGIN STRING3; hb_comp_iState = RETURN; return RETURN; }
<INITIAL>\[                          { BEGIN STRING3; }

<STRING1>[^'^\n]*   { hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_STRING_TERMINATOR, yytext, NULL ); BEGIN 0; }
<STRING2>[^\"^\n]*  { hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_STRING_TERMINATOR, yytext, NULL ); BEGIN 0; }
<STRING3>[^\]]*\n   { hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_STRING_TERMINATOR, yytext, NULL ); BEGIN 0; }

<STRING1>[^']*'   { if( i_INDEX_STATE )
                       BEGIN INDEX;
                    else
                       BEGIN 0;

                    yyleng--;
                    yytext[yyleng] = 0;
                    yylval.string = hb_strdup( yytext );
                    /*printf( "\nLITERAL = %s\n", yylval.string );*/
                    return LITERAL;
                  }

<STRING2>[^\"]*\" { if( i_INDEX_STATE )
                       BEGIN INDEX;
                    else
                       BEGIN 0;

                    yyleng--;
                    yytext[yyleng] = 0;
                    yylval.string = hb_strdup( yytext );
                    /*printf( "\nLITERAL = %s\n", yylval.string );*/
                    return LITERAL;
                  }

<STRING3>[^\]]*\] { if( i_INDEX_STATE )
                       BEGIN INDEX;
                    else
                       BEGIN 0;

                    yyleng--;
                    yytext[yyleng] = 0;
                    yylval.string = hb_strdup( yytext );
                    /*printf( "\nLITERAL = %s\n", yylval.string );*/
                    return LITERAL;
                  }

<INDEX>\n    { hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_UNTERM_ARRAY_INDEX, NULL, NULL ); }

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
                hb_comp_iState =OPERATOR;
                return yytext[ 0 ];
             }

{SpaceTab}     ;

\n.*         {
                hb_comp_iState = LOOKUP;
                yyless( 1 );
                /*++hb_comp_iLine;*/
#if 0
                if( ! hb_comp_bQuiet )
                {
                   printf( "\r%i", hb_comp_iLine );
                }
#endif
                if( ! hb_comp_bQuiet && ( hb_comp_iLine % 100 ) == 0 )
                {
                   printf( "\r%i", hb_comp_iLine );
                   fflush( stdout );
                }
                return '\n';
             }
%{
/* ************************************************************************ */
%}
;              hb_comp_iState = LOOKUP; if( ! i_INDEX_STATE ) return ';';
%{
/* ************************************************************************ */
%}
"anno"("unce"|"unc"|"un"|"u")?        return ANNOUNCE;
%{
/* ************************************************************************ */
%}
"begin"{Separator}+"sequ"("ence"|"enc"|"en"|"e")?        return BEGINSEQ;
%{
/* ************************************************************************ */
%}
"break"        { if( hb_comp_iState == LOOKUP )
                    BEGIN BREAK_;
                 else
                 {
                    yylval.string = hb_strupr( hb_strdup( yytext ) );
                    hb_comp_iState =IDENTIFIER;
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
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, yytext, NULL );
             }
*/
%}
<BREAK_>{Separator}*(":="|"+="|"-="|"->"|"*="|"/="|"^="|"==") { /* operators */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = hb_strdup( "BREAK" );
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               hb_comp_iState =IDENTIFIER;
               return IDENTIFIER;
             }
<BREAK_>{Separator}*("++"|"--") { /* operators */
               /* NOTE: It is not possible to distinguish between
                * break++ and break ++i
                * For this reason we are allowing the BREAK statement only
                */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = hb_strdup( "BREAK" );
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               hb_comp_iState =BREAK;
               return BREAK;
             }
<BREAK_>{Separator}*[\=\(] { /* operators = ( */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = hb_strdup( "BREAK" );
               unput( yytext[ yyleng-1 ] );
               hb_comp_iState =IDENTIFIER;
               return IDENTIFIER;
             }
<BREAK_>{Separator}*. { /* all other cases */
               /* NOTE: This state includes break&var
                */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               hb_comp_iState =BREAK;
               return BREAK;
             }
%{
/* ************************************************************************ */
%}
"case"         BEGIN CASE_;
<CASE_>{Separator}*[\:\=\|\$\%\*\,\/\]\)\}\^] { /* there is an operator after "case" */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = hb_strdup( "CASE" );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<CASE_>{Separator}*[\[] { /* array */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               /* Clipper does not like case[] at all */
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, yytext, NULL );
             }
<CASE_>{Separator}*("+="|"-="|"->") { /* operators */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = hb_strdup( "CASE" );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<CASE_>{Separator}*("::") { /* send operators */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               hb_comp_iState =CASE;
               return CASE;
             }
<CASE_>{Separator}*(\n|.) { /* not operator */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( hb_comp_iState == LOOKUP )
               { /* it is first item in the line */
                 hb_comp_iState =CASE;
                 return CASE;
               }
               else
               { /* there is another item in line already */
                 yylval.string = hb_strdup( "CASE" );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
             }
%{
/* ************************************************************************ */
%}
"decl"("are"|"ar"|"a")? { BEGIN PRIVATE_;
                  yylval.string = hb_strupr( hb_strdup( yytext ) );
                }
%{
/* ************************************************************************ */
%}
"do"            BEGIN DO_;
<DO_>{Separator}+"case"         { /* DO CASE statement */
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                hb_comp_iState =DOCASE;
                return DOCASE;
              }
<DO_>{Separator}+"while" { /* DO WHILE found -move it to WHILE state */
                /* NOTE: we cannot decide here if it is DO WHILE <condition>
                 * or DO while [WITH <args>]
                 */
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                hb_comp_iState =DO;
                yyless( yyleng-5 );
              }
<DO_>{Separator}+"whil" { /* DO WHILE found -move it to WHILE state */
                /* NOTE: we cannot decide here if it is DO WHILE <condition>
                 * or DO while [WITH <args>]
                 */
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                hb_comp_iState =DO;
                yyless( yyleng-5 );
              }
<DO_>{Separator}+[_a-zA-Z\&]   { /* an identifier 'DO id WITH' or 'DO &id WITH' */
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                unput( yytext[ yyleng-1 ] );
                if( hb_comp_iState == LOOKUP )
                { /* it is first item in the line */
                  hb_comp_iState =DO;
                  return DO;
                }
                else
                { /* there is another item in line already */
                  yylval.string = hb_strdup( "DO" );
                  hb_comp_iState =IDENTIFIER;
                  return IDENTIFIER;
                }
              }
<DO_>{Separator}*(.|\n)         { /* end of line or any operator */
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                unput( yytext[ yyleng-1 ] );
                yylval.string = hb_strdup( "DO" );
                hb_comp_iState =IDENTIFIER;
                return IDENTIFIER;
              }
%{
/* ************************************************************************ */
%}
"else"         { /* ELSE can be used in one context only */
                 if( hb_comp_wIfCounter == 0 )
                   hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_UNMATCHED_ELSE, NULL, NULL );
                 hb_comp_iState =ELSE;
                 return ELSE;
               }
"elseif"       { /* ELSEIF can be used in one context only */
                 if( hb_comp_wIfCounter == 0 )
                   hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_UNMATCHED_ELSEIF, NULL, NULL );
                 hb_comp_iState =ELSEIF;
                 return ELSEIF;
               }
"end"{Separator}+"sequ"("ence"|"enc"|"en"|"e")?  {
                 if( hb_comp_wSeqCounter == 0 )
                   hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_ENDIF, NULL, NULL );
                 return END;
               }
%{
/* ************************************************************************ */
%}

"endif"|"endi" { /* ENDIF can be used in one context only */
                 if( hb_comp_wIfCounter == 0 )
                   hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_ENDIF, NULL, NULL );
                 return ENDIF;
               }
"endc"("ase"|"as"|"a")? { /* ENDCASE can be used in one context only */
                 if( hb_comp_wCaseCounter == 0 )
                   hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_ENDCASE, NULL, NULL );
                 return ENDCASE;
               }
"enddo"|"endd" { /* ENDDO can be used in one context only */
                 if( hb_comp_wWhileCounter == 0 )
                   hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_ENDDO, NULL, NULL );
                 return ENDDO;
               }
%{
/* ************************************************************************ */
%}
"end"          { BEGIN END_; }
<END_>{Separator}*[\[\(] { /* array, function call */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               if( hb_comp_iState == LOOKUP )
               { /* Clipper does not like end[] & end() at the begining of line */
                 hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_ENDIF, NULL, NULL );
               }
               yylval.string = hb_strdup( "END" );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<END_>{Separator}*("->"|"++"|"--") { /* operators */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               if( hb_comp_iState == LOOKUP )
               { /* Clipper does not like end-> & end++ at the begining of line */
                 hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_ENDIF, NULL, NULL );
               }
               yylval.string = hb_strdup( "END" );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<END_>{Separator}*[\+\-\:\=\|\$\%\*\,\/\[\]\)\}\^] { /* there is an operator after "end" */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = hb_strdup( "END" );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<END_>{Separator}*(.|\n) { /* not operator */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( hb_comp_iState == LOOKUP )
               { /* it is first item in the line */
                 hb_comp_iState =END;
                 return END;
               }
               else
               { /* there is another item in line already */
                 yylval.string = hb_strdup( "END" );
                 hb_comp_iState =IDENTIFIER;
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
                 if( hb_comp_iState == LOOKUP )
                 { /* it is first item in the line */
                   if( hb_comp_wForCounter == 0 && hb_comp_wWhileCounter == 0 )
                     hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_UNMATCHED_EXIT, "EXIT", NULL );
                   hb_comp_iState =EXITLOOP;
                   return EXITLOOP;
                 }
                 else
                 { /* there is another item in line already */
                   yylval.string = hb_strdup( "EXIT" );
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<EXIT_>{Separator}+[fFpP] { /* FUNCTION or PROCEDURE after EXIT */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( hb_comp_iState == LOOKUP )
                 { /* it is first item in the line */
                   hb_comp_iState =EXIT;
                   return EXIT;
                 }
                 else
                 { /* there is another item in line already */
                   yylval.string = hb_strdup( "EXIT" );
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<EXIT_>{Separator}*. { /* any character (not identifier) after EXIT */
                 unput( yytext[ yyleng-1 ] );
                 yylval.string = hb_strdup( "EXIT" );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"exte"|"exter"|"extern"|"externa"|"external" { BEGIN EXTERNAL_;
                   yylval.string = hb_strupr( hb_strdup( yytext ) );
                 }
<EXTERNAL_>{Separator}+[_a-zA-Z] { /* an identifier after the EXTERNAL */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( hb_comp_iState == LOOKUP )
                 {
                   hb_xfree( (void *) yylval.string );
                   hb_comp_iState =EXTERN;
                   return EXTERN;
                 }
                 else
                 {
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<EXTERNAL_>{Separator}*[^_a-zA-Z]   {
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
<EXTERNAL_>.    { if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0; unput( yytext[ yyleng-1 ] ); }
%{
/* ************************************************************************ */
%}
"_fie"|"_fiel"|"_field" { BEGIN FIELD_;
                   yylval.string = hb_strupr( hb_strdup( yytext ) );
                 }

"fiel"|"field"   { BEGIN FIELD_;
                   yylval.string = hb_strupr( hb_strdup( yytext ) );
                 }
<FIELD_>{Separator}+[_a-zA-Z] { /* an identifier after the FIELD */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( hb_comp_iState == LOOKUP )
                 {
                   hb_xfree( (void *) yylval.string );
                   hb_comp_iState =FIELD;
                   return FIELD;
                 }
                 else
                 {
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<FIELD_>{Separator}*"->" { /* alias expression */
                 unput( yytext[ yyleng-1 ] );
                 unput( yytext[ yyleng-2 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 hb_xfree( (void *) yylval.string );
                 hb_comp_iState =FIELD;
                 return FIELD;
               }
<FIELD_>{Separator}*[^_a-zA-Z]   {
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
<FIELD_>.     { if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0; unput( yytext[ yyleng-1 ] ); }
%{
/* ************************************************************************ */
%}
"for"          { BEGIN FOR_; }
<FOR_>{Separator}+[&_a-zA-Z] { /* an identifier or a macro after the FOR */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( hb_comp_iState == LOOKUP )
                 {
                   hb_comp_iState =FOR;
                   return FOR;
                 }
                 else
                 { /* for example: DO for WITH variable */
                   yylval.string = hb_strdup( "FOR" );
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<FOR_>{Separator}*[\(] { /* function call */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               if( hb_comp_iState == LOOKUP )
               { /* Clipper does not like FOR() at the begining of line */
                 hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, yytext, NULL );
               }
               yylval.string = hb_strdup( "FOR" );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<FOR_>{Separator}*[^_a-zA-Z] { /* there is no identifier after "FOR" */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = hb_strdup( "FOR" );
               unput( yytext[ yyleng-1 ] );
               hb_comp_iState =IDENTIFIER;
               return IDENTIFIER;
             }
%{
/* ************************************************************************ */
%}
"func"|"funct"|"functi"|"functio"|"function" { BEGIN FUNCTION_; }
<FUNCTION_>{Separator}+[_a-zA-Z] {
               BEGIN 0; /* we can don't care about INDEX_STATE here */
               unput( yytext[ yyleng-1 ] );
               hb_comp_iState=FUNCTION;
               return FUNCTION;
             }
<FUNCTION_>{Separator}*[^_a-zA-Z] { /* Clipper needs FUNCTION in one context only */
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, ((yytext[ yyleng-1 ]=='\n')?"FUNCTION":yytext), NULL );
             }
%{
/* ************************************************************************ */
%}
"iif"          {
                 if( hb_comp_iState == FUNCTION || hb_comp_iState == PROCEDURE )
                   hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, "IIF", NULL );
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
               hb_comp_iState=IIF;
               return IIF;
             }
<IIF_>{Separator}*[^\(] {
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, ((yytext[ yyleng-1 ]=='\n')?"IIF":yytext), NULL );
             }
%{
/* ************************************************************************ */
%}
"if"           {
                 if( hb_comp_iState == FUNCTION || hb_comp_iState == PROCEDURE )
                   hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, "IF", NULL );
                 else
                   BEGIN IF_;
              }
<IF_>{Separator}*"(" {
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( hb_comp_iState == LOOKUP )
                 hb_comp_iState =IF;
               else
                 hb_comp_iState =IIF;
               return hb_comp_iState;
             }
<IF_>{Separator}*[\)\[\]\/\^\*\%\=\$\@] {
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX2, yytext, "IF" );
             }
<IF_>{Separator}*"->" {
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX2, yytext, "IF" );
             }
<IF_>{Separator}*[\n] {
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, "IF", NULL );
             }
<IF_>{Separator}*("++"|"--")/[\n] {
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX2, yytext, "IF" );
             }
<IF_>{Separator}*. {
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               hb_comp_iState =IF;
               return IF;
             }
%{
/* ************************************************************************ */
%}
"in"           BEGIN IN_;
<IN_>{Separator}+[_a-zA-Z]       {
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == IDENTIFIER )
                   return IN;
                 else
                 {
                   yylval.string =hb_strdup( "IN" );
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
             }
<IN_>{Separator}*\n     {
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               yylval.string =hb_strdup( "IN" );
               hb_comp_iState =IDENTIFIER;
               return IDENTIFIER;
             }
<IN_>{Separator}*[0-9]      {
                hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, yytext, NULL );
             }
<IN_>{Separator}*.      {
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               yylval.string =hb_strdup( "IN" );
               hb_comp_iState =IDENTIFIER;
               return IDENTIFIER;
             }
%{
/* ************************************************************************ */
%}
"init"         BEGIN INIT_;
<INIT_>{Separator}+[fFpP] { /* FUNCTION or PROCEDURE after INIT */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( hb_comp_iState == LOOKUP )
                 { /* it is first item in the line */
                   hb_comp_iState =INIT;
                   return INIT;
                 }
                 else
                 { /* there is another item in line already */
                   yylval.string = hb_strdup( "INIT" );
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<INIT_>{Separator}*[^fFpP] { /* any character (not identifier) after EXIT */
                 unput( yytext[ yyleng-1 ] );
                 yylval.string = hb_strdup( "INIT" );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"#"{Separator}*"line"  return LINE;
"loca"|"local"  {
                  yylval.string = hb_strupr( hb_strdup( yytext ) );
		  BEGIN LOCAL_;
                }
<LOCAL_>{Separator}+[_a-zA-Z]   { /* an identifier after LOCAL */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( hb_comp_iState == LOOKUP )
                 { /* it is first item in the line */
                   hb_xfree( (void *) yylval.string );
                   hb_comp_iState =LOCAL;
                   return LOCAL;
                 }
                 else
                 { /* there is another item in line already */
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<LOCAL_>{Separator}*[^a-zA-Z] { /* any character (not identifier) after LOCAL */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"loop"         BEGIN LOOP_;
<LOOP_>{Separator}*[\n;]   { /* at the end of the line */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( hb_comp_iState == LOOKUP )
                 { /* it is first item in the line */
                   if( hb_comp_wWhileCounter == 0 && hb_comp_wForCounter == 0 )
                     hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_UNMATCHED_EXIT, "LOOP", NULL );
                   hb_comp_iState =LOOP;
                   return LOOP;
                 }
                 else
                 { /* there is another item in line already */
                   yylval.string = hb_strdup( "LOOP" );
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<LOOP_>{Separator}*. { /* any character (not LF) after LOOP */
                 unput( yytext[ yyleng-1 ] );
                 yylval.string = hb_strdup( "LOOP" );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"memv"|"memva"|"memvar" { BEGIN MEMVAR_;
                  yylval.string = hb_strupr( hb_strdup( yytext ) );
                }
<MEMVAR_>{Separator}+[_a-zA-Z]   { /* an identifier after MEMVAR */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( hb_comp_iState == LOOKUP )
                 { /* it is the first item in the line */
                   hb_xfree( (void *) yylval.string );
                   hb_comp_iState =MEMVAR;
                   return MEMVAR;
                 }
                 else
                 { /* there is another item in line already */
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<MEMVAR_>{Separator}*[^a-zA-Z] { /* any character (not identifier) after MEMVAR */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"next"         BEGIN NEXT_;
<NEXT_>{Separator}*[\n\;] { /* at the end of line */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( hb_comp_iState == LOOKUP )
               { /* it is first item in the line */
                 if( hb_comp_wForCounter == 0 )
                   hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_NEXTFOR, NULL, NULL );
                 hb_comp_iState =NEXT;
                 return NEXT;
               }
               else
               { /* there is another item in line already */
                 yylval.string = hb_strdup( "NEXT" );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
             }
<NEXT_>{Separator}*[\[\(] { /* array, function call */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               if( hb_comp_iState == LOOKUP )
               { /* Clipper does not like NEXT[] & NEXT() at the begining of line */
                 hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_NEXTFOR, NULL, NULL );
               }
               yylval.string = hb_strdup( "NEXT" );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<NEXT_>{Separator}*("->"|"++"|"--") { /* operators */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               if( hb_comp_iState == LOOKUP )
               { /* Clipper does not like next-> & next++ at the begining of line */
                 hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_NEXTFOR, NULL, NULL );
               }
               yylval.string = hb_strdup( "NEXT" );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<NEXT_>{Separator}*[^_a-zA-Z] { /* there is no identifier after "next" */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = hb_strdup( "NEXT" );
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<NEXT_>{Separator}*. { /* an identifier follows NEXT statement */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( hb_comp_iState == LOOKUP )
               {
                 if( hb_comp_wForCounter == 0 )
                   hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_NEXTFOR, NULL, NULL );
                 hb_comp_iState =NEXT;
                 return NEXT;
               }
               else
               {
                 yylval.string = hb_strdup( "NEXT" );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
             }
%{
/* ************************************************************************ */
%}
"nil"          hb_comp_iState =LITERAL; return NIL;
%{
/* ************************************************************************ */
%}
"othe"|"other"|"otherw"|"otherwi"|"otherwis"|"otherwise"    {
                  yylval.string = hb_strupr( hb_strdup( yytext ) );
                  BEGIN OTHERWISE_;
               }
<OTHERWISE_>{Separator}*[\n\;]     { /* end of line */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( hb_comp_iState == LOOKUP )
                 { /* it is the first item in the line */
                   hb_xfree( (void *) yylval.string );
                   hb_comp_iState = OTHERWISE;
                   return OTHERWISE;
                 }
                 else
                 { /* there is another item in line already */
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<OTHERWISE_>{Separator}*.     {
                   unput( yytext[ yyleng-1 ] );
                   if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                   hb_comp_iState = IDENTIFIER;
                   return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"para"|"param"|"parame"|"paramet"|"paramete"|"parameter"|"parameters"   {
                  yylval.string = hb_strupr( hb_strdup( yytext ) );
                  BEGIN PARAM_;
                }
<PARAM_>{Separator}+[_a-zA-Z]   { /* an identifier after PARAMETERS */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( hb_comp_iState == LOOKUP )
                 { /* it is the first item in the line */
                   hb_xfree( (void *) yylval.string );
                   hb_comp_iState =PARAMETERS;
                   return PARAMETERS;
                 }
                 else
                 { /* there is another item in line already */
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<PARAM_>{Separator}*[^a-zA-Z] { /* any character (not identifier) after PARAMETERS */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"priv"("ate"|"at"|"a")? { BEGIN PRIVATE_;
                  yylval.string = hb_strupr( hb_strdup( yytext ) );
                }
<PRIVATE_>{Separator}+[_a-zA-Z\&] { /* an Identifier after PRIVATE */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( hb_comp_iState == LOOKUP )
                 { /* it is first item in the line */
                   hb_xfree( (void *) yylval.string );
                   hb_comp_iState =PRIVATE;
                   return PRIVATE;
                 }
                 else
                 { /* there is another item in line already */
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<PRIVATE_>{Separator}*[^a-zA-Z] { /* any character (not identifier) after PRIVATE */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"proc"|"proce"|"proced"|"procedu"|"procedur"|"procedure"    BEGIN PROCEDURE_;
<PROCEDURE_>{Separator}+[_a-zA-Z] {
               BEGIN 0; /* we can don't care about INDEX_STATE here */
               unput( yytext[ yyleng-1 ] );
               hb_comp_iState = PROCEDURE;
               return PROCEDURE;
             }
<PROCEDURE_>{Separator}*[^_a-zA-Z] { /* Clipper needs PROCEDURE in one context only */
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, ((yytext[ yyleng-1 ]=='\n')?"PROCEDURE":yytext), NULL );
             }
%{
/* ************************************************************************ */
%}
"publ"("ic"|"i")? { BEGIN PUBLIC_;
                  yylval.string = hb_strupr( hb_strdup( yytext ) );
                }
<PUBLIC_>{Separator}+[_a-zA-Z\&]   { /* an identifier after PUBLIC */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( hb_comp_iState == LOOKUP )
                 { /* it is first item in the line */
                   hb_xfree( (void *) yylval.string );
                   hb_comp_iState =PUBLIC;
                   return PUBLIC;
                 }
                 else
                 { /* there is another item in line already */
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<PUBLIC_>{Separator}*[^a-zA-Z] { /* any character (not identifier) after PUBLIC */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"qself"{SpaceTab}*[(]{SpaceTab}*[)] return SELF;
%{
/* ************************************************************************ */
%}
"reco"|"recov"|"recove"|"recover"   {
                  yylval.string = hb_strupr( hb_strdup( yytext ) );
                  BEGIN RECOVER_;
                }
<RECOVER_>{Separator}*[\n]    { /* end of line */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( hb_comp_iState == LOOKUP )
                 { /* it is first item in the line */
                   hb_xfree( (void *) yylval.string );
                   hb_comp_iState = RECOVER;
                   return RECOVER;
                 }
                 else
                 { /* there is another item in line already */
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<RECOVER_>{Separator}+("using"|"usin")    { /* USING */
                   if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                   hb_xfree( (void *) yylval.string );
                   hb_comp_iState = RECOVERUSING;
                   return RECOVERUSING;
                 }
<RECOVER_>{Separator}*.            { /* all other cases */
                   unput( yytext[ yyleng-1 ] );
                   if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
%{
/* ************************************************************************ */
%}
"retu"|"retur"|"return"  {
                  yylval.string = hb_strupr( hb_strdup( yytext ) );
                  BEGIN RETURN_;
                }
<RETURN_>{Separator}+[\&_a-zA-Z0-9]   { /* an identifier, numbers or macro */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( hb_comp_iState == LOOKUP )
                 { /* it is the first item in the line */
                   hb_xfree( (void *) yylval.string );
                   hb_comp_iState = RETURN;
                   return RETURN;
                 }
                 else
                 { /* there is another item in line already */
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<RETURN_>{Separator}*("+="|"-="|"->") { /* operators */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<RETURN_>{Separator}*("++"|"--") { /* operators */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
                 if( hb_comp_iState == LOOKUP )
                 { /* it is the first item in the line */
                   hb_xfree( (void *) yylval.string );
                   hb_comp_iState = RETURN;
                   return RETURN;
                 }
                 else
                 { /* there is another item in line already */
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
             }
<RETURN_>{Separator}*("::") { /* SELF operator */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               hb_xfree( (void *) yylval.string );
               hb_comp_iState = RETURN;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return RETURN;
             }
<RETURN_>{Separator}*[\n\;\(\[\{\"\'\.\-\+\!]   {
                 /* EOL or '()', '[]', '{}', '""', "''" , '.T.', '-', '+', '!' */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( hb_comp_iState == LOOKUP )
                 { /* it is the first item in the line */
                   hb_xfree( (void *) yylval.string );
                   hb_comp_iState = RETURN;
                   return RETURN;
                 }
                 else
                 { /* there is another item in line already */
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<RETURN_>{Separator}*. { /* any other character after RETURN */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"stat"|"stati"|"static"  {
                  yylval.string = hb_strupr( hb_strdup( yytext ) );
                  BEGIN STATIC_;
                }
<STATIC_>{Separator}+[_a-zA-Z]   { /* an identifier after STATIC */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 if( hb_comp_iState == LOOKUP )
                 { /* it is first item in the line */
                   hb_xfree( (void *) yylval.string );
                   hb_comp_iState = STATIC;
                   return STATIC;
                 }
                 else
                 { /* there is another item in line already */
                   hb_comp_iState = IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<STATIC_>{Separator}*[^a-zA-Z] { /* any character (not identifier) after STATIC */
                 unput( yytext[ yyleng-1 ] );
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"step"         return STEP;
"to"           return TO;
%{
/* ************************************************************************ */
%}
"while"|"whil"  BEGIN WHILE_;
<WHILE_>{Separator}*\n      { /* end of line */
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                unput( '\n' );
                if( hb_comp_iState == DO )
                { /* we have DO while - replace it with while() */
                  unput( ')' ); unput( '(' );
                }
                yylval.string = hb_strdup( "WHILE" );
                return IDENTIFIER;
              }
<WHILE_>{Separator}*[\[] { /* array */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               /* Clipper does not like while[] at all */
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, yytext, NULL );
             }
<WHILE_>{Separator}*[\:\=\|\$\%\*\,\/\]\)\}\^] { /* there is an operator after "case" */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = hb_strdup( "WHILE" );
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<WHILE_>{Separator}*("+="|"-="|"->") { /* operators */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               yylval.string = hb_strdup( "WHILE" );
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<WHILE_>{Separator}*("::") { /* send operators */
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               hb_comp_iState =WHILE;
               return WHILE;
             }
<WHILE_>{Separator}*.      { /* identifiers and literals */
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                unput( yytext[ yyleng-1 ] );
                if( hb_comp_iState == LOOKUP || hb_comp_iState == DO )
                { /* it is first item in the line or after DO or FIELD */
                  hb_comp_iState =WHILE;
                  return WHILE;
                }
                else
                { /* there is another item in line already */
                  yylval.string = hb_strdup( "WHILE" );
                  hb_comp_iState =IDENTIFIER;
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
                yylval.string = hb_strdup( "WITH" );
                return IDENTIFIER;
              }
<WITH_>{Separator}*"with"       {
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                yyless( yyleng-4 );
                if( hb_comp_iState == DO )
                { /* DO with */
                  hb_comp_iState =IDENTIFIER;
                  yylval.string = hb_strdup( "WITH" );
                  return IDENTIFIER;
                }
                else
                { /* DO WITH with <arg> */
                  hb_comp_iState =WITH;
                  return WITH;
                }
              }
<WITH_>{Separator}*[\[] { /* array */
               /* Clipper does not like with[] at all */
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, yytext, NULL );
             }
<WITH_>{Separator}*.            {
                if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                unput( yytext[ yyleng-1 ] );
                if( hb_comp_iState == WHILE ||
                    hb_comp_iState == DO ||
                    hb_comp_iState == MACROVAR ||
                    hb_comp_iState == MACROTEXT ||
                    hb_comp_iState == IDENTIFIER ||
                    hb_comp_iState == SEPARATOR )
                { /* DO <ident> WITH <arg> */
                  hb_comp_iState =WITH;
                  return WITH;
                }
                else
                {
                  yylval.string = hb_strdup( "WITH" );
                  hb_comp_iState =IDENTIFIER;
                  return IDENTIFIER;
                }
              }

"as numeric"       { return AS_NUMERIC; }
"as num"           { return AS_NUMERIC; }
"as character"     { return AS_CHARACTER; }
"as char"          { return AS_CHARACTER; }
"as string"        { return AS_CHARACTER; }
"as logical"       { return AS_LOGICAL; }
"as date"          { return AS_DATE; }
"as array"         { return AS_ARRAY; }
"as block"         { return AS_BLOCK; }
"as object"        { return AS_OBJECT; }
"declare function" { return DECLARE_FUN; }

%{
/* ************************************************************************ */
%}
"#"            hb_comp_iState =OPERATOR; return NE1;
"="            hb_comp_iState =OPERATOR; return yytext[ 0 ];
"+"            hb_comp_iState =OPERATOR; return yytext[ 0 ];
"-"            hb_comp_iState =OPERATOR; return yytext[ 0 ];
"*"            hb_comp_iState =OPERATOR; return yytext[ 0 ];
[\/]           hb_comp_iState =OPERATOR; return yytext[ 0 ];
"%"            hb_comp_iState =OPERATOR; return yytext[ 0 ];
"$"            hb_comp_iState =OPERATOR; return yytext[ 0 ];
"<>"|"!="      hb_comp_iState =OPERATOR; return NE2;
":="           hb_comp_iState =OPERATOR; return INASSIGN;
"=="           hb_comp_iState =OPERATOR; return EQ;
"++"           hb_comp_iState =OPERATOR; return INC;
"--"           hb_comp_iState =OPERATOR; return DEC;
"->"           hb_comp_iState =OPERATOR; return ALIASOP;
"<="           hb_comp_iState =OPERATOR; return LE;
">="           hb_comp_iState =OPERATOR; return GE;
"+="           hb_comp_iState =OPERATOR; return PLUSEQ;
"-="           hb_comp_iState =OPERATOR; return MINUSEQ;
"*="           hb_comp_iState =OPERATOR; return MULTEQ;
"/="           hb_comp_iState =OPERATOR; return DIVEQ;
"^="           hb_comp_iState =OPERATOR; return EXPEQ;
"%="           hb_comp_iState =OPERATOR; return MODEQ;
"**"|"^"       hb_comp_iState =OPERATOR; return POWER;
".and."        hb_comp_iState =OPERATOR; return AND;
".or."         hb_comp_iState =OPERATOR; return OR;
"!"|".not."    hb_comp_iState =OPERATOR; return NOT;
"::"           unput( ':' ); unput( 'f' ); unput( 'l' ); unput( 'e' ); unput( 'S' );
[,\{\}\|\#\&\.\:\<\>\[\]\@] hb_comp_iState =OPERATOR; return yytext[ 0 ];
[\(]           ++_iOpenBracket; hb_comp_iState =SEPARATOR; return yytext[ 0 ];
[\)]           --_iOpenBracket; hb_comp_iState =SEPARATOR; return yytext[ 0 ];

[\x00-\x1F]	return yytext[ 0 ]; /* see below */
[\x80-\xFF]	{
            /* This have to be the last rule - any nonstandard and not handled
	         * characters should go to grammar analyser instead of printing it
	         * on stdout.
 	         */
            return yytext[ 0 ];
	    }

%{
/* ************************************************************************ */
%}

{InvalidNumber} BEGIN INVALIDNUM_; yylval.string = hb_strupr( hb_strdup( yytext ) );
<INVALIDNUM_>("."|{Separator}+) {
                   hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_NUMERIC_FORMAT, NULL, NULL );
                 }
<INVALIDNUM_>.   {
                    unput( yytext[ yyleng-1 ] );
                    unput( '.' );
                    if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                    yylval.string[ strlen(yylval.string) - 1 ] = '\0';
                    return yy_ConvertNumber( yylval.string );
                 }


{Number}       { return yy_ConvertNumber( yytext ); }

{HexNumber}   {
                 long lNumber = 0;

                 sscanf( yytext, "%lxI", &lNumber );

                 if( ( double ) SHRT_MIN <= lNumber &&
                        lNumber <= ( double ) SHRT_MAX )
                 {
                    yylval.valInteger.iNumber = lNumber;
                    yylval.valInteger.szValue = yytext;
                    return NUM_INTEGER;
                 }
                 else if( ( double ) LONG_MIN <= lNumber &&
                          lNumber <= ( double ) LONG_MAX )
                 {
                    yylval.valLong.lNumber = lNumber;
                    yylval.valLong.szValue = yytext;
                    return NUM_LONG;
                 }
                 else
                 {
                    /* NOTE: This will never happen */
                    yylval.valDouble.dNumber = lNumber;
                    yylval.valDouble.bWidth = HB_DEFAULT_WIDTH;
                    yylval.valDouble.bDec = 0;
                    yylval.valDouble.szValue = yytext;
                    return NUM_DOUBLE;
                 }
              }

{TrueValue}   { hb_comp_iState =SEPARATOR; return TRUEVALUE;  }
{FalseValue}  { hb_comp_iState =SEPARATOR; return FALSEVALUE; }

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
                  if( strlen( yytext ) > HB_SYMBOL_NAME_LEN )
                  {
                     yytext[ HB_SYMBOL_NAME_LEN ] = '\0';
                     yyleng = HB_SYMBOL_NAME_LEN;
                  }
                  yylval.string = hb_strupr( hb_strdup( yytext ) );
                  /*printf( "\nIdentifier = '%s'\n", hb_strupr( hb_strdup( yytext ) ) );*/
                  hb_comp_iState = IDENTIFIER;
                  return IDENTIFIER;
               }
            }

{MacroVarArray} {
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
               if( yytext[ yyleng-1 ] == '.' )
                  yytext[ yyleng-1 ] = '\0';
               yylval.string = hb_strupr( hb_strdup( yytext+1 ) );
               hb_comp_iState = MACROVAR;
               return MACROVAR;
            }

{MacroTxtArray} {
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
               yylval.string = hb_strupr( hb_strdup( yytext ) );
               hb_comp_iState = MACROTEXT;
               return MACROTEXT;
            }

{ExpArray}  {
               /* Must be recursive */
               if( ! i_INDEX_STATE )
               {
                  BEGIN INDEX;
                  i_INDEX_STATE = 1;
               }

               unput( '[' );
               hb_comp_iState = OPERATOR;
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
               hb_comp_iState = OPERATOR;
               return ']';
            }

{AtArray}  {
               /* Must be recursive */
               if( ! i_INDEX_STATE )
               {
                  BEGIN INDEX;
                  i_INDEX_STATE = 1;
               }

               unput( '[' );
               hb_comp_iState = OPERATOR;
               --_iOpenBracket;
               return '}';
            }

{MacroVar}     {
                  if( yytext[ yyleng-1 ] == '.' )
                     yytext[ yyleng-1 ] = '\0';
                  yylval.string = hb_strupr( hb_strdup( yytext+1 ) );
                  hb_comp_iState = MACROVAR;
                  return MACROVAR;
               }

{MacroEnd}     {
                  yylval.string = hb_strupr( hb_strdup( yytext ) );
                  hb_comp_iState = MACROTEXT;
                  return MACROTEXT;
               }

{MacroId}      {
                  yylval.string = hb_strupr( hb_strdup( yytext ) );
                  hb_comp_iState = MACROTEXT;
                  return MACROTEXT;
               }

{MacroTxt}     {
                  yylval.string = hb_strupr( hb_strdup( yytext ) );
                  hb_comp_iState = MACROTEXT;
                  return MACROTEXT;
               }


{Identifier}    {
                   if( strlen( yytext ) > HB_SYMBOL_NAME_LEN )
                   {
                      yytext[ HB_SYMBOL_NAME_LEN ] = '\0';
                      yyleng = HB_SYMBOL_NAME_LEN;
                   }
                   yylval.string = hb_strupr( hb_strdup( yytext ) );
                   hb_comp_iState = IDENTIFIER;
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

   return hb_pp_Internal( hb_comp_bPPO ? hb_comp_yyppo : NULL, buffer );
}

static int yy_ConvertNumber( char * szBuffer )
{
   char * ptr;

   yylval.valDouble.dNumber = atof( szBuffer );
   ptr = strchr( szBuffer, '.' );
   if( ptr )
   {
      yylval.valDouble.bDec = strlen( ptr + 1 );
      yylval.valDouble.bWidth = strlen( szBuffer ) - yylval.valDouble.bDec;
      if( yylval.valDouble.bDec )
         yylval.valDouble.bWidth--;
      yylval.valDouble.szValue = szBuffer;
      return NUM_DOUBLE;
   }
   else
   {
      if( ( double )SHRT_MIN <= yylval.valDouble.dNumber &&
            yylval.valDouble.dNumber <= ( double )SHRT_MAX )
      {
         yylval.valInteger.iNumber = ( int ) yylval.valDouble.dNumber;
         yylval.valInteger.szValue = szBuffer;
         return NUM_INTEGER;
      }
      else if( ( double )LONG_MIN <= yylval.valDouble.dNumber &&
                 yylval.valDouble.dNumber <= ( double )LONG_MAX )
      {
         yylval.valLong.lNumber = ( long ) yylval.valDouble.dNumber;
         yylval.valLong.szValue = szBuffer;
         return NUM_LONG;
      }
      else
      {
         yylval.valDouble.bWidth = strlen( szBuffer ) + 1;
         yylval.valDouble.bDec = 0;
         yylval.valDouble.szValue = szBuffer;
         return NUM_DOUBLE;
      }
   }
}
