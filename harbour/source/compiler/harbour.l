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

/* TODO: 4 character abbreviation for strong typing keywords should be added
         to be VO compatible [vszakats] */
/* TODO: VOID strong typing keyword should be added as a new type for NIL.
         [vszakats] */

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

#define YY_USER_ACTION \
           if ( yyleng > 0 ) \
              yy_current_buffer->yy_at_bol = ( yytext[yyleng - 1] == '\n' || yytext[0] == '\n' );


#define LOOKUP          0       /* scan from the begining of line */
#define OPERATOR        -1
#define LSEPARATOR      -2
#define RSEPARATOR      -4
#define LINDEX          -8
#define RINDEX          -16
#define LARRAY          -32
#define RARRAY          -64
static int hb_comp_iState = LOOKUP;
static int _iOpenBracket = 0;

#define DEBUG_STRINGS

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

Separator     {SpaceTab}

%x            STRING1 STRING2 STRING3
%x            NEXT_ BREAK_ CASE_ DO_ WHILE_ WITH_ END_ EXIT_ EXTERNAL_ FIELD_
%x            FOR_ FUNCTION_ IIF_ IF_ IN_ INIT_ LOCAL_ LOOP_
%x            MEMVAR_ PARAM_ PRIVATE_ PUBLIC_ STATIC_ RETURN_ RECOVER_
%x            INVALIDNUM_ OTHERWISE_ PROCEDURE_ ANNOUNCE_ PROCREQ_
%x            DECLARE_ DECLARE_ID_ OPTIONAL_

%%

"&"("'"|\"|\[) { hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, yytext, NULL ); }

'              BEGIN STRING1;
\"             BEGIN STRING2;

\[             {
                    if( (hb_comp_iState == OPERATOR) ||
                        (hb_comp_iState == LSEPARATOR) ||
                        (hb_comp_iState == LARRAY) ||
                        (hb_comp_iState == IF) ||
                        (hb_comp_iState == ELSEIF) ||
                        (hb_comp_iState == CASE) ||
                        (hb_comp_iState == WHILE)
                      )
                        BEGIN STRING3;
                    else
                    {
                        hb_comp_iState = LINDEX;
                        return '[';
                    }
                  }

<STRING1>[^'\n]*\n  { BEGIN 0;
                      unput( '\n' );
                      yytext[--yyleng] = '\0';
                      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_STRING_TERMINATOR, yytext, NULL );
                      hb_comp_iState = LOOKUP;

                      return LITERAL;
                    }

<STRING1>[^'\n]*'   { BEGIN 0;
                      yytext[--yyleng] = '\0';
                      yylval.string = hb_compIdentifierNew( yytext, TRUE );
                      hb_comp_iState = LITERAL;

                      return LITERAL;
                    }

<STRING2>[^\"\n]*\"  { BEGIN 0;
                      yytext[--yyleng] = '\0';
                      yylval.string = hb_compIdentifierNew( yytext, TRUE );
                      hb_comp_iState = LITERAL;

                      return LITERAL;
                    }

<STRING2>[^\"\n]*\n  { BEGIN 0;
		               unput( '\n' );
                       yytext[--yyleng] = '\0';
                       hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_STRING_TERMINATOR, yytext, NULL );
                       hb_comp_iState = LOOKUP;

                       return LITERAL;
                    }

<STRING3>[^\]\n]*\]  { BEGIN 0;
                      yytext[--yyleng] = '\0';
                      yylval.string = hb_compIdentifierNew( yytext, TRUE );
                      hb_comp_iState = LITERAL;

                      return LITERAL;
                    }

<STRING3>[^\]\n]*\n { BEGIN 0;
                      unput( '\n' );
                      yytext[--yyleng] = '\0';
                      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_STRING_TERMINATOR, yytext, NULL );
                      hb_comp_iState = LOOKUP;

                      return LITERAL;
                    }

{SpaceTab}     ;

\n           {
                hb_comp_iState = LOOKUP;

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
;            {
		 #ifdef DEBUG_NEWLINE
		    printf( "New Line\n" );
		 #endif

                 yy_set_bol(1);
                 hb_comp_iState = LOOKUP;
                 return ';';
	     }
%{
/* ************************************************************************ */
%}
"announce"|"announc"|"announ"|"annou"|"anno"        { BEGIN ANNOUNCE_;
                  yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
                }
<ANNOUNCE_>{Separator}+[_a-zA-Z]   { /* an identifier after ANNOUNCE */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 { /* it is the first item in the line */
                   hb_comp_iState = ANNOUNCE;
                   return ANNOUNCE;
                 }
                 else
                 { /* there is another item in line already */
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<ANNOUNCE_>.|\n { /* any character after ANNOUNCE */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 hb_comp_iState = IDENTIFIER;
                 return IDENTIFIER;
               }
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
                    yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
                    hb_comp_iState =IDENTIFIER;
                    return IDENTIFIER;
                 }
               }
<BREAK_>{Separator}*[\n;] { /* at the end of line */
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               return BREAK;
             }
%{
/* NOTE: Clipper does not like break[] in any context
 *       There are no resons to limit this use in Harbour.
 */
/*
<BREAK_>{Separator}*[\[] {  BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, yytext, NULL );
             }
*/
%}
<BREAK_>{Separator}*(":="|"+="|"-="|"->"|"*="|"/="|"^="|"==") { /* operators */
               BEGIN 0;
               yylval.string = hb_compIdentifierNew( "BREAK", TRUE );
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
               BEGIN 0;
               yylval.string = hb_compIdentifierNew( "BREAK", TRUE );
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               hb_comp_iState =BREAK;
               return BREAK;
             }
<BREAK_>{Separator}*[\=\(] { /* operators = ( */
               BEGIN 0;
               yylval.string = hb_compIdentifierNew( "BREAK", TRUE );
               unput( yytext[ yyleng-1 ] );
               hb_comp_iState =IDENTIFIER;
               return IDENTIFIER;
             }
<BREAK_>{Separator}*. { /* all other cases */
               /* NOTE: This state includes break&var
                */
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               hb_comp_iState =BREAK;
               return BREAK;
             }
%{
/* ************************************************************************ */
%}
"case"         BEGIN CASE_;
<CASE_>{Separator}*[\:\=\|\$\%\*\,\/\]\)\}\^] { /* there is an operator after "case" */
               BEGIN 0;
               yylval.string = hb_compIdentifierNew( "CASE", TRUE );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<CASE_>{Separator}*[\[] { /* array */
               BEGIN 0;
               /* Clipper does not like case[] at all */
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, yytext, NULL );
             }
<CASE_>{Separator}*("+="|"-="|"->") { /* operators */
               BEGIN 0;
               yylval.string = hb_compIdentifierNew( "CASE", TRUE );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<CASE_>{Separator}*("::") { /* send operators */
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               hb_comp_iState =CASE;
               return CASE;
             }
<CASE_>{Separator}*(\n|.) { /* not operator */
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( hb_comp_iState == LOOKUP )
               { /* it is first item in the line */
                 hb_comp_iState =CASE;
                 return CASE;
               }
               else
               { /* there is another item in line already */
                 yylval.string = hb_compIdentifierNew( "CASE", TRUE );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
             }
%{
/* ************************************************************************ */
%}
"_procreq_"    { BEGIN PROCREQ_; }
<PROCREQ_>{Separator}+[_a-zA-Z]   { /* an identifier after PROCREQ */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 { /* it is the first item in the line */
                   hb_comp_iState = PROCREQ;
                   return PROCREQ;
                 }
                 else
                 { /* there is another item in line already */
                   yylval.string = hb_compIdentifierNew( "_PROCREQ_", TRUE );
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<PROCREQ_>.|\n { /* any character after PROCREQ */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 yylval.string = hb_compIdentifierNew( "_PROCREQ_", TRUE );
                 hb_comp_iState = IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}

"decl"|"decla"|"declar"|"declare" {
    BEGIN DECLARE_;
    yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
}
<DECLARE_>{Separator}+[_a-zA-Z] { /* an Identifier after DECLARE */
    unput( yytext[ yyleng-1 ] );
    BEGIN DECLARE_ID_;
}
<DECLARE_>{Separator}+[\&] { /* a macro after DECLARE */
    BEGIN 0;
    unput( yytext[ yyleng-1 ] );
    hb_comp_iState = PRIVATE;
    return PRIVATE;
}
<DECLARE_>.|\n   { /* any other character after DECLARE */
    BEGIN 0;
    unput( yytext[ yyleng-1 ] );
    hb_comp_iState = IDENTIFIER;
    return IDENTIFIER;
}

<DECLARE_ID_>({Identifier}|{MacroTxt}){Separator}*[\n\,\[\:\;] { /* variable declaration */
    BEGIN 0;
    yyless(0);
    hb_comp_iState = PRIVATE;
    return PRIVATE;
}
<DECLARE_ID_>.|\n          {
    BEGIN 0;
    unput( yytext[ yyleng-1 ] );
    hb_comp_iState = DECLARE;
    return DECLARE;
}
%{
/* ************************************************************************ */
%}
"opti"|"optio"|"option"|"optiona"|"optional"    { BEGIN OPTIONAL_;
                                     yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
                                  }
<OPTIONAL_>{Separator}+"with"{Separator}+[^ .] { /* DO OPTIONAL WITH ...*/
                         BEGIN 0;
						 /* Push back the last character. */
                         unput( yytext[ --yyleng ] );

                         /* Remove possible white space trailing the "with". */
                         while( yytext[ yyleng - 1 ] < 48 )
                             yyleng--;

						 unput( ' ' );

						 /* Push back the "with". */
						 unput( 'h' );
						 unput( 't' );
						 unput( 'i' );
						 unput( 'w' );

						 yyleng -= 4;
						 yytext[ yyleng ] = 0;

                         hb_comp_iState = IDENTIFIER;
						 return IDENTIFIER;
                      }
<OPTIONAL_>{Separator}+"to"{Separator}+[^ .]   { /* FOR nVar := OPTIONAL TO ...*/
                         BEGIN 0;
                         /* Push back the last character. */
                         unput( yytext[ --yyleng ] );

                         /* Remove possible white space trailing the "to". */
                         while( yytext[ yyleng - 1 ] < 48 )
                            yyleng--;

						 unput( ' ' );

						 /* Push back the "to". */
						 unput( 'o' );
						 unput( 't' );

						 yyleng -= 2;
						 yytext[ yyleng ] = 0;

   		                 hb_comp_iState = IDENTIFIER;
						 return IDENTIFIER;
				       }

<OPTIONAL_>{Separator}+[_a-zA-Z\&] { /* an Identifier after OPTIONAL */
                         BEGIN 0;
                 		 unput( yytext[ yyleng-1 ] );

				     /* OPTIONAL as first item on a line can't be a qualifier. */
                 		     if( hb_comp_iState == LOOKUP )
                 		     { /* it is first item in the line */
                   		       hb_comp_iState = IDENTIFIER;
                   		       return IDENTIFIER;
                 		     }
                 		     else
                 		     { /* Now it should be a qalifier. */
                   		       hb_comp_iState = OPTIONAL;
                   		       return OPTIONAL;
                 		     }
        	           }
<OPTIONAL_>.|\n   { /* any character (not identifier) after OPTIONAL */
                            BEGIN 0;
                 		    unput( yytext[ yyleng-1 ] );

                            hb_comp_iState = IDENTIFIER;
                 		    return IDENTIFIER;
               			  }
%{
/* ************************************************************************ */
%}

"do"            BEGIN DO_;
<DO_>{Separator}+"case"         { /* DO CASE statement */
                BEGIN 0;
                hb_comp_iState =DOCASE;
                return DOCASE;
              }
<DO_>{Separator}+"while" { /* DO WHILE found -move it to WHILE state */
                /* NOTE: we cannot decide here if it is DO WHILE <condition>
                 * or DO while [WITH <args>]
                 */
                BEGIN 0;
                hb_comp_iState =DO;
                yyless( yyleng-5 );
              }
<DO_>{Separator}+"whil" { /* DO WHILE found -move it to WHILE state */
                /* NOTE: we cannot decide here if it is DO WHILE <condition>
                 * or DO while [WITH <args>]
                 */
                BEGIN 0;
                hb_comp_iState =DO;
                yyless( yyleng-4 );
              }
<DO_>{Separator}+[_a-zA-Z\&]   { /* an identifier 'DO id WITH' or 'DO &id WITH' */
                BEGIN 0;
                unput( yytext[ yyleng-1 ] );
                if( hb_comp_iState == LOOKUP )
                { /* it is first item in the line */
                  hb_comp_iState =DO;
                  return DO;
                }
                else
                { /* there is another item in line already */
                  yylval.string = hb_compIdentifierNew( "DO", TRUE );
                  hb_comp_iState =IDENTIFIER;
                  return IDENTIFIER;
                }
              }
<DO_>{Separator}*(.|\n)         { /* end of line or any operator */
                BEGIN 0;
                unput( yytext[ yyleng-1 ] );
                yylval.string = hb_compIdentifierNew( "DO", TRUE );
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
               BEGIN 0;
               if( hb_comp_iState == LOOKUP )
               { /* Clipper does not like end[] & end() at the begining of line */
                 hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_ENDIF, NULL, NULL );
               }
               yylval.string = hb_compIdentifierNew( "END", TRUE );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<END_>{Separator}*("->"|"++"|"--") { /* operators */
               BEGIN 0;
               if( hb_comp_iState == LOOKUP )
               { /* Clipper does not like end-> & end++ at the begining of line */
                 hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_ENDIF, NULL, NULL );
               }
               yylval.string = hb_compIdentifierNew( "END", TRUE );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<END_>{Separator}*[\+\-\:\=\|\$\%\*\,\/\[\]\)\}\^] { /* there is an operator after "end" */
               BEGIN 0;
               yylval.string = hb_compIdentifierNew( "END", TRUE );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<END_>{Separator}*(.|\n) { /* not operator */
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( hb_comp_iState == LOOKUP )
               { /* it is first item in the line */
                 hb_comp_iState =END;
                 return END;
               }
               else
               { /* there is another item in line already */
                 yylval.string = hb_compIdentifierNew( "END", TRUE );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
             }
%{
/* ************************************************************************ */
%}

"exit"         { BEGIN EXIT_; }
<EXIT_>{Separator}*[\n;]    { /* EXIT last item in the line */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 { /* it is first item in the line */
                   if( hb_comp_wForCounter == 0 && hb_comp_wWhileCounter == 0 )
                     hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_UNMATCHED_EXIT, "EXIT", NULL );
                   hb_comp_iState =EXITLOOP;
                   return EXITLOOP;
                 }
                 else
                 { /* there is another item in line already */
                   yylval.string = hb_compIdentifierNew( "EXIT", TRUE );
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<EXIT_>{Separator}+[fFpP] { /* FUNCTION or PROCEDURE after EXIT */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 { /* it is first item in the line */
                   hb_comp_iState =EXIT;
                   return EXIT;
                 }
                 else
                 { /* there is another item in line already */
                   yylval.string = hb_compIdentifierNew( "EXIT", TRUE );
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<EXIT_>{Separator}*. { /* any character (not identifier) after EXIT */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 yylval.string = hb_compIdentifierNew( "EXIT", TRUE );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"exte"|"exter"|"extern"|"externa"|"external" { BEGIN EXTERNAL_;
                   yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
                 }
<EXTERNAL_>{Separator}+[_a-zA-Z] { /* an identifier after the EXTERNAL */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 {
                   hb_comp_iState =EXTERN;
                   return EXTERN;
                 }
                 else
                 {
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<EXTERNAL_>.|\n   {
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"_fie"|"_fiel"|"_field" { BEGIN FIELD_;
                   yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
                 }

"fiel"|"field"   { BEGIN FIELD_;
                   yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
                 }
<FIELD_>{Separator}+[_a-zA-Z] { /* an identifier after the FIELD */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 {
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
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 unput( yytext[ yyleng-2 ] );
                 hb_comp_iState =FIELD;
                 return FIELD;
               }
<FIELD_>.|\n   {
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"for"          { BEGIN FOR_; }
<FOR_>{Separator}+[&_a-zA-Z] { /* an identifier or a macro after the FOR */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 {
                   hb_comp_iState =FOR;
                   return FOR;
                 }
                 else
                 { /* for example: DO for WITH variable */
                   yylval.string = hb_compIdentifierNew( "FOR", TRUE );
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<FOR_>{Separator}*[\(] { /* function call */
               BEGIN 0;
               if( hb_comp_iState == LOOKUP )
               { /* Clipper does not like FOR() at the begining of line */
                 hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, yytext, NULL );
               }
               yylval.string = hb_compIdentifierNew( "FOR", TRUE );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<FOR_>.|\n { /* there is no identifier after "FOR" */
               BEGIN 0;
               yylval.string = hb_compIdentifierNew( "FOR", TRUE );
               unput( yytext[ yyleng-1 ] );
               hb_comp_iState =IDENTIFIER;
               return IDENTIFIER;
             }
%{
/* ************************************************************************ */
%}
"func"|"funct"|"functi"|"functio"|"function" { BEGIN FUNCTION_; }
<FUNCTION_>{Separator}+[_a-zA-Z] {
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               hb_comp_iState=FUNCTION;
               return FUNCTION;
             }
<FUNCTION_>.|\n { /* Clipper needs FUNCTION in one context only */
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
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
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               hb_comp_iState=IIF;
               return IIF;
             }
<IIF_>{Separator}*[^\(] {
               BEGIN 0;
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
<IF_>{Separator}*"(" { BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( hb_comp_iState == LOOKUP )
                 hb_comp_iState =IF;
               else
                 hb_comp_iState =IIF;
               return hb_comp_iState;
             }
<IF_>{Separator}*[\)\]\/\^\*\%\=\$\@] { BEGIN 0;
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX2, yytext, "IF" );
             }
<IF_>{Separator}*"->" { BEGIN 0;
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX2, yytext, "IF" );
             }
<IF_>{Separator}*[\n] { BEGIN 0;
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, "IF", NULL );
             }
<IF_>{Separator}*("++"|"--")/[\n] { BEGIN 0;
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX2, yytext, "IF" );
             }
<IF_>{Separator}*. { BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               hb_comp_iState =IF;
               return IF;
             }
%{
/* ************************************************************************ */
%}
"in"           BEGIN IN_;
<IN_>{Separator}+[_a-zA-Z]       { BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == IDENTIFIER )
                   return IN;
                 else
                 {
                   yylval.string = hb_compIdentifierNew( "IN", TRUE );
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
             }
<IN_>{Separator}*[0-9]      { BEGIN 0;
                hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, yytext, NULL );
             }
<IN_>.|\n      { BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               yylval.string = hb_compIdentifierNew( "IN", TRUE );
               hb_comp_iState =IDENTIFIER;
               return IDENTIFIER;
             }
%{
/* ************************************************************************ */
%}
"init"         BEGIN INIT_;
<INIT_>{Separator}+[fFpP] { /* FUNCTION or PROCEDURE after INIT */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 { /* it is first item in the line */
                   hb_comp_iState =INIT;
                   return INIT;
                 }
                 else
                 { /* there is another item in line already */
                   yylval.string = hb_compIdentifierNew( "INIT", TRUE );
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<INIT_>.|\n { /* any character (not identifier) after EXIT */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 yylval.string = hb_compIdentifierNew( "INIT", TRUE );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"#"{Separator}*"line"  return LINE;
"loca"|"local"  {
                  yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
                  BEGIN LOCAL_;
                }
<LOCAL_>{Separator}+[_a-zA-Z]   { /* an identifier after LOCAL */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 { /* it is first item in the line */
                   hb_comp_iState =LOCAL;
                   return LOCAL;
                 }
                 else
                 { /* there is another item in line already */
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<LOCAL_>.|\n { /* any character (not identifier) after LOCAL */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"loop"         BEGIN LOOP_;
<LOOP_>{Separator}*[\n;]   { /* at the end of the line */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 { /* it is first item in the line */
                   if( hb_comp_wWhileCounter == 0 && hb_comp_wForCounter == 0 )
                     hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_UNMATCHED_EXIT, "LOOP", NULL );
                   hb_comp_iState =LOOP;
                   return LOOP;
                 }
                 else
                 { /* there is another item in line already */
                   yylval.string = hb_compIdentifierNew( "LOOP", TRUE );
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<LOOP_>{Separator}*. { /* any character (not LF) after LOOP */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 yylval.string = hb_compIdentifierNew( "LOOP", TRUE );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"memv"|"memva"|"memvar" { BEGIN MEMVAR_;
                  yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
                }
<MEMVAR_>{Separator}+[_a-zA-Z]   { /* an identifier after MEMVAR */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 { /* it is the first item in the line */
                   hb_comp_iState =MEMVAR;
                   return MEMVAR;
                 }
                 else
                 { /* there is another item in line already */
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<MEMVAR_>.|\n { /* any character (not identifier) after MEMVAR */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"next"         BEGIN NEXT_;
<NEXT_>{Separator}*[\n\;] { /* at the end of line */
               BEGIN 0;
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
                 yylval.string = hb_compIdentifierNew( "NEXT", TRUE );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
             }
<NEXT_>{Separator}*[\[\(] { /* array, function call */
               BEGIN 0;
               if( hb_comp_iState == LOOKUP )
               { /* Clipper does not like NEXT[] & NEXT() at the begining of line */
                 hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_NEXTFOR, NULL, NULL );
               }
               yylval.string = hb_compIdentifierNew( "NEXT", TRUE );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<NEXT_>{Separator}*("->"|"++"|"--") { /* operators */
               BEGIN 0;
               if( hb_comp_iState == LOOKUP )
               { /* Clipper does not like next-> & next++ at the begining of line */
                 hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_NEXTFOR, NULL, NULL );
               }
               yylval.string = hb_compIdentifierNew( "NEXT", TRUE );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<NEXT_>{Separator}*[^_a-zA-Z] { /* there is no identifier after "next" */
               BEGIN 0;
               yylval.string = hb_compIdentifierNew( "NEXT", TRUE );
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<NEXT_>{Separator}*. { /* an identifier follows NEXT statement */
               BEGIN 0;
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
                 yylval.string = hb_compIdentifierNew( "NEXT", TRUE );
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
                  yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
                  BEGIN OTHERWISE_;
               }
<OTHERWISE_>{Separator}*[\n\;]     { /* end of line */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 { /* it is the first item in the line */
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
                   BEGIN 0;
                   unput( yytext[ yyleng-1 ] );
                   hb_comp_iState = IDENTIFIER;
                   return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"para"|"param"|"parame"|"paramet"|"paramete"|"parameter"|"parameters"   {
                  yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
                  BEGIN PARAM_;
                }
<PARAM_>{Separator}+[_a-zA-Z]   { /* an identifier after PARAMETERS */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 { /* it is the first item in the line */
                   hb_comp_iState =PARAMETERS;
                   return PARAMETERS;
                 }
                 else
                 { /* there is another item in line already */
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<PARAM_>.|\n { /* any character (not identifier) after PARAMETERS */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"priv"("ate"|"at"|"a")? { BEGIN PRIVATE_;
                  yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
                }
<PRIVATE_>{Separator}+[_a-zA-Z\&] { /* an Identifier after PRIVATE */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 { /* it is first item in the line */
                   hb_comp_iState =PRIVATE;
                   return PRIVATE;
                 }
                 else
                 { /* there is another item in line already */
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<PRIVATE_>.|\n { /* any character (not identifier) after PRIVATE */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"proc"|"proce"|"proced"|"procedu"|"procedur"|"procedure"    BEGIN PROCEDURE_;
<PROCEDURE_>{Separator}+[_a-zA-Z] {
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               hb_comp_iState = PROCEDURE;
               return PROCEDURE;
             }
<PROCEDURE_>.|\n   { /* Clipper needs PROCEDURE in one context only */
               BEGIN 0;
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, ((yytext[ yyleng-1 ]=='\n')?"PROCEDURE":yytext), NULL );
             }
%{
/* ************************************************************************ */
%}
"publ"("ic"|"i")? { BEGIN PUBLIC_;
                  yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
                }
<PUBLIC_>{Separator}+[_a-zA-Z\&]   { /* an identifier after PUBLIC */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 { /* it is first item in the line */
                   hb_comp_iState =PUBLIC;
                   return PUBLIC;
                 }
                 else
                 { /* there is another item in line already */
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<PUBLIC_>.|\n { /* any character (not identifier) after PUBLIC */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
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
                  yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
                  BEGIN RECOVER_;
                }
<RECOVER_>{Separator}*[\n]    { /* end of line */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 { /* it is first item in the line */
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
                   BEGIN 0;
                   hb_comp_iState = RECOVERUSING;
                   return RECOVERUSING;
                 }
<RECOVER_>{Separator}*.            { /* all other cases */
                   BEGIN 0;
                   unput( yytext[ yyleng-1 ] );
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
%{
/* ************************************************************************ */
%}
"retu"|"retur"|"return"  {
                  yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
                  BEGIN RETURN_;
                }
<RETURN_>{Separator}+[\&_a-zA-Z0-9]   { /* an identifier, numbers or macro */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 { /* it is the first item in the line */
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
               BEGIN 0;
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<RETURN_>{Separator}*("++"|"--") { /* operators */
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
                 if( hb_comp_iState == LOOKUP )
                 { /* it is the first item in the line */
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
               BEGIN 0;
               hb_comp_iState = RETURN;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return RETURN;
             }
<RETURN_>{Separator}*[\n\;\(\[\{\"\'\.\-\+\!]   {
                 /* EOL or '()', '[]', '{}', '""', "''" , '.T.', '-', '+', '!' */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 { /* it is the first item in the line */
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
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"stat"|"stati"|"static"  {
                  yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
                  BEGIN STATIC_;
                }
<STATIC_>{Separator}+[_a-zA-Z]   { /* an identifier after STATIC */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 { /* it is first item in the line */
                   hb_comp_iState = STATIC;
                   return STATIC;
                 }
                 else
                 { /* there is another item in line already */
                   hb_comp_iState = IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<STATIC_>.|\n { /* any character (not identifier) after STATIC */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
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
                BEGIN 0;
                unput( '\n' );
                if( hb_comp_iState == DO )
                { /* we have DO while - replace it with while() */
                  unput( ')' ); unput( '(' );
                }
                yylval.string = hb_compIdentifierNew( "WHILE", TRUE );
                return IDENTIFIER;
              }
<WHILE_>{Separator}*[\[] { /* array */
               /* Clipper does not like while[] at all */
               BEGIN 0;
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, yytext, NULL );
             }
<WHILE_>{Separator}*[\:\=\|\$\%\*\,\/\]\)\}\^] { /* there is an operator after "case" */
               BEGIN 0;
               yylval.string = hb_compIdentifierNew( "WHILE", TRUE );
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<WHILE_>{Separator}*("+="|"-="|"->") { /* operators */
               BEGIN 0;
               yylval.string = hb_compIdentifierNew( "WHILE", TRUE );
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<WHILE_>{Separator}*("::") { /* send operators */
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               hb_comp_iState =WHILE;
               return WHILE;
             }
<WHILE_>{Separator}*.      { /* identifiers and literals */
                BEGIN 0;
                unput( yytext[ yyleng-1 ] );
                if( hb_comp_iState == LOOKUP || hb_comp_iState == DO )
                { /* it is first item in the line or after DO or FIELD */
                  hb_comp_iState =WHILE;
                  return WHILE;
                }
                else
                { /* there is another item in line already */
                  yylval.string = hb_compIdentifierNew( "WHILE", TRUE );
                  hb_comp_iState =IDENTIFIER;
                  return IDENTIFIER;
                }
              }
%{
/* ************************************************************************ */
%}
"with"        BEGIN WITH_;
<WITH_>{Separator}*\n           { /* at the end of line */
                BEGIN 0;
                unput( '\n' );
                yylval.string = hb_compIdentifierNew( "WITH", TRUE );
                return IDENTIFIER;
              }
<WITH_>{Separator}*"with"       {
                BEGIN 0;
                yyless( yyleng-4 );
                if( hb_comp_iState == DO )
                { /* DO with */
                  hb_comp_iState =IDENTIFIER;
                  yylval.string = hb_compIdentifierNew( "WITH", TRUE );
                  return IDENTIFIER;
                }
                else
                { /* DO WITH with <arg> */
                  hb_comp_iState =WITH;
                  return WITH;
                }
              }
<WITH_>{Separator}*[\)\(] { /* ( with ) or with() */
                BEGIN 0;
                unput( yytext[ yyleng-1 ] );
                hb_comp_iState =IDENTIFIER;
                yylval.string = hb_compIdentifierNew( "WITH", TRUE );
                return IDENTIFIER;
              }
<WITH_>{Separator}*[\[] { /* array */
               /* Clipper does not like with[] at all */
               BEGIN 0;
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, yytext, NULL );
             }
<WITH_>{Separator}*.            {
                BEGIN 0;
                unput( yytext[ yyleng-1 ] );
                if( hb_comp_iState == WHILE ||
                    hb_comp_iState == DO ||
                    hb_comp_iState == MACROVAR ||
                    hb_comp_iState == MACROTEXT ||
                    hb_comp_iState == IDENTIFIER ||
                    hb_comp_iState == LSEPARATOR ||
                    hb_comp_iState == RSEPARATOR )
                { /* DO <ident> WITH <arg> */
                  hb_comp_iState =WITH;
                  return WITH;
                }
                else
                {
                  yylval.string = hb_compIdentifierNew( "WITH", TRUE );
                  hb_comp_iState =IDENTIFIER;
                  return IDENTIFIER;
                }
              }

%{
/* ************************************************************************ */
%}
"as array"           { return AS_ARRAY; }
"as codeblock"       { return AS_BLOCK; }
"as string"          { return AS_CHARACTER; }
"as class"           { return AS_CLASS; }
"as date"            { return AS_DATE; }
"as logical"         { return AS_LOGICAL; }
"as numeric"         { return AS_NUMERIC; }
"as object"          { return AS_OBJECT; }
"as anytype"         { return AS_VARIANT; }

"as array of anytype"   { return AS_ARRAY; }
"as array of array"     { return AS_ARRAY_ARRAY; }
"as array of codeblock" { return AS_BLOCK_ARRAY; }
"as array of string"    { return AS_CHARACTER_ARRAY; }
"as array of class"     { return AS_CLASS_ARRAY; }
"as array of date"      { return AS_DATE_ARRAY; }
"as array of logical"   { return AS_LOGICAL_ARRAY; }
"as array of numeric"   { return AS_NUMERIC_ARRAY; }
"as array of object"    { return AS_OBJECT_ARRAY; }

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
[,\|\#\&\.\:\<\>\@] hb_comp_iState =OPERATOR; return yytext[ 0 ];
[\{]           hb_comp_iState =LARRAY; return yytext[ 0 ];
[\}]           hb_comp_iState =RARRAY; return yytext[ 0 ];
[\]]           hb_comp_iState =RINDEX; return yytext[ 0 ];
[\(]           ++_iOpenBracket; hb_comp_iState =LSEPARATOR; return yytext[ 0 ];
[\)]           --_iOpenBracket; hb_comp_iState =RSEPARATOR; return yytext[ 0 ];

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
                   BEGIN 0;
                   hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_NUMERIC_FORMAT, NULL, NULL );
                 }
<INVALIDNUM_>.   {
                    BEGIN 0;
                    unput( yytext[ yyleng-1 ] );
                    unput( '.' );
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

{TrueValue}   { hb_comp_iState =RSEPARATOR; return TRUEVALUE;  }
{FalseValue}  { hb_comp_iState =RSEPARATOR; return FALSEVALUE; }

{MacroVar}     {
                  if( yytext[ yyleng-1 ] == '.' )
                     yytext[ yyleng-1 ] = '\0';
                  yylval.string = hb_compIdentifierNew( hb_strupr( yytext+1 ), TRUE );
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
                   yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
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

