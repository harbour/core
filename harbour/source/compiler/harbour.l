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

/* NOTE: Uncomment this YY_USER_ACTION definition if you want to use
 '^' match marker (beginning of line)
*/
/*
#define YY_USER_ACTION \
           if ( yyleng > 0 ) \
              yy_current_buffer->yy_at_bol = ( yytext[yyleng - 1] == '\n' || yytext[0] == '\n' );
*/

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

%x            STRING1 STRING2 STRING3 STRING4START STRING4
%x            NEXT_ BREAK_ CASE_ DO_ WHILE_ WITH_ END_ FIELD_
%x            FOR_ FUNCTION_ IIF_ IF_ IN_ INIT_
%x            RETURN_ RECOVER_
%x            INVALIDNUM_ OTHERWISE_ PROCEDURE_
%x            DECLARE_ DECLARE_ID_

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
                        (hb_comp_iState == BREAK) ||
                        (hb_comp_iState == RETURN) ||
                        (hb_comp_iState == WITH) ||
                        (hb_comp_iState == WHILE)
                      )
                        BEGIN STRING3;
                    else
                    {
                        hb_comp_iState = LINDEX;
                        return '[';
                    }
                  }

"QOUT(["    {
    /* preprocessed TEXT/ENDTEXT line
    NOTE: Clipper preprocesses TEXT/ENDTEXT block into a series of QOUT calls
    QOUT("...")<EndOfLine>
    or
    QOUT([...])<EndOfLine>
    [] is used if text contains any string delimiters "'[]
    NOTE: Clipper allows for [[nested]] only if it is preprocessed from
    TEXT/ENDTEXT block - in normal code the preprocesor uses the first
    closing ] as a string terminator.
    However we have to get some information from the preprocessor about
    TEXT/ENDTEXT block start/end condition to distinguish code:
    TEXT
    some string]) ; qout([some more
    ENDTEXT
    from direct calls for qout function
    QOUT([some string]) ; QOUT([some more])
    */
/* TODO: implement hb_ppInsideTextBlock it is PP
    if( hb_ppInsideTextBlock )
    {
        BEGIN STRING4START;
        unput( '(' );
    }
    else
*/
        yyless( 4 );    /* len of QOUT */

    yylval.string = hb_compIdentifierNew( "QOUT", TRUE );
    hb_comp_iState = IDENTIFIER;
    return IDENTIFIER;
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

<STRING3>.*\n  {
    /* Preprocessed strings can contain embeded ] chars, for example
    #translate TEST_STRING( <x> ) => #<x>
    ? TEST_STRING( "['']" ) is preprocessed into ["['']"] so the string
    terminator is "] pair
    ? TEST_STRING( ['""'] ) is preprocessed into [['""']] so the string
    terminator is ]] pair
    There is however a single problem: any string with nested []
    created by the preprocessor is compiled correctly in Clipper but
    the same string passed directly in the code is compiled until
    the first closing bracket is found! We are not following it here
    because at this moment there is no possibility to distinguish
    strings created by the preprocessor.
    */
    char StopChar = yytext[ 0 ];
    int iFirstPos = 0;
    BEGIN 0;

    if( (StopChar == '"') || (StopChar == '\'')  || (StopChar == '[') )
    {
       int i;
       if( StopChar == '[' )
           StopChar = ']';
       for( i = 0; i < yyleng-1; ++i )
       {
           if( (yytext[ i ] == StopChar) && yytext[ i + 1 ] == ']' )
           {
               /* "] or '] terminator was found */
               yyless( i+2 );
               yytext[ i+1 ] = '\0';
               yylval.string = hb_compIdentifierNew( yytext, TRUE );
               hb_comp_iState = LITERAL;

               return LITERAL;
           }
           else if( (yytext[ i ] == ']') && iFirstPos == 0 )
               iFirstPos = i;
       }
       if( iFirstPos > 0 )
       {
           yyless( iFirstPos+1 );
           yytext[ iFirstPos ] = '\0';
           yylval.string = hb_compIdentifierNew( yytext, TRUE );
           hb_comp_iState = LITERAL;

           return LITERAL;
       }
    }
    else
    {
        /* look for the first closing ] character */
        int i;
        for( i = 0; i < yyleng - 1; ++i )
        {
            if( yytext[ i ] == ']' )
            {
               yyless( i+1 );
               yytext[ i ] = '\0';
               yylval.string = hb_compIdentifierNew( yytext, TRUE );
               hb_comp_iState = LITERAL;

               return LITERAL;
            }
        }
    }
    /* If we are here then the terminator was not found - report an error */
    unput( '\n' );
    yytext[--yyleng] = '\0';
    hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_STRING_TERMINATOR, yytext, NULL );
    hb_comp_iState = LOOKUP;

    return LITERAL;
}

<STRING4START>\(   { BEGIN STRING4; return( '(' ); }
<STRING4START>.    { BEGIN 0; unput( yytext[ yyleng-1 ] ); }

<STRING4>.*\]\)\n  { BEGIN 0;
                      unput( '\n' );
                      unput( ')' );
                      yyleng -= 3;
                      yytext[ yyleng ] = '\0';
                      yylval.string = hb_compIdentifierNew( yytext, TRUE );
                      hb_comp_iState = LITERAL;

                      return LITERAL;
                    }
<STRING4>.        { BEGIN STRING3; unput( yytext[ yyleng-1 ] ); }

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
"announce"|"announc"|"announ"|"annou"|"anno"        {
                  hb_comp_iState =IDENTIFIER;
                  yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
                  return ANNOUNCE;
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
<BREAK_>{Separator}*    ;
<BREAK_>[\n;] { /* at the end of line */
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               return BREAK;
             }
%{
/* NOTE: Clipper does not like break[] in any context
 *       There are no resons to limit this use in Harbour.
 */
/*
<BREAK_>[\[] {  BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, yytext, NULL );
             }
*/
%}
<BREAK_>(":="|"+="|"-="|"->"|"*="|"/="|"^="|"==") { /* operators */
               BEGIN 0;
               yylval.string = hb_compIdentifierNew( "BREAK", TRUE );
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               hb_comp_iState =IDENTIFIER;
               return IDENTIFIER;
             }
<BREAK_>("++"|"--") { /* operators */
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
<BREAK_>[\=\(] { /* operators = ( */
               BEGIN 0;
               yylval.string = hb_compIdentifierNew( "BREAK", TRUE );
               unput( yytext[ yyleng-1 ] );
               hb_comp_iState =IDENTIFIER;
               return IDENTIFIER;
             }
<BREAK_>. { /* all other cases */
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
<CASE_>{Separator}*    ;
<CASE_>[\:\=\|\$\%\*\,\/\]\)\}\^] { /* there is an operator after "case" */
               BEGIN 0;
               yylval.string = hb_compIdentifierNew( "CASE", TRUE );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<CASE_>("+="|"-="|"->") { /* operators */
               BEGIN 0;
               yylval.string = hb_compIdentifierNew( "CASE", TRUE );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<CASE_>("::") { /* send operators */
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               if( hb_comp_wCaseCounter == 0 )
                  hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_CASE, NULL, NULL );
               hb_comp_iState =CASE;
               return CASE;
             }
<CASE_>(\n|.) { /* not operator */
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( hb_comp_iState == LOOKUP )
               { /* it is first item in the line */
                  if( hb_comp_wCaseCounter == 0 )
                     hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_CASE, NULL, NULL );
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
"_procreq_("    {
                   yylval.string = hb_compIdentifierNew( "_PROCREQ_", TRUE );
                   hb_comp_iState =IDENTIFIER;
                   return PROCREQ;
               }
%{
/* ************************************************************************ */
%}

"decl"|"decla"|"declar"|"declare" {
    yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
    if( hb_comp_iState == DO )
    {
        hb_comp_iState = IDENTIFIER;
        return IDENTIFIER;
    }
    else
        BEGIN DECLARE_;
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

<DECLARE_ID_>({Identifier}|{MacroTxt}){Separator}*"as"{Separator}+ { /* variable declaration */
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
"opti"|"optio"|"option"|"optiona"|"optional"    {
    yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
    return OPTIONAL;
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
<END_>{Separator}*    ;
<END_>[\[\(] { /* array, function call */
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
<END_>("->"|"++"|"--") { /* operators */
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
<END_>[\+\-\:\=\|\$\%\*\,\/\[\]\)\}\^] { /* there is an operator after "end" */
               BEGIN 0;
               yylval.string = hb_compIdentifierNew( "END", TRUE );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<END_>(.|\n) { /* not operator */
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

"exit"         {
                   hb_comp_iState =IDENTIFIER;
                   return EXIT;
               }
%{
/* ************************************************************************ */
%}
"exte"|"exter"|"extern"|"externa"|"external" {
                   yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
                   hb_comp_iState =IDENTIFIER;
                   return EXTERN;
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
               unput( yytext[ yyleng-1 ] );
               if( hb_comp_iState == LOOKUP )
               { /* Clipper always assume FOR (somevar):=1 TO ... here */
                   hb_comp_iState =FOR;
                   return FOR;
               }
               else
               {
                   yylval.string = hb_compIdentifierNew( "FOR", TRUE );
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
               }
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

"hb_inline" {
	    char sBuffer[ YY_BUF_SIZE ], *pBuffer, sInlineSym[] = "HB_INLINE_0";
	    int iSize, iBraces = 0;
	    extern BOOL hb_pp_bInline;
            PINLINE pInline;

            hb_pp_bInline = TRUE;

	    sInlineSym[10] = hb_comp_cInlineID++;

	    switch( sInlineSym[10] )
	    {
	       case '9' + 1 :
		  sInlineSym[10] = 'A';
		  break;

	       case 'Z' + 1 :
                  hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_TOOMANY_INLINE, NULL, NULL );
		  break;
	    }

            pInline = hb_compInlineAdd( hb_compIdentifierNew( sInlineSym, TRUE ) );

	  DigestInline :

            YY_INPUT( (char*) sBuffer, iSize, YY_BUF_SIZE );
	    if( iSize == 0 )
	    {
               hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_INVALID_INLINE, hb_comp_functions.pLast->szName, NULL );
	       hb_pp_bInline = FALSE;
	       return '\n';
	    }
	    pBuffer = (char*) sBuffer;

	    while( *pBuffer && ( *pBuffer == ' ' || *pBuffer == '\t' ) )
	    {
               pBuffer++;
	    }

	    if( *pBuffer == '\0' )
	    {
	       goto DigestInline;
	    }
	    else if( *pBuffer == '{' )
	    {
	       iBraces++;
	    }
	    else if( *pBuffer == '}' && iBraces > 1 )
	    {
	       iBraces--;
	    }
	    else if( *pBuffer == '}' )
	    {
	       if( pInline->pCode == NULL )
	       {
	          pInline->pCode = hb_xgrab( ( iSize = strlen( (char*) sBuffer ) ) + 1 );
	          strcpy( pInline->pCode, (char*) sBuffer );
	       }
	       else
	       {
	          pInline->pCode = hb_xrealloc( pInline->pCode, pInline->lPCodeSize + ( iSize = strlen( (char*) sBuffer ) ) + 1 );
	          strcpy( pInline->pCode + pInline->lPCodeSize, (char*) sBuffer );
	       }

	       pInline->lPCodeSize += iSize;

	       hb_pp_bInline = FALSE;
               yylval.string = hb_compIdentifierNew( sInlineSym, TRUE );
               return IDENTIFIER;
	    }

	    if( pInline->pCode == NULL )
	    {
	       pInline->pCode = hb_xgrab( ( iSize = strlen( (char*) sBuffer ) ) + 1 );
	       strcpy( pInline->pCode, (char*) sBuffer );
	    }
	    else
	    {
	       pInline->pCode = hb_xrealloc( pInline->pCode, pInline->lPCodeSize + ( iSize = strlen( (char*) sBuffer ) ) + 1 );
	       strcpy( pInline->pCode + pInline->lPCodeSize, (char*) sBuffer );
	    }

	    pInline->lPCodeSize += iSize;

	    goto DigestInline;
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
<IIF_>{Separator}*   ;
<IIF_>"(" {
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               hb_comp_iState=IIF;
               return IIF;
             }
<IIF_>[^\(] {
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
<IF_>{Separator}*    ;
<IF_>"(" { BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( hb_comp_iState == LOOKUP )
                 hb_comp_iState =IF;
               else
                 hb_comp_iState =IIF;
               return hb_comp_iState;
             }
<IF_>[\)\]\/\^\*\%\=\$\@] { BEGIN 0;
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX2, yytext, "IF" );
             }
<IF_>"->" { BEGIN 0;
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX2, yytext, "IF" );
             }
<IF_>\n { BEGIN 0;
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, "IF", NULL );
             }
<IF_>("++"|"--")/[\n] { BEGIN 0;
               hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX2, yytext, "IF" );
             }
<IF_>. { BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               hb_comp_iState =IF;
               return IF;
             }
%{
/* ************************************************************************ */
%}
"in"          { hb_comp_iState =IDENTIFIER; return IN; }
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
<INIT_>.|\n { /* any character (not identifier) after INIT */
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
%{
/* ************************************************************************ */
%}
"loca"|"local"  {
                  yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
                  hb_comp_iState =IDENTIFIER;
                  return LOCAL;
                }
%{
/* ************************************************************************ */
%}
"loop"         { hb_comp_iState =IDENTIFIER; return LOOP; }
%{
/* ************************************************************************ */
%}
"memv"|"memva"|"memvar" {
                  yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
                  hb_comp_iState =IDENTIFIER;
                  return MEMVAR;
                }
%{
/* ************************************************************************ */
%}
"next"         BEGIN NEXT_;
<NEXT_>{Separator}*        ;
<NEXT_>[\n\;] { /* at the end of line */
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
<NEXT_>[\[\(] { /* array, function call */
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
<NEXT_>("->"|"++"|"--") { /* operators */
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
<NEXT_>[^_a-zA-Z] { /* there is no identifier after "next" */
               BEGIN 0;
               yylval.string = hb_compIdentifierNew( "NEXT", TRUE );
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<NEXT_>. { /* an identifier follows NEXT statement */
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
<OTHERWISE_>{Separator}*   ;
<OTHERWISE_>[\n\;]     { /* end of line */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 { /* it is the first item in the line */
                    if( hb_comp_wCaseCounter == 0 )
                       hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_CASE, NULL, NULL );
                    hb_comp_iState = OTHERWISE;
                    return OTHERWISE;
                 }
                 else
                 { /* there is another item in line already */
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<OTHERWISE_>.     {
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
                  hb_comp_iState =IDENTIFIER;
                  return PARAMETERS;
                }
%{
/* ************************************************************************ */
%}
"priv"("ate"|"at"|"a")? {
                  yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
                  hb_comp_iState =IDENTIFIER;
                  return PRIVATE;
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
"publ"("ic"|"i")? {
                  yylval.string = hb_compIdentifierNew( hb_strupr( yytext ), TRUE );
                  hb_comp_iState =IDENTIFIER;
                  return PUBLIC;
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
<RECOVER_>{Separator}*    ;
<RECOVER_>\n    { /* end of line */
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
<RECOVER_>("using"|"usin")    { /* USING */
                   BEGIN 0;
                   hb_comp_iState = RECOVERUSING;
                   return RECOVERUSING;
                 }
<RECOVER_>.            { /* all other cases */
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
<RETURN_>{Separator}*
<RETURN_>[\&_a-zA-Z0-9]   { /* an identifier, numbers or macro */
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
<RETURN_>("+="|"-="|"->") { /* operators */
               BEGIN 0;
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<RETURN_>("++"|"--") { /* operators */
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
<RETURN_>"::" { /* SELF operator */
               BEGIN 0;
               hb_comp_iState = RETURN;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return RETURN;
             }
<RETURN_>[\n\;\(\[\{\"\'\.\-\+\!]   {
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
<RETURN_>. { /* any other character after RETURN */
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
                  hb_comp_iState = IDENTIFIER;
                  return STATIC;
               }
%{
/* ************************************************************************ */
%}
"step"         { hb_comp_iState = IDENTIFIER; return STEP; }
"to"           { hb_comp_iState = IDENTIFIER; return TO; }
%{
/* ************************************************************************ */
%}
"while"|"whil"  BEGIN WHILE_;
<WHILE_>{Separator}*        ;
<WHILE_>\n      { /* end of line */
                BEGIN 0;
                unput( '\n' );
                if( hb_comp_iState == DO )
                { /* we have DO while - replace it with while() */
                  unput( ')' ); unput( '(' );
                }
                yylval.string = hb_compIdentifierNew( "WHILE", TRUE );
                return IDENTIFIER;
              }
<WHILE_>[\:\=\|\$\%\*\,\/\]\)\}\^] { /* there is an operator after "case" */
               BEGIN 0;
               yylval.string = hb_compIdentifierNew( "WHILE", TRUE );
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<WHILE_>("+="|"-="|"->") { /* operators */
               BEGIN 0;
               yylval.string = hb_compIdentifierNew( "WHILE", TRUE );
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<WHILE_>"::" { /* send operators */
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               hb_comp_iState =WHILE;
               return WHILE;
             }
<WHILE_>.      { /* identifiers and literals */
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
<WITH_>{Separator}*            ;
<WITH_>\n           { /* at the end of line */
                BEGIN 0;
                unput( '\n' );
                yylval.string = hb_compIdentifierNew( "WITH", TRUE );
                return IDENTIFIER;
              }
<WITH_>"with"       {
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
<WITH_>[\)] { /* ( with ) or with() */
                BEGIN 0;
                unput( yytext[ yyleng-1 ] );
                hb_comp_iState =IDENTIFIER;
                yylval.string = hb_compIdentifierNew( "WITH", TRUE );
                return IDENTIFIER;
              }
<WITH_>.            {
                BEGIN 0;
                unput( yytext[ yyleng-1 ] );
                if( hb_comp_iState == WHILE ||
                    hb_comp_iState == DO ||
                    hb_comp_iState == MACROVAR ||
                    hb_comp_iState == MACROTEXT ||
                    hb_comp_iState == IDENTIFIER ||
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
"as"{Separator}+("arra"|"array")                                           { return AS_ARRAY; }
"as"{Separator}+("code"|"codeb"|"codebl"|"codeblo"|"codebloc"|"codeblock") { return AS_BLOCK; }
"as"{Separator}+("stri"|"strin"|"string")                                  { return AS_CHARACTER; }
"as"{Separator}+("char"|"chara"|"charac"|"charact"|"characte"|"character") { return AS_CHARACTER; }
"as"{Separator}+("clas"|"class")                                           { return AS_CLASS; }
"as"{Separator}+"date"                                                     { return AS_DATE; }
"as"{Separator}+("logi"|"logic"|"logica"|"logical")                        { return AS_LOGICAL; }
"as"{Separator}+("nume"|"numer"|"numeri"|"numeric")                        { return AS_NUMERIC; }
"as"{Separator}+("obje"|"objec"|"object")                                  { return AS_OBJECT; }
"as"{Separator}+("usua"|"usual")                                           { return AS_VARIANT; }
"as"{Separator}+("anyt"|"anytyp"|"anytype")                                { return AS_VARIANT; }

"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+("usua"|"usual")                                           { return AS_ARRAY; }
"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+("anyt"|"anytyp"|"anytype")                                { return AS_ARRAY; }
"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+("arra"|"array")                                           { return AS_ARRAY_ARRAY; }
"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+("code"|"codeb"|"codebl"|"codeblo"|"codebloc"|"codeblock") { return AS_BLOCK_ARRAY; }
"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+("stri"|"strin"|"string")                                  { return AS_CHARACTER_ARRAY; }
"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+("char"|"chara"|"charac"|"charact"|"characte"|"character") { return AS_CHARACTER_ARRAY; }
"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+("clas"|"class")                                           { return AS_CLASS_ARRAY; }
"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+"date"                                                     { return AS_DATE_ARRAY; }
"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+("logi"|"logic"|"logica"|"logical")                        { return AS_LOGICAL_ARRAY; }
"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+("nume"|"numer"|"numeri"|"numeric")                        { return AS_NUMERIC_ARRAY; }
"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+("obje"|"objec"|"object")                                  { return AS_OBJECT_ARRAY; }

"_hb_class"             { hb_comp_iState = OPERATOR; return DECLARE_CLASS; }
"_hb_member"            { hb_comp_iState = OPERATOR; return DECLARE_MEMBER; }

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

[\x00-\x1F]	    return yytext[ 0 ]; /* see below */
[\~\`\?\_\\]    return yytext[ 0 ]; /* see below */
[\x7F-\xFF]	{
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

