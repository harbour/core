%option noyywrap
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
 * (at your option) any later version.
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
#include "hbdate.h"

#undef alloca
#define alloca  hb_xgrab
#undef malloc
#define malloc  hb_xgrab
#undef realloc
#define realloc hb_xrealloc
#undef free
#define free    hb_xfree

/* declaration of yylex function
 * NOTE: yylval is paassed automaticaly by bison if %pure_parser is used
 */
#undef YY_DECL
#define YY_DECL int yylex( YYSTYPE *yylval_ptr, HB_COMP_DECL )

/* helper functions */
static int yy_ConvertNumber( YYSTYPE *yylval_ptr, char * szBuffer );
static int yy_ConvertDate( YYSTYPE *yylval_ptr, char * szBuffer );

/* YACC functions */
static void yyunput( int, char * );
#undef YY_INPUT      /* to implement our own YY_INPUT function to manage PRGs without \n at the end */

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
static int hb_comp_iLineINLINE = 0;
static int hb_comp_iLinePRG;

/* flex is not MT safe :-( */
static HB_COMP_PTR s_pComp;

#define YY_OPERATOR_RET(t)    hb_comp_iState = OPERATOR; pComp->pLex->lasttok = yytext; return (t)
#define YY_TOKEN_RET(s,t)     hb_comp_iState = (s); pComp->pLex->lasttok = yytext; return (t)

#define DEBUG_STRINGS

void yy_breakFlex( void ){ };

%}

%{
#ifdef __WATCOMC__
/* disable warnings for unreachable code */
#pragma warning 13 9
#endif

#ifdef _MSC_VER
   #define YY_NEVER_INTERACTIVE TRUE
#endif
%}

SpaceTab      [ \t]+
Number        ([0-9]+)|([0-9]*\.[0-9]+)|(0x[0-9A-Fa-f]+)
InvalidNumber [0-9]+\.
Identifier    (([a-zA-Z])|([_a-zA-Z][_a-zA-Z0-9]+))
Date          0d[0-9]{8}

MacroVar      \&{Identifier}[\.]?
MacroEnd      \&{Identifier}\.([_a-zA-Z0-9]*)
%{
/*
MacroId       ({Identifier}\&(({Identifier}[\.]?)|({Identifier}\.([_a-zA-Z0-9]*))))
*/
%}
MacroId       ({Identifier}({MacroVar}|{MacroEnd}))
MacroTxt      ([_]*({MacroVar}|{MacroEnd}|{MacroId}))+

TrueValue     "."[t|y]"."
FalseValue    "."[f|n]"."

Separator     {SpaceTab}

%x            STRINGEXT STRING1 STRING2 STRING3 STRING4START STRING4 STRING5
%x            NEXT_ BREAK_ CASE_ DO_ DOIDENT_ WHILE_ WITH_ END_ FIELD_
%x            FOR_ FOREACH_ FUNCTION_ IIF_ IF_ IN_ INIT_
%x            RETURN_ RECOVER_
%x            SWITCH_
%x            INVALIDNUM_ OTHERWISE_ PROCEDURE_
%x            DECLARE_ DECLARE_ID_

%%

%{
   /* flex is not MT safe :-( */
   s_pComp = HB_COMP_PARAM;
%}

"&"(\x27|\x22|\[) { hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, yytext, NULL ); }

"e"\x22        BEGIN STRINGEXT;
\x27           BEGIN STRING1;
\x22           BEGIN STRING2;

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
                  {
                     BEGIN STRING3;
                  }
                  else
                  {
                     hb_comp_iState = LINDEX;
                     pComp->pLex->lasttok = "[";
                     return '[';
                  }
               }

<STRINGEXT>([^\x22\n]|\\\x22)*\n  { BEGIN 0;
                      unput( '\n' );
                      yytext[--yyleng] = '\0';
                      hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_STRING_TERMINATOR, yytext, NULL );
                      hb_comp_iState = LOOKUP;
                      pComp->pLex->lasttok = yytext;
                      return NIL;
                    }

<STRINGEXT>([^\x22\n]|\\\x22)*\x22  { BEGIN 0;
                        yytext[--yyleng] = '\0';
                        {
                           ULONG len = yyleng;
                           hb_strRemEscSeq( yytext, &len );
                           yyleng = len;
                        }
                        pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, yytext, HB_IDENT_COPY );
                        hb_comp_iState = LITERAL;

                        return LITERAL;
                     }

<STRING1>[^\x27\n]*\n  { BEGIN 0;
                      unput( '\n' );
                      yytext[--yyleng] = '\0';
                      hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_STRING_TERMINATOR, yytext, NULL );
                      hb_comp_iState = LOOKUP;
                      pComp->pLex->lasttok = yytext;

                      return NIL;
                    }

<STRING1>[^\x27\n]*\x27   { BEGIN 0;
                      yytext[--yyleng] = '\0';
                      pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, yytext, HB_IDENT_COPY );
                      hb_comp_iState = LITERAL;

                      return LITERAL;
                    }

<STRING2>[^\x22\n]*\x22  { BEGIN 0;
                      yytext[--yyleng] = '\0';
                      pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, yytext, HB_IDENT_COPY );
                      hb_comp_iState = LITERAL;

                      return LITERAL;
                    }

<STRING2>[^\x22\n]*\n  { BEGIN 0;
                       unput( '\n' );
                       yytext[--yyleng] = '\0';
                       hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_STRING_TERMINATOR, yytext, NULL );
                       hb_comp_iState = LOOKUP;
                       pComp->pLex->lasttok = yytext;

                       return NIL;
                    }

<STRING3>.*\n  {
    /* Preprocessed strings can contain embeded ] chars, for example
    #translate TEST_STRING( <x> ) => #<x>
    ? TEST_STRING( "['']" ) is preprocessed into ["['']"] so the string
    terminator is "] pair
    ? TEST_STRING( ['""'] ) is preprocessed into \[\['""'\]\] so the string
    terminator is \]\] pair
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
               pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, yytext, HB_IDENT_COPY );
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
           pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, yytext, HB_IDENT_COPY );
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
               pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, yytext, HB_IDENT_COPY );
               hb_comp_iState = LITERAL;

               return LITERAL;
            }
        }
    }
    /* If we are here then the terminator was not found - report an error */
    unput( '\n' );
    yytext[--yyleng] = '\0';
    hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_STRING_TERMINATOR, yytext, NULL );
    hb_comp_iState = LOOKUP;
    pComp->pLex->lasttok = yytext;

    return NIL;
}

<STRING5>.*\n  {
    /* Preprocessed strings can contain embeded ] chars - look for the last
	 	closing ']'
	 */
    int i;

    BEGIN 0;
   yy_breakFlex();
	 for( i = yyleng-1; i; --i )
    {
       if( yytext[ i ] == ']' )
       {
          /* ] terminator was found */
          yyless( i+1 );
          yytext[ i ] = '\0';
          pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, yytext, HB_IDENT_COPY );
          hb_comp_iState = LITERAL;

          return LITERAL;
       }
    }
}

<STRING4START>\(   { BEGIN STRING4; return( '(' ); }
<STRING4START>.    { BEGIN 0; unput( yytext[ yyleng-1 ] ); }

<STRING4>.*\]\)\n  { BEGIN 0;
                      unput( '\n' );
                      unput( ')' );
                      yyleng -= 3;
                      yytext[ yyleng ] = '\0';
                      pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, yytext, HB_IDENT_COPY );
                      hb_comp_iState = LITERAL;

                      return LITERAL;
                    }
<STRING4>.        { BEGIN STRING3; unput( yytext[ yyleng-1 ] ); }

%{
/* ************************************************************************ */
%}

{SpaceTab}     ;

\n           {
                hb_comp_iState = LOOKUP;
                pComp->pLex->lasttok = "\n";
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
                 pComp->pLex->lasttok = ";";
                 return ';';
             }
%{
/* ************************************************************************ */
%}
"announce"|"announc"|"announ"|"annou"|"anno"        {
                  hb_comp_iState =IDENTIFIER;
                  pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( yytext ), HB_IDENT_COPY );
                  return ANNOUNCE;
                }
%{
/* ************************************************************************ */
%}
"begin"{Separator}+"sequ"("ence"|"enc"|"en"|"e")?   {
                  pComp->pLex->lasttok = yytext;
                  return BEGINSEQ;
                }
%{
/* ************************************************************************ */
%}
"break"        { if( hb_comp_iState == LOOKUP )
                    BEGIN BREAK_;
                 else
                 {
                    pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( yytext ), HB_IDENT_COPY );
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
<BREAK_>[\[] {  BEGIN 0;
/* NOTE: Clipper does not like break[] in any context
 *       There are no resons to limit this use in Harbour.
 */
               unput( yytext[ yyleng-1 ] );
               if( HB_COMP_ISSUPPORTED( HB_COMPFLAG_HARBOUR ) )
               {
                  pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "BREAK", HB_IDENT_STATIC );
                  hb_comp_iState =IDENTIFIER;
                  return IDENTIFIER;
               }
               else
                  hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, yytext, NULL );
             }
<BREAK_>(":="|"+="|"-="|"->"|"*="|"/="|"^="|"==") { /* operators */
               BEGIN 0;
               pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "BREAK", HB_IDENT_STATIC );
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
               pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "BREAK", HB_IDENT_STATIC );
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               hb_comp_iState =BREAK;
               return BREAK;
             }
<BREAK_>[\=\(] { /* operators = ( */
               BEGIN 0;
               pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "BREAK", HB_IDENT_STATIC );
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
               pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "CASE", HB_IDENT_STATIC );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<CASE_>("+="|"-="|"->") { /* operators */
               BEGIN 0;
               pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "CASE", HB_IDENT_STATIC );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<CASE_>("::") { /* send operators */
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               if( HB_COMP_PARAM->wCaseCounter == 0 && HB_COMP_PARAM->wSwitchCounter == 0 )
                  hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_CASE, NULL, NULL );
               hb_comp_iState =CASE;
               return CASE;
             }
<CASE_>(\n|.) { /* not operator */
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( hb_comp_iState == LOOKUP )
               { /* it is first item in the line */
                  if( HB_COMP_PARAM->wCaseCounter == 0 && HB_COMP_PARAM->wSwitchCounter == 0 )
                     hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_CASE, NULL, NULL );
                 hb_comp_iState =CASE;
                 return CASE;
               }
               else
               { /* there is another item in line already */
                 pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "CASE", HB_IDENT_STATIC );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
             }
%{
/* ************************************************************************ */
%}
"_procreq_("    {
                   pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "_PROCREQ_", HB_IDENT_STATIC );
                   hb_comp_iState =IDENTIFIER;
                   return PROCREQ;
               }
%{
/* ************************************************************************ */
%}

"decl"|"decla"|"declar"|"declare" {
    pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( yytext ), HB_IDENT_COPY );
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
    pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( yytext ), HB_IDENT_COPY );
    return OPTIONAL;
}
%{
/* ************************************************************************ */
%}
"do"            BEGIN DO_;
<DO_>{Separator}+"case"         { /* DO CASE statement */
                BEGIN 0;
                hb_comp_iState =DOCASE;
                pComp->pLex->lasttok = yytext;
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
<DO_>{Separator}+[\&]   { /* 'DO &id WITH' */
                BEGIN 0;
                unput( yytext[ yyleng-1 ] );
                if( hb_comp_iState == LOOKUP )
                { /* it is first item in the line */
                  hb_comp_iState =DO;
                  pComp->pLex->lasttok = yytext;
                  return DO;
                }
                else
                { /* there is another item in line already */
                  pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "DO", HB_IDENT_STATIC );
                  hb_comp_iState =IDENTIFIER;
                  return IDENTIFIER;
                }
              }
<DO_>{Separator}+[_a-zA-Z]   { /* an identifier 'DO id WITH' */
                unput( yytext[ yyleng-1 ] );
                if( hb_comp_iState == LOOKUP )
                { /* it is first item in the line */
                  BEGIN DOIDENT_;
                }
                else
                { /* there is another item in line already */
                  pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "DO", HB_IDENT_STATIC );
                  hb_comp_iState =IDENTIFIER;
                  BEGIN 0;
                  return IDENTIFIER;
                }
              }
<DO_>{Separator}*(.|\n)         { /* end of line or any operator */
                BEGIN 0;
                unput( yytext[ yyleng-1 ] );
                pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "DO", HB_IDENT_STATIC );
                hb_comp_iState =IDENTIFIER;
                return IDENTIFIER;
              }
<DOIDENT_>{Identifier}			{ /* DO identifier WITH */
                BEGIN 0;
                pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, yytext, HB_IDENT_COPY );
                hb_comp_iState =IDENTIFIER;
                return DOIDENT;
					}
%{
/* ************************************************************************ */
%}
"descend"    {
                   pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "DESCEND", HB_IDENT_STATIC );
                   hb_comp_iState =IDENTIFIER;
                   return DESCEND;
               }
%{
/* ************************************************************************ */
%}
"else"         { /* ELSE can be used in one context only */
                 if( HB_COMP_PARAM->wIfCounter == 0 )
                   hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_UNMATCHED_ELSE, NULL, NULL );
                 hb_comp_iState =ELSE;
                 pComp->pLex->lasttok = yytext;
                 return ELSE;
               }
"elseif"       { /* ELSEIF can be used in one context only */
                 if( HB_COMP_PARAM->wIfCounter == 0 )
                   hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_UNMATCHED_ELSEIF, NULL, NULL );
                 hb_comp_iState =ELSEIF;
                 pComp->pLex->lasttok = yytext;
                 return ELSEIF;
               }
"end"{Separator}+"sequ"("ence"|"enc"|"en"|"e")?  {
                 if( HB_COMP_PARAM->wSeqCounter == 0 )
                   hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_ENDIF, NULL, NULL );
                 pComp->pLex->lasttok = yytext;
                 return END;
               }
%{
/* ************************************************************************ */
%}

"endif"|"endi" { /* ENDIF can be used in one context only */
                 if( HB_COMP_PARAM->wIfCounter == 0 )
                   hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_ENDIF, NULL, NULL );
                 pComp->pLex->lasttok = yytext;
                 return ENDIF;
               }
"endc"("ase"|"as"|"a")? { /* ENDCASE can be used in one context only */
                 if( HB_COMP_PARAM->wCaseCounter == 0 )
                   hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_ENDCASE, NULL, NULL );
                 pComp->pLex->lasttok = yytext;
                 return ENDCASE;
               }
"enddo"|"endd" { /* ENDDO can be used in one context only */
                 if( HB_COMP_PARAM->wWhileCounter == 0 )
                   hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_ENDDO, NULL, NULL );
                 pComp->pLex->lasttok = yytext;
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
                 hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_ENDIF, NULL, NULL );
               }
               pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "END", HB_IDENT_STATIC );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<END_>("->"|"++"|"--") { /* operators */
               BEGIN 0;
               if( hb_comp_iState == LOOKUP )
               { /* Clipper does not like end-> & end++ at the begining of line */
                 hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_ENDIF, NULL, NULL );
               }
               pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "END", HB_IDENT_STATIC );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<END_>[\+\-\:\=\|\$\%\*\,\/\[\]\)\}\^] { /* there is an operator after "end" */
               BEGIN 0;
               pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "END", HB_IDENT_STATIC );
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
                 pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "END", HB_IDENT_STATIC );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
             }
%{
/* ************************************************************************ */
%}

"exit"         {
                   hb_comp_iState =IDENTIFIER;
                   pComp->pLex->lasttok = yytext;
                   return EXIT;
               }
%{
/* ************************************************************************ */
%}
"exte"|"exter"|"extern"|"externa"|"external" {
                   pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( yytext ), HB_IDENT_COPY );
                   hb_comp_iState =IDENTIFIER;
                   return EXTERN;
               }
%{
/* ************************************************************************ */
%}
"_fie"|"_fiel"|"_field" { BEGIN FIELD_;
                   pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( yytext ), HB_IDENT_COPY );
                 }

"fiel"|"field"   { BEGIN FIELD_;
                   pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( yytext ), HB_IDENT_COPY );
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
<FOR_>{Separator}+"each" { BEGIN FOREACH_;}
<FOR_>{Separator}+[&_a-zA-Z] { /* an identifier or a macro after the FOR */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 {
                   hb_comp_iState =FOR;
                   pComp->pLex->lasttok = yytext;
                   pComp->pLex->lasttok = yytext;
                   return FOR;
                 }
                 else
                 { /* for example: DO for WITH variable */
                   pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "FOR", HB_IDENT_STATIC );
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
                   pComp->pLex->lasttok = yytext;
                   return FOR;
               }
               else
               {
                   pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "FOR", HB_IDENT_STATIC );
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
               }
             }
<FOR_>.|\n { /* there is no identifier after "FOR" */
               BEGIN 0;
               pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "FOR", HB_IDENT_STATIC );
               unput( yytext[ yyleng-1 ] );
               hb_comp_iState =IDENTIFIER;
               return IDENTIFIER;
             }
<FOREACH_>{Separator}*[\:\=] { /* FOR each:=  or FOR each= */
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               unput( 'h' ); unput( 'c' ); unput( 'a' ); unput( 'e' );
               hb_comp_iState = FOR;
               pComp->pLex->lasttok = yytext;
               return FOR;
             }
<FOREACH_>. {
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               hb_comp_iState = FOREACH;
               pComp->pLex->lasttok = yytext;
               return FOREACH;
            }
%{
/* ************************************************************************ */
%}
"func"|"funct"|"functi"|"functio"|"function" { BEGIN FUNCTION_; }
<FUNCTION_>{Separator}+[_a-zA-Z] {
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               hb_comp_iState=FUNCTION;
               pComp->pLex->lasttok = yytext;
               return FUNCTION;
             }
<FUNCTION_>.|\n { /* Clipper needs FUNCTION in one context only */
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, ((yytext[ yyleng-1 ]=='\n')?"FUNCTION":yytext), NULL );
             }

%{
/* ************************************************************************ */
%}

"hb_inline" {

      HB_SYMBOL_UNUSED( pComp );

      /* NOTE: hb_compiLineINLINE is being RESET in ppcomp.c - hb_pp_Internal() */

      if( ! HB_COMP_ISSUPPORTED( HB_COMPFLAG_HB_INLINE ) )
      {
         pComp->pLex->lasttok = yylval_ptr->string = "HB_INLINE";
         hb_comp_iState = IDENTIFIER;
         return IDENTIFIER;
      }

      if( hb_comp_iLineINLINE )
      {
         hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_TOOMANY_INLINE, "on the same line", NULL );
	}
	else
	{
         #define INLINE_NORMAL 0
         #define INLINE_SINGLE_QUOT 1
         #define INLINE_DOUBLE_QUOT 2
         #define INLINE_COMMENT 3

         char sBuffer[ YY_BUF_SIZE ], *pBuffer, sInlineSym[] = "HB_INLINE_0", cMode = INLINE_NORMAL;
         int iSize, iBraces = 0;
         PINLINE pInline;

         hb_comp_iLineINLINE = hb_pp_line( HB_COMP_PARAM->pLex->pPP ) + 1;
         hb_pp_StreamBlock = HB_PP_STREAM_DUMP_C;

         sInlineSym[10] = hb_comp_cInlineID++;

         switch( sInlineSym[10] )
         {
            case '9' + 1 :
               sInlineSym[10] = 'A';
               break;

            case 'Z' + 1 :
               hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_TOOMANY_INLINE, NULL, NULL );
               break;
         }

         pInline = hb_compInlineAdd( pComp, hb_compIdentifierNew( HB_COMP_PARAM, sInlineSym, HB_IDENT_COPY ), hb_pp_line( pComp->pLex->pPP ) + 1 );

      DigestInline:

         YY_INPUT( (char*) sBuffer, iSize, YY_BUF_SIZE );
         if( iSize == 0 )
         {
            hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_INVALID_INLINE, HB_COMP_PARAM->functions.pLast->szName, NULL );
            hb_pp_StreamBlock = 0;
            return '\n';
         }
         pBuffer = (char*) sBuffer;

         while( *pBuffer )
         {
            switch( cMode )
            {
               case INLINE_NORMAL :
                  if( *pBuffer == '{' )
                  {
                     iBraces++;
                  }
                  else if( *pBuffer == '}' && iBraces > 1 )
                  {
                     iBraces--;
                  }
                  else if( *pBuffer == '}' )
                  {
                     hb_pp_StreamBlock = 0;
                     break;
                  }
                  else if( *pBuffer == '\'' )
                  {
                     cMode = INLINE_SINGLE_QUOT;
                  }
                  else if( *pBuffer == '"' )
                  {
                     cMode = INLINE_DOUBLE_QUOT;
                  }
                  else if( *pBuffer == '/' && *(pBuffer+1) == '/' )
                  {
                     goto SaveInline;
                  }
                  else if( *pBuffer == '/' && *(pBuffer+1) == '*' )
                  {
                     pBuffer++;
                     cMode = INLINE_COMMENT;
                  }
                  break;

               case INLINE_SINGLE_QUOT :
                  if( *pBuffer == '\\' )
                  {
                     pBuffer++;
                  }
                  else if( *pBuffer == '\'' )
                  {
                     cMode = INLINE_NORMAL;
                  }
                  break;

               case INLINE_DOUBLE_QUOT :
                  if( *pBuffer == '\\' )
                  {
                     pBuffer++;
                  }
                  else if( *pBuffer == '"' )
                  {
                     cMode = INLINE_NORMAL;
                  }
                  break;

               case INLINE_COMMENT :
                  if( *pBuffer == '*' && *(pBuffer+1) == '/' )
                  {
                     pBuffer++;
                     cMode = INLINE_NORMAL;
                  }
                  break;
            }

            pBuffer++;
         }

      SaveInline:

         iSize = strlen( (char*) sBuffer );
         if( pInline->pCode == NULL )
         {
            pInline->pCode = (BYTE *) hb_strdup( sBuffer );
         }
         else
         {
            pInline->pCode = (BYTE *) hb_xrealloc( pInline->pCode, pInline->lPCodeSize + iSize + 1 );
            memcpy( (char *) (pInline->pCode + pInline->lPCodeSize), sBuffer, iSize + 1 );
         }
         pInline->lPCodeSize += iSize;

         if( hb_pp_StreamBlock )
         {
            goto DigestInline;
         }
         else
         {
            if( hb_comp_iLanguage != HB_LANG_C )
            {
               hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_REQUIRES_C, NULL, NULL );
               hb_xfree( ( void * ) pInline->pCode );
               hb_xfree( ( void * ) pInline->szFileName );
               hb_xfree( ( void * ) pInline );  /* NOTE: szName will be released by hb_compSymbolKill() */
            }

            hb_comp_iLinePRG = hb_pp_line( HB_COMP_PARAM->pLex->pPP );
            pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, sInlineSym, HB_IDENT_COPY );
            return IDENTIFIER;
         }
      }
   }
%{
/* ************************************************************************ */
%}
"iif"          {
                 if( hb_comp_iState == FUNCTION || hb_comp_iState == PROCEDURE )
                   hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, "IIF", NULL );
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
               pComp->pLex->lasttok = yytext;
               return IIF;
             }
<IIF_>[^\(] {
               BEGIN 0;
               hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, ((yytext[ yyleng-1 ]=='\n')?"IIF":yytext), NULL );
             }
%{
/* ************************************************************************ */
%}
"if"           {
                 if( hb_comp_iState == FUNCTION || hb_comp_iState == PROCEDURE )
                   hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, "IF", NULL );
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
               pComp->pLex->lasttok = yytext;
               return hb_comp_iState;
             }
<IF_>[\)\]\/\^\*\%\=\$\@] { BEGIN 0;
               hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX2, yytext, "IF" );
             }
<IF_>"->" { BEGIN 0;
               hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX2, yytext, "IF" );
             }
<IF_>\n { BEGIN 0;
               hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, "IF", NULL );
             }
<IF_>("++"|"--")/[\n] { BEGIN 0;
               hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX2, yytext, "IF" );
             }
<IF_>. { BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               hb_comp_iState =IF;
               pComp->pLex->lasttok = yytext;
               return IF;
             }
%{
/* ************************************************************************ */
%}
"in"          { hb_comp_iState =IDENTIFIER; pComp->pLex->lasttok = yytext;
                return IN; }
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
                   pComp->pLex->lasttok = yytext;
                   return INIT;
                 }
                 else
                 { /* there is another item in line already */
                   pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "INIT", HB_IDENT_STATIC );
                   hb_comp_iState =IDENTIFIER;
                   return IDENTIFIER;
                 }
               }
<INIT_>.|\n { /* any character (not identifier) after INIT */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "INIT", HB_IDENT_STATIC );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
%{
/* ************************************************************************ */
%}
"#"{Separator}*"line"  { pComp->pLex->lasttok = yytext; return LINE; }
%{
/* ************************************************************************ */
%}
"loca"|"local"  {
                  pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( yytext ), HB_IDENT_COPY );
                  hb_comp_iState =IDENTIFIER;
                  return LOCAL;
                }
%{
/* ************************************************************************ */
%}
"loop"         { hb_comp_iState =IDENTIFIER; pComp->pLex->lasttok = yytext;
                 return LOOP; }
%{
/* ************************************************************************ */
%}
"memv"|"memva"|"memvar" {
                  pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( yytext ), HB_IDENT_COPY );
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
                 if( HB_COMP_PARAM->wForCounter == 0 )
                   hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_NEXTFOR, NULL, NULL );
                 hb_comp_iState =NEXT;
                 pComp->pLex->lasttok = yytext;
                 return NEXT;
               }
               else
               { /* there is another item in line already */
                 pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "NEXT", HB_IDENT_STATIC );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
             }
<NEXT_>[\[\(] { /* array, function call */
               BEGIN 0;
               if( hb_comp_iState == LOOKUP )
               { /* Clipper does not like NEXT[] & NEXT() at the begining of line */
                 hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_NEXTFOR, NULL, NULL );
               }
               pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "NEXT", HB_IDENT_STATIC );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<NEXT_>("->"|"++"|"--") { /* operators */
               BEGIN 0;
               if( hb_comp_iState == LOOKUP )
               { /* Clipper does not like next-> & next++ at the begining of line */
                  hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_NEXTFOR, NULL, NULL );
               }
               pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "NEXT", HB_IDENT_STATIC );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<NEXT_>[^_a-zA-Z] { /* there is no identifier after "next" */
               BEGIN 0;
               pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "NEXT", HB_IDENT_STATIC );
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<NEXT_>. { /* an identifier follows NEXT statement */
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( hb_comp_iState == LOOKUP )
               {
                 if( HB_COMP_PARAM->wForCounter == 0 )
                   hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_NEXTFOR, NULL, NULL );
                 hb_comp_iState =NEXT;
                 pComp->pLex->lasttok = yytext;
                 return NEXT;
               }
               else
               {
                 pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "NEXT", HB_IDENT_STATIC );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
             }
%{
/* ************************************************************************ */
%}
"nil"        { hb_comp_iState =LITERAL; pComp->pLex->lasttok = yytext;
               return NIL; }
%{
/* ************************************************************************ */
%}
"othe"|"other"|"otherw"|"otherwi"|"otherwis"|"otherwise"    {
                  pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( yytext ), HB_IDENT_COPY );
                  BEGIN OTHERWISE_;
               }
<OTHERWISE_>{Separator}*   ;
<OTHERWISE_>[\n\;]     { /* end of line */
                 BEGIN 0;
                 unput( yytext[ yyleng-1 ] );
                 if( hb_comp_iState == LOOKUP )
                 { /* it is the first item in the line */
                    if( HB_COMP_PARAM->wCaseCounter == 0 && HB_COMP_PARAM->wSwitchCounter == 0 )
                       hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_CASE, NULL, NULL );
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
                  pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( yytext ), HB_IDENT_COPY );
                  hb_comp_iState =IDENTIFIER;
                  return PARAMETERS;
                }
%{
/* ************************************************************************ */
%}
"priv"("ate"|"at"|"a")? {
                  pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( yytext ), HB_IDENT_COPY );
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
               pComp->pLex->lasttok = yytext;
               return PROCEDURE;
             }
<PROCEDURE_>.|\n   { /* Clipper needs PROCEDURE in one context only */
               BEGIN 0;
               hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_SYNTAX, ((yytext[ yyleng-1 ]=='\n')?"PROCEDURE":yytext), NULL );
             }
%{
/* ************************************************************************ */
%}
"publ"("ic"|"i")? {
                  pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( yytext ), HB_IDENT_COPY );
                  hb_comp_iState =IDENTIFIER;
                  return PUBLIC;
               }
%{
/* ************************************************************************ */
%}
"qself"{SpaceTab}*[(]{SpaceTab}*[)] { pComp->pLex->lasttok = yytext; return SELF; }
%{
/* ************************************************************************ */
%}
"reco"|"recov"|"recove"|"recover"   {
                  pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( yytext ), HB_IDENT_COPY );
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
                  pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( yytext ), HB_IDENT_COPY );
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
                  pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( yytext ), HB_IDENT_COPY );
                  hb_comp_iState = IDENTIFIER;
                  return STATIC;
               }
%{
/* ************************************************************************ */
%}
"step"         { hb_comp_iState = IDENTIFIER; return STEP; }
%{
/* ************************************************************************ */
%}
"switch"         BEGIN SWITCH_;
<SWITCH_>{Separator}*    ;
<SWITCH_>[\:\=\|\$\%\*\,\/\]\)\}\^] { /* there is an operator after "switch" */
               BEGIN 0;
               pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "SWITCH", HB_IDENT_STATIC );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<SWITCH_>("+="|"-="|"->") { /* operators */
               BEGIN 0;
               pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "SWITCH", HB_IDENT_STATIC );
               hb_comp_iState =IDENTIFIER;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               return IDENTIFIER;
             }
<SWITCH_>("::") { /* send operators */
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               unput( yytext[ yyleng-2 ] );
               hb_comp_iState =DOSWITCH;
               pComp->pLex->lasttok = yytext;
               return DOSWITCH;
             }
<SWITCH_>(\n|.) { /* not operator */
               BEGIN 0;
               unput( yytext[ yyleng-1 ] );
               if( hb_comp_iState == LOOKUP )
               { /* it is first item in the line */
                 hb_comp_iState =DOSWITCH;
                 pComp->pLex->lasttok = yytext;
                 return DOSWITCH;
               }
               else
               { /* there is another item in line already */
                 pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "SWITCH", HB_IDENT_STATIC );
                 hb_comp_iState =IDENTIFIER;
                 return IDENTIFIER;
               }
             }
%{
/* ************************************************************************ */
%}
"to"         { hb_comp_iState = IDENTIFIER; pComp->pLex->lasttok = yytext;
               return TO; }
%{
/* ************************************************************************ */
%}
"while"|"whil"  { BEGIN WHILE_;
                  /* store it for later use */
                  pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, yytext, HB_IDENT_COPY );
                }
<WHILE_>{Separator}*        ;
<WHILE_>\n      { /* end of line */
                BEGIN 0;
                unput( '\n' );
                if( hb_comp_iState == DO )
                { /* we have DO while */
                  return DOIDENT;
                }
					 else
					 {
                	pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "WHILE", HB_IDENT_STATIC );
                	return IDENTIFIER;
					 }
              }
<WHILE_>[\:\=\|\$\%\*\,\/\]\)\}\^] { /* there is an operator after "while" */
               BEGIN 0;
               pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "WHILE", HB_IDENT_STATIC );
               unput( yytext[ yyleng-1 ] );
               return IDENTIFIER;
             }
<WHILE_>("+="|"-="|"->") { /* operators */
               BEGIN 0;
               pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "WHILE", HB_IDENT_STATIC );
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
                  pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "WHILE", HB_IDENT_STATIC );
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
                pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "WITH", HB_IDENT_STATIC );
                return IDENTIFIER;
              }
<WITH_>"with"       {
                BEGIN 0;
                yyless( yyleng-4 );
                if( hb_comp_iState == DO )
                { /* DO with WITH */
                  hb_comp_iState =IDENTIFIER;
                  pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "WITH", HB_IDENT_STATIC );
                  return IDENTIFIER;
                }
                else
                { /* DO WITH with <arg> */
                  hb_comp_iState =WITH;
                  pComp->pLex->lasttok = yytext;
                  return WITH;
                }
              }
<WITH_>"object"   {
               BEGIN 0;
               if( hb_comp_iState == LOOKUP )
               {
                  hb_comp_iState = WITHOBJECT;
                  pComp->pLex->lasttok = yytext;
                  return WITHOBJECT;
               }
               else
               {
                  yyless( yyleng-6 );
                  hb_comp_iState =WITH;
                  pComp->pLex->lasttok = yytext;
                  return WITH;
               }

               }
<WITH_>[\)] { /* ( with ) or with() */
                BEGIN 0;
                unput( yytext[ yyleng-1 ] );
                hb_comp_iState =IDENTIFIER;
                pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "WITH", HB_IDENT_STATIC );
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
                  pComp->pLex->lasttok = yytext;
                  return WITH;
                }
                else
                {
                  pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, "WITH", HB_IDENT_STATIC );
                  hb_comp_iState =IDENTIFIER;
                  return IDENTIFIER;
                }
              }

%{
/* ************************************************************************ */
%}
"\{"{Separator}*"\|".*/\n	{	/* codeblock scanning */
		/*
		  We have to know if the codeblock contains the macro
		expression &var or &(var)
		In case of &var we have to store in the pcode the source code
		of the codeblock - the string with the whole codeblock
		have to be saved. At runtime, this string is macro
		expanded and the result string is next macro compiled to
		create a codeblock - this is called 'the early macro evaluation'.
		  In contrast, the late macro evaluation &(var) creates
		the codeblock at compile time - the macro is expanded
		during the codeblock evaluation.
		 The following code:
		 //early evaluation
		 PRIVATE a
		 FOR i:=1 TO 3
		   a:=STR(i)
		   cb[i] := {|| &a} //the same as cb[i] := &( '{||'+a+'}' )
		 NEXT
		 ....
		 FOR i:=1 TO 3
		   ? EVAL( cb[i] ) //prints '1','2','3'
		 NEXT
		 //late evaluation
		 FOR i:=1 TO 3
		   a:=STR(i)
		   cb[i] := {|| &(a)}
		 NEXT
		 ....
		 FOR i:=1 TO 3
		   ? EVAL( cb[i] ) //prints '3','3','3'
		 NEXT
		*/
		char *cText;
		int iLen;
		int iPos=0;
		int iCode=1;
		char cMark='\0';

		yylval_ptr->asCodeblock.flags = 0;
		cText = yytext+1;
		iLen = 1;

		while( *cText || iLen < yyleng )
		{
		    iLen++;
		    if( *cText == '\'' && cMark == '\0' )
			    	cMark = '\'';
		    else if( *cText == '"' && cMark == '\0' )
			    	cMark = '"';
		    else if( *cText == '[' && cMark == '\0' )
			    	cMark = ']';
			 else if( *cText == cMark )
			 	cMark = '\0';
		    else if( *cText == '|' && iPos == 0 )
				iPos = iLen;
		    else if( *cText == '&' && cMark == '\0' )
		    {
				yylval_ptr->asCodeblock.falgs |= HB_BLOCK_MACRO;
				++cText;
				while( *cText == ' ' || *cText == '\t' )
			    	++cText;
				if( *cText == '(' )
			    	yylval_ptr->asCodeblock.flags |= HB_BLOCK_LATEEVAL;
				--cText;
		    }
		    else if( *cText == '{' && cMark == '\0' )
				iCode++;
		    else if( *cText == '}' && cMark == '\0' )
		    {
				iCode--;
				if( iCode == 0 )
			    	break;
		    }
		    cText++;
		}
		cMark = yytext[ iLen ];
		yylval_ptr->asCodeblock.string = (char *)hb_xgrab( iLen+1 );
		memcpy( (void *)yylval_ptr->asCodeblock.string, (void *)yytext, iLen );
		yylval_ptr->asCodeblock.string[iLen] = '\0';
		yylval_ptr->asCodeblock.length = iLen;
		yyless( iPos ); /* restart scanning after '{|' */
		pComp->pLex->lasttok = yytext;
            return CBSTART;
		}

%{
/* ************************************************************************ */
%}
"as"{Separator}+("arra"|"array")                                           { pComp->pLex->lasttok = yytext; return AS_ARRAY; }
"as"{Separator}+("code"|"codeb"|"codebl"|"codeblo"|"codebloc"|"codeblock") { pComp->pLex->lasttok = yytext; return AS_BLOCK; }
"as"{Separator}+("stri"|"strin"|"string")                                  { pComp->pLex->lasttok = yytext; return AS_CHARACTER; }
"as"{Separator}+("char"|"chara"|"charac"|"charact"|"characte"|"character") { pComp->pLex->lasttok = yytext; return AS_CHARACTER; }
"as"{Separator}+("clas"|"class")                                           { pComp->pLex->lasttok = yytext; return AS_CLASS; }
"as"{Separator}+"date"                                                     { pComp->pLex->lasttok = yytext; return AS_DATE; }
"as"{Separator}+("logi"|"logic"|"logica"|"logical")                        { pComp->pLex->lasttok = yytext; return AS_LOGICAL; }
"as"{Separator}+("nume"|"numer"|"numeri"|"numeric")                        { pComp->pLex->lasttok = yytext; return AS_NUMERIC; }
"as"{Separator}+("obje"|"objec"|"object")                                  { pComp->pLex->lasttok = yytext; return AS_OBJECT; }
"as"{Separator}+("usua"|"usual")                                           { pComp->pLex->lasttok = yytext; return AS_VARIANT; }
"as"{Separator}+("anyt"|"anytyp"|"anytype")                                { pComp->pLex->lasttok = yytext; return AS_VARIANT; }

"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+("usua"|"usual")                                           { pComp->pLex->lasttok = yytext; return AS_ARRAY; }
"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+("anyt"|"anytyp"|"anytype")                                { pComp->pLex->lasttok = yytext; return AS_ARRAY; }
"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+("arra"|"array")                                           { pComp->pLex->lasttok = yytext; return AS_ARRAY_ARRAY; }
"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+("code"|"codeb"|"codebl"|"codeblo"|"codebloc"|"codeblock") { pComp->pLex->lasttok = yytext; return AS_BLOCK_ARRAY; }
"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+("stri"|"strin"|"string")                                  { pComp->pLex->lasttok = yytext; return AS_CHARACTER_ARRAY; }
"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+("char"|"chara"|"charac"|"charact"|"characte"|"character") { pComp->pLex->lasttok = yytext; return AS_CHARACTER_ARRAY; }
"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+("clas"|"class")                                           { pComp->pLex->lasttok = yytext; return AS_CLASS_ARRAY; }
"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+"date"                                                     { pComp->pLex->lasttok = yytext; return AS_DATE_ARRAY; }
"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+("logi"|"logic"|"logica"|"logical")                        { pComp->pLex->lasttok = yytext; return AS_LOGICAL_ARRAY; }
"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+("nume"|"numer"|"numeri"|"numeric")                        { pComp->pLex->lasttok = yytext; return AS_NUMERIC_ARRAY; }
"as"{Separator}+("arra"|"array"){Separator}+"of"{Separator}+("obje"|"objec"|"object")                                  { pComp->pLex->lasttok = yytext; return AS_OBJECT_ARRAY; }

"_hb_class"             { YY_OPERATOR_RET( DECLARE_CLASS ); }
"_hb_member"            { YY_OPERATOR_RET( DECLARE_MEMBER ); }

%{
/* ************************************************************************ */
%}
"#"            YY_OPERATOR_RET( NE1 );
"="            YY_OPERATOR_RET( yytext[ 0 ] );
"+"            YY_OPERATOR_RET( yytext[ 0 ] );
"-"            YY_OPERATOR_RET( yytext[ 0 ] );
"*"            YY_OPERATOR_RET( yytext[ 0 ] );
[\/]           YY_OPERATOR_RET( yytext[ 0 ] );
"%"            YY_OPERATOR_RET( yytext[ 0 ] );
"$"            YY_OPERATOR_RET( yytext[ 0 ] );
"<>"|"!="      YY_OPERATOR_RET( NE2 );
":="           YY_OPERATOR_RET( INASSIGN );
"=="           YY_OPERATOR_RET( EQ );
"++"           YY_OPERATOR_RET( INC );
"--"           YY_OPERATOR_RET( DEC );
"->"           YY_OPERATOR_RET( ALIASOP );
"<="           YY_OPERATOR_RET( LE );
">="           YY_OPERATOR_RET( GE );
"+="           YY_OPERATOR_RET( PLUSEQ );
"-="           YY_OPERATOR_RET( MINUSEQ );
"*="           YY_OPERATOR_RET( MULTEQ );
"/="           YY_OPERATOR_RET( DIVEQ );
"^="           YY_OPERATOR_RET( EXPEQ );
"%="           YY_OPERATOR_RET( MODEQ );
"**"|"^"       YY_OPERATOR_RET( POWER );
".and."        YY_OPERATOR_RET( AND );
".or."         YY_OPERATOR_RET( OR );
"!"|".not."    YY_OPERATOR_RET( NOT );
"::"           unput( ':' ); unput( 'f' ); unput( 'l' ); unput( 'e' ); unput( 'S' );
"..."          YY_OPERATOR_RET( EPSILON );
[,\|\#\&\.\:\<\>\@] YY_OPERATOR_RET( yytext[ 0 ] );
[\{]           YY_TOKEN_RET( LARRAY, yytext[ 0 ] );
[\}]           YY_TOKEN_RET( RARRAY, yytext[ 0 ] );
[\]]           YY_TOKEN_RET( RINDEX, yytext[ 0 ] );
[\(]           YY_TOKEN_RET( LSEPARATOR, yytext[ 0 ] );
[\)]           YY_TOKEN_RET( RSEPARATOR, yytext[ 0 ] );

[\x00-\x1F]     pComp->pLex->lasttok = yytext; return yytext[ 0 ]; /* see below */
[\~\`\?\_\\]    pComp->pLex->lasttok = yytext; return yytext[ 0 ]; /* see below */
[\x7F-\xFF]     {
            /* This have to be the last rule - any nonstandard and not handled
                 * characters should go to grammar analyser instead of printing it
                 * on stdout.
                 */
            pComp->pLex->lasttok = yytext;
            return yytext[ 0 ];
            }

%{
/* ************************************************************************ */
%}

{InvalidNumber} BEGIN INVALIDNUM_; pComp->pLex->lasttok = yylval_ptr->string = hb_strdup( yytext );
<INVALIDNUM_>("."|{Separator}+) {
                   BEGIN 0;
                   hb_xfree( yylval_ptr->string );
                   pComp->pLex->lasttok = NULL;
                   hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_NUMERIC_FORMAT, NULL, NULL );
                 }
<INVALIDNUM_>.   {
                    int iRet;
                    char *pTmp = yylval_ptr->string;
                    BEGIN 0;
                    unput( yytext[ yyleng-1 ] );
                    unput( '.' );
                    yylval_ptr->string[ strlen(yylval_ptr->string) - 1 ] = '\0';
                    pComp->pLex->lasttok = yylval_ptr->string;
                    iRet = yy_ConvertNumber( yylval_ptr, yylval_ptr->string );
                    hb_xfree( pTmp );
                    return iRet;
                 }


{Number}       { pComp->pLex->lasttok = yytext;
                 return yy_ConvertNumber( yylval_ptr, yytext ); }

{TrueValue}   { YY_TOKEN_RET( RSEPARATOR, TRUEVALUE ); }
{FalseValue}  { YY_TOKEN_RET( RSEPARATOR, FALSEVALUE ); }

{Date}        { pComp->pLex->lasttok = yytext;
                return yy_ConvertDate( yylval_ptr, yytext ); }

{MacroVar}     {
                  if( yytext[ yyleng-1 ] == '.' )
                     yytext[ yyleng-1 ] = '\0';
                  pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( yytext+1 ), HB_IDENT_COPY );
                  hb_comp_iState = MACROVAR;
                  return MACROVAR;
               }

{MacroEnd}     {
                  pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( yytext ), HB_IDENT_COPY );
                  hb_comp_iState = MACROTEXT;
                  return MACROTEXT;
               }

{MacroId}      {
                  pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( yytext ), HB_IDENT_COPY );
                  hb_comp_iState = MACROTEXT;
                  return MACROTEXT;
               }

{MacroTxt}     {
                  pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( yytext ), HB_IDENT_COPY );
                  hb_comp_iState = MACROTEXT;
                  return MACROTEXT;
               }


{Identifier}    {
                   if( strlen( yytext ) > HB_SYMBOL_NAME_LEN )
                   {
                      yytext[ HB_SYMBOL_NAME_LEN ] = '\0';
                      yyleng = HB_SYMBOL_NAME_LEN;
                   }
                   pComp->pLex->lasttok = yylval_ptr->string = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( yytext ), HB_IDENT_COPY );
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
   /* FLEX is not MT safe :-( */
   HB_COMP_DECL = s_pComp;
   char * szLine;
   ULONG ulLen = 0;

   szLine = hb_pp_nextLine( HB_COMP_PARAM->pLex->pPP, &ulLen );

   if( ulLen >= ( ULONG ) iBufferSize )
      hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_BUFFER_OVERFLOW, NULL, NULL );
   else if( szLine )
      memcpy( buffer, szLine, ulLen + 1 );
   else
      * buffer = '\0';

   return ulLen;
}

static int yy_ConvertNumber( YYSTYPE *yylval_ptr, char * szBuffer )
{
   HB_LONG lNumber;
   double dNumber;
   int iDec, iWidth;

   if( hb_compStrToNum( szBuffer, strlen( szBuffer ), &lNumber, &dNumber, &iDec, &iWidth ) )
   {
      yylval_ptr->valDouble.dNumber = dNumber;
      yylval_ptr->valDouble.bDec = iDec;
      yylval_ptr->valDouble.bWidth = iWidth;
      yylval_ptr->valDouble.szValue = szBuffer;
      return NUM_DOUBLE;
   }
   else
   {
      yylval_ptr->valLong.lNumber = lNumber;
      yylval_ptr->valLong.szValue = szBuffer;
      return NUM_LONG;
   }
}

static int yy_ConvertDate( YYSTYPE *yylval_ptr, char * szBuffer )
{
   int year, month, day;

   hb_dateStrGet( szBuffer+2, &year, &month, &day );
   yylval_ptr->valLong.lNumber = hb_dateEncode( year, month, day );
   yylval_ptr->valLong.szValue = szBuffer;

   return NUM_DATE;
}

void hb_compParserStop( HB_COMP_PTR pComp )
{
   HB_SYMBOL_UNUSED( pComp );

   yy_delete_buffer( yy_current_buffer );
}
