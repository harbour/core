%option noyywrap
%option never-interactive
%{
/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Macro Compiler LEX rules
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* Compile using: flex -i -8 -omacrol.c -Phb_comp -C macro.l

NOTE: -C  controls the speed/size ratio of generated scanner
-Cf   = fastest/biggest
-CF
-C    = in between
-Cm
-Ce
-Cem  = slowest/smallest
*/

#define HB_MACRO_SUPPORT

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "hbmacro.h"
#include "hbcomp.h"
#include "hbdate.h"

#include "macroy.h"
#include "hbsetup.h"    /* main configuration file */
#include "hberrors.h"
#include "hbdefs.h"

/* NOTE: these symbols are used internally in bison.simple
 */
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
#define YY_DECL int yylex( YYSTYPE *yylval_ptr, HB_MACRO_PTR pMacro )
#define YYLEX_PARAM pMacro

/* code that fills input buffer
 */
#define YY_INPUT( buf, result, max_size ) result = 0;


#define LOOKUP          0       /* scan from the begining of line */
#define SEPARATOR       -1
#define HB_FLEX_STATE   ( ( PHB_MACRO_FLEX ) pMacro->pLex )->FlexState
typedef struct
{
   ULONG  pos;              /* current position inside of compiled string */
   int    FlexState;        /* internal flex state during parsing */
   YY_BUFFER_STATE pBuffer; /* FLEX buffer */
}
HB_MACRO_FLEX, * PHB_MACRO_FLEX;

%}

%{
#ifdef __WATCOMC__
/* disable warnings for unreachable code */
#pragma warning 13 9
#endif
%}

SpaceTab      [ \t]+
Number        ([0-9]+)|([0-9]*\.[0-9]+)|(0x[0-9A-Fa-f]+)
InvalidNumber [0-9]+\.
Identifier    (([a-zA-Z])|([_a-zA-Z][_a-zA-Z0-9]+))
Date          0d[0-9]{8}

MacroVar      \&{Identifier}[\.]?
MacroEnd      \&{Identifier}\.({Identifier})|([0-9]+)
MacroId       ({Identifier}\&(({Identifier}[\.]?)|({Identifier}\.({Identifier})|([0-9]+))))
MacroTxt      ({MacroVar}|{MacroEnd}|{MacroId})+

%x            STRING1 STRING2 STRING3

%%


"&"("'"|\"|\[) { hb_macroError( EG_SYNTAX, YYLEX_PARAM ); }

'              BEGIN STRING1;
\"             BEGIN STRING2;

\[             {
                   if( HB_FLEX_STATE == SEPARATOR )
                      BEGIN STRING3;
                   else
                      return '[';
               }


<STRING1>[^'^\n]*   { hb_macroError( EG_SYNTAX, YYLEX_PARAM ); BEGIN 0; }
<STRING2>[^\"^\n]*  { hb_macroError( EG_SYNTAX, YYLEX_PARAM ); BEGIN 0; }
<STRING3>[^\]]*\n   { hb_macroError( EG_SYNTAX, YYLEX_PARAM ); BEGIN 0; }

<STRING1>[^']*'   { BEGIN 0;
    HB_FLEX_STATE = LOOKUP;
    yyleng--;
    yytext[yyleng] = 0;
    yylval_ptr->string = hb_strdup( yytext );
    return LITERAL;
}

<STRING2>[^\"]*\" { BEGIN 0;
    HB_FLEX_STATE = LOOKUP;
    yyleng--;
    yytext[yyleng] = 0;
    yylval_ptr->string = hb_strdup( yytext );
    return LITERAL;
}

<STRING3>[^\]]*\] { BEGIN 0;
    HB_FLEX_STATE = LOOKUP;
    yyleng--;
    yytext[yyleng] = 0;
    yylval_ptr->string = hb_strdup( yytext );
    return LITERAL;
}

{SpaceTab}     ;

\n           { HB_FLEX_STATE = LOOKUP; return '\n'; }

%{
/* ************************************************************************ */
%}

"_fie"|"_fiel"|"_field" {
    HB_FLEX_STATE = LOOKUP;
    return FIELD;
}

"fiel"|"field"   {
    HB_FLEX_STATE = LOOKUP;
    return FIELD;
}

"iif"        {
    HB_FLEX_STATE = LOOKUP;
    return IIF;
}

"if"         {
    HB_FLEX_STATE = LOOKUP;
    return IIF;
}

"nil"        { HB_FLEX_STATE = LOOKUP; return NIL; }

"qself"{SpaceTab}*\({SpaceTab}*\)    { HB_FLEX_STATE = LOOKUP; return SELF; }

%{
/* ************************************************************************ */
%}

"#"            { HB_FLEX_STATE = SEPARATOR; return NE1; }
"<>"|"!="      { HB_FLEX_STATE = SEPARATOR; return NE2; }
":="           { HB_FLEX_STATE = SEPARATOR; return INASSIGN; }
"=="           { HB_FLEX_STATE = SEPARATOR; return EQ; }
"++"           { HB_FLEX_STATE = SEPARATOR; return INC; }
"--"           { HB_FLEX_STATE = SEPARATOR; return DEC; }
"->"           { HB_FLEX_STATE = SEPARATOR; return ALIASOP; }
"<="           { HB_FLEX_STATE = SEPARATOR; return LE; }
">="           { HB_FLEX_STATE = SEPARATOR; return GE; }
"+="           { HB_FLEX_STATE = SEPARATOR; return PLUSEQ; }
"-="           { HB_FLEX_STATE = SEPARATOR; return MINUSEQ; }
"*="           { HB_FLEX_STATE = SEPARATOR; return MULTEQ; }
"/="           { HB_FLEX_STATE = SEPARATOR; return DIVEQ; }
"^="           { HB_FLEX_STATE = SEPARATOR; return EXPEQ; }
"%="           { HB_FLEX_STATE = SEPARATOR; return MODEQ; }
"**"|"^"       { HB_FLEX_STATE = SEPARATOR; return POWER; }
".and."        { HB_FLEX_STATE = SEPARATOR; return AND; }
".or."         { HB_FLEX_STATE = SEPARATOR; return OR; }
"."[t|y]"."    { HB_FLEX_STATE = SEPARATOR; return TRUEVALUE; }
"."[f|n]"."    { HB_FLEX_STATE = SEPARATOR; return FALSEVALUE; }
"!"|".not."    { HB_FLEX_STATE = SEPARATOR; return NOT; }
"::"           { HB_FLEX_STATE = SEPARATOR; unput( ':' ); unput( 'f' ); unput( 'l' ); unput( 'e' ); unput( 'S' ); }
[\{\(]         { HB_FLEX_STATE = SEPARATOR; return yytext[ 0 ]; }
[\=\+\-\*\/\%\$\,\|\#\&\.\:\<\>\@] { HB_FLEX_STATE = SEPARATOR; return yytext[ 0 ]; }
[\]\}\)]       { HB_FLEX_STATE = LOOKUP; return yytext[ 0 ]; }

[\x00-\x1F]    return yytext[ 0 ]; /* see below */
[\~\`\?\_\\]   return yytext[ 0 ]; /* see below */
[\x7F-\xFF]    {
                  /* This have to be the last rule - any nonstandard and
                   * unhandled characters should go to grammar analyser
                   * instead of printing it on stdout.
                   */
                  return yytext[ 0 ];
               }

%{
/* ************************************************************************ */
%}

{Number}       {
   HB_LONG lNumber;
   double dNumber;
   int iDec, iWidth;

   HB_FLEX_STATE = LOOKUP;

   if( hb_compStrToNum( yytext, strlen( yytext ), &lNumber, &dNumber, &iDec, &iWidth ) )
   {
      yylval_ptr->valDouble.dNumber = dNumber;
      yylval_ptr->valDouble.bDec = iDec;
      yylval_ptr->valDouble.bWidth = iWidth;
      return NUM_DOUBLE;
   }
   else
   {
      yylval_ptr->valLong.lNumber = lNumber;
      yylval_ptr->valLong.bWidth = iWidth;
      return NUM_LONG;
   }
}

{Date}        {
   int year, month, day;
   
   hb_dateStrGet( yytext+2, &year, &month, &day );
   yylval_ptr->valLong.lNumber = hb_dateEncode( year, month, day );
   
   return NUM_DATE;
}

{MacroVar}     {
    HB_TRACE(HB_TR_DEBUG, ("{MacroVar}(%s)", yytext));
    if( yytext[ yyleng-1 ] == '.' )
    yytext[ yyleng-1 ] = '\0';
    yylval_ptr->string = hb_strupr( hb_strdup( yytext+1 ) );
    HB_FLEX_STATE = LOOKUP;
    return MACROVAR;
}

{MacroEnd}     {
    HB_TRACE(HB_TR_DEBUG, ("{MacroEnd}(%s)", yytext));
    yylval_ptr->string = hb_strupr( hb_strdup( yytext ) );
    HB_FLEX_STATE = LOOKUP;
    return MACROTEXT;
}

{MacroId}      {
    HB_TRACE(HB_TR_DEBUG, ("{MacroId}(%s)", yytext));
    yylval_ptr->string = hb_strupr( hb_strdup( yytext ) );
    HB_FLEX_STATE = LOOKUP;
    return MACROTEXT;
}

{MacroTxt}     {
    HB_TRACE(HB_TR_DEBUG, ("{MacroTxt}(%s)", yytext));
    yylval_ptr->string = hb_strupr( hb_strdup( yytext ) );
    HB_FLEX_STATE = LOOKUP;
    return MACROTEXT;
}

{Identifier}    {
    HB_TRACE(HB_TR_DEBUG, ("{Identifier}(%s)", yytext));
    if( ( USHORT ) strlen( yytext ) > YYLEX_PARAM->uiNameLen )
    {
        yytext[ YYLEX_PARAM->uiNameLen ] = '\0';
        yyleng = YYLEX_PARAM->uiNameLen;
    }
    yylval_ptr->string = hb_strupr( hb_strdup( yytext ) );
    HB_FLEX_STATE = LOOKUP;
    return IDENTIFIER;
}

%%

#ifdef __WATCOMC__
/* enable warnings for unreachable code */
#pragma warning 13 1
#endif


BOOL hb_macroLexNew( HB_MACRO_PTR pMacro )
{
   PHB_MACRO_FLEX pLex;
   YY_BUFFER_STATE pBuffer;

   HB_TRACE(HB_TR_DEBUG, ("hb_compFlexNew(%s, %i)", pMacro->string, pMacro->length));

   /* This creates the scanner buffer based on passed string.
    * Unfortunately it creates a copy of this string - the string can be
    * modified during scanning and the string have to end with TWO zero bytes
    * NOTE: It must be used in macro.l because yy_scan_bytes is not
    * visible in macro.y
    */
   pBuffer = yy_scan_bytes( pMacro->string, pMacro->length );
   if( pBuffer )
   {
      pLex = ( PHB_MACRO_FLEX ) hb_xgrab( sizeof( HB_MACRO_FLEX ) );
      pLex->pos = 0;
      pLex->FlexState = LOOKUP;
      pLex->pBuffer = pBuffer;
      pMacro->pLex = ( void * ) pLex;
   }

   return pMacro->pLex != NULL;
}

void hb_macroLexDelete( HB_MACRO_PTR pMacro )
{
   if( pMacro->pLex )
   {
      PHB_MACRO_FLEX pLex = ( PHB_MACRO_FLEX ) pMacro->pLex;
      pMacro->pLex = NULL;
      yy_delete_buffer( pLex->pBuffer );
      hb_xfree( pLex );
   }
}
