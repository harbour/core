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
#undef free
#define free hb_xfree


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

%x            STRING1 STRING2 STRING3

%%


"&"("'"|\"|\[) { hb_macroError( EG_SYNTAX, YYLEX_PARAM ); }

'              BEGIN STRING1;
\"             BEGIN STRING2;

\[             {
                   if( pMacro->FlexState == SEPARATOR )
                      BEGIN STRING3;
                   else
                      return '[';
               }


<STRING1>[^'^\n]*   { hb_macroError( EG_SYNTAX, YYLEX_PARAM ); BEGIN 0; }
<STRING2>[^\"^\n]*  { hb_macroError( EG_SYNTAX, YYLEX_PARAM ); BEGIN 0; }
<STRING3>[^\]]*\n   { hb_macroError( EG_SYNTAX, YYLEX_PARAM ); BEGIN 0; }

<STRING1>[^']*'   { BEGIN 0;
    pMacro->FlexState = LOOKUP;
    yyleng--;
    yytext[yyleng] = 0;
    yylval_ptr->string = hb_strdup( yytext );
    return LITERAL;
}

<STRING2>[^\"]*\" { BEGIN 0;
    pMacro->FlexState = LOOKUP;
    yyleng--;
    yytext[yyleng] = 0;
    yylval_ptr->string = hb_strdup( yytext );
    return LITERAL;
}

<STRING3>[^\]]*\] { BEGIN 0;
    pMacro->FlexState = LOOKUP;
    yyleng--;
    yytext[yyleng] = 0;
    yylval_ptr->string = hb_strdup( yytext );
    return LITERAL;
}

{SpaceTab}     ;

\n           { pMacro->FlexState = LOOKUP; return '\n'; }

%{
/* ************************************************************************ */
%}

"_fie"|"_fiel"|"_field" {
    pMacro->FlexState = LOOKUP;
    return FIELD;
}

"fiel"|"field"   {
    pMacro->FlexState = LOOKUP;
    return FIELD;
}

"iif"        {
    pMacro->FlexState = LOOKUP;
    return IIF;
}

"if"         {
    pMacro->FlexState = LOOKUP;
    return IF;
}

"nil"        { pMacro->FlexState = LOOKUP; return NIL; }

"qself"{SpaceTab}*\({SpaceTab}*\)    { pMacro->FlexState = LOOKUP; return SELF; }

%{
/* ************************************************************************ */
%}

"#"            { pMacro->FlexState = SEPARATOR; return NE1; }
"<>"|"!="      { pMacro->FlexState = SEPARATOR; return NE2; }
":="           { pMacro->FlexState = SEPARATOR; return INASSIGN; }
"=="           { pMacro->FlexState = SEPARATOR; return EQ; }
"++"           { pMacro->FlexState = SEPARATOR; return INC; }
"--"           { pMacro->FlexState = SEPARATOR; return DEC; }
"->"           { pMacro->FlexState = SEPARATOR; return ALIASOP; }
"<="           { pMacro->FlexState = SEPARATOR; return LE; }
">="           { pMacro->FlexState = SEPARATOR; return GE; }
"+="           { pMacro->FlexState = SEPARATOR; return PLUSEQ; }
"-="           { pMacro->FlexState = SEPARATOR; return MINUSEQ; }
"*="           { pMacro->FlexState = SEPARATOR; return MULTEQ; }
"/="           { pMacro->FlexState = SEPARATOR; return DIVEQ; }
"^="           { pMacro->FlexState = SEPARATOR; return EXPEQ; }
"%="           { pMacro->FlexState = SEPARATOR; return MODEQ; }
"**"|"^"       { pMacro->FlexState = SEPARATOR; return POWER; }
".and."        { pMacro->FlexState = SEPARATOR; return AND; }
".or."         { pMacro->FlexState = SEPARATOR; return OR; }
"."[t|y]"."    { pMacro->FlexState = SEPARATOR; return TRUEVALUE; }
"."[f|n]"."    { pMacro->FlexState = SEPARATOR; return FALSEVALUE; }
"!"|".not."    { pMacro->FlexState = SEPARATOR; return NOT; }
"::"           { pMacro->FlexState = SEPARATOR; unput( ':' ); unput( 'f' ); unput( 'l' ); unput( 'e' ); unput( 'S' ); }
[\{\(]         { pMacro->FlexState = SEPARATOR; return yytext[ 0 ]; }
[\=\+\-\*\/\%\$\,\|\#\&\.\:\<\>\@] { pMacro->FlexState = SEPARATOR; return yytext[ 0 ]; }
[\]\}\)]       { pMacro->FlexState = LOOKUP; return yytext[ 0 ]; }

[\x00-\x1F]	return yytext[ 0 ]; /* see below */
[\~\`\?\_\\]    return yytext[ 0 ]; /* see below */
[\x7F-\xFF]	{
	    /* This have to be the last rule - any nonstandard and unhandled
	     * characters should go to grammar analyser instead of printing it
	     * on stdout.
 	     */
	    return yytext[ 0 ];
	}

%{
/* ************************************************************************ */
%}

{Number}       {
   char * ptr;

   pMacro->FlexState = LOOKUP;
   yylval_ptr->valDouble.dNumber = atof( yytext );
   ptr = strchr( yytext, '.' );
   if( ptr )
   {
      yylval_ptr->valDouble.bDec = strlen( ptr + 1 );
      yylval_ptr->valDouble.bWidth = strlen( yytext ) - yylval_ptr->valDouble.bDec;
      if( yylval_ptr->valDouble.bDec )
         yylval_ptr->valDouble.bWidth--;
      yylval_ptr->valDouble.szValue = yytext ;
      return NUM_DOUBLE;
   }
   else
   {
      if( ( double )LONG_MIN <= yylval_ptr->valDouble.dNumber &&
                 yylval_ptr->valDouble.dNumber <= ( double )LONG_MAX )
      {
         yylval_ptr->valLong.lNumber = ( long ) yylval_ptr->valDouble.dNumber;
         yylval_ptr->valLong.szValue = yytext;
         return NUM_LONG;
      }
      else
      {
         yylval_ptr->valDouble.bWidth = strlen( yytext ) + 1;
         yylval_ptr->valDouble.bDec = 0;
         yylval_ptr->valDouble.szValue = yytext;
         return NUM_DOUBLE;
      }
   }
}


{HexNumber}   {
    long lNumber = 0;
    pMacro->FlexState = LOOKUP;

    sscanf( yytext, "%lxI", &lNumber );

    if( ( double )LONG_MIN <= lNumber && lNumber <= ( double )LONG_MAX )
    {
        yylval_ptr->valLong.lNumber = lNumber;
        yylval_ptr->valLong.szValue = yytext;
        return NUM_LONG;
    }
    else
    {
        yylval_ptr->valDouble.dNumber = lNumber;
        yylval_ptr->valDouble.bWidth = HB_DEFAULT_WIDTH;
        yylval_ptr->valDouble.bDec = 0;
        yylval_ptr->valDouble.szValue = yytext;
        return NUM_DOUBLE;
    }
}

{MacroVar}     {
    HB_TRACE(HB_TR_DEBUG, ("{MacroVar}(%s)", yytext));
    if( yytext[ yyleng-1 ] == '.' )
    yytext[ yyleng-1 ] = '\0';
    yylval_ptr->string = hb_strupr( hb_strdup( yytext+1 ) );
    pMacro->FlexState = LOOKUP;
    return MACROVAR;
}

{MacroEnd}     {
    HB_TRACE(HB_TR_DEBUG, ("{MacroEnd}(%s)", yytext));
    yylval_ptr->string = hb_strupr( hb_strdup( yytext ) );
    pMacro->FlexState = LOOKUP;
    return MACROTEXT;
}

{MacroId}      {
    HB_TRACE(HB_TR_DEBUG, ("{MacroId}(%s)", yytext));
    yylval_ptr->string = hb_strupr( hb_strdup( yytext ) );
    pMacro->FlexState = LOOKUP;
    return MACROTEXT;
}

{MacroTxt}     {
    HB_TRACE(HB_TR_DEBUG, ("{MacroTxt}(%s)", yytext));
    yylval_ptr->string = hb_strupr( hb_strdup( yytext ) );
    pMacro->FlexState = LOOKUP;
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
    pMacro->FlexState = LOOKUP;
    return IDENTIFIER;
}

%%

#ifdef __WATCOMC__
/* enable warnings for unreachable code */
#pragma warning 13 1
#endif


void * hb_compFlexNew( HB_MACRO_PTR pMacro )
{
   /* This creates the scanner buffer based on passed string.
    * Unfortunately it creates a copy of this string - the string can be
    * modified during scanning and the string have to end with TWO zero bytes
    * NOTE: It must be used in macro.l because yy_scan_bytes is not
    * visible in macro.y
    */
   HB_TRACE(HB_TR_DEBUG, ("hb_compFlexNew(%s, %i)", pMacro->string, pMacro->length));
   pMacro->FlexState = LOOKUP;
   return (void *) yy_scan_bytes( pMacro->string, pMacro->length );
}

void hb_compFlexDelete( void * pBuffer )
{
   yy_delete_buffer( ( YY_BUFFER_STATE ) pBuffer );
}

/*
static int hb_compInput( char * buf, int max_size, HB_MACRO_PTR pMacro )
{
   int iRead;

   if( pMacro->length < max_size )
   {
      memcpy( buf, pMacro->string, pMacro->length );
      buf[ pMacro->length++ ] = '\n';
      buf[ pMacro->length ] = 0;
      iRead = pMacro->length;
   }
   else
   {
      int iLen = pMacro->length - pMacro->pos;
      if( iLen <= 0 )
      {
        iRead = 0;
      }
      else if( iLen >= max_size )
      {
        iRead = max_size;
        memcpy( buf, pMacro->string + pMacro->pos, iRead );
        pMacro->pos += iRead;
      }
      else
      {
        memcpy( buf, pMacro->string + pMacro->pos, iLen );
        buf[ iLen++ ] = '\n';
        buf[ iLen ] = 0;
        iRead = iLen;
        pMacro->pos += iRead;
      }
   }
   return iRead;
}
*/

