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

/* Compile using: flex -i -8 -omacrol.c -Phb_comp -C macro.l

NOTE: -C  controls the speed/size ratio of generated scanner
-Cf   = fastest/biggest
-CF
-C    = in between
-Cm
-Ce
-Cem  = slowest/smallest
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "macro.h"
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


Array         {Identifier}[ \t]*"["
MacroVarArray {MacroVar}[ \t]*"["
MacroTxtArray ({MacroEnd}|{MacroId}|{MacroTxt})[ \t]*"["
AtArray       "}"[ \t]*"["
ExpArray      ")"[ \t]*"["
SubArray      "]"[ \t]*"["

Separator     {SpaceTab}

%x            STRING1 STRING2 STRING3
%x            FIELD_ IIF_ IF_
%s            INDEX

%%


"&"("'"|\"|\[) { hb_macroError( EG_SYNTAX, YYLEX_PARAM ); }

'              BEGIN STRING1;
\"             BEGIN STRING2;

"="[ \t]*"["                         { BEGIN STRING3; return '=';      }
"+"[ \t]*"["                         { BEGIN STRING3; return '+';      }
"-"[ \t]*"["                         { BEGIN STRING3; return '-';      }
"*"[ \t]*"["                         { BEGIN STRING3; return '*';      }
"/"[ \t]*"["                         { BEGIN STRING3; return '/';      }
"%"[ \t]*"["                         { BEGIN STRING3; return '%';      }
"$"[ \t]*"["                         { BEGIN STRING3; return '$';      }
("<>"|"!=")[ \t]*"["                 { BEGIN STRING3; return NE2;      }
":="[ \t]*"["                        { BEGIN STRING3; return INASSIGN; }
"=="[ \t]*"["                        { BEGIN STRING3; return EQ;       }
"<="[ \t]*"["                        { BEGIN STRING3; return LE;       }
">="[ \t]*"["                        { BEGIN STRING3; return GE;       }
"+="[ \t]*"["                        { BEGIN STRING3; return PLUSEQ;   }
"-="[ \t]*"["                        { BEGIN STRING3; return MINUSEQ;  }
"*="[ \t]*"["                        { BEGIN STRING3; return MULTEQ;   }
"/="[ \t]*"["                        { BEGIN STRING3; return DIVEQ;    }
"^="[ \t]*"["                        { BEGIN STRING3; return EXPEQ;    }
"%="[ \t]*"["                        { BEGIN STRING3; return MODEQ;    }
("**"|"^")[ \t]*"["                  { BEGIN STRING3; return POWER;    }
".and."[ \t]*"["                     { BEGIN STRING3; return AND;      }
".or."[ \t]*"["                      { BEGIN STRING3; return OR;       }
("!"|".not.")[ \t]*"["               { BEGIN STRING3; return NOT;      }
(","|"{"|"<"|">"|"(")[ \t]*"["   { BEGIN STRING3; yyleng = 1; yytext[1] = 0; return yytext[ 0 ]; }

<INITIAL>\[    BEGIN STRING3;

<STRING1>[^'^\n]*   { hb_macroError( EG_SYNTAX, YYLEX_PARAM ); BEGIN 0; }
<STRING2>[^\"^\n]*  { hb_macroError( EG_SYNTAX, YYLEX_PARAM ); BEGIN 0; }
<STRING3>[^\]]*\n   { hb_macroError( EG_SYNTAX, YYLEX_PARAM ); BEGIN 0; }

<STRING1>[^']*'   { if( i_INDEX_STATE )
                       BEGIN INDEX;
                    else
                       BEGIN 0;

                    yyleng--;
                    yytext[yyleng] = 0;
                    yylval_ptr->string = hb_strdup( yytext );
                    return LITERAL;
                  }

<STRING2>[^\"]*\" { if( i_INDEX_STATE )
                       BEGIN INDEX;
                    else
                       BEGIN 0;

                    yyleng--;
                    yytext[yyleng] = 0;
                    yylval_ptr->string = hb_strdup( yytext );
                    return LITERAL;
                  }

<STRING3>[^\]]*\] { if( i_INDEX_STATE )
                       BEGIN INDEX;
                    else
                       BEGIN 0;

                    yyleng--;
                    yytext[yyleng] = 0;
                    yylval_ptr->string = hb_strdup( yytext );
                    return LITERAL;
                  }

<INDEX>\n    { hb_macroError( EG_SYNTAX, YYLEX_PARAM );  }

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
                return yytext[ 0 ];
             }

{SpaceTab}     ;

\n.*         {
                yyless( 1 );
                return '\n';
             }
%{
/* ************************************************************************ */
%}
"_fie"|"_fiel"|"_field" {
                        if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                   return FIELD;
                 }

"fiel"|"field"   {
                    if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                   hb_comp_iState =FIELD;
                 }

"iif"        {
               if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
               return IIF;
             }

"if"           {
                 if( i_INDEX_STATE ) BEGIN INDEX; else BEGIN 0;
                 return IF;
             }

"nil"          return NIL;

"qself"{SpaceTab}*\({SpaceTab}*\) return SELF;

%{
/* ************************************************************************ */
%}

"#"            return NE1;
"="            return yytext[ 0 ];
"+"            return yytext[ 0 ];
"-"            return yytext[ 0 ];
"*"            return yytext[ 0 ];
[\/]           return yytext[ 0 ];
"%"            return yytext[ 0 ];
"$"            return yytext[ 0 ];
"<>"|"!="      return NE2;
":="           return INASSIGN;
"=="           return EQ;
"++"           return INC;
"--"           return DEC;
"->"           return ALIASOP;
"<="           return LE;
">="           return GE;
"+="           return PLUSEQ;
"-="           return MINUSEQ;
"*="           return MULTEQ;
"/="           return DIVEQ;
"^="           return EXPEQ;
"%="           return MODEQ;
"**"|"^"       return POWER;
".and."        return AND;
".or."         return OR;
"."[t|y]"."    return TRUEVALUE;
"."[f|n]"."    return FALSEVALUE;
"!"|".not."    return NOT;
"::"           unput( ':' ); unput( 'f' ); unput( 'l' ); unput( 'e' ); unput( 'S' );
[,\{\}\|\#\&\.\:\<\>\[\]\@] return yytext[ 0 ];
[\(]           ++_iOpenBracket; return yytext[ 0 ];
[\)]           --_iOpenBracket; return yytext[ 0 ];

[\x00-\x1F]	return yytext[ 0 ]; /* see below */
[\x80-\xFF]	{ 
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

   yylval_ptr->valDouble.dNumber = atof( yytext );
   ptr = strchr( yytext, '.' );
   if( ptr )
   {
      yylval_ptr->valDouble.bDec = strlen( ptr + 1 );
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
         yylval_ptr->valDouble.bDec = 0;
         yylval_ptr->valDouble.szValue = yytext;
         return NUM_DOUBLE;
      }
   }
}


{HexNumber}   {
                long lNumber = 0;

                 sscanf( yytext, "%lxI", &lNumber );

                 if( ( double )LONG_MIN <= lNumber &&
                          lNumber <= ( double )LONG_MAX )
                 {
                    yylval_ptr->valLong.lNumber = lNumber;
                    yylval_ptr->valLong.szValue = yytext;
                    return NUM_LONG;
                 }
                 else
                 {
                    yylval_ptr->valDouble.dNumber = lNumber;
                    yylval_ptr->valDouble.bDec = 0;
                    yylval_ptr->valDouble.szValue = yytext;
                    return NUM_DOUBLE;
                 }
              }

{Array}     {
               HB_TRACE(HB_TR_DEBUG, ("{Array}(%s)", yytext));
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
                  if( YYLEX_PARAM->bName10 && strlen( yytext ) > 10 )
                  {
                     yytext[ 10 ] = 0;
                     yyleng = 10;
                  }
                  yylval_ptr->string = hb_strupr( hb_strdup( yytext ) );
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
               yylval_ptr->string = hb_strupr( hb_strdup( yytext+1 ) );
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
               yylval_ptr->string = hb_strupr( hb_strdup( yytext ) );
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
               --_iOpenBracket;
               return '}';
            }

{MacroVar}     {
                   HB_TRACE(HB_TR_DEBUG, ("{MacroVar}(%s)", yytext));
                  if( yytext[ yyleng-1 ] == '.' )
                     yytext[ yyleng-1 ] = '\0';
                  yylval_ptr->string = hb_strupr( hb_strdup( yytext+1 ) );
                  hb_comp_iState = MACROVAR;
                  return MACROVAR;
               }

{MacroEnd}     {
                   HB_TRACE(HB_TR_DEBUG, ("{MacroEnd}(%s)", yytext));
                  yylval_ptr->string = hb_strupr( hb_strdup( yytext ) );
                  hb_comp_iState = MACROTEXT;
                  return MACROTEXT;
               }

{MacroId}      {
                   HB_TRACE(HB_TR_DEBUG, ("{MacroId}(%s)", yytext));
                  yylval_ptr->string = hb_strupr( hb_strdup( yytext ) );
                  hb_comp_iState = MACROTEXT;
                  return MACROTEXT;
               }

{MacroTxt}     {
                   HB_TRACE(HB_TR_DEBUG, ("{MacroTxt}(%s)", yytext));
                  yylval_ptr->string = hb_strupr( hb_strdup( yytext ) );
                  hb_comp_iState = MACROTEXT;
                  return MACROTEXT;
               }

{Identifier}    {
                   HB_TRACE(HB_TR_DEBUG, ("{Identifier}(%s)", yytext));
                   if( YYLEX_PARAM->bName10 && strlen( yytext ) > 10 )
                   {
                        yytext[ 10 ] = 0;
                        yyleng = 10;
                   }
                   yylval_ptr->string = hb_strupr( hb_strdup( yytext ) );
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

