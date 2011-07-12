/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    small and MT safe lexer for macro compiler
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
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

#define HB_MACRO_SUPPORT

#include "hbmacro.h"
#include "hbcomp.h"
#include "hbdate.h"
#include "macroy.h"

typedef struct _HB_MACRO_LEX
{
   const char *   pString;
   char *         pDst;
   HB_SIZE        nLen;
   HB_SIZE        nSrc;
   HB_BOOL        quote;
   char           pBuffer[ 2 ];
}
HB_MACRO_LEX, * PHB_MACRO_LEX;

HB_BOOL hb_macroLexNew( HB_MACRO_PTR pMacro )
{
   if( pMacro->length )
   {
      /*
       * the total maximum size for parsed tokens delimited with ASCII NUL
       * cannot be bigger then the size of macro string because only
       * identifiers, strings, macrovars and macrotexts have to be returned
       * as string and all these tokens have to be separated by some non
       * value tokens or strings which will have not used delimiters
       */
      pMacro->pLex = hb_xgrab( sizeof( HB_MACRO_LEX ) + pMacro->length );
      ( ( PHB_MACRO_LEX ) pMacro->pLex )->pString = pMacro->string;
      ( ( PHB_MACRO_LEX ) pMacro->pLex )->nLen    = pMacro->length;
      ( ( PHB_MACRO_LEX ) pMacro->pLex )->nSrc  = 0;
      ( ( PHB_MACRO_LEX ) pMacro->pLex )->quote = HB_TRUE;
      ( ( PHB_MACRO_LEX ) pMacro->pLex )->pDst  =
                                 ( ( PHB_MACRO_LEX ) pMacro->pLex )->pBuffer;
      return HB_TRUE;
   }

   return HB_FALSE;
}

void hb_macroLexDelete( HB_MACRO_PTR pMacro )
{
   if( pMacro->pLex )
   {
      hb_xfree( pMacro->pLex );
      pMacro->pLex = NULL;
   }
}

static void hb_lexSkipBlank( PHB_MACRO_LEX pLex )
{
   while( pLex->nSrc < pLex->nLen &&
          ( pLex->pString[ pLex->nSrc ] == ' ' ||
            pLex->pString[ pLex->nSrc ] == '\t' ) )
      pLex->nSrc++;
}

static void hb_lexIdentCopy( PHB_MACRO_LEX pLex )
{
   while( pLex->nSrc < pLex->nLen )
   {
      char ch = pLex->pString[ pLex->nSrc ];
      if( ch >= 'a' && ch <= 'z' )
         *pLex->pDst++ = ch - ( 'a' - 'A' );
      else if( ( ch >= 'A' && ch <= 'Z' ) ||
               ( ch >= '0' && ch <= '9' ) || ch == '_' )
         *pLex->pDst++ = ch;
      else
         break;
      pLex->nSrc++;
   }
}

static int hb_lexTimestampGet( YYSTYPE *yylval_ptr, HB_MACRO_PTR pMacro,
                               PHB_MACRO_LEX pLex )
{
   HB_BOOL fOK = HB_FALSE;
   char * dst = pLex->pDst;

   pLex->quote = HB_FALSE;
   while( pLex->nSrc < pLex->nLen )
   {
      char ch = pLex->pString[ pLex->nSrc++ ];
      if( ch == '"' )
      {
         fOK = HB_TRUE;
         break;
      }
      *dst++ = ch;
   }
   *dst = '\0';
   if( !hb_timeStampStrGetDT( pLex->pDst,
                              &yylval_ptr->valTimeStamp.date,
                              &yylval_ptr->valTimeStamp.time ) )
      fOK = HB_FALSE;
   if( !fOK )
      hb_macroError( EG_SYNTAX, pMacro );
   return TIMESTAMP;
}

static int hb_lexDateGet( YYSTYPE *yylval_ptr, HB_MACRO_PTR pMacro,
                          PHB_MACRO_LEX pLex )
{
   HB_BOOL fOK = HB_FALSE;
   char * dst = pLex->pDst;
   int iYear, iMonth, iDay;

   pLex->quote = HB_FALSE;
   while( pLex->nSrc < pLex->nLen )
   {
      char ch = pLex->pString[ pLex->nSrc++ ];
      if( ch == '"' )
      {
         fOK = HB_TRUE;
         break;
      }
      *dst++ = ch;
   }
   *dst = '\0';
   if( fOK && hb_timeStampStrGet( pLex->pDst, &iYear, &iMonth, &iDay, NULL, NULL, NULL, NULL ) )
   {
      yylval_ptr->valLong.lNumber = hb_dateEncode( iYear, iMonth, iDay );
   }
   else
   {
      yylval_ptr->valLong.lNumber = 0;
      hb_macroError( EG_SYNTAX, pMacro );
   }

   return NUM_DATE;
}

static int hb_lexStringCopy( YYSTYPE *yylval_ptr, HB_MACRO_PTR pMacro,
                             PHB_MACRO_LEX pLex, char cDelim )
{
   pLex->quote = HB_FALSE;
   yylval_ptr->valChar.string = pLex->pDst;
   while( pLex->nSrc < pLex->nLen )
   {
      char ch = pLex->pString[ pLex->nSrc++ ];
      if( ch == cDelim )
      {
         yylval_ptr->valChar.length = pLex->pDst - yylval_ptr->valChar.string;
         *pLex->pDst++ = '\0';
         return LITERAL;
      }
      *pLex->pDst++ = ch;
   }
   yylval_ptr->valChar.length = pLex->pDst - yylval_ptr->valChar.string;
   *pLex->pDst++ = '\0';
   hb_macroError( EG_SYNTAX, pMacro );
   return LITERAL;
}

static int hb_lexStringExtCopy( YYSTYPE *yylval_ptr, HB_MACRO_PTR pMacro,
                                PHB_MACRO_LEX pLex )
{
   HB_SIZE nLen;
   char * string;
   pLex->quote = HB_FALSE;
   string = pLex->pDst;
   while( pLex->nSrc < pLex->nLen )
   {
      char ch = pLex->pString[ pLex->nSrc++ ];
      if( ch == '\\' )
      {
         if( pLex->nSrc < pLex->nLen )
         {
            *pLex->pDst++ = ch;
            ch = pLex->pString[ pLex->nSrc++ ];
         }
      }
      else if( ch == '"' )
      {
         nLen = pLex->pDst - string;
         *pLex->pDst++ = '\0';
         hb_strRemEscSeq( string, &nLen );
         yylval_ptr->valChar.length = nLen;
         yylval_ptr->valChar.string = string;
         return LITERAL;
      }
      *pLex->pDst++ = ch;
   }
   nLen = pLex->pDst - string;
   *pLex->pDst++ = '\0';
   hb_strRemEscSeq( string, &nLen );
   yylval_ptr->valChar.length = nLen;
   yylval_ptr->valChar.string = string;
   hb_macroError( EG_SYNTAX, pMacro );
   return LITERAL;
}

static int hb_lexNumConv( YYSTYPE *yylval_ptr, PHB_MACRO_LEX pLex, HB_SIZE nLen )
{
   HB_MAXINT lNumber;
   double dNumber;
   int iDec, iWidth;

   if( hb_compStrToNum( pLex->pString + pLex->nSrc, nLen,
                        &lNumber, &dNumber, &iDec, &iWidth ) )
   {
      yylval_ptr->valDouble.dNumber = dNumber;
      yylval_ptr->valDouble.bDec = ( HB_UCHAR ) iDec;
      yylval_ptr->valDouble.bWidth = ( HB_UCHAR ) iWidth;
      pLex->nSrc += nLen;
      return NUM_DOUBLE;
   }
   else
   {
      yylval_ptr->valLong.lNumber = lNumber;
      yylval_ptr->valLong.bWidth = ( HB_UCHAR ) iWidth;
      pLex->nSrc += nLen;
      return NUM_LONG;
   }
}

extern int hb_macro_yylex( YYSTYPE *yylval_ptr, HB_MACRO_PTR pMacro );

int hb_macro_yylex( YYSTYPE *yylval_ptr, HB_MACRO_PTR pMacro )
{
   PHB_MACRO_LEX pLex = ( PHB_MACRO_LEX ) pMacro->pLex;

   while( pLex->nSrc < pLex->nLen )
   {
      unsigned char ch = ( unsigned char ) pLex->pString[ pLex->nSrc++ ];
      switch( ch )
      {
         case ' ':
         case '\t':
            break;

         case '$':
         case ',':
         case '|':
         case '@':
         case '(':
         case '{':
            pLex->quote = HB_TRUE;
            return ch;

         case ')':
         case '}':
         case ']':
            pLex->quote = HB_FALSE;
            return ch;

         case '#':
            pLex->quote = HB_TRUE;
            return NE1;

         case '!':
            pLex->quote = HB_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return NE2;
            }
            return NOT;

         case '<':
            pLex->quote = HB_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '>' )
            {
               pLex->nSrc++;
               return NE2;
            }
            else if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return LE;
            }
            return '<';

         case '>':
            pLex->quote = HB_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return GE;
            }
            return '>';

         case '=':
            pLex->quote = HB_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return EQ;
            }
            else if( pLex->pString[ pLex->nSrc ] == '>' && HB_SUPPORT_HARBOUR )
            {
               pLex->nSrc++;
               return HASHOP;
            }
            return '=';

         case '+':
            pLex->quote = HB_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '+' )
            {
               pLex->nSrc++;
               return INC;
            }
            else if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return PLUSEQ;
            }
            return '+';

         case '-':
            pLex->quote = HB_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '-' )
            {
               pLex->nSrc++;
               return DEC;
            }
            else if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return MINUSEQ;
            }
            else if( pLex->pString[ pLex->nSrc ] == '>' )
            {
               pLex->nSrc++;
               return ALIASOP;
            }
            return '-';

         case '*':
            pLex->quote = HB_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '*' )
            {
               pLex->nSrc++;
               if( pLex->pString[ pLex->nSrc ] == '=' )
               {
                  pLex->nSrc++;
                  return EXPEQ;
               }
               return POWER;
            }
            else if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return MULTEQ;
            }
            return '*';

         case '/':
            pLex->quote = HB_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return DIVEQ;
            }
            return '/';

         case '%':
            pLex->quote = HB_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return MODEQ;
            }
            return '%';

         case '^':
            pLex->quote = HB_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return EXPEQ;
            }
            return POWER;

         case ':':
            pLex->quote = HB_TRUE;
            if( pLex->pString[ pLex->nSrc ] == '=' )
            {
               pLex->nSrc++;
               return INASSIGN;
            }
            else if( pLex->pString[ pLex->nSrc ] == ':' )
            {
               yylval_ptr->string = "SELF";
               return IDENTIFIER;
            }
            return ':';

         case '.':
            pLex->quote = HB_TRUE;
            if( pLex->nSrc < pLex->nLen &&
                HB_ISDIGIT( pLex->pString[ pLex->nSrc ] ) )
            {
               HB_SIZE ul = pLex->nSrc;
               while( ++ul < pLex->nLen &&
                      HB_ISDIGIT( pLex->pString[ ul ] ) ) {};
               ul -= --pLex->nSrc;
               return hb_lexNumConv( yylval_ptr, pLex, ul );
            }
            if( pLex->nLen - pLex->nSrc >= 4 &&
                pLex->pString[ pLex->nSrc + 3 ] == '.' )
            {
               if( ( pLex->pString[ pLex->nSrc + 0 ] | ('a' - 'A') ) == 'a' &&
                   ( pLex->pString[ pLex->nSrc + 1 ] | ('a' - 'A') ) == 'n' &&
                   ( pLex->pString[ pLex->nSrc + 2 ] | ('a' - 'A') ) == 'd' )
               {
                  pLex->nSrc += 4;
                  return AND;
               }
               if( ( pLex->pString[ pLex->nSrc + 0 ] | ('a' - 'A') ) == 'n' &&
                   ( pLex->pString[ pLex->nSrc + 1 ] | ('a' - 'A') ) == 'o' &&
                   ( pLex->pString[ pLex->nSrc + 2 ] | ('a' - 'A') ) == 't' )
               {
                  pLex->nSrc += 4;
                  return NOT;
               }
            }
            if( pLex->nLen - pLex->nSrc >= 3 &&
                pLex->pString[ pLex->nSrc + 2 ] == '.' )
            {
               if( ( pLex->pString[ pLex->nSrc + 0 ] | ('a' - 'A') ) == 'o' &&
                   ( pLex->pString[ pLex->nSrc + 1 ] | ('a' - 'A') ) == 'r' )
               {
                  pLex->nSrc += 3;
                  return OR;
               }
            }
            if( pLex->nLen - pLex->nSrc >= 2 &&
                pLex->pString[ pLex->nSrc + 1 ] == '.' )
            {
               if( ( pLex->pString[ pLex->nSrc ] | ('a' - 'A') ) == 't' ||
                   ( pLex->pString[ pLex->nSrc ] | ('a' - 'A') ) == 'y' )
               {
                  pLex->quote = HB_FALSE;
                  pLex->nSrc += 2;
                  return TRUEVALUE;
               }
               if( ( pLex->pString[ pLex->nSrc ] | ('a' - 'A') ) == 'f' ||
                   ( pLex->pString[ pLex->nSrc ] | ('a' - 'A') ) == 'n' )
               {
                  pLex->quote = HB_FALSE;
                  pLex->nSrc += 2;
                  return FALSEVALUE;
               }
               if( pLex->pString[ pLex->nSrc ] == '.' )
               {
                  pLex->nSrc += 2;
                  return EPSILON;
               }
            }
            return '.';

         case '[':
            if( pLex->quote )
               return hb_lexStringCopy( yylval_ptr, pMacro, pLex, ']' );
            pLex->quote = HB_TRUE;
            return '[';

         case '`':
         case '\'':
            return hb_lexStringCopy( yylval_ptr, pMacro, pLex, '\'' );

         case '"':
            return hb_lexStringCopy( yylval_ptr, pMacro, pLex, '"' );

         case '&':
            if( pLex->nSrc < pLex->nLen )
            {
               if( HB_ISFIRSTIDCHAR( pLex->pString[ pLex->nSrc ] ) )
               {
                  /* [&<keyword>[.[<nextidchars>]]]+ */
                  int iParts = 0;
                  pLex->quote = HB_FALSE;
                  yylval_ptr->string = pLex->pDst;
                  pLex->nSrc--;
                  do
                  {
                     ++iParts;
                     *pLex->pDst++ = '&';
                     pLex->nSrc++;
                     hb_lexIdentCopy( pLex );
                     if( pLex->pString[ pLex->nSrc ] == '.' )
                     {
                        ++iParts;
                        *pLex->pDst++ = '.';
                        pLex->nSrc++;
                        hb_lexIdentCopy( pLex );
                     }
                  }
                  while( pLex->nLen - pLex->nSrc > 1 &&
                         pLex->pString[ pLex->nSrc ] == '&' &&
                         HB_ISFIRSTIDCHAR( pLex->pString[ pLex->nSrc + 1 ] ) );
                  if( iParts == 2 && *( pLex->pDst - 1 ) == '.' )
                  {
                     pLex->pDst--;
                     iParts = 1;
                  }
                  *pLex->pDst++ = '\0';
                  if( iParts == 1 )
                  {
                     yylval_ptr->string++;
                     if( pLex->pDst - yylval_ptr->string > HB_SYMBOL_NAME_LEN + 1 )
                        ( ( char * ) yylval_ptr->string )[ HB_SYMBOL_NAME_LEN ] = '\0';
                     return MACROVAR;
                  }
                  return MACROTEXT;
               }
               else if( pLex->pString[ pLex->nSrc ] == '\'' ||
                        pLex->pString[ pLex->nSrc ] == '"' ||
                        pLex->pString[ pLex->nSrc ] == '[' )
                  hb_macroError( EG_SYNTAX, pMacro );
            }
            pLex->quote = HB_TRUE;
            return '&';

         default:
            if( HB_ISDIGIT( ch ) )
            {
               HB_SIZE n = pLex->nSrc;

               pLex->quote = HB_FALSE;
               if( ch == '0' && n < pLex->nLen )
               {
                  if( pLex->pString[ n ] == 'd' || pLex->pString[ n ] == 'D' )
                  {
                     while( ++n < pLex->nLen &&
                            HB_ISDIGIT( pLex->pString[ n ] ) ) {};
                     if( n - pLex->nSrc == 9 )
                     {
                        int year, month, day;
                        hb_dateStrGet( pLex->pString + pLex->nSrc + 1,
                                       &year, &month, &day );
                        yylval_ptr->valLong.lNumber =
                                       hb_dateEncode( year, month, day );
                        pLex->nSrc = n;
                        if( yylval_ptr->valLong.lNumber == 0 &&
                            ( year != 0 || month != 0 || day != 0 ) )
                        {
                           hb_macroError( EG_SYNTAX, pMacro );
                        }
                        return NUM_DATE;
                     }
                     else if( n - pLex->nSrc == 2 &&
                              pLex->pString[ pLex->nSrc + 1 ] == '0' )
                     {
                        yylval_ptr->valLong.lNumber = 0;
                        return NUM_DATE;
                     }
                     n = pLex->nSrc;
                  }
                  else if( pLex->pString[ n ] == 'x' ||
                           pLex->pString[ n ] == 'X' )
                  {
                     while( ++n < pLex->nLen &&
                            HB_ISXDIGIT( pLex->pString[ n ] ) ) {};
                     if( n == pLex->nSrc + 1 )
                        --n;
                  }
                  else
                  {
                     while( n < pLex->nLen &&
                            HB_ISDIGIT( pLex->pString[ n ] ) )
                        ++n;
                     if( pLex->nLen - n > 1 && pLex->pString[ n ] == '.' &&
                         HB_ISDIGIT( pLex->pString[ n + 1 ] ) )
                     {
                        while( ++n < pLex->nLen &&
                               HB_ISDIGIT( pLex->pString[ n ] ) ) {};
                     }
                  }
               }
               else
               {
                  while( n < pLex->nLen &&
                         HB_ISDIGIT( pLex->pString[ n ] ) )
                     ++n;
                  if( pLex->nLen - n > 1 && pLex->pString[ n ] == '.' &&
                      HB_ISDIGIT( pLex->pString[ n + 1 ] ) )
                  {
                     while( ++n < pLex->nLen &&
                            HB_ISDIGIT( pLex->pString[ n ] ) ) {};
                  }
               }
               n -= --pLex->nSrc;
               return hb_lexNumConv( yylval_ptr, pLex, n );
            }
            else if( HB_ISFIRSTIDCHAR( ch ) )
            {
               HB_SIZE nLen;
               pLex->quote = HB_FALSE;
               yylval_ptr->string = pLex->pDst;
               *pLex->pDst++ = ch - ( ( ch >= 'a' && ch <= 'z' ) ? 'a' - 'A' : 0 );
               hb_lexIdentCopy( pLex );
               if( pLex->nLen - pLex->nSrc > 1 &&
                   pLex->pString[ pLex->nSrc ] == '&' &&
                   HB_ISFIRSTIDCHAR( pLex->pString[ pLex->nSrc + 1 ] ) )
               {
                  /* [<keyword>][&<keyword>[.[<nextidchars>]]]+ */
                  do
                  {
                     *pLex->pDst++ = '&';
                     pLex->nSrc++;
                     hb_lexIdentCopy( pLex );
                     if( pLex->pString[ pLex->nSrc ] == '.' )
                     {
                        *pLex->pDst++ = '.';
                        pLex->nSrc++;
                        hb_lexIdentCopy( pLex );
                     }
                  }
                  while( pLex->nLen - pLex->nSrc > 1 &&
                         pLex->pString[ pLex->nSrc ] == '&' &&
                         HB_ISFIRSTIDCHAR( pLex->pString[ pLex->nSrc + 1 ] ) );
                  *pLex->pDst++ = '\0';
                  return MACROTEXT;
               }
               nLen = pLex->pDst - yylval_ptr->string;
               *pLex->pDst++ = '\0';
               if( nLen == 1 )
               {
                  if( pLex->nLen > pLex->nSrc &&
                      pLex->pString[ pLex->nSrc ] == '"' )
                  {
                     switch( yylval_ptr->string[ 0 ] )
                     {
                        case 'E':
                           pLex->nSrc++;
                           return hb_lexStringExtCopy( yylval_ptr, pMacro, pLex );
                        case 'T':
                           pLex->nSrc++;
                           return hb_lexTimestampGet( yylval_ptr, pMacro, pLex );
                        case 'D':
                           pLex->nSrc++;
                           return hb_lexDateGet( yylval_ptr, pMacro, pLex );
                     }
                  }
               }
               else if( nLen == 2 )
               {
                  if( yylval_ptr->string[ 0 ] == 'I' &&
                      yylval_ptr->string[ 1 ] == 'F' )
                  {
                     hb_lexSkipBlank( pLex );
                     if( pLex->nSrc < pLex->nLen &&
                         pLex->pString[ pLex->nSrc ] == '(' )
                        return IIF;
                  }
               }
               else if( nLen == 3 )
               {
                  if( yylval_ptr->string[ 0 ] == 'I' &&
                      yylval_ptr->string[ 1 ] == 'I' &&
                      yylval_ptr->string[ 2 ] == 'F' )
                  {
                     hb_lexSkipBlank( pLex );
                     if( pLex->nSrc < pLex->nLen &&
                         pLex->pString[ pLex->nSrc ] == '(' )
                        return IIF;
                  }
                  else if( yylval_ptr->string[ 0 ] == 'N' &&
                           yylval_ptr->string[ 1 ] == 'I' &&
                           yylval_ptr->string[ 2 ] == 'L' )
                     return NIL;
               }
               else /* nLen >= 4 */
               {
                  switch( yylval_ptr->string[ 0 ] )
                  {
                     case '_':
                        if( nLen <= 6 && memcmp( "FIELD", yylval_ptr->string + 1,
                                                 nLen - 1 ) == 0 )
                        {
                           hb_lexSkipBlank( pLex );
                           if( pLex->nSrc + 1 < pLex->nLen &&
                               pLex->pString[ pLex->nSrc ] == '-' &&
                               pLex->pString[ pLex->nSrc + 1 ] == '>' )
                           return FIELD;
                        }
                        break;
                     case 'F':
                        if( nLen <= 5 && memcmp( "IELD", yylval_ptr->string + 1,
                                                 nLen - 1 ) == 0 )
                        {
                           hb_lexSkipBlank( pLex );
                           if( pLex->nSrc + 1 < pLex->nLen &&
                               pLex->pString[ pLex->nSrc ] == '-' &&
                               pLex->pString[ pLex->nSrc + 1 ] == '>' )
                           return FIELD;
                        }
                        break;
                     case 'Q':
                        if( nLen == 5 && memcmp( "SELF", yylval_ptr->string + 1,
                                                 4 ) == 0 )
                        {
                           hb_lexSkipBlank( pLex );
                           if( pLex->nSrc < pLex->nLen &&
                               pLex->pString[ pLex->nSrc ] == '(' )
                           {
                              HB_SIZE n = pLex->nSrc;
                              while( ++n < pLex->nLen )
                              {
                                 if( pLex->pString[ n ] == ')' )
                                 {
                                    pLex->nSrc = n + 1;
                                    return SELF;
                                 }
                                 else if( pLex->pString[ n ] != ' ' &&
                                          pLex->pString[ n ] != '\t' )
                                    break;
                              }
                           }
                        }
                        break;
                  }
                  if( pLex->pDst - yylval_ptr->string > HB_SYMBOL_NAME_LEN + 1 )
                     ( ( char * ) yylval_ptr->string )[ HB_SYMBOL_NAME_LEN ] = '\0';
               }
               return IDENTIFIER;
            }
            return ch;
      }
   }

   return 0;
}
