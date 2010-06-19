/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

#define _HB_TOK_RESPECT_DQUOTE   1
#define _HB_TOK_RESPECT_SQUOTE   2
#define _HB_TOK_ISDELIM          4
#define _HB_TOK_STRIP_QUUTE      8

static HB_SIZE hb_tokenCount( const char * szLine, HB_SIZE ulLen,
                              const char * szDelim, HB_SIZE ulDelim,
                              int iFlags )
{
   HB_SIZE ul = 0, ulTokens = 1;
   char cQuote = 0;

   while( ul < ulLen )
   {
      if( cQuote )
      {
         if( szLine[ ul ] == cQuote )
            cQuote = 0;
      }
      else if( ( szLine[ ul ] == '"' && ( iFlags & _HB_TOK_RESPECT_DQUOTE ) ) ||
               ( szLine[ ul ] == '\'' && ( iFlags & _HB_TOK_RESPECT_SQUOTE ) ) )
         cQuote = szLine[ ul ];
      else if( szLine[ ul ] == szDelim[ 0 ] &&
               ( ulDelim == 1 || !memcmp( szLine + ul, szDelim, ulDelim ) ) )
      {
         ++ulTokens;
         if( ( iFlags & _HB_TOK_ISDELIM ) == 0 )
         {
            while( ul + 1 < ulLen && szLine[ ul + 1 ] == szDelim[ 0 ] )
               ++ul;
         }
         ul += ulDelim - 1;
      }
      ++ul;
   }

   return ulTokens;
}

static const char * hb_tokenGet( const char * szLine, HB_SIZE ulLen,
                                 const char * szDelim, HB_SIZE ulDelim,
                                 int iFlags, HB_SIZE ulToken, HB_SIZE * pulLen )
{
   HB_SIZE ul, ulStart;
   char cQuote = 0;

   for( ul = ulStart = 0; ul < ulLen; ++ul )
   {
      if( cQuote )
      {
         if( szLine[ ul ] == cQuote )
            cQuote = 0;
      }
      else if( ( szLine[ ul ] == '"' && ( iFlags & _HB_TOK_RESPECT_DQUOTE ) ) ||
               ( szLine[ ul ] == '\'' && ( iFlags & _HB_TOK_RESPECT_SQUOTE ) ) )
         cQuote = szLine[ ul ];
      else if( szLine[ ul ] == szDelim[ 0 ] &&
               ( ulDelim == 1 || !memcmp( szLine + ul, szDelim, ulDelim ) ) )
      {
         if( --ulToken == 0 )
         {
            * pulLen = ul - ulStart;
            return szLine + ulStart;
         }
         if( ( iFlags & _HB_TOK_ISDELIM ) == 0 )
         {
            while( ul + 1 < ulLen && szLine[ ul + 1 ] == szDelim[ 0 ] )
               ++ul;
         }
         ulStart = ul + ulDelim;
      }
   }
   if( --ulToken == 0 )
   {
      * pulLen = ul - ulStart;
      return szLine + ulStart;
   }
   * pulLen = 0;
   return NULL;
}

static PHB_ITEM hb_tokenArray( const char * szLine, HB_SIZE ulLen,
                               const char * szDelim, HB_SIZE ulDelim,
                               int iFlags )
{
   HB_SIZE ulTokens = hb_tokenCount( szLine, ulLen, szDelim, ulDelim, iFlags );
   PHB_ITEM pArray = hb_itemArrayNew( ulTokens );

   if( ulTokens )
   {
      HB_SIZE ul, ulStart, ulToken;
      char cQuote = 0;

      for( ul = ulStart = ulToken = 0; ul < ulLen; ++ul )
      {
         if( cQuote )
         {
            if( szLine[ ul ] == cQuote )
               cQuote = 0;
         }
         else if( ( szLine[ ul ] == '"' && ( iFlags & _HB_TOK_RESPECT_DQUOTE ) ) ||
                  ( szLine[ ul ] == '\'' && ( iFlags & _HB_TOK_RESPECT_SQUOTE ) ) )
            cQuote = szLine[ ul ];
         else if( szLine[ ul ] == szDelim[ 0 ] &&
                  ( ulDelim == 1 || !memcmp( szLine + ul, szDelim, ulDelim ) ) )
         {
            hb_arraySetCL( pArray, ++ulToken, szLine + ulStart, ul - ulStart );
            if( ( iFlags & _HB_TOK_ISDELIM ) == 0 )
            {
               while( ul + 1 < ulLen && szLine[ ul + 1 ] == szDelim[ 0 ] )
                  ++ul;
            }
            ulStart = ul + ulDelim;
         }
      }
      hb_arraySetCL( pArray, ++ulToken, szLine + ulStart, ul - ulStart );
   }

   return pArray;
}

static HB_BOOL hb_tokenParam( int iParam, HB_SIZE ulSkip,
                              const char ** pszLine, HB_SIZE * pulLen,
                              const char ** pszDelim, HB_SIZE * pulDelim,
                              int * piFlags )
{
   const char * szLine = hb_parc( 1 ), * szDelim = NULL;
   HB_SIZE ulLen = hb_parclen( 1 ), ulDelim = 0;
   int iFlags = 0;

   if( ulLen )
   {
      if( ulSkip )
      {
         szLine += ulSkip;
         if( ulLen <= ulSkip )
            ulLen = 0;
         else
            ulLen -= ulSkip;
      }

      ulDelim = hb_parclen( iParam );
      if( ulDelim )
      {
         szDelim = hb_parc( iParam );
         iFlags |= _HB_TOK_ISDELIM;
      }
      else
      {
         szDelim = " ";
         ulDelim = 1;
      }

      if( ( iFlags & _HB_TOK_ISDELIM ) == 0 )
      {
         while( ulLen && * szLine == szDelim[ 0 ] )
         {
            ++szLine;
            --ulLen;
         }
         while( ulLen && szLine[ ulLen - 1 ] == szDelim[ 0 ] )
            --ulLen;
      }
      if( hb_parl( iParam + 1 ) )
      {
         iFlags |= _HB_TOK_RESPECT_DQUOTE | _HB_TOK_RESPECT_SQUOTE;
         if( hb_parl( iParam + 2 ) )
            iFlags &= ~_HB_TOK_RESPECT_SQUOTE;
      }
   }

   *pulLen = ulLen;
   *pulDelim = ulDelim;
   *pszLine = szLine;
   *pszDelim = szDelim;
   *piFlags = iFlags;

   return szLine != NULL;
}

HB_FUNC( HB_TOKENCOUNT )
{
   const char * szLine, * szDelim;
   HB_SIZE ulLen, ulDelim;
   int iFlags;

   if( hb_tokenParam( 2, 0, &szLine, &ulLen, &szDelim, &ulDelim, &iFlags ) )
      hb_retns( hb_tokenCount( szLine, ulLen, szDelim, ulDelim, iFlags ) );
   else
      hb_retns( 0 );
}

HB_FUNC( HB_TOKENGET )
{
   const char * szLine, * szDelim;
   HB_SIZE ulLen, ulDelim;
   int iFlags;

   if( hb_tokenParam( 3, 0, &szLine, &ulLen, &szDelim, &ulDelim, &iFlags ) )
   {
      szLine = hb_tokenGet( szLine, ulLen, szDelim, ulDelim, iFlags,
                            hb_parns( 2 ), &ulLen );
      hb_retclen( szLine, ulLen );
   }
   else
      hb_retc_null();
}

/* like HB_TOKENGET() but returns next token starting from passed position
 * (0 based) inside string, f.e.:
 *    HB_TOKENPTR( cString, @nTokPos, Chr( 9 ) ) -> cToken
 */
HB_FUNC( HB_TOKENPTR )
{
   const char * szLine, * szDelim, * szToken;
   HB_SIZE ulLen, ulDelim, ulSkip, ulToken;
   int iFlags;

   if( hb_tokenParam( 3, hb_parns( 2 ), &szLine, &ulLen, &szDelim, &ulDelim, &iFlags ) )
   {
      szToken = hb_tokenGet( szLine, ulLen, szDelim, ulDelim, iFlags,
                             1, &ulToken );
      if( szToken && ulLen > ulToken )
         ulSkip = szToken - hb_parc( 1 ) + ulToken + ulDelim;
      else
         ulSkip = hb_parclen( 1 ) + 1;

      /* return position to start next search from */
      hb_storns( ulSkip, 2 );
      /* return token */
      hb_retclen( szToken, ulToken );
   }
   else
   {
      hb_storns( 0, 2 );
      hb_retc_null();
   }
}

HB_FUNC( HB_ATOKENS )
{
   const char * szLine, * szDelim;
   HB_SIZE ulLen, ulDelim;
   int iFlags;

   if( hb_tokenParam( 2, 0, &szLine, &ulLen, &szDelim, &ulDelim, &iFlags ) )
      hb_itemReturnRelease( hb_tokenArray( szLine, ulLen, szDelim, ulDelim, iFlags ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

#if defined( HB_LEGACY_LEVEL3 )

HB_FUNC( __STRTOKEN )
{
   HB_FUNC_EXEC( HB_TOKENGET );
}

HB_FUNC( __STRTKPTR )
{
   HB_FUNC_EXEC( HB_TOKENPTR );
}

HB_FUNC( __STRTOKENCOUNT )
{
   HB_FUNC_EXEC( HB_TOKENCOUNT );
}

#endif
