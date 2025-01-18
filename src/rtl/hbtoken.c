/*
 * hb_token*() functions
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

#define _HB_TOK_QUOTE_MASK       0x07
#define _HB_TOK_RESPECT_DQUOTE   0x01
#define _HB_TOK_RESPECT_SQUOTE   0x02
#define _HB_TOK_RESPECT_BQUOTE   0x04
#define _HB_TOK_ISDELIM          0x08
#define _HB_TOK_EOL_DELIM        0x10
#define _HB_TOK_STRIP_QUOTE      0x20

static HB_SIZE hb_tokenCount( const char * szLine, HB_SIZE nLen,
                              const char * szDelim, HB_SIZE nDelim,
                              int iFlags )
{
   HB_SIZE nPos = 0, nTokens = 1;
   char cQuote = 0;

   while( nPos < nLen )
   {
      char ch = szLine[ nPos ];

      if( cQuote )
      {
         if( ch == cQuote )
            cQuote = 0;
      }
      else if( ( iFlags & _HB_TOK_QUOTE_MASK ) != 0 &&
               ( ( ch == '"' && ( iFlags & _HB_TOK_RESPECT_DQUOTE ) ) ||
                 ( ch == '\'' && ( iFlags & _HB_TOK_RESPECT_SQUOTE ) ) ||
                 ( ch == '`' && ( iFlags & _HB_TOK_RESPECT_BQUOTE ) ) ) )
         cQuote = ch;
      else if( ( iFlags & _HB_TOK_EOL_DELIM ) != 0 &&
               ( ch == '\n' || ch == '\r' ) )
      {
         ++nTokens;
         if( nPos + 1 < nLen && szLine[ nPos + 1 ] == ( char ) ( ch == '\n' ? '\r' : '\n' ) )
            ++nPos;
      }
      else if( nDelim && ch == szDelim[ 0 ] &&
               ( nDelim == 1 || ! memcmp( szLine + nPos, szDelim, nDelim ) ) )
      {
         ++nTokens;
         if( ( iFlags & _HB_TOK_ISDELIM ) == 0 )
         {
            while( nPos + 1 < nLen && szLine[ nPos + 1 ] == szDelim[ 0 ] )
               ++nPos;
         }
         nPos += nDelim - 1;
      }
      ++nPos;
   }

   return nTokens;
}

static const char * hb_tokenGet( const char * szLine, HB_SIZE nLen,
                                 const char * szDelim, HB_SIZE * pnDelim,
                                 int iFlags, HB_SIZE nToken, HB_SIZE * pnLen )
{
   HB_SIZE nPos, nStart, nDelim = *pnDelim;
   char cQuote = 0;

   for( nPos = nStart = 0; nPos < nLen; ++nPos )
   {
      char ch = szLine[ nPos ];

      if( cQuote )
      {
         if( ch == cQuote )
            cQuote = 0;
      }
      else if( ( iFlags & _HB_TOK_QUOTE_MASK ) != 0 &&
               ( ( ch == '"' && ( iFlags & _HB_TOK_RESPECT_DQUOTE ) ) ||
                 ( ch == '\'' && ( iFlags & _HB_TOK_RESPECT_SQUOTE ) ) ||
                 ( ch == '`' && ( iFlags & _HB_TOK_RESPECT_BQUOTE ) ) ) )
         cQuote = ch;
      else if( ( iFlags & _HB_TOK_EOL_DELIM ) != 0 &&
               ( ch == '\n' || ch == '\r' ) )
      {
         HB_SIZE nL = ( nPos + 1 < nLen &&
                        szLine[ nPos + 1 ] == ( char ) ( ch == '\n' ? '\r' : '\n' ) ) ? 1 : 0;
         if( --nToken == 0 )
         {
            *pnDelim = nL + 1;
            *pnLen = nPos - nStart;
            return szLine + nStart;
         }
         nPos += nL;
         nStart = nPos + 1;
      }
      else if( nDelim && ch == szDelim[ 0 ] &&
               ( nDelim == 1 || ! memcmp( szLine + nPos, szDelim, nDelim ) ) )
      {
         if( --nToken == 0 )
         {
            *pnLen = nPos - nStart;
            return szLine + nStart;
         }
         if( ( iFlags & _HB_TOK_ISDELIM ) == 0 )
         {
            while( nPos + 1 < nLen && szLine[ nPos + 1 ] == szDelim[ 0 ] )
               ++nPos;
         }
         nPos += nDelim - 1;
         nStart = nPos + 1;
      }
   }
   if( --nToken == 0 )
   {
      *pnLen = nPos - nStart;
      return szLine + nStart;
   }
   *pnLen = 0;
   return NULL;
}

static PHB_ITEM hb_tokenArray( const char * szLine, HB_SIZE nLen,
                               const char * szDelim, HB_SIZE nDelim,
                               int iFlags )
{
   HB_SIZE nTokens = hb_tokenCount( szLine, nLen, szDelim, nDelim, iFlags );
   PHB_ITEM pArray = hb_itemArrayNew( nTokens );

   if( nTokens )
   {
      HB_SIZE nPos, nStart, nToken;
      char cQuote = 0;

      for( nPos = nStart = nToken = 0; nPos < nLen; ++nPos )
      {
         char ch = szLine[ nPos ];

         if( cQuote )
         {
            if( ch == cQuote )
               cQuote = 0;
         }
         else if( ( iFlags & _HB_TOK_QUOTE_MASK ) != 0 &&
                  ( ( ch == '"' && ( iFlags & _HB_TOK_RESPECT_DQUOTE ) ) ||
                    ( ch == '\'' && ( iFlags & _HB_TOK_RESPECT_SQUOTE ) ) ||
                    ( ch == '`' && ( iFlags & _HB_TOK_RESPECT_BQUOTE ) ) ) )
            cQuote = ch;
         else if( ( iFlags & _HB_TOK_EOL_DELIM ) != 0 &&
                  ( ch == '\n' || ch == '\r' ) )
         {
            hb_arraySetCL( pArray, ++nToken, szLine + nStart, nPos - nStart );
            if( nPos + 1 < nLen && szLine[ nPos + 1 ] == ( char ) ( ch == '\n' ? '\r' : '\n' ) )
               ++nPos;
            nStart = nPos + 1;
         }
         else if( nDelim && ch == szDelim[ 0 ] &&
                  ( nDelim == 1 || ! memcmp( szLine + nPos, szDelim, nDelim ) ) )
         {
            hb_arraySetCL( pArray, ++nToken, szLine + nStart, nPos - nStart );
            if( ( iFlags & _HB_TOK_ISDELIM ) == 0 )
            {
               while( nPos + 1 < nLen && szLine[ nPos + 1 ] == szDelim[ 0 ] )
                  ++nPos;
            }
            nPos += nDelim - 1;
            nStart = nPos + 1;
         }
      }
      hb_arraySetCL( pArray, ++nToken, szLine + nStart, nPos - nStart );
   }

   return pArray;
}

static HB_BOOL hb_tokenParam( int iParam, HB_SIZE nSkip,
                              const char ** pszLine, HB_SIZE * pnLen,
                              const char ** pszDelim, HB_SIZE * pnDelim,
                              int * piFlags )
{
   const char * szLine = hb_parc( 1 ), * szDelim = NULL;
   HB_SIZE nLen = hb_parclen( 1 ), nDelim = 0;
   int iFlags = 0;

   if( nLen )
   {
      if( nSkip )
      {
         szLine += nSkip;
         if( nLen <= nSkip )
            nLen = 0;
         else
            nLen -= nSkip;
      }

      nDelim = hb_parclen( iParam );
      if( nDelim )
      {
         szDelim = hb_parc( iParam );
         iFlags |= _HB_TOK_ISDELIM;
      }
      else if( hb_parl( iParam ) )
      {
         iFlags |= _HB_TOK_EOL_DELIM;
      }
      else
      {
         szDelim = " ";
         nDelim = 1;
      }

      if( nDelim && ( iFlags & _HB_TOK_ISDELIM ) == 0 )
      {
         while( nLen && *szLine == szDelim[ 0 ] )
         {
            ++szLine;
            --nLen;
         }
         while( nLen && szLine[ nLen - 1 ] == szDelim[ 0 ] )
            --nLen;
      }
      if( hb_parl( iParam + 1 ) )
      {
         iFlags |= _HB_TOK_RESPECT_DQUOTE | _HB_TOK_RESPECT_SQUOTE;
         if( hb_parl( iParam + 2 ) )
            iFlags &= ~_HB_TOK_RESPECT_SQUOTE;
      }
      else
         iFlags |= hb_parni( iParam + 1 );
   }

   *pnLen = nLen;
   *pnDelim = nDelim;
   *pszLine = szLine;
   *pszDelim = szDelim;
   *piFlags = iFlags;

   return szLine != NULL;
}

HB_FUNC( HB_TOKENCOUNT )
{
   const char * szLine, * szDelim;
   HB_SIZE nLen, nDelim;
   int iFlags;

   if( hb_tokenParam( 2, 0, &szLine, &nLen, &szDelim, &nDelim, &iFlags ) )
      hb_retns( hb_tokenCount( szLine, nLen, szDelim, nDelim, iFlags ) );
   else
      hb_retns( 0 );
}

HB_FUNC( HB_TOKENGET )
{
   const char * szLine, * szDelim;
   HB_SIZE nLen, nDelim;
   int iFlags;

   if( hb_tokenParam( 3, 0, &szLine, &nLen, &szDelim, &nDelim, &iFlags ) )
   {
      szLine = hb_tokenGet( szLine, nLen, szDelim, &nDelim, iFlags,
                            hb_parns( 2 ), &nLen );
      hb_retclen( szLine, nLen );
   }
   else
      hb_retc_null();
}

/* like hb_tokenGet() but returns next token starting from passed position
 * (0 based) inside string, f.e.:
 *    hb_tokenPtr( cString, @nTokPos, Chr( 9 ) ) --> cToken
 */
HB_FUNC( HB_TOKENPTR )
{
   const char * szLine, * szDelim;
   HB_SIZE nLen, nDelim;
   int iFlags;

   if( hb_tokenParam( 3, hb_parns( 2 ), &szLine, &nLen, &szDelim, &nDelim, &iFlags ) )
   {
      const char * szToken;
      HB_SIZE nSkip, nToken;

      szToken = hb_tokenGet( szLine, nLen, szDelim, &nDelim, iFlags,
                             1, &nToken );
      if( szToken && nLen > nToken )
         nSkip = szToken - hb_parc( 1 ) + nToken + nDelim;
      else
         nSkip = hb_parclen( 1 ) + 1;

      /* return position to start next search from */
      hb_storns( nSkip, 2 );
      /* return token */
      hb_retclen( szToken, nToken );
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
   HB_SIZE nLen, nDelim;
   int iFlags;

   if( hb_tokenParam( 2, 0, &szLine, &nLen, &szDelim, &nDelim, &iFlags ) )
      hb_itemReturnRelease( hb_tokenArray( szLine, nLen, szDelim, nDelim, iFlags ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
