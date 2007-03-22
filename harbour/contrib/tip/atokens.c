/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    HB_ATOKENS()
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

HB_FUNC( HB_ATOKENS )
{
   char * szLine = hb_parc( 1 );

   if( szLine )
   {
      ULONG ulLen = hb_parclen( 1 ), ulDelim = hb_parclen( 2 ),
            ulTokens, ulStart, ul;
      char * szDelim, cQuote = 0;
      BOOL fSkipStrings = hb_parl( 3 );
      BOOL fDoubleQuoteOnly = hb_parl( 4 );
      PHB_ITEM pArray;

      if( ulDelim )
         szDelim = hb_parc( 2 );
      else
      {
         szDelim = " ";
         ulDelim = 1;
      }

      for( ul = ulStart = ulTokens = 0; ul < ulLen; ++ul )
      {
         if( cQuote )
         {
            if( szLine[ ul ] == cQuote )
               cQuote = 0;
         }
         else if( fSkipStrings && ( szLine[ ul ] == '"' ||
                  ( !fDoubleQuoteOnly && szLine[ ul ] == '\'' ) ) )
            cQuote = szLine[ ul ];
         else if( szLine[ ul ] == szDelim[ 0 ] &&
                  ( ulDelim == 1 || !memcmp( szLine + ul, szDelim, ulDelim ) ) )
         {
            ++ulTokens;
            ulStart = ul + 1;
         }
      }
      if( !cQuote && ulStart < ulLen )
         ++ulTokens;

      pArray = hb_itemArrayNew( ulTokens );
      for( ul = ulStart = ulTokens = 0; ul < ulLen; ++ul )
      {
         if( cQuote )
         {
            if( szLine[ ul ] == cQuote )
               cQuote = 0;
         }
         else if( fSkipStrings && ( szLine[ ul ] == '"' ||
                  ( !fDoubleQuoteOnly && szLine[ ul ] == '\'' ) ) )
            cQuote = szLine[ ul ];
         else if( szLine[ ul ] == szDelim[ 0 ] &&
                  ( ulDelim == 1 || !memcmp( szLine + ul, szDelim, ulDelim ) ) )
         {
            hb_itemPutCL( hb_arrayGetItemPtr( pArray, ++ulTokens ), szLine + ulStart, ul - ulStart );
            ulStart = ul + 1;
         }
      }
      if( !cQuote && ulStart < ulLen )
         hb_itemPutCL( hb_arrayGetItemPtr( pArray, ++ulTokens ), szLine + ulStart, ulLen - ulStart );

      hb_itemRelease( hb_itemReturn( pArray ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL, &hb_errFuncName, HB_ERR_ARGS_BASEPARAMS );
}
