/*
 * CT3 string function: CharSpread()
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

HB_FUNC( CHARSPREAD )
{
   HB_SIZE nLen = hb_parclen( 1 );

   if( nLen == 0 )
      hb_retc_null();
   else
   {
      HB_ISIZ nSize = hb_parns( 2 );

      if( nSize < 0 || ( HB_SIZE ) nSize <= nLen )
         hb_itemReturn( hb_param( 1, HB_IT_ANY ) );
      else
      {
         const char * szText = hb_parc( 1 );
         char cDelim = ' ';
         HB_ISIZ nTokens = 0;
         HB_SIZE nPos;

         if( HB_ISCHAR( 3 ) )
            cDelim = hb_parc( 3 )[ 0 ];
         else if( HB_ISNUM( 3 ) )
            cDelim = ( char ) hb_parni( 3 );

         for( nPos = 0; nPos < nLen; ++nPos )
         {
            if( szText[ nPos ] == cDelim )
            {
               nTokens++;
               while( nPos + 1 < nLen && szText[ nPos + 1 ] == cDelim )
                  ++nPos;
            }
         }

         if( nTokens == 0 )
            hb_itemReturn( hb_param( 1, HB_IT_ANY ) );
         else
         {
            HB_ISIZ iRepl, iRest, iFirst;
            HB_SIZE nDst, nRest;
            char * szDest;

            nRest = ( HB_SIZE ) nSize - nLen;
            iRepl = nRest / nTokens;
            iRest = nRest % nTokens;
            iFirst = ( iRest + 1 ) >> 1;
            iRest >>= 1;
            szDest = ( char * ) hb_xgrab( nSize + 1 );
            for( nDst = nPos = 0; nPos < nLen; ++nPos )
            {
               szDest[ nDst++ ] = szText[ nPos ];
               if( szText[ nPos ] == cDelim )
               {
                  HB_ISIZ i;

                  while( nPos + 1 < nLen && szText[ nPos + 1 ] == cDelim )
                     szDest[ nDst++ ] = szText[ ++nPos ];
                  i = iRepl;
                  if( iFirst )
                  {
                     --iFirst;
                     ++i;
                  }
                  else if( nTokens <= iRest )
                     ++i;
                  while( --i >= 0 )
                     szDest[ nDst++ ] = cDelim;
                  nTokens--;
               }
            }
            hb_retclen_buffer( szDest, nSize );
         }
      }
   }
}
