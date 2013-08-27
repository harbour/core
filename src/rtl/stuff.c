/*
 * Harbour Project source code:
 * Stuff() function
 *
 * Copyright 2012 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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
#include "hbapicdp.h"

/* replaces characters in a string */
HB_FUNC( STUFF )
{
   const char * szText = hb_parc( 1 );
   const char * szIns = hb_parc( 4 );

   if( szText && szIns && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      PHB_CODEPAGE cdp = hb_vmCDP();
      HB_SIZE nLen = hb_parclen( 1 );
      HB_SIZE nPos = hb_parns( 2 );
      HB_SIZE nDel = hb_parns( 3 );
      HB_SIZE nIns = hb_parclen( 4 );
      HB_SIZE nTot;

      if( HB_CDP_ISCHARIDX( cdp ) )
      {
         if( nPos )
            nPos = nPos < 1 ? nLen : hb_cdpTextPos( cdp, szText, nLen, nPos - 1 );
         if( nDel )
         {
            if( nPos < nLen )
            {
               nDel = hb_cdpTextPos( cdp, szText + nPos, nLen - nPos, nDel );
               if( nDel == 0 )
                  nDel = nLen - nPos;
            }
            else
               nDel = 0;
         }
      }
      else
      {
         if( nPos )
         {
            if( nPos < 1 || nPos > nLen )
               nPos = nLen;
            else
               nPos--;
         }
         if( nDel )
         {
            if( nDel < 1 || nDel > nLen - nPos )
               nDel = nLen - nPos;
         }
      }

      if( ( nTot = nLen + nIns - nDel ) > 0 )
      {
         char * szResult = ( char * ) hb_xgrab( nTot + 1 );

         hb_xmemcpy( szResult, szText, nPos );
         hb_xmemcpy( szResult + nPos, szIns, nIns );
         hb_xmemcpy( szResult + nPos + nIns, szText + nPos + nDel,
                     nLen - ( nPos + nDel ) );
         hb_retclen_buffer( szResult, nTot );
      }
      else
         hb_retc_null();
   }
   else
      hb_retc_null();
}
