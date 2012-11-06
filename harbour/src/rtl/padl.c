/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * PADL() function
 *
 * Copyright 2012 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * Copyright 1999 Matthew Hamilton <mhamilton@bunge.com.au>
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
#include "hbapicdp.h"
#include "hbapierr.h"

static HB_SIZE hb_cdpItemLen( PHB_CODEPAGE cdp, PHB_ITEM pItem )
{
   HB_SIZE nLen = hb_itemGetCLen( pItem );

   return nLen && HB_CDP_ISCHARIDX( cdp ) ?
          hb_cdpTextLen( cdp, hb_itemGetCPtr( pItem ), nLen ) : nLen;
}

static const char * s_hb_padGet( PHB_CODEPAGE cdp, HB_SIZE * pnPad )
{
   const char * szPad = hb_parc( 3 );

   *pnPad = 1;
   if( szPad == NULL )
      szPad = " ";
   else if( HB_CDP_ISCHARIDX( cdp ) )
   {
      *pnPad = hb_cdpTextPos( cdp, szPad, hb_parclen( 3 ), 1 );
      if( *pnPad == 0 )
         szPad = "";
   }
   return szPad;
}

/* left-pads a date, number, or string with spaces or supplied character */
HB_FUNC( PADL )
{
   HB_SIZE nSize;
   HB_BOOL bFreeReq;
   char * szText;
   HB_ISIZ nLen = hb_parns( 2 );

   if( nLen > 0 )
   {
      PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );
      PHB_CODEPAGE cdp = hb_vmCDP();

      if( pItem && HB_IS_STRING( pItem ) &&
          ( HB_SIZE ) nLen == hb_cdpItemLen( cdp, pItem ) )
      {
         hb_itemReturn( pItem );
      }
      else
      {
         szText = hb_itemPadConv( pItem, &nSize, &bFreeReq );
         if( szText )
         {
            if( HB_CDP_ISCHARIDX( cdp ) )
            {
               HB_SIZE nText = nLen;
               nLen = hb_cdpTextPosEx( cdp, szText, nSize, &nText );
               nLen += nText;
            }

            if( ( HB_SIZE ) nLen > nSize )
            {
               HB_SIZE nPad = 0;
               const char * szPad = s_hb_padGet( cdp, &nPad );
               char * szResult;

               if( nPad > 1 )
               {
                  HB_SIZE nRep = ( ( HB_SIZE ) nLen - nSize ), nPos = 0;
                  nLen += nRep * ( nPad - 1 );
                  szResult = ( char * ) hb_xgrab( nLen + 1 );
                  while( nRep-- )
                  {
                     hb_xmemcpy( szResult + nPos, szPad, nPad );
                     nPos += nPad;
                  }
                  hb_xmemcpy( szResult + nPos, szText, nSize );
               }
               else
               {
                  szResult = ( char * ) hb_xgrab( nLen + 1 );
                  hb_xmemset( szResult, szPad[ 0 ], ( HB_SIZE ) nLen - nSize );
                  hb_xmemcpy( szResult + ( HB_SIZE ) nLen - nSize, szText, nSize );
               }

               hb_retclen_buffer( szResult, ( HB_SIZE ) nLen );
               if( bFreeReq )
                  hb_xfree( szText );
            }
            else
            {
               if( bFreeReq )
                  hb_retclen_buffer( szText, ( HB_SIZE ) nLen );
               else
                  hb_retclen( szText, nLen );
            }
         }
         else
            hb_retc_null();
      }
   }
   else
      hb_retc_null();
}
