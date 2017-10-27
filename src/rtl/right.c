/*
 * Right() function
 *
 * Copyright 2012 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
#include "hbapicdp.h"
#include "hbapierr.h"

/* returns the right-most n characters in string */

HB_FUNC( RIGHT )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText && HB_ISNUM( 2 ) )
   {
      HB_ISIZ nLen = hb_parns( 2 );
      if( nLen <= 0 )
         hb_retc_null();
      else
      {
         HB_SIZE nText = hb_itemGetCLen( pText );
         if( ( HB_SIZE ) nLen < nText )
         {
            PHB_CODEPAGE cdp = hb_vmCDP();
            if( HB_CDP_ISCHARIDX( cdp ) )
            {
               HB_SIZE nChars = hb_cdpTextLen( cdp, hb_itemGetCPtr( pText ), nText );
               if( nChars > ( HB_SIZE ) nLen )
                  nLen = nText - hb_cdpTextPos( cdp, hb_itemGetCPtr( pText ), nText, nChars - nLen );
               else
                  nLen = nText;
            }
         }
         if( ( HB_SIZE ) nLen >= nText )
            hb_itemReturn( pText );
         else
            hb_retclen( hb_itemGetCPtr( pText ) + nText - nLen, nLen );
      }
   }
   else
      hb_retc_null();  /* Clipper doesn't error */
}
