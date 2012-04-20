/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SUBSTR() function
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

/* returns l characters from n characters into string */

HB_FUNC( SUBSTR )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );
   int iPCount = hb_pcount();

   if( pText && HB_ISNUM( 2 ) && ( iPCount < 3 || HB_ISNUM( 3 ) ) )
   {
      PHB_CODEPAGE cdp = hb_vmCDP();
      const char * pszText = hb_itemGetCPtr( pText );
      HB_ISIZ nSize = hb_itemGetCLen( pText );
      HB_ISIZ nFrom = hb_parns( 2 );
      HB_ISIZ nCount = iPCount < 3 ? nSize : hb_parns( 3 );

      if( nFrom > 0 )
      {
         if( --nFrom > nSize )
            nCount = 0;
      }

      if( nCount > 0 )
      {
         if( HB_CDP_ISCHARIDX( cdp ) )
         {
            if( nFrom < 0 )
               nFrom += hb_cdpTextLen( cdp, pszText, nSize );
            if( nFrom > 0 )
            {
               nFrom = hb_cdpTextPos( cdp, pszText, nSize, nFrom );
               pszText += nFrom;
               nSize -= nFrom;
            }
            nCount = hb_cdpTextPos( cdp, pszText, nSize, nCount );
         }
         else
         {
            if( nFrom < 0 )
               nFrom += nSize;
            if( nFrom > 0 )
            {
               pszText += nFrom;
               nSize -= nFrom;
            }
            if( nCount > nSize )
               nCount = nSize;
         }
      }

      if( nCount > 0 )
      {
         if( nFrom <= 0 && nCount == nSize )
            hb_itemReturn( pText );
         else
            hb_retclen( pszText, nCount );
      }
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1110, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
