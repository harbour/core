/*
 * Harbour Project source code:
 * Left() function
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
#include "hbapiitm.h"
#include "hbapicdp.h"
#include "hbapierr.h"

/* returns the left-most n characters in string */

HB_FUNC( LEFT )
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
               nLen = hb_cdpTextPos( cdp, hb_itemGetCPtr( pText ), nText, nLen );
         }
         if( ( HB_SIZE ) nLen >= nText )
            hb_itemReturn( pText );
         else
            hb_retclen( hb_itemGetCPtr( pText ), nLen );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1124, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_LEFTIS )
{
   PHB_ITEM pItem1 = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pItem2 = hb_param( 2, HB_IT_STRING );

   if( pItem1 && pItem2 )
      hb_retl( hb_cdpcmp( hb_itemGetCPtr( pItem1 ), hb_itemGetCLen( pItem1 ),
                          hb_itemGetCPtr( pItem2 ), hb_itemGetCLen( pItem2 ), hb_vmCDP(), HB_FALSE ) == 0 );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1071, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_LEFTISI )
{
   PHB_ITEM pItem1 = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pItem2 = hb_param( 2, HB_IT_STRING );

   if( pItem1 && pItem2 )
      hb_retl( hb_cdpicmp( hb_itemGetCPtr( pItem1 ), hb_itemGetCLen( pItem1 ),
                           hb_itemGetCPtr( pItem2 ), hb_itemGetCLen( pItem2 ), hb_vmCDP(), HB_FALSE ) == 0 );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1071, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
