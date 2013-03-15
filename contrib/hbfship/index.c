/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    IndexCount() -> <nIndexCount>
 *    IndexNames() -> <acIndexNames>
 * FlagShip compatible functions
 *
 * Copyright 2011 Przemyslaw Czerpak <druzus@acn.waw.pl>
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
#include "hbapirdd.h"

HB_FUNC( INDEXCOUNT )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmResult = hb_itemPutNI( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_ORDERCOUNT, &pOrderInfo );
      hb_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( INDEXNAMES )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      PHB_ITEM    pArray;
      int         iSize, i;

      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmResult = hb_itemPutNI( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_ORDERCOUNT, &pOrderInfo );
      iSize = hb_itemGetNI( pOrderInfo.itmResult );

      pArray = hb_itemArrayNew( iSize );
      for( i = 1; i <= iSize; ++i )
      {
         pOrderInfo.itmOrder  = hb_itemPutNI( pOrderInfo.itmOrder, i );
         pOrderInfo.itmResult = hb_itemPutC( pOrderInfo.itmResult, 0 );
         if( SELF_ORDINFO( pArea, DBOI_NAME, &pOrderInfo ) != HB_SUCCESS )
            break;
         hb_arraySet( pArray, i, pOrderInfo.itmResult );
      }
      hb_itemRelease( pOrderInfo.itmOrder );
      hb_itemRelease( pOrderInfo.itmResult );
      hb_itemReturnRelease( pArray );
   }
   else
      hb_reta( 0 );
}
