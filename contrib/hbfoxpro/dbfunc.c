/*
 * Harbour Project source code:
 * FoxPro compatible database functions.
 *
 * Copyright 2013 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#include "hbapierr.h"
#include "hbset.h"

static AREAP s_foxAreaPointer( int iParam )
{
   if( HB_ISNIL( iParam ) )
      return ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   else
   {
      const char * szAlias = hb_parc( iParam );
      int iArea;

      if( szAlias )
         hb_rddGetAliasNumber( szAlias, &iArea );
      else
         iArea = hb_parni( iParam );

      return ( AREAP ) hb_rddGetWorkAreaPointer( iArea );
   }
}

HB_FUNC( FILTER )
{
   AREAP pArea = s_foxAreaPointer( 1 );

   if( pArea )
   {
      PHB_ITEM pFilter = hb_itemPutC( NULL, NULL );
      SELF_FILTERTEXT( pArea, pFilter );
      hb_itemReturnRelease( pFilter );
   }
   else
      hb_retc_null();
}

HB_FUNC( NDX )
{
   AREAP pArea = s_foxAreaPointer( 2 );

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      if( hb_itemGetNI( pOrderInfo.itmOrder ) == 0 )
         pOrderInfo.itmOrder = NULL;
      pOrderInfo.itmResult   = hb_itemPutC( NULL, NULL );
      SELF_ORDINFO( pArea, DBOI_NAME, &pOrderInfo );
      hb_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      hb_retc_null();
}

HB_FUNC( RELATION )
{
   AREAP pArea = s_foxAreaPointer( 2 );

   if( pArea )
   {
      PHB_ITEM pRelExpr = hb_itemPutC( NULL, NULL );
      HB_USHORT uiRelNo = ( HB_USHORT ) hb_parni( 1 );
      SELF_RELTEXT( pArea, uiRelNo ? uiRelNo : 1, pRelExpr );
      hb_itemReturnRelease( pRelExpr );
   }
   else
      hb_retc_null();
}

HB_FUNC( FSIZE )
{
   AREAP pArea = s_foxAreaPointer( 2 );

   if( pArea )
   {
      HB_FIELDNO uiIndex;
      const char * szField;

      if( HB_ISNIL( 1 ) )
         uiIndex = 1;
      else if( ( szField = hb_parc( 1 ) ) != NULL )
         uiIndex = hb_rddFieldIndex( pArea, szField );
      else
         uiIndex = ( HB_FIELDNO ) hb_parni( 1 );

      if( uiIndex > 0 )
      {
         PHB_ITEM pItem = hb_itemNew( NULL );

         if( SELF_FIELDINFO( pArea, uiIndex, DBS_LEN, pItem ) == HB_SUCCESS )
         {
            hb_itemReturnRelease( pItem );
            return;
         }
         hb_itemRelease( pItem );
      }
   }

   hb_retni( 0 );
}

HB_FUNC( __FOX_USED )
{
   hb_retl( s_foxAreaPointer( 1 ) != NULL );
}

HB_FUNC( __FOX_SEEK )
{
   AREAP pArea = s_foxAreaPointer( 4 );

   if( pArea )
   {
      if( ! HB_ISNIL( 1 ) )
      {
         PHB_ITEM pKey = hb_param( 1, HB_IT_ANY );
         HB_BOOL fSoftSeek = HB_ISLOG( 2 ) ? ( HB_BOOL ) hb_parl( 2 ) : hb_setGetSoftSeek();
         HB_BOOL fFindLast = hb_parl( 3 ), fFound = HB_FALSE;
         PHB_ITEM pTag = hb_param( 5, HB_IT_NUMERIC | HB_IT_STRING );
         HB_ERRCODE errCode = HB_SUCCESS;

         if( pTag )
         {
            DBORDERINFO pInfo;
            memset( &pInfo, 0, sizeof( pInfo ) );
            pInfo.itmOrder = pTag;
            pInfo.itmResult = hb_itemNew( NULL );
            errCode = SELF_ORDLSTFOCUS( pArea, &pInfo );
            hb_itemRelease( pInfo.itmResult );
         }

         if( errCode == HB_SUCCESS )
         {
            if( SELF_SEEK( pArea, fSoftSeek, pKey, fFindLast ) == HB_SUCCESS )
            {
               if( SELF_FOUND( pArea, &fFound ) != HB_SUCCESS )
                  fFound = HB_FALSE;
            }
         }

         hb_retl( fFound );
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_SEEK_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}
