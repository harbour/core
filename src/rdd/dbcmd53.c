/*
 * Harbour Project source code:
 * Base RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 * Copyright 2004-2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

/*
 * The following functions are added by
 *       Horacio Roldan <harbour_ar@yahoo.com.ar>
 *
 * ordKeyVal()
 * ordKeyAdd()
 * ordKeyDel()
 *
 */

#include "hbapi.h"
#include "hbapirdd.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbset.h"

#ifdef HB_COMPAT_C53

HB_FUNC( ORDKEYCOUNT )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING | HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* Either or both may be NIL */

      pOrderInfo.itmResult = hb_itemPutNL( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_KEYCOUNT, &pOrderInfo );
      hb_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );

}

HB_FUNC( ORDKEYNO )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING | HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* Either or both may be NIL */
      pOrderInfo.itmNewVal = NULL;
      pOrderInfo.itmResult = hb_itemPutNL( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_POSITION, &pOrderInfo );
      hb_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ORDKEYGOTO )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmNewVal = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.itmResult = hb_itemPutL( NULL, HB_FALSE );
      SELF_ORDINFO( pArea, DBOI_POSITION, &pOrderInfo );
      hb_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ORDKEYRELPOS )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmNewVal = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.itmResult = hb_itemPutNI( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_RELKEYPOS, &pOrderInfo );
      hb_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ORDFINDREC )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmNewVal = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.itmResult = hb_itemPutL( NULL, HB_FALSE );
      SELF_ORDINFO( pArea, hb_parl( 2 ) ? DBOI_FINDRECCONT :
                                          DBOI_FINDREC, &pOrderInfo );
      hb_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ORDSKIPRAW )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_SKIPRAW( pArea, hb_parnldef( 1, 1 ) );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}


HB_FUNC( ORDSKIPUNIQUE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmNewVal = hb_param( 1, HB_IT_ANY );
      pOrderInfo.itmResult = hb_itemPutL( NULL, HB_FALSE );
      SELF_ORDINFO( pArea, DBOI_SKIPUNIQUE, &pOrderInfo );
      hb_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ORDKEYVAL )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmResult = hb_itemNew( NULL );
      SELF_ORDINFO( pArea, DBOI_KEYVAL, &pOrderInfo );
      hb_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ORDKEYADD )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING | HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* Either or both may be NIL */
      pOrderInfo.itmNewVal = hb_param( 3, HB_IT_ANY );
      pOrderInfo.itmResult = hb_itemPutNL( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_KEYADD, &pOrderInfo );
      hb_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ORDKEYDEL )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING | HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* Either or both may be NIL */
      pOrderInfo.itmNewVal = hb_param( 3, HB_IT_ANY );
      pOrderInfo.itmResult = hb_itemPutNL( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_KEYDELETE, &pOrderInfo );
      hb_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ORDDESCEND )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING | HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* Either or both may be NIL */
      pOrderInfo.itmNewVal = hb_param( 3, HB_IT_LOGICAL );
      pOrderInfo.itmResult = hb_itemPutL( NULL, HB_FALSE );
      SELF_ORDINFO( pArea, DBOI_ISDESC, &pOrderInfo );
      hb_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ORDISUNIQUE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING | HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* Harbour extension: NewVal to set/reset unique flag */
      pOrderInfo.itmNewVal = hb_param( 3, HB_IT_LOGICAL );
      pOrderInfo.itmResult = hb_itemPutL( NULL, HB_FALSE );
      SELF_ORDINFO( pArea, DBOI_UNIQUE, &pOrderInfo );
      hb_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ORDCUSTOM )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING | HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* Either or both may be NIL */
      pOrderInfo.itmNewVal = hb_param( 3, HB_IT_LOGICAL );
      pOrderInfo.itmResult = hb_itemPutL( NULL, HB_FALSE );
      SELF_ORDINFO( pArea, DBOI_CUSTOM, &pOrderInfo );
      hb_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( DBINFO )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PHB_ITEM pIndex;

      pIndex = hb_param( 1, HB_IT_NUMERIC );
      if( pIndex )
      {
         PHB_ITEM pInfo = hb_itemNew( hb_param( 2, HB_IT_ANY ) );

         SELF_INFO( pArea, ( HB_USHORT ) hb_itemGetNI( pIndex ), pInfo );
         hb_itemReturnRelease( pInfo );
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBINFOBADPARAMETER, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( DBORDERINFO )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PHB_ITEM pType = hb_param( 1, HB_IT_NUMERIC );
      if( pType )
      {
         DBORDERINFO pOrderInfo;

         /* atomBagName may be NIL */
         pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING | HB_IT_NUMERIC );
         pOrderInfo.itmOrder = hb_param( 3, HB_IT_STRING | HB_IT_NUMERIC );

         pOrderInfo.itmNewVal  = hb_param( 4, HB_IT_ANY );
         pOrderInfo.itmResult  = hb_itemNew( NULL );
         pOrderInfo.itmCobExpr = NULL;
         pOrderInfo.fAllTags   = HB_FALSE;
         SELF_ORDINFO( pArea, ( HB_USHORT ) hb_itemGetNI( pType ), &pOrderInfo );
         hb_itemReturnRelease( pOrderInfo.itmResult );
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( DBFIELDINFO )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      HB_USHORT uiFields, uiIndex;
      PHB_ITEM pType;

      pType = hb_param( 1, HB_IT_NUMERIC );
      uiIndex = ( HB_FIELDNO ) hb_parni( 2 );
      if( pType && SELF_FIELDCOUNT( pArea, &uiFields ) == HB_SUCCESS &&
          uiIndex > 0 && uiIndex <= uiFields )
      {
         PHB_ITEM pInfo = hb_itemNew( hb_param( 3, HB_IT_ANY ) );

         SELF_FIELDINFO( pArea, uiIndex, ( HB_USHORT ) hb_itemGetNI( pType ), pInfo );
         hb_itemReturnRelease( pInfo );
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( DBRECORDINFO )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PHB_ITEM pType, pRecNo;

      pType = hb_param( 1, HB_IT_NUMERIC );
      pRecNo = hb_param( 2, HB_IT_ANY );
      if( pType )
      {
         PHB_ITEM pInfo = hb_itemNew( hb_param( 3, HB_IT_ANY ) );

         SELF_RECINFO( pArea, pRecNo, ( HB_USHORT ) hb_itemGetNI( pType ), pInfo );
         hb_itemReturnRelease( pInfo );
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_INFOBADPARAMETER, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

/*
 * DBFILEPUT/BLOB2FILE - retrieve memo contents into file
 */
HB_FUNC( DBFILEGET )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      HB_USHORT uiFields, uiIndex;
      PHB_ITEM pMode;
      const char * szField = hb_parc( 1 );

      if( szField )
         uiIndex = hb_rddFieldIndex( pArea, szField );
      else
         uiIndex = ( HB_FIELDNO ) hb_parni( 1 );

      pMode = hb_param( 3, HB_IT_NUMERIC );
      if( uiIndex > 0 && pMode && hb_parclen( 2 ) > 0 &&
          SELF_FIELDCOUNT( pArea, &uiFields ) == HB_SUCCESS &&
          uiIndex <= uiFields )
      {
         hb_retl( SELF_GETVALUEFILE( pArea, uiIndex, hb_parc( 2 ),
                                     ( HB_USHORT ) hb_itemGetNI( pMode ) ) == HB_SUCCESS );
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBFILEGETBADPARAMETER, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

/*
 * DBFILEPUT/FILE2BLOB - store file contents in MEMO
 */
HB_FUNC( DBFILEPUT )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      HB_USHORT uiFields, uiIndex;
      const char * szField = hb_parc( 1 );

      if( szField )
         uiIndex = hb_rddFieldIndex( pArea, szField );
      else
         uiIndex = ( HB_FIELDNO ) hb_parni( 1 );
      if( uiIndex > 0 && hb_parclen( 2 ) > 0 &&
          SELF_FIELDCOUNT( pArea, &uiFields ) == HB_SUCCESS &&
          uiIndex <= uiFields )
      {
         hb_retl( SELF_PUTVALUEFILE( pArea, uiIndex, hb_parc( 2 ),
                                     ( HB_USHORT ) hb_parni( 3 ) ) == HB_SUCCESS );
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBFILEPUTBADPARAMETER, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

#endif
