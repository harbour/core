/*
 * $Id$
 */

/*
 * SQL Base Database Driver
 *
 * Copyright 2007 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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
#include "hbvm.h"
#include "hbset.h"
#include "hbrddsql.h"
#include "rddsys.ch"

#include "hbtrace.h"

#define SUPERTABLE              ( &sqlbaseSuper )

#define CONNECTION_LIST_EXPAND  4

static HB_USHORT s_rddidSQLBASE = 0;

static SQLDDCONNECTION ** s_pConnection = NULL;
static HB_ULONG s_ulConnectionCount     = 0;
static HB_ULONG s_ulConnectionCurrent   = 0;

static char *     s_szError = NULL;
static HB_ERRCODE s_errCode = 0;

static char *        s_szQuery        = NULL;
static PHB_ITEM      s_pItemNewID     = NULL;
static unsigned long s_ulAffectedRows = 0;

static RDDFUNCS sqlbaseSuper;


void hb_rddsqlSetError( HB_ERRCODE errCode, const char * szError, const char * szQuery, PHB_ITEM pItem, unsigned long ulAffectedRows )
{
   s_errCode = errCode;

   if( s_szError )
   {
      hb_xfree( s_szError );
      s_szError = NULL;
   }
   if( szError )
      s_szError = hb_strdup( szError );

   if( s_szQuery )
   {
      hb_xfree( s_szQuery );
      s_szQuery = NULL;
   }
   if( szQuery )
      s_szQuery = hb_strdup( szQuery );

   if( pItem )
      hb_itemCopy( s_pItemNewID, pItem );
   else
      hb_itemClear( s_pItemNewID );

   s_ulAffectedRows = ulAffectedRows;
}


static HB_ERRCODE hb_errRT_SQLBASE( HB_ERRCODE errGenCode, HB_ERRCODE errSubCode, const char * szDescription, const char * szOperation )
{
   PHB_ITEM   pError;
   HB_ERRCODE iRet = HB_FAILURE;

   if( hb_vmRequestQuery() == 0 )
   {
      pError = hb_errRT_New( ES_ERROR, "SQLBASE", errGenCode, errSubCode, szDescription, szOperation, 0, EF_NONE );
      iRet   = hb_errLaunch( pError );
      hb_itemRelease( pError );
   }
   return iRet;
}


/*================ NULL SDD ==========================================================*/

static HB_ERRCODE sddConnect( SQLDDCONNECTION * pConnection, PHB_ITEM pItem );
static HB_ERRCODE sddDisconnect( SQLDDCONNECTION * pConnection );
static HB_ERRCODE sddExecute( SQLDDCONNECTION * pConnection, PHB_ITEM pItem );
static HB_ERRCODE sddOpen( SQLBASEAREAP pArea );
static HB_ERRCODE sddClose( SQLBASEAREAP pArea );
static HB_ERRCODE sddGoTo( SQLBASEAREAP pArea, HB_ULONG ulRecNo );
static HB_ERRCODE sddGetValue( SQLBASEAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem );
static HB_ERRCODE sddGetVarLen( SQLBASEAREAP pArea, HB_USHORT uiIndex, HB_ULONG * pLength );


static SDDNODE sddNull = {
   NULL,
   "NULL",
   ( SDDFUNC_CONNECT ) sddConnect,
   ( SDDFUNC_DISCONNECT ) sddDisconnect,
   ( SDDFUNC_EXECUTE ) sddExecute,
   ( SDDFUNC_OPEN ) sddOpen,
   ( SDDFUNC_CLOSE ) sddClose,
   ( SDDFUNC_GOTO ) sddGoTo,
   ( SDDFUNC_GETVALUE ) sddGetValue,
   ( SDDFUNC_GETVARLEN ) sddGetVarLen
};


static HB_ERRCODE sddConnect( SQLDDCONNECTION * pConnection, PHB_ITEM pItem )
{
   HB_SYMBOL_UNUSED( pConnection );
   HB_SYMBOL_UNUSED( pItem );
   hb_errRT_SQLBASE( EG_UNSUPPORTED, ESQLDD_NULLSDD, NULL, NULL );
   return HB_FAILURE;
}


static HB_ERRCODE sddDisconnect( SQLDDCONNECTION * pConnection )
{
   HB_SYMBOL_UNUSED( pConnection );
   hb_errRT_SQLBASE( EG_UNSUPPORTED, ESQLDD_NULLSDD, NULL, NULL );
   return HB_FAILURE;
}


static HB_ERRCODE sddExecute( SQLDDCONNECTION * pConnection, PHB_ITEM pItem )
{
   HB_SYMBOL_UNUSED( pConnection );
   HB_SYMBOL_UNUSED( pItem );
   hb_errRT_SQLBASE( EG_UNSUPPORTED, ESQLDD_NULLSDD, NULL, NULL );
   return HB_FAILURE;
}


static HB_ERRCODE sddOpen( SQLBASEAREAP pArea )
{
   HB_SYMBOL_UNUSED( pArea );
   hb_errRT_SQLBASE( EG_UNSUPPORTED, ESQLDD_NULLSDD, NULL, NULL );
   return HB_FAILURE;
}


static HB_ERRCODE sddClose( SQLBASEAREAP pArea )
{
   HB_SYMBOL_UNUSED( pArea );
   return HB_SUCCESS;
}


static HB_ERRCODE sddGoTo( SQLBASEAREAP pArea, HB_ULONG ulRecNo )
{
   if( ulRecNo == 0 || ulRecNo > pArea->ulRecCount )
   {
      pArea->pRecord      = pArea->pRow[ 0 ];
      pArea->bRecordFlags = pArea->pRowFlags[ 0 ];

      pArea->fPositioned = HB_FALSE;
   }
   else
   {
      pArea->pRecord      = pArea->pRow[ ulRecNo ];
      pArea->bRecordFlags = pArea->pRowFlags[ ulRecNo ];

      pArea->fPositioned = HB_TRUE;
   }
   return HB_SUCCESS;
}


static HB_ERRCODE sddGetValue( SQLBASEAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
{
   HB_SYMBOL_UNUSED( pArea );
   HB_SYMBOL_UNUSED( uiIndex );
   HB_SYMBOL_UNUSED( pItem );
   hb_errRT_SQLBASE( EG_UNSUPPORTED, ESQLDD_NULLSDD, NULL, NULL );
   return HB_FAILURE;
}


static HB_ERRCODE sddGetVarLen( SQLBASEAREAP pArea, HB_USHORT uiIndex, HB_ULONG * pLength )
{
   HB_SYMBOL_UNUSED( pArea );
   HB_SYMBOL_UNUSED( uiIndex );
   HB_SYMBOL_UNUSED( pLength );
   hb_errRT_SQLBASE( EG_UNSUPPORTED, ESQLDD_NULLSDD, NULL, NULL );
   return HB_SUCCESS;
}


/*==================== SDD registration =====================================*/

static PSDDNODE s_pSdd = NULL;


int hb_sddRegister( PSDDNODE pSdd )
{
   PSDDNODE pNode = s_pSdd;

   /* "Inheritance" from NULL SDD */
   if( pSdd->Connect == NULL )
      pSdd->Connect = sddNull.Connect;
   if( pSdd->Disconnect == NULL )
      pSdd->Disconnect = sddNull.Disconnect;
   if( pSdd->Execute == NULL )
      pSdd->Execute = sddNull.Execute;
   if( pSdd->Open == NULL )
      pSdd->Open = sddNull.Open;
   if( pSdd->Close == NULL )
      pSdd->Close = sddNull.Close;
   if( pSdd->GoTo == NULL )
      pSdd->GoTo = sddNull.GoTo;
   if( pSdd->GetValue == NULL )
      pSdd->GetValue = sddNull.GetValue;
   if( pSdd->GetVarLen == NULL )
      pSdd->GetVarLen = sddNull.GetVarLen;

   while( pNode )
   {
      if( ! hb_stricmp( pNode->Name, pSdd->Name ) )
         return 0;
      pNode = pNode->pNext;
   }
   pSdd->pNext = s_pSdd;
   s_pSdd      = pSdd;
   return 1;
}


/*============= RDD METHODS =============================================================*/

static HB_ERRCODE sqlbaseGoBottom( SQLBASEAREAP pArea )
{
   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;


   if( ! pArea->fFetched && pArea->pSDD->GoTo( pArea, ( HB_ULONG ) -1 ) == HB_FAILURE )
      return HB_FAILURE;

   pArea->area.fTop    = HB_FALSE;
   pArea->area.fBottom = HB_TRUE;

   if( SELF_GOTO( ( AREAP ) pArea, pArea->ulRecCount ) != HB_SUCCESS )
      return HB_FAILURE;

   return SELF_SKIPFILTER( ( AREAP ) pArea, -1 );
}


static HB_ERRCODE sqlbaseGoTo( SQLBASEAREAP pArea, HB_ULONG ulRecNo )
{
   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   if( pArea->pSDD->GoTo( pArea, ulRecNo ) == HB_FAILURE )
      return HB_FAILURE;

   if( pArea->fPositioned )
   {
      pArea->ulRecNo   = ulRecNo;
      pArea->area.fBof = pArea->area.fEof = HB_FALSE;
   }
   else
   {
      pArea->ulRecNo   = pArea->ulRecCount + 1;
      pArea->area.fBof = pArea->area.fEof = HB_TRUE;
   }
   pArea->area.fFound = HB_FALSE;

   return HB_SUCCESS;
}


static HB_ERRCODE sqlbaseGoToId( SQLBASEAREAP pArea, PHB_ITEM pItem )
{
   PHB_ITEM pError;

   if( HB_IS_NUMERIC( pItem ) )
      return SELF_GOTO( ( AREAP ) pArea, hb_itemGetNL( pItem ) );
   else
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_DATATYPE );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_DATATYPE ) );
      hb_errPutSubCode( pError, EDBF_DATATYPE );
      SELF_ERROR( ( AREAP ) pArea, pError );
      hb_itemRelease( pError );
      return HB_FAILURE;
   }
}


static HB_ERRCODE sqlbaseGoTop( SQLBASEAREAP pArea )
{
   pArea->area.fTop    = HB_TRUE;
   pArea->area.fBottom = HB_FALSE;

   if( SELF_GOTO( ( AREAP ) pArea, 1 ) == HB_FAILURE )
      return HB_FAILURE;

   return SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
}


static HB_ERRCODE sqlbaseSkip( SQLBASEAREAP pArea, HB_LONG lToSkip )
{
   HB_ERRCODE errCode;

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   pArea->area.fTop = pArea->area.fBottom = HB_FALSE;

   if( lToSkip == 0 || hb_setGetDeleted() ||
       pArea->area.dbfi.itmCobExpr || pArea->area.dbfi.fFilter )
      return SUPER_SKIP( ( AREAP ) pArea, lToSkip );

   errCode = SELF_SKIPRAW( ( AREAP ) pArea, lToSkip );

   /* Move first record and set Bof flag */
   if( errCode == HB_SUCCESS && pArea->area.fBof && lToSkip < 0 )
   {
      errCode = SELF_GOTOP( ( AREAP ) pArea );
      pArea->area.fBof = HB_TRUE;
   }

   if( lToSkip < 0 )
      pArea->area.fEof = HB_FALSE;
   else /* if( lToSkip > 0 ) */
      pArea->area.fBof = HB_FALSE;

   return errCode;
}


static HB_ERRCODE sqlbaseSkipRaw( SQLBASEAREAP pArea, HB_LONG lToSkip )
{
   HB_ERRCODE errCode;

   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }

   if( lToSkip == 0 )
   {
      /* TODO: maybe gocold is enough here?! */
      HB_BOOL bBof, bEof;

      /* Save flags */
      bBof = pArea->area.fBof;
      bEof = pArea->area.fEof;

      errCode = SELF_GOTO( ( AREAP ) pArea, pArea->ulRecNo );

      /* Restore flags */
      pArea->area.fBof = bBof;
      pArea->area.fEof = bEof;
   }
   else if( lToSkip < 0 && ( HB_ULONG ) ( -lToSkip ) >= pArea->ulRecNo )
   {
      errCode = SELF_GOTO( ( AREAP ) pArea, 1 );
      pArea->area.fBof = HB_TRUE;
   }
   else
      errCode = SELF_GOTO( ( AREAP ) pArea, pArea->ulRecNo + lToSkip );

   return errCode;
}


static HB_ERRCODE sqlbaseAppend( SQLBASEAREAP pArea, HB_BOOL bUnLockAll )
{
   HB_SYMBOL_UNUSED( bUnLockAll );

   /* This GOTO is GOCOLD + GOEOF */
   if( SELF_GOTO( ( AREAP ) pArea, 0 ) == HB_FAILURE )
      return HB_FAILURE;

   if( ! pArea->fRecordChanged && SELF_GOHOT( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   if( pArea->ulRecCount + 1 >= pArea->ulRecMax )
   {
      pArea->pRow      = ( void ** ) hb_xrealloc( pArea->pRow, ( pArea->ulRecMax + SQLDD_ROWSET_RESIZE ) * sizeof( void * ) );
      pArea->pRowFlags = ( HB_BYTE * ) hb_xrealloc( pArea->pRowFlags, ( pArea->ulRecMax + SQLDD_ROWSET_RESIZE ) * sizeof( HB_BYTE ) );
      pArea->ulRecMax += SQLDD_ROWSET_RESIZE;
   }

   pArea->fAppend = pArea->fPositioned = HB_TRUE;
   pArea->ulRecCount++;
   pArea->ulRecNo   = pArea->ulRecCount;
   pArea->area.fBof = pArea->area.fEof = pArea->area.fFound = HB_FALSE;
   return HB_SUCCESS;
}


static HB_ERRCODE sqlbaseDeleteRec( SQLBASEAREAP pArea )
{
   if( ! pArea->fPositioned )
      return HB_SUCCESS;

   if( ! pArea->fRecordChanged && SELF_GOHOT( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   pArea->bRecordFlags |= SQLDD_FLAG_DELETED;
   return HB_SUCCESS;
}


static HB_ERRCODE sqlbaseDeleted( SQLBASEAREAP pArea, HB_BOOL * pDeleted )
{
   *pDeleted = pArea->bRecordFlags & SQLDD_FLAG_DELETED;
   return HB_SUCCESS;
}


static HB_ERRCODE sqlbaseGetValue( SQLBASEAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
{
   if( uiIndex == 0 || uiIndex > pArea->area.uiFieldCount )
      return HB_FAILURE;

   if( pArea->bRecordFlags & SQLDD_FLAG_CACHED )
   {
      hb_arrayGet( ( PHB_ITEM ) pArea->pRecord, uiIndex, pItem );
      return HB_SUCCESS;
   }
   return pArea->pSDD->GetValue( pArea, uiIndex, pItem );
}


static HB_ERRCODE sqlbaseGetVarLen( SQLBASEAREAP pArea, HB_USHORT uiIndex, HB_ULONG * pLength )
{
   /*  TODO: should we use this code? */
#if 0
   if( pArea->area.lpFields[ uiIndex ].uiType == HB_IT_MEMO )
      return pArea->pSDD->GetVarLen( pArea, uiIndex, pLength );
#endif

   *pLength = pArea->area.lpFields[ uiIndex - 1 ].uiLen;
   return HB_SUCCESS;
}


static HB_ERRCODE sqlbaseGoCold( SQLBASEAREAP pArea )
{
   if( pArea->fRecordChanged )
   {
      if( ! pArea->fAppend && pArea->pRowFlags[ pArea->ulRecNo ] & SQLDD_FLAG_CACHED )
         hb_itemRelease( ( PHB_ITEM ) ( pArea->pRow[ pArea->ulRecNo ] ) );
      pArea->pRow[ pArea->ulRecNo ]      = pArea->pRecord;
      pArea->pRowFlags[ pArea->ulRecNo ] = pArea->bRecordFlags;
      pArea->fRecordChanged = HB_FALSE;
      pArea->fAppend        = HB_FALSE;
   }
   return HB_SUCCESS;
}


static HB_ERRCODE sqlbaseGoHot( SQLBASEAREAP pArea )
{
   PHB_ITEM  pArray, pItem;
   HB_USHORT us;

   pArray = hb_itemArrayNew( pArea->area.uiFieldCount );
   for( us = 1; us <= pArea->area.uiFieldCount; us++ )
   {
      pItem = hb_itemNew( NULL );
      if( SELF_GETVALUE( ( AREAP ) pArea, us, pItem ) == HB_SUCCESS )
         hb_arraySetForward( pArray, us, pItem );
      hb_itemRelease( pItem );
   }
   pArea->pRecord        = pArray;
   pArea->bRecordFlags  |= SQLDD_FLAG_CACHED;
   pArea->fRecordChanged = HB_TRUE;
   return HB_SUCCESS;
}


static HB_ERRCODE sqlbasePutValue( SQLBASEAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
{
   LPFIELD    pField;
   HB_ERRCODE errCode;

   if( uiIndex == 0 || uiIndex > pArea->area.uiFieldCount )
      return HB_FAILURE;

   if( ! pArea->fPositioned )
      return HB_SUCCESS;

   if( ! pArea->fRecordChanged && SELF_GOHOT( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   errCode = HB_SUCCESS;
   pField  = pArea->area.lpFields + ( uiIndex - 1 );

   if( ( ( HB_IS_MEMO( pItem ) || HB_IS_STRING( pItem ) ) && ( pField->uiType == HB_FT_STRING || pField->uiType == HB_FT_MEMO ) ) ||
       ( HB_IS_DATE( pItem ) && pField->uiType == HB_FT_DATE ) ||
       ( HB_IS_TIMESTAMP( pItem ) && pField->uiType == HB_FT_TIMESTAMP ) ||
       ( HB_IS_NUMBER( pItem ) && ( pField->uiType == HB_FT_INTEGER || pField->uiType == HB_FT_LONG ||
                                    pField->uiType == HB_FT_FLOAT || pField->uiType == HB_FT_DOUBLE ) ) ||
       ( HB_IS_LOGICAL( pItem ) && pField->uiType == HB_FT_LOGICAL ) ||
       HB_IS_NIL( pItem ) )
   {
      hb_arraySet( ( PHB_ITEM ) pArea->pRecord, uiIndex, pItem );
   }
   else
   {
      PHB_ITEM pError;

      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_DATATYPE );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_DATATYPE ) );
      hb_errPutOperation( pError, hb_dynsymName( ( PHB_DYNS ) pField->sym ) );
      hb_errPutSubCode( pError, errCode );
      hb_errPutFlags( pError, EF_CANDEFAULT );
      errCode = SELF_ERROR( ( AREAP ) pArea, pError );
      hb_itemRelease( pError );
      return errCode == E_DEFAULT ? HB_SUCCESS : HB_FAILURE;
   }
   return HB_SUCCESS;
}


static HB_ERRCODE sqlbaseRecall( SQLBASEAREAP pArea )
{
   if( ! pArea->fPositioned )
      return HB_SUCCESS;

   if( ! pArea->fRecordChanged && SELF_GOHOT( ( AREAP ) pArea ) != HB_SUCCESS )
      return HB_FAILURE;

   pArea->bRecordFlags &= ~SQLDD_FLAG_DELETED;
   return HB_SUCCESS;
}


static HB_ERRCODE sqlbaseRecCount( SQLBASEAREAP pArea, HB_ULONG * pRecCount )
{
   *pRecCount = pArea->ulRecCount;
   return HB_SUCCESS;
}


static HB_ERRCODE sqlbaseRecNo( SQLBASEAREAP pArea, HB_ULONG * ulRecNo )
{
   *ulRecNo = pArea->ulRecNo;
   return HB_SUCCESS;
}


static HB_ERRCODE sqlbaseRecId( SQLBASEAREAP pArea, PHB_ITEM pRecNo )
{
   HB_ERRCODE errCode;
   HB_ULONG   ulRecNo;

   errCode = SELF_RECNO( ( AREAP ) pArea, &ulRecNo );
   hb_itemPutNInt( pRecNo, ulRecNo );
   return errCode;
}


static HB_ERRCODE sqlbaseClose( SQLBASEAREAP pArea )
{
   if( SELF_GOCOLD( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   if( SUPER_CLOSE( ( AREAP ) pArea ) == HB_FAILURE )
      return HB_FAILURE;

   if( pArea->pSDD )
      pArea->pSDD->Close( pArea );

   if( pArea->pRow )
   {
      HB_ULONG ulIndex;

      for( ulIndex = 0; ulIndex <= pArea->ulRecCount; ulIndex++ )
      {
         if( pArea->pRowFlags[ ulIndex ] & SQLDD_FLAG_CACHED )
            hb_itemRelease( ( PHB_ITEM ) pArea->pRow[ ulIndex ] );
      }
      hb_xfree( pArea->pRow );
      hb_xfree( pArea->pRowFlags );
      pArea->pRow      = NULL;
      pArea->pRowFlags = NULL;
   }

   if( pArea->szQuery )
   {
      hb_xfree( pArea->szQuery );
      pArea->szQuery = NULL;
   }
   if( pArea->pConnection )
   {
      /* It is possible to have areas without connection and SDD driver. Ex., arrayrdd. [Mindaugas] */
      pArea->pConnection->uiAreaCount--;
      pArea->pConnection = NULL;
   }
   return HB_SUCCESS;
}


static HB_ERRCODE sqlbaseCreate( SQLBASEAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   PHB_ITEM  pItemEof, pItem;
   HB_USHORT uiCount;
   HB_BOOL   bError;

   pArea->ulConnection = pOpenInfo->ulConnection ? pOpenInfo->ulConnection : s_ulConnectionCurrent;

   if( pArea->ulConnection > s_ulConnectionCount ||
       ( pArea->ulConnection && ! s_pConnection[ pArea->ulConnection - 1 ] ) )
   {
      hb_errRT_SQLBASE( EG_OPEN, ESQLDD_NOTCONNECTED, "Not connected", NULL );
      return HB_FAILURE;
   }

   if( pArea->ulConnection )
   {
      pArea->pConnection = s_pConnection[ pArea->ulConnection - 1 ];
      pArea->pConnection->uiAreaCount++;
      pArea->pSDD = pArea->pConnection->pSDD;
   }
   else
      pArea->pSDD = &sddNull;

   pItemEof = hb_itemArrayNew( pArea->area.uiFieldCount );

   bError = HB_FALSE;
   for( uiCount = 0; uiCount < pArea->area.uiFieldCount; uiCount++ )
   {
      LPFIELD pField = pArea->area.lpFields + uiCount;

      switch( pField->uiType )
      {
         case HB_FT_STRING:
         {
            char * pStr;

            pStr = ( char * ) hb_xgrab( pField->uiLen + 1 );
            memset( pStr, ' ', pField->uiLen );
            pStr[ pField->uiLen ] = '\0';

            pItem = hb_itemPutCL( NULL, pStr, pField->uiLen );
            hb_xfree( pStr );
            break;
         }

         case HB_FT_MEMO:
            pItem = hb_itemPutC( NULL, NULL );
            break;

         case HB_FT_INTEGER:
            if( pField->uiDec )
               pItem = hb_itemPutND( NULL, 0.0 );
            else
               pItem = hb_itemPutNI( NULL, 0 );
            break;

         case HB_FT_LONG:
            if( pField->uiDec )
               pItem = hb_itemPutND( NULL, 0.0 );
            else
               pItem = hb_itemPutNL( NULL, 0 );
            break;

         case HB_FT_FLOAT:
            pItem = hb_itemPutND( NULL, 0.0 );
            break;

         case HB_FT_DOUBLE:
            pItem = hb_itemPutND( NULL, 0.0 );
            break;

         case HB_FT_DATE:
            pItem = hb_itemPutDS( NULL, NULL );
            break;

         case HB_FT_LOGICAL:
            pItem = hb_itemPutL( NULL, HB_FALSE );
            break;

         default:
            pItem  = hb_itemNew( NULL );
            bError = HB_TRUE;
            break;
      }

      hb_arraySetForward( pItemEof, uiCount + 1, pItem );
      hb_itemRelease( pItem );

      if( bError )
         break;
   }

   if( bError )
   {
      hb_itemClear( pItemEof );
      hb_itemRelease( pItemEof );
      hb_errRT_SQLBASE( EG_CORRUPTION, ESQLDD_INVALIDFIELD, "Invalid field type", NULL );
      SELF_CLOSE( ( AREAP ) pArea );
      return HB_FAILURE;
   }

   pArea->ulRecCount = 0;

   pArea->pRow      = ( void ** ) hb_xalloc( SQLDD_ROWSET_RESIZE * sizeof( void * ) );
   pArea->pRowFlags = ( HB_BYTE * ) hb_xalloc( SQLDD_ROWSET_RESIZE * sizeof( HB_BYTE ) );
   pArea->ulRecMax  = SQLDD_ROWSET_RESIZE;

   *( pArea->pRow )      = pItemEof;
   pArea->pRowFlags[ 0 ] = SQLDD_FLAG_CACHED;
   pArea->fFetched       = HB_TRUE;

   if( SUPER_CREATE( ( AREAP ) pArea, pOpenInfo ) != HB_SUCCESS )
   {
      SELF_CLOSE( ( AREAP ) pArea );
      return HB_FAILURE;
   }

   return SELF_GOTOP( ( AREAP ) pArea );
}


static HB_ERRCODE sqlbaseInfo( SQLBASEAREAP pArea, HB_USHORT uiIndex, PHB_ITEM pItem )
{
   switch( uiIndex )
   {
      case DBI_QUERY:
         hb_itemPutC( pItem, pArea->szQuery );
         break;

      default:
         return SUPER_INFO( ( AREAP ) pArea, uiIndex, pItem );
   }

   return HB_SUCCESS;
}


static HB_ERRCODE sqlbaseOpen( SQLBASEAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   HB_ERRCODE errCode;

   pArea->ulConnection = pOpenInfo->ulConnection ? pOpenInfo->ulConnection : s_ulConnectionCurrent;

   if( pArea->ulConnection == 0 || pArea->ulConnection > s_ulConnectionCount ||
       ! s_pConnection[ pArea->ulConnection - 1 ] )
   {
      hb_errRT_SQLBASE( EG_OPEN, ESQLDD_NOTCONNECTED, "Not connected", NULL );
      return HB_FAILURE;
   }

   if( pArea->area.uiFieldCount )
      /* This should not happen (in __dbTrans()), because RDD is registered with RDT_FULL */
      return HB_FAILURE;

   pArea->pConnection = s_pConnection[ pArea->ulConnection - 1 ];
   pArea->pConnection->uiAreaCount++;
   pArea->pSDD = pArea->pConnection->pSDD;

   /* filename is a query */
   pArea->szQuery = hb_strdup( pOpenInfo->abName );

   errCode = pArea->pSDD->Open( pArea );

   if( errCode == HB_SUCCESS )
      errCode = SUPER_OPEN( ( AREAP ) pArea, pOpenInfo );

   if( errCode != HB_SUCCESS )
   {
      SELF_CLOSE( ( AREAP ) pArea );
      return HB_FAILURE;
   }
   return SELF_GOTOP( ( AREAP ) pArea );
}


static HB_ERRCODE sqlbaseStructSize( SQLBASEAREAP pArea, HB_USHORT * uiSize )
{
   HB_SYMBOL_UNUSED( pArea );

   *uiSize = sizeof( SQLBASEAREA );
   return HB_SUCCESS;
}


#if 0
static HB_ERRCODE sqlbaseChildEnd( SQLBASEAREAP pArea, LPDBRELINFO pRelInfo )
{
   HB_ERRCODE errCode;

   if( pArea->lpdbPendingRel == pRelInfo )
      errCode = SELF_FORCEREL( ( AREAP ) pArea );
   else
      errCode = HB_SUCCESS;
   SUPER_CHILDEND( ( AREAP ) pArea, pRelInfo );
   return errCode;
}


static HB_ERRCODE sqlbaseChildStart( SQLBASEAREAP pArea, LPDBRELINFO pRelInfo )
{
   if( SELF_CHILDSYNC( ( AREAP ) pArea, pRelInfo ) != HB_SUCCESS )
      return HB_FAILURE;
   return SUPER_CHILDSTART( ( AREAP ) pArea, pRelInfo );
}


static HB_ERRCODE sqlbaseChildSync( SQLBASEAREAP pArea, LPDBRELINFO pRelInfo )
{
   if( SELF_GOCOLD( ( AREAP ) pArea ) != HB_SUCCESS )
      return HB_FAILURE;

   pArea->lpdbPendingRel = pRelInfo;

   if( pArea->lpdbRelations )
      return SELF_SYNCCHILDREN( ( AREAP ) pArea );

   return HB_SUCCESS;
}


static HB_ERRCODE sqlbaseForceRel( SQLBASEAREAP pArea )
{
   if( pArea->lpdbPendingRel )
   {
      LPDBRELINFO lpdbPendingRel;

      lpdbPendingRel        = pArea->lpdbPendingRel;
      pArea->lpdbPendingRel = NULL;
      return SELF_RELEVAL( ( AREAP ) pArea, lpdbPendingRel );
   }
   return HB_SUCCESS;
}


static HB_ERRCODE sqlbaseSetFilter( SQLBASEAREAP pArea, LPDBFILTERINFO pFilterInfo )
{
   if( pArea->lpdbPendingRel )
   {
      if( SELF_FORCEREL( ( AREAP ) pArea ) != HB_SUCCESS )
         return HB_FAILURE;
   }
   return SUPER_SETFILTER( ( AREAP ) pArea, pFilterInfo );
}
#endif


static HB_ERRCODE sqlbaseInit( LPRDDNODE pRDD )
{
   HB_SYMBOL_UNUSED( pRDD );

   s_pItemNewID = hb_itemNew( NULL );
   return HB_SUCCESS;
}


static HB_ERRCODE sqlbaseExit( LPRDDNODE pRDD )
{
   HB_ULONG ul;

   HB_SYMBOL_UNUSED( pRDD );

   if( s_pConnection )
   {
      /* Disconnect all connections */
      for( ul = 0; ul < s_ulConnectionCount; ul++ )
      {
         if( s_pConnection[ ul ] )
         {
            s_pConnection[ ul ]->pSDD->Disconnect( s_pConnection[ ul ] );
            hb_xfree( s_pConnection[ ul ] );
         }
      }
      hb_xfree( s_pConnection );
      s_pConnection         = NULL;
      s_ulConnectionCount   = 0;
      s_ulConnectionCurrent = 0;
      if( s_szError )
      {
         hb_xfree( s_szError );
         s_szError = NULL;
      }
      if( s_szQuery )
      {
         hb_xfree( s_szQuery );
         s_szQuery = NULL;
      }
      hb_itemRelease( s_pItemNewID );
      s_pItemNewID = NULL;
   }

   return HB_SUCCESS;
}


static HB_ERRCODE sqlbaseRddInfo( LPRDDNODE pRDD, HB_USHORT uiIndex, HB_ULONG ulConnect, PHB_ITEM pItem )
{
   HB_ULONG ulConn;
   SQLDDCONNECTION * pConn;

   HB_SYMBOL_UNUSED( pRDD );

   ulConn = ulConnect ? ulConnect : s_ulConnectionCurrent;
   if( ulConn > 0 && ulConn <= s_ulConnectionCount )
      pConn = s_pConnection[ ulConn - 1 ];
   else
      pConn = NULL;

   switch( uiIndex )
   {
      case RDDI_REMOTE:
         hb_itemPutL( pItem, HB_TRUE );
         break;

      case RDDI_CONNECTION:
      {
         HB_ULONG ulNewConnection = 0;

         if( hb_itemType( pItem ) & HB_IT_NUMERIC )
            ulNewConnection = hb_itemGetNL( pItem );

         hb_itemPutNL( pItem, ulConnect ? ulConnect : s_ulConnectionCurrent );

         if( ulNewConnection )
            s_ulConnectionCurrent = ulNewConnection;
         break;
      }

      case RDDI_ISDBF:
         hb_itemPutL( pItem, HB_FALSE );
         break;

      case RDDI_CANPUTREC:
         hb_itemPutL( pItem, HB_TRUE );
         break;

      case RDDI_CONNECT:
      {
         PSDDNODE     pNode = NULL;
         HB_ULONG     ul;
         const char * pStr;

         pStr = hb_arrayGetCPtr( pItem, 1 );
         if( pStr )
         {
            pNode = s_pSdd;
            while( pNode )
            {
               if( ! hb_stricmp( pNode->Name, pStr ) )
                  break;
               pNode = pNode->pNext;
            }
         }

         hb_rddsqlSetError( 0, NULL, NULL, NULL, 0 );
         pConn = ( SQLDDCONNECTION * ) hb_xgrab( sizeof( SQLDDCONNECTION ) );
         memset( pConn, 0, sizeof( SQLDDCONNECTION ) );
         if( pNode && pNode->Connect( pConn, pItem ) == HB_SUCCESS )
         {
            pConn->pSDD = pNode;

            /* Find free connection handle */
            for( ul = 0; ul < s_ulConnectionCount; ul++ )
            {
               if( ! s_pConnection[ ul ] )
                  break;
            }
            if( ul >= s_ulConnectionCount )
            {
               /* Realloc connection table */
               if( s_pConnection )
                  s_pConnection = ( SQLDDCONNECTION ** ) hb_xrealloc( s_pConnection, sizeof( SQLDDCONNECTION * ) * ( s_ulConnectionCount + CONNECTION_LIST_EXPAND ) );
               else
                  s_pConnection = ( SQLDDCONNECTION ** ) hb_xgrab( sizeof( SQLDDCONNECTION * ) * CONNECTION_LIST_EXPAND );

               memset( s_pConnection + s_ulConnectionCount, 0, sizeof( SQLDDCONNECTION * ) * CONNECTION_LIST_EXPAND );
               ul = s_ulConnectionCount;
               s_ulConnectionCount += CONNECTION_LIST_EXPAND;
            }
            s_pConnection[ ul ] = pConn;
            ul++;
            s_ulConnectionCurrent = ul;
         }
         else
         {
            hb_xfree( pConn );
            ul = 0;
         }

         hb_itemPutNI( pItem, ul );
         break;
      }

      case RDDI_DISCONNECT:
         hb_rddsqlSetError( 0, NULL, NULL, NULL, 0 );
         if( pConn && ! pConn->uiAreaCount && pConn->pSDD->Disconnect( pConn ) == HB_SUCCESS )
         {
            hb_xfree( pConn );
            s_pConnection[ ulConn - 1 ] = NULL;
            if( s_ulConnectionCurrent == ulConn )
               s_ulConnectionCurrent = 0;

            hb_itemPutL( pItem, HB_TRUE );
            return HB_SUCCESS;
         }
         hb_itemPutL( pItem, HB_FALSE );
         return HB_SUCCESS;

      case RDDI_EXECUTE:
         hb_rddsqlSetError( 0, NULL, NULL, NULL, 0 );
         if( pConn )
            hb_itemPutL( pItem, pConn->pSDD->Execute( pConn, pItem ) == HB_SUCCESS );
         else
            hb_itemPutL( pItem, HB_FALSE );

         return HB_SUCCESS;

      case RDDI_ERROR:
         hb_itemPutC( pItem, s_szError );
         return HB_SUCCESS;

      case RDDI_ERRORNO:
         hb_itemPutNI( pItem, s_errCode );
         return HB_SUCCESS;

      case RDDI_QUERY:
         hb_itemPutC( pItem, s_szQuery );
         return HB_SUCCESS;

      case RDDI_NEWID:
         hb_itemCopy( pItem, s_pItemNewID );
         return HB_SUCCESS;

      case RDDI_AFFECTEDROWS:
         hb_itemPutNInt( pItem, s_ulAffectedRows );
         return HB_SUCCESS;

#if 0
      default:
         return SUPER_RDDINFO( pRDD, uiIndex, ulConnect, pItem );
#endif

   }

   return HB_SUCCESS;
}


/*====================================================================================*/

static RDDFUNCS sqlbaseTable =
{
   ( DBENTRYP_BP ) NULL,             /* sqlbaseBof */
   ( DBENTRYP_BP ) NULL,             /* sqlbaseEof */
   ( DBENTRYP_BP ) NULL,             /* sqlbaseFound */
   ( DBENTRYP_V ) sqlbaseGoBottom,
   ( DBENTRYP_UL ) sqlbaseGoTo,
   ( DBENTRYP_I ) sqlbaseGoToId,
   ( DBENTRYP_V ) sqlbaseGoTop,
   ( DBENTRYP_BIB ) NULL,            /* sqlbaseSeek */
   ( DBENTRYP_L ) sqlbaseSkip,
   ( DBENTRYP_L ) NULL,              /* sqlbaseSkipFilter */
   ( DBENTRYP_L ) sqlbaseSkipRaw,
   ( DBENTRYP_VF ) NULL,             /* sqlbaseAddField */
   ( DBENTRYP_B ) sqlbaseAppend,
   ( DBENTRYP_I ) NULL,              /* sqlbaseCreateFields */
   ( DBENTRYP_V ) sqlbaseDeleteRec,
   ( DBENTRYP_BP ) sqlbaseDeleted,
   ( DBENTRYP_SP ) NULL,             /* sqlbaseFieldCount */
   ( DBENTRYP_VF ) NULL,             /* sqlbaseFieldDisplay */
   ( DBENTRYP_SSI ) NULL,            /* sqlbaseFieldInfo */
   ( DBENTRYP_SCP ) NULL,            /* sqlbaseFieldName */
   ( DBENTRYP_V ) NULL,              /* sqlbaseFlush */
   ( DBENTRYP_PP ) NULL,             /* sqlbaseGetRec */
   ( DBENTRYP_SI ) sqlbaseGetValue,
   ( DBENTRYP_SVL ) sqlbaseGetVarLen,
   ( DBENTRYP_V ) sqlbaseGoCold,
   ( DBENTRYP_V ) sqlbaseGoHot,
   ( DBENTRYP_P ) NULL,              /* sqlbasePutRec */
   ( DBENTRYP_SI ) sqlbasePutValue,
   ( DBENTRYP_V ) sqlbaseRecall,
   ( DBENTRYP_ULP ) sqlbaseRecCount,
   ( DBENTRYP_ISI ) NULL,            /* sqlbaseRecInfo */
   ( DBENTRYP_ULP ) sqlbaseRecNo,
   ( DBENTRYP_I ) sqlbaseRecId,
   ( DBENTRYP_S ) NULL,              /* sqlbaseSetFieldExtent */
   ( DBENTRYP_CP ) NULL,             /* sqlbaseAlias */
   ( DBENTRYP_V ) sqlbaseClose,
   ( DBENTRYP_VO ) sqlbaseCreate,
   ( DBENTRYP_SI ) sqlbaseInfo,
   ( DBENTRYP_V ) NULL,              /* sqlbaseNewArea */
   ( DBENTRYP_VO ) sqlbaseOpen,
   ( DBENTRYP_V ) NULL,              /* sqlbaseRelease */
   ( DBENTRYP_SP ) sqlbaseStructSize,
   ( DBENTRYP_CP ) NULL,             /* sqlbaseSysName */
   ( DBENTRYP_VEI ) NULL,            /* sqlbaseEval */
   ( DBENTRYP_V ) NULL,              /* sqlbasePack */
   ( DBENTRYP_LSP ) NULL,            /* sqlbasePackRec */
   ( DBENTRYP_VS ) NULL,             /* sqlbaseSort */
   ( DBENTRYP_VT ) NULL,             /* sqlbaseTrans */
   ( DBENTRYP_VT ) NULL,             /* sqlbaseTransRec */
   ( DBENTRYP_V ) NULL,              /* sqlbaseZap */
   ( DBENTRYP_VR ) NULL,             /* sqlbaseChildEnd */
   ( DBENTRYP_VR ) NULL,             /* sqlbaseChildStart */
   ( DBENTRYP_VR ) NULL,             /* sqlbaseChildSync */
   ( DBENTRYP_V ) NULL,              /* sqlbaseSyncChildren */
   ( DBENTRYP_V ) NULL,              /* sqlbaseClearRel */
   ( DBENTRYP_V ) NULL,              /* sqlbaseForceRel */
   ( DBENTRYP_SSP ) NULL,            /* sqlbaseRelArea */
   ( DBENTRYP_VR ) NULL,             /* sqlbaseRelEval */
   ( DBENTRYP_SI ) NULL,             /* sqlbaseRelText */
   ( DBENTRYP_VR ) NULL,             /* sqlbaseSetRel */
   ( DBENTRYP_VOI ) NULL,            /* sqlbaseOrderListAdd */
   ( DBENTRYP_V ) NULL,              /* sqlbaseOrderListClear */
   ( DBENTRYP_VOI ) NULL,            /* sqlbaseOrderListDelete */
   ( DBENTRYP_VOI ) NULL,            /* sqlbaseOrderListFocus */
   ( DBENTRYP_V ) NULL,              /* sqlbaseOrderListRebuild */
   ( DBENTRYP_VOO ) NULL,            /* sqlbaseOrderCondition */
   ( DBENTRYP_VOC ) NULL,            /* sqlbaseOrderCreate */
   ( DBENTRYP_VOI ) NULL,            /* sqlbaseOrderDestroy */
   ( DBENTRYP_SVOI ) NULL,           /* sqlbaseOrderInfo */
   ( DBENTRYP_V ) NULL,              /* sqlbaseClearFilter */
   ( DBENTRYP_V ) NULL,              /* sqlbaseClearLocate */
   ( DBENTRYP_V ) NULL,              /* sqlbaseClearScope */
   ( DBENTRYP_VPLP ) NULL,           /* sqlbaseCountScope */
   ( DBENTRYP_I ) NULL,              /* sqlbaseFilterText */
   ( DBENTRYP_SI ) NULL,             /* sqlbaseScopeInfo */
   ( DBENTRYP_VFI ) NULL,            /* sqlbaseSetFilter */
   ( DBENTRYP_VLO ) NULL,            /* sqlbaseSetLocate */
   ( DBENTRYP_VOS ) NULL,            /* sqlbaseSetScope */
   ( DBENTRYP_VPL ) NULL,            /* sqlbaseSkipScope */
   ( DBENTRYP_B ) NULL,              /* sqlbaseLocate */
   ( DBENTRYP_CC ) NULL,             /* sqlbaseCompile */
   ( DBENTRYP_I ) NULL,              /* sqlbaseError */
   ( DBENTRYP_I ) NULL,              /* sqlbaseEvalBlock */
   ( DBENTRYP_VSP ) NULL,            /* sqlbaseRawLock */
   ( DBENTRYP_VL ) NULL,             /* sqlbaseLock */
   ( DBENTRYP_I ) NULL,              /* sqlbaseUnLock */
   ( DBENTRYP_V ) NULL,              /* sqlbaseCloseMemFile */
   ( DBENTRYP_VO ) NULL,             /* sqlbaseCreateMemFile */
   ( DBENTRYP_SCCS ) NULL,           /* sqlbaseGetValueFile */
   ( DBENTRYP_VO ) NULL,             /* sqlbaseOpenMemFile */
   ( DBENTRYP_SCCS ) NULL,           /* sqlbasePutValueFile */
   ( DBENTRYP_V ) NULL,              /* sqlbaseReadDBHeader */
   ( DBENTRYP_V ) NULL,              /* sqlbaseWriteDBHeader */
   ( DBENTRYP_R ) sqlbaseInit,
   ( DBENTRYP_R ) sqlbaseExit,
   ( DBENTRYP_RVVL ) NULL,           /* sqlbaseDrop */
   ( DBENTRYP_RVVL ) NULL,           /* sqlbaseExists */
   ( DBENTRYP_RVVVL ) NULL,          /* sqlbaseRename */
   ( DBENTRYP_RSLV ) sqlbaseRddInfo,
   ( DBENTRYP_SVP ) NULL             /* sqlbaseWhoCares */
};


/*================ Module initialization code ========================================*/

HB_FUNC( SQLBASE )
{
   ;
}

HB_FUNC_STATIC( SQLBASE_GETFUNCTABLE )
{
   RDDFUNCS *  pTable;
   HB_USHORT * puiCount, uiRddId;

   puiCount = ( HB_USHORT * ) hb_parptr( 1 );
   pTable   = ( RDDFUNCS * ) hb_parptr( 2 );
   uiRddId  = ( HB_USHORT ) hb_parni( 4 );

   if( pTable )
   {
      HB_ERRCODE errCode;

      if( puiCount )
         *puiCount = RDDFUNCSCOUNT;

      errCode = hb_rddInheritEx( pTable, &sqlbaseTable, &sqlbaseSuper, NULL, NULL );
      if( errCode == HB_SUCCESS )
         s_rddidSQLBASE = uiRddId;

      hb_retni( errCode );
   }
   else
      hb_retni( HB_FAILURE );
}

static void hb_sqlbaseInit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( hb_rddRegister( "SQLBASE", RDT_FULL ) > 1 )
      hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
}

HB_INIT_SYMBOLS_BEGIN( sqlbase__InitSymbols )
{
   "SQLBASE", { HB_FS_PUBLIC }, { HB_FUNCNAME( SQLBASE ) }, NULL
},
{ "SQLBASE_GETFUNCTABLE", { HB_FS_PUBLIC }, { HB_FUNCNAME( SQLBASE_GETFUNCTABLE ) }, NULL }
HB_INIT_SYMBOLS_END( sqlbase__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_sqlbase_init_ )
hb_vmAtInit( hb_sqlbaseInit, NULL );
HB_CALL_ON_STARTUP_END( _hb_sqlbase_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup sqlbase__InitSymbols
   #pragma startup _hb_sqlbase_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY  HB_DATASEG_FUNC( sqlbase__InitSymbols ) \
   HB_DATASEG_FUNC( _hb_sqlbase_init_ )
   #include "hbiniseg.h"
#endif
