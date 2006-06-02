/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    USRRDD
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
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
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbapirdd.h"
#include "hbvm.h"
#include "hbxvm.h"
#include "hbstack.h"
#include "hbinit.h"
#include "rddsys.ch"
#include "usrrdd.ch"

#define SELF_USRNODE( w )     ( s_pUsrRddNodes[ (w)->rddID ] )
#define SELF_USRDATA( w )     ( ( LPUSRRDDDATA ) ( ( BYTE * )( w ) + \
                                 SELF_USRNODE( w )->uiDataOffset ) )

#undef _SUPERTABLE
#define _SUPERTABLE(w)        ( SELF_USRNODE(w)->pSuperTable )
#undef __SUPERTABLE
#define __SUPERTABLE(r)       ( &((r)->pSuperTable) )

typedef struct _USRRDDNODE
{
   USHORT         uiDataOffset;
   PRDDFUNCS      pSuperTable;
   PHB_ITEM       pMethods;
   PHB_ITEM       pItem;
} USRRDDNODE;
typedef USRRDDNODE * LPUSRRDDNODE;

typedef struct _USRRDDDATA
{
   PHB_ITEM pItem;
} USRRDDDATA;
typedef USRRDDDATA * LPUSRRDDDATA;

static USHORT s_uiUsrNodes = 0;
static LPUSRRDDNODE * s_pUsrRddNodes = NULL;

static BOOL hb_usrIsMethod( PHB_ITEM pMethods, USHORT uiMethod )
{
   PHB_ITEM pItem = hb_arrayGetItemPtr( pMethods, uiMethod );

   return pItem && ( HB_IS_POINTER( pItem ) || HB_IS_SYMBOL( pItem ) ||
                     HB_IS_BLOCK( pItem ) );
}

static BOOL hb_usrPushMethod( PHB_ITEM pMethods, USHORT uiMethod )
{
   PHB_ITEM pItem = hb_arrayGetItemPtr( pMethods, uiMethod );

   if( pItem )
   {
      if( HB_IS_POINTER( pItem ) )
      {
         hb_vmPushSymbol( ( PHB_SYMB ) hb_itemGetPtr( pItem ) );
         hb_vmPushNil();
         return TRUE;
      }
      else if( HB_IS_SYMBOL( pItem ) )
      {
         hb_vmPushSymbol( hb_itemGetSymbol( pItem ) );
         hb_vmPushNil();
         return TRUE;
      }
      else if( HB_IS_BLOCK( pItem ) )
      {
         hb_vmPushSymbol( &hb_symEval );
         hb_vmPush( pItem );
         return TRUE;
      }
   }
   return FALSE;
}

static ERRCODE hb_usrReturn( void )
{
   ERRCODE errCode = hb_parni( -1 );

   /*
    * clear the return value - it's not strictly necessary and Clipper
    * does not make it in many functions but it will much nicer ;-)
    */
   hb_ret();

   return errCode;
}

static ERRCODE hb_usrEvalRddFunc( PHB_ITEM pMethods, USHORT uiMethod, USHORT uiRddID )
{
   if( hb_usrPushMethod( pMethods, uiMethod ) )
   {
      hb_vmPushInteger( uiRddID );
      hb_vmDo( 1 );
      return hb_usrReturn();
   }

   return SUCCESS;
}

static ERRCODE hb_usrEvalAreaFunc( PHB_ITEM pMethods, USHORT uiMethod, AREAP pArea )
{
   if( hb_usrPushMethod( pMethods, uiMethod ) )
   {
      hb_vmPushPointer( pArea );
      hb_vmDo( 1 );
      return hb_usrReturn();
   }

   return SUCCESS;
}

static AREAP hb_usrGetAreaPointer( int iArea )
{
   if( iArea != 0 )
   {
      int iOldArea = hb_rddGetCurrentWorkAreaNumber();
      AREAP pArea;

      hb_rddSelectWorkAreaNumber( iArea );
      pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
      hb_rddSelectWorkAreaNumber( iOldArea );
      return pArea;
   }
   return NULL;
}



/*
 * RDD structures conversions
 */

static PHB_ITEM hb_usrFieldInfoToItem( LPDBFIELDINFO pFieldInfo )
{
   PHB_ITEM pItem;

   pItem = hb_itemArrayNew( UR_FI_SIZE );
   hb_itemPutC( hb_arrayGetItemPtr( pItem, UR_FI_NAME ), ( char * ) pFieldInfo->atomName );
   hb_itemPutNI( hb_arrayGetItemPtr( pItem, UR_FI_TYPE ), pFieldInfo->uiType );
   hb_itemPutNI( hb_arrayGetItemPtr( pItem, UR_FI_TYPEEXT ), pFieldInfo->uiTypeExtended );
   hb_itemPutNI( hb_arrayGetItemPtr( pItem, UR_FI_LEN ), pFieldInfo->uiLen );
   hb_itemPutNI( hb_arrayGetItemPtr( pItem, UR_FI_DEC ), pFieldInfo->uiDec );

   return pItem;
}

static BOOL hb_usrItemToFieldInfo( PHB_ITEM pItem, LPDBFIELDINFO pFieldInfo )
{
   if( pItem && hb_arrayLen( pItem ) == UR_FI_SIZE )
   {
      pFieldInfo->atomName       = ( BYTE * ) hb_arrayGetCPtr( pItem, UR_FI_NAME );
      pFieldInfo->uiType         = hb_arrayGetNI( pItem, UR_FI_TYPE );
      pFieldInfo->uiTypeExtended = hb_arrayGetNI( pItem, UR_FI_TYPEEXT );
      pFieldInfo->uiLen          = hb_arrayGetNI( pItem, UR_FI_LEN );
      pFieldInfo->uiDec          = hb_arrayGetNI( pItem, UR_FI_DEC );
      return TRUE;
   }
   return FALSE;
}

static PHB_ITEM hb_usrOpenInfoToItem( LPDBOPENINFO pOpenInfo )
{
   PHB_ITEM pItem;

   pItem = hb_itemArrayNew( UR_OI_SIZE );
   hb_itemPutNI( hb_arrayGetItemPtr( pItem, UR_OI_AREA ), pOpenInfo->uiArea );
   hb_itemPutC( hb_arrayGetItemPtr( pItem, UR_OI_NAME ), ( char * ) pOpenInfo->abName );
   if( pOpenInfo->atomAlias )
      hb_itemPutC( hb_arrayGetItemPtr( pItem, UR_OI_ALIAS ), ( char * ) pOpenInfo->atomAlias );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_OI_SHARED ), pOpenInfo->fShared );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_OI_READONLY ), pOpenInfo->fReadonly );
   hb_itemPutC( hb_arrayGetItemPtr( pItem, UR_OI_CDPID ), ( char * ) pOpenInfo->cdpId );
   hb_itemPutNL( hb_arrayGetItemPtr( pItem, UR_OI_CONNECT ), pOpenInfo->ulConnection );
   hb_itemPutPtr( hb_arrayGetItemPtr( pItem, UR_OI_HEADER ), pOpenInfo->lpdbHeader );

   return pItem;
}

static BOOL hb_usrItemToOpenInfo( PHB_ITEM pItem, LPDBOPENINFO pOpenInfo )
{
   if( pItem && hb_arrayLen( pItem ) == UR_OI_SIZE )
   {
      pOpenInfo->uiArea       = hb_arrayGetNI( pItem, UR_OI_AREA );
      pOpenInfo->abName       = ( BYTE * ) hb_arrayGetCPtr( pItem, UR_OI_NAME );
      if( HB_IS_STRING( hb_arrayGetItemPtr( pItem, UR_OI_ALIAS ) ) )
         pOpenInfo->atomAlias    = ( BYTE * ) hb_arrayGetCPtr( pItem, UR_OI_ALIAS );
      else
         pOpenInfo->atomAlias    = NULL;
      pOpenInfo->fShared      = hb_arrayGetL( pItem, UR_OI_SHARED );
      pOpenInfo->fReadonly    = hb_arrayGetL( pItem, UR_OI_READONLY );
      pOpenInfo->cdpId        = ( BYTE * ) hb_arrayGetCPtr( pItem, UR_OI_CDPID );
      pOpenInfo->ulConnection = hb_arrayGetNL( pItem, UR_OI_CONNECT );
      pOpenInfo->lpdbHeader   = hb_arrayGetPtr( pItem, UR_OI_HEADER );
      return TRUE;
   }
   return FALSE;
}

static PHB_ITEM hb_usrFilterInfoToItem( LPDBFILTERINFO pFilterInfo )
{
   PHB_ITEM pItem;

   pItem = hb_itemArrayNew( UR_FRI_SIZE );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_FRI_BEXPR ), pFilterInfo->itmCobExpr );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_FRI_CEXPR ), pFilterInfo->abFilterText );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_FRI_ACTIVE ), pFilterInfo->fFilter );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_FRI_OPTIMIZED ), pFilterInfo->fOptimized );
   hb_itemPutPtr( hb_arrayGetItemPtr( pItem, UR_FRI_CARGO ), pFilterInfo->lpvCargo );

   return pItem;
}

static BOOL hb_usrItemToFilterInfo( PHB_ITEM pItem, LPDBFILTERINFO pFilterInfo )
{
   if( pItem && hb_arrayLen( pItem ) == UR_FRI_SIZE )
   {
      pFilterInfo->itmCobExpr   = hb_arrayGetItemPtr( pItem, UR_FRI_BEXPR );
      pFilterInfo->abFilterText = hb_arrayGetItemPtr( pItem, UR_FRI_CEXPR );
      pFilterInfo->fFilter      = hb_arrayGetL( pItem, UR_FRI_ACTIVE );
      pFilterInfo->fOptimized   = hb_arrayGetL( pItem, UR_FRI_OPTIMIZED );
      pFilterInfo->lpvCargo     = hb_arrayGetPtr( pItem, UR_FRI_CARGO );
      return TRUE;
   }
   return FALSE;
}

static PHB_ITEM hb_usrRelInfoToItem( LPDBRELINFO pRelInfo )
{
   PHB_ITEM pItem;

   pItem = hb_itemArrayNew( UR_RI_SIZE );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_RI_BEXPR ), pRelInfo->itmCobExpr );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_RI_CEXPR ), pRelInfo->abKey );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_RI_SCOPED ), pRelInfo->isScoped );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_RI_OPTIMIZED ), pRelInfo->isOptimized );
   hb_itemPutNI( hb_arrayGetItemPtr( pItem, UR_RI_PARENT ), pRelInfo->lpaParent->uiArea );
   hb_itemPutNI( hb_arrayGetItemPtr( pItem, UR_RI_CHILD ), pRelInfo->lpaChild->uiArea );
   hb_itemPutPtr( hb_arrayGetItemPtr( pItem, UR_RI_NEXT ), pRelInfo->lpdbriNext );

   return pItem;
}

static BOOL hb_usrItemToRelInfo( PHB_ITEM pItem, LPDBRELINFO pRelInfo )
{
   if( pItem && hb_arrayLen( pItem ) == UR_RI_SIZE )
   {
      pRelInfo->itmCobExpr  = hb_arrayGetItemPtr( pItem, UR_RI_BEXPR );
      pRelInfo->abKey       = hb_arrayGetItemPtr( pItem, UR_RI_CEXPR );
      pRelInfo->isScoped    = hb_arrayGetL( pItem, UR_RI_SCOPED );
      pRelInfo->isOptimized = hb_arrayGetL( pItem, UR_RI_OPTIMIZED );
      pRelInfo->lpaParent   = hb_usrGetAreaPointer( hb_arrayGetNI( pItem, UR_RI_PARENT ) );
      pRelInfo->lpaChild    = hb_usrGetAreaPointer( hb_arrayGetNI( pItem, UR_RI_CHILD ) );
      pRelInfo->lpdbriNext  = ( LPDBRELINFO ) hb_arrayGetPtr( pItem, UR_RI_NEXT );
      return TRUE;
   }
   return FALSE;
}

static PHB_ITEM hb_usrLockInfoToItem( LPDBLOCKINFO pLockInfo )
{
   PHB_ITEM pItem;

   pItem = hb_itemArrayNew( UR_LI_SIZE );
   if( pLockInfo->itmRecID )
      hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_LI_RECORD ), pLockInfo->itmRecID );
   hb_itemPutNI( hb_arrayGetItemPtr( pItem, UR_LI_METHOD ), pLockInfo->uiMethod );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_LI_RESULT ), pLockInfo->fResult );

   return pItem;
}

static BOOL hb_usrItemToLockInfo( PHB_ITEM pItem, LPDBLOCKINFO pLockInfo )
{
   if( pItem && hb_arrayLen( pItem ) == UR_LI_SIZE )
   {
      pLockInfo->itmRecID = hb_arrayGetItemPtr( pItem, UR_LI_RECORD );
      pLockInfo->uiMethod = hb_arrayGetNI( pItem, UR_LI_METHOD );
      pLockInfo->fResult  = hb_arrayGetL( pItem, UR_LI_RESULT );
      return TRUE;
   }
   return FALSE;
}

static PHB_ITEM hb_usrScopeInfoToItem( LPDBSCOPEINFO pScopeInfo )
{
   PHB_ITEM pItem;

   pItem = hb_itemArrayNew( UR_SI_SIZE );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_SI_BFOR ), pScopeInfo->itmCobFor );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_SI_CFOR ), pScopeInfo->lpstrFor );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_SI_BWHILE ), pScopeInfo->itmCobWhile );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_SI_CWHILE ), pScopeInfo->lpstrWhile );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_SI_NEXT ), pScopeInfo->lNext );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_SI_RECORD ), pScopeInfo->itmRecID );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_SI_REST ), pScopeInfo->fRest );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_SI_IGNOREFILTER ), pScopeInfo->fIgnoreFilter );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_SI_INCLUDEDELETED ), pScopeInfo->fIncludeDeleted );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_SI_LAST ), pScopeInfo->fLast );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_SI_IGNOREDUPS ), pScopeInfo->fIgnoreDuplicates );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_SI_BACKWARD ), pScopeInfo->fBackward );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_SI_OPTIMIZED ), pScopeInfo->fOptimized );

   return pItem;
}

static BOOL hb_usrItemToScopeInfo( PHB_ITEM pItem, LPDBSCOPEINFO pScopeInfo )
{
   if( pItem && hb_arrayLen( pItem ) == UR_SI_SIZE )
   {
      pScopeInfo->itmCobFor         = hb_arrayGetItemPtr( pItem, UR_SI_BFOR );
      pScopeInfo->lpstrFor          = hb_arrayGetItemPtr( pItem, UR_SI_CFOR );
      pScopeInfo->itmCobWhile       = hb_arrayGetItemPtr( pItem, UR_SI_BWHILE );
      pScopeInfo->lpstrWhile        = hb_arrayGetItemPtr( pItem, UR_SI_CWHILE );
      pScopeInfo->lNext             = hb_arrayGetItemPtr( pItem, UR_SI_NEXT );
      pScopeInfo->itmRecID          = hb_arrayGetItemPtr( pItem, UR_SI_RECORD );
      pScopeInfo->fRest             = hb_arrayGetItemPtr( pItem, UR_SI_REST );
      pScopeInfo->fIgnoreFilter     = hb_arrayGetL( pItem, UR_SI_IGNOREFILTER );
      pScopeInfo->fIncludeDeleted   = hb_arrayGetL( pItem, UR_SI_INCLUDEDELETED );
      pScopeInfo->fLast             = hb_arrayGetL( pItem, UR_SI_LAST );
      pScopeInfo->fIgnoreDuplicates = hb_arrayGetL( pItem, UR_SI_IGNOREDUPS );
      pScopeInfo->fBackward         = hb_arrayGetL( pItem, UR_SI_BACKWARD );
      pScopeInfo->fOptimized        = hb_arrayGetL( pItem, UR_SI_OPTIMIZED );
      return TRUE;
   }
   return FALSE;
}

static PHB_ITEM hb_usrEvalInfoToItem( LPDBEVALINFO pEvalInfo )
{
   PHB_ITEM pItem, pScope;

   pScope = hb_usrScopeInfoToItem( &pEvalInfo->dbsci );
   pItem = hb_itemArrayNew( UR_EI_SIZE );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_EI_BLOCK ), pEvalInfo->itmBlock );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_EI_CEXPR ), pEvalInfo->abBlock );
   hb_itemMove( hb_arrayGetItemPtr( pItem, UR_EI_SCOPE ), pScope );
   hb_itemRelease( pScope );

   return pItem;
}

static BOOL hb_usrItemToEvalInfo( PHB_ITEM pItem, LPDBEVALINFO pEvalInfo )
{
   if( pItem && hb_arrayLen( pItem ) == UR_EI_SIZE )
   {
      pEvalInfo->itmBlock = hb_arrayGetItemPtr( pItem, UR_EI_BLOCK );
      pEvalInfo->abBlock  = hb_arrayGetItemPtr( pItem, UR_EI_CEXPR );
      return hb_usrItemToScopeInfo( hb_arrayGetItemPtr( pItem, UR_EI_SCOPE ),
                                    &pEvalInfo->dbsci );
   }
   return FALSE;
}

static PHB_ITEM hb_usrTransInfoToItem( LPDBTRANSINFO pTransInfo )
{
   PHB_ITEM pItem, pScope;

   pScope = hb_usrScopeInfoToItem( &pTransInfo->dbsci );
   pItem = hb_itemArrayNew( UR_TI_SIZE );
   hb_itemPutNI( hb_arrayGetItemPtr( pItem, UR_TI_SRCAREA ), pTransInfo->lpaSource->uiArea );
   hb_itemPutNI( hb_arrayGetItemPtr( pItem, UR_TI_DSTAREA ), pTransInfo->lpaDest->uiArea );
   hb_itemMove( hb_arrayGetItemPtr( pItem, UR_TI_SCOPE ), pScope );
   hb_itemPutNI( hb_arrayGetItemPtr( pItem, UR_TI_FLAGS ), pTransInfo->uiFlags );
   hb_itemPutNI( hb_arrayGetItemPtr( pItem, UR_TI_ITEMCOUNT ), pTransInfo->uiItemCount );
   if( pTransInfo->uiItemCount )
   {
      PHB_ITEM pItems = hb_arrayGetItemPtr( pItem, UR_TI_ITEMS ), pItm;
      LPDBTRANSITEM pTransItem = pTransInfo->lpTransItems;
      USHORT uiCount;

      hb_arrayNew( pItems, pTransInfo->uiItemCount );
      for( uiCount = 1; uiCount <= pTransInfo->uiItemCount; ++uiCount, ++pTransItem )
      {
         pItm = hb_arrayGetItemPtr( pItems, uiCount );
         hb_arrayNew( pItm, UR_TITEM_SIZE );
         hb_itemPutNI( hb_arrayGetItemPtr( pItm, UR_TITEM_SOURCE ), pTransItem->uiSource );
         hb_itemPutNI( hb_arrayGetItemPtr( pItm, UR_TITEM_DESTIN ), pTransItem->uiDest );
      }
   }
   hb_itemRelease( pScope );

   return pItem;
}

static BOOL hb_usrItemToTransInfo( PHB_ITEM pItem, LPDBTRANSINFO pTransInfo )
{
   if( pItem && hb_arrayLen( pItem ) == UR_TI_SIZE )
   {
      USHORT uiItemCount = hb_arrayGetNI( pItem, UR_TI_ITEMCOUNT ), uiCount;
      PHB_ITEM pItems = hb_arrayGetItemPtr( pItem, UR_TI_ITEMS ), pItm;

      if( hb_arrayLen( pItems ) == ( ULONG ) uiItemCount &&
          hb_usrItemToScopeInfo( hb_arrayGetItemPtr( pItem, UR_TI_SCOPE ),
                                 &pTransInfo->dbsci ) )
      {
         pTransInfo->lpaSource   = hb_usrGetAreaPointer( hb_arrayGetNI( pItem, UR_TI_SRCAREA ) );
         pTransInfo->lpaDest     = hb_usrGetAreaPointer( hb_arrayGetNI( pItem, UR_TI_DSTAREA ) );
         pTransInfo->uiFlags     = hb_arrayGetNI( pItem, UR_TI_FLAGS );
         pTransInfo->uiItemCount = uiItemCount;
         if( uiItemCount )
         {
            LPDBTRANSITEM pTransItem;

            pTransInfo->lpTransItems = pTransItem =
               ( LPDBTRANSITEM ) hb_xgrab( uiItemCount * sizeof( DBTRANSITEM ) );

            for( uiCount = 1; uiCount <= uiItemCount; ++uiCount, ++pTransItem )
            {
               pItm = hb_arrayGetItemPtr( pItems, uiCount );
               pTransItem->uiSource = hb_arrayGetNI( pItm, UR_TITEM_SOURCE );
               pTransItem->uiDest   = hb_arrayGetNI( pItm, UR_TITEM_DESTIN );
            }
         }
         else
         {
            pTransInfo->lpTransItems = NULL;
         }
         return TRUE;
      }
   }
   return FALSE;
}

static void hb_usrTransInfoFree( LPDBTRANSINFO pTransInfo )
{
   if( pTransInfo->uiItemCount )
      hb_xfree( pTransInfo->lpTransItems );
}

static PHB_ITEM hb_usrSortInfoToItem( LPDBSORTINFO pSortInfo )
{
   PHB_ITEM pItem, pTrans;

   pTrans = hb_usrTransInfoToItem( &pSortInfo->dbtri );
   pItem = hb_itemArrayNew( UR_SRI_SIZE );
   hb_itemMove( hb_arrayGetItemPtr( pItem, UR_SRI_TRANSINFO ), pTrans );
   hb_itemPutNI( hb_arrayGetItemPtr( pItem, UR_SRI_ITEMCOUNT ), pSortInfo->uiItemCount );
   if( pSortInfo->uiItemCount )
   {
      PHB_ITEM pItems = hb_arrayGetItemPtr( pItem, UR_SRI_ITEMS ), pItm;
      LPDBSORTITEM pSortItem = pSortInfo->lpdbsItem;
      USHORT uiCount;

      hb_arrayNew( pItems, pSortInfo->uiItemCount );
      for( uiCount = 1; uiCount <= pSortInfo->uiItemCount; ++uiCount, ++pSortItem )
      {
         pItm = hb_arrayGetItemPtr( pItems, uiCount );
         hb_arrayNew( pItm, UR_SITEM_SIZE );
         hb_itemPutNI( hb_arrayGetItemPtr( pItm, UR_SITEM_FIELD ), pSortItem->uiField );
         hb_itemPutNI( hb_arrayGetItemPtr( pItm, UR_SITEM_FLAGS ), pSortItem->uiFlags );
      }
   }
   hb_itemRelease( pTrans );

   return pItem;
}

static BOOL hb_usrItemToSortInfo( PHB_ITEM pItem, LPDBSORTINFO pSortInfo )
{
   if( pItem && hb_arrayLen( pItem ) == UR_SRI_SIZE )
   {
      USHORT uiItemCount = hb_arrayGetNI( pItem, UR_SRI_ITEMCOUNT ), uiCount;
      PHB_ITEM pItems = hb_arrayGetItemPtr( pItem, UR_SRI_ITEMS ), pItm;

      if( hb_arrayLen( pItems ) == ( ULONG ) uiItemCount &&
          hb_usrItemToTransInfo( hb_arrayGetItemPtr( pItem, UR_SRI_TRANSINFO ),
                                 &pSortInfo->dbtri ) )
      {
         pSortInfo->uiItemCount = uiItemCount;
         if( uiItemCount )
         {
            LPDBSORTITEM pSortItem;

            pSortInfo->lpdbsItem = pSortItem =
               ( LPDBSORTITEM ) hb_xgrab( uiItemCount * sizeof( DBSORTITEM ) );

            for( uiCount = 1; uiCount <= uiItemCount; ++uiCount, ++pSortItem )
            {
               pItm = hb_arrayGetItemPtr( pItems, uiCount );
               pSortItem->uiField = hb_arrayGetNI( pItm, UR_SITEM_FIELD );
               pSortItem->uiFlags = hb_arrayGetNI( pItm, UR_SITEM_FLAGS );
            }
         }
         else
         {
            pSortInfo->lpdbsItem = NULL;
         }
         return TRUE;
      }
   }
   return FALSE;
}

static void hb_usrSortInfoFree( LPDBSORTINFO pSortInfo )
{
   hb_usrTransInfoFree( &pSortInfo->dbtri );
   if( pSortInfo->uiItemCount )
      hb_xfree( pSortInfo->lpdbsItem );
}

static PHB_ITEM hb_usrOrderInfoToItem( LPDBORDERINFO pOrderInfo )
{
   PHB_ITEM pItem;

   pItem = hb_itemArrayNew( UR_ORI_SIZE );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_ORI_BAG ), pOrderInfo->atomBagName );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_ORI_TAG ), pOrderInfo->itmOrder );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_ORI_BLOCK ), pOrderInfo->itmCobExpr );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_ORI_RESULT ), pOrderInfo->itmResult );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_ORI_NEWVAL ), pOrderInfo->itmNewVal );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_ORI_ALLTAGS ), pOrderInfo->fAllTags );

   return pItem;
}

static BOOL hb_usrItemToOrderInfo( PHB_ITEM pItem, LPDBORDERINFO pOrderInfo )
{
   if( pItem && hb_arrayLen( pItem ) == UR_ORI_SIZE )
   {
      pOrderInfo->atomBagName = hb_arrayGetItemPtr( pItem, UR_ORI_BAG );
      pOrderInfo->itmOrder    = hb_arrayGetItemPtr( pItem, UR_ORI_TAG );
      pOrderInfo->itmCobExpr  = hb_arrayGetItemPtr( pItem, UR_ORI_BLOCK );
      pOrderInfo->itmResult   = hb_arrayGetItemPtr( pItem, UR_ORI_RESULT );
      pOrderInfo->itmNewVal   = hb_arrayGetItemPtr( pItem, UR_ORI_NEWVAL );
      pOrderInfo->fAllTags    = hb_arrayGetL( pItem, UR_ORI_ALLTAGS );
      return TRUE;
   }
   return FALSE;
}

static PHB_ITEM hb_usrOrderCondInfoToItem( LPDBORDERCONDINFO pOrderCondInfo )
{
   PHB_ITEM pItem;

   pItem = hb_itemArrayNew( UR_ORC_SIZE );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_ORC_ACTIVE ), pOrderCondInfo->fActive );
   hb_itemPutC( hb_arrayGetItemPtr( pItem, UR_ORC_CFOR ), ( char * ) pOrderCondInfo->abFor );
   hb_itemPutC( hb_arrayGetItemPtr( pItem, UR_ORC_CWHILE ), ( char * ) pOrderCondInfo->abWhile );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_ORC_BFOR ), pOrderCondInfo->itmCobFor );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_ORC_BWHILE ), pOrderCondInfo->itmCobWhile );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_ORC_BEVAL ), pOrderCondInfo->itmCobEval );
   hb_itemPutNL( hb_arrayGetItemPtr( pItem, UR_ORC_STEP ), pOrderCondInfo->lStep );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_ORC_STARTREC ), pOrderCondInfo->itmStartRecID );
   hb_itemPutNL( hb_arrayGetItemPtr( pItem, UR_ORC_NEXT ), pOrderCondInfo->lNextCount );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_ORC_RECORD ), pOrderCondInfo->itmRecID );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_ORC_REST ), pOrderCondInfo->fRest );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_ORC_DESCEND ), pOrderCondInfo->fDescending );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_ORC_SCOPED ), pOrderCondInfo->fScoped );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_ORC_ALL ), pOrderCondInfo->fAll );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_ORC_ADDITIVE ), pOrderCondInfo->fAdditive );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_ORC_USECURRENT ), pOrderCondInfo->fUseCurrent );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_ORC_CUSTOM ), pOrderCondInfo->fCustom );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_ORC_NOOPTIMIZE ), pOrderCondInfo->fNoOptimize );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_ORC_COMPOUND ), pOrderCondInfo->fCompound );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_ORC_USEFILTER ), pOrderCondInfo->fUseFilter );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_ORC_TEMPORARY ), pOrderCondInfo->fTemporary );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_ORC_EXCLUSIVE ), pOrderCondInfo->fExclusive );
   hb_itemPutPtr( hb_arrayGetItemPtr( pItem, UR_ORC_CARGO ), pOrderCondInfo->lpvCargo );

   return pItem;
}

static BOOL hb_usrItemToOrderCondInfo( PHB_ITEM pItem,
                                       LPDBORDERCONDINFO pOrderCondInfo )
{
   if( pItem && hb_arrayLen( pItem ) == UR_ORC_SIZE )
   {
      pOrderCondInfo->fActive       = hb_arrayGetL( pItem, UR_ORC_ACTIVE );
      pOrderCondInfo->abFor         = ( BYTE * ) hb_arrayGetC( pItem, UR_ORC_CFOR );
      pOrderCondInfo->abWhile       = ( BYTE * ) hb_arrayGetC( pItem, UR_ORC_CWHILE );
      pOrderCondInfo->itmCobFor     = hb_arrayGetItemPtr( pItem, UR_ORC_BFOR );
      pOrderCondInfo->itmCobWhile   = hb_arrayGetItemPtr( pItem, UR_ORC_BWHILE );
      pOrderCondInfo->itmCobEval    = hb_arrayGetItemPtr( pItem, UR_ORC_BEVAL );
      pOrderCondInfo->lStep         = hb_arrayGetNL( pItem, UR_ORC_STEP );
      pOrderCondInfo->itmStartRecID = hb_arrayGetItemPtr( pItem, UR_ORC_STARTREC );
      pOrderCondInfo->lNextCount    = hb_arrayGetNL( pItem, UR_ORC_NEXT );
      pOrderCondInfo->itmRecID      = hb_arrayGetItemPtr( pItem, UR_ORC_RECORD );
      pOrderCondInfo->fRest         = hb_arrayGetL( pItem, UR_ORC_REST );
      pOrderCondInfo->fDescending   = hb_arrayGetL( pItem, UR_ORC_DESCEND );
      pOrderCondInfo->fScoped       = hb_arrayGetL( pItem, UR_ORC_SCOPED );
      pOrderCondInfo->fAll          = hb_arrayGetL( pItem, UR_ORC_ALL );
      pOrderCondInfo->fAdditive     = hb_arrayGetL( pItem, UR_ORC_ADDITIVE );
      pOrderCondInfo->fUseCurrent   = hb_arrayGetL( pItem, UR_ORC_USECURRENT );
      pOrderCondInfo->fCustom       = hb_arrayGetL( pItem, UR_ORC_CUSTOM );
      pOrderCondInfo->fNoOptimize   = hb_arrayGetL( pItem, UR_ORC_NOOPTIMIZE );
      pOrderCondInfo->fCompound     = hb_arrayGetL( pItem, UR_ORC_COMPOUND );
      pOrderCondInfo->fUseFilter    = hb_arrayGetL( pItem, UR_ORC_USEFILTER );
      pOrderCondInfo->fTemporary    = hb_arrayGetL( pItem, UR_ORC_TEMPORARY );
      pOrderCondInfo->fExclusive    = hb_arrayGetL( pItem, UR_ORC_EXCLUSIVE );
      pOrderCondInfo->lpvCargo      = hb_arrayGetPtr( pItem, UR_ORC_CARGO );
      return TRUE;
   }
   return FALSE;
}

static PHB_ITEM hb_usrOrderCreateInfoToItem( LPDBORDERCREATEINFO pOrderCreateInfo )
{
   PHB_ITEM pItem, pCond;

   pItem = hb_itemArrayNew( UR_ORCR_SIZE );
   if( pOrderCreateInfo->lpdbOrdCondInfo )
   {
      pCond = hb_usrOrderCondInfoToItem( pOrderCreateInfo->lpdbOrdCondInfo );
      hb_arraySet( pItem, UR_ORCR_CONDINFO, pCond );
      hb_itemRelease( pCond );
   }
   hb_itemPutC( hb_arrayGetItemPtr( pItem, UR_ORCR_BAGNAME ), ( char * ) pOrderCreateInfo->abBagName );
   hb_itemPutC( hb_arrayGetItemPtr( pItem, UR_ORCR_TAGNAME ), ( char * ) pOrderCreateInfo->atomBagName );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_ORCR_ORDER ), pOrderCreateInfo->itmOrder );
   hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_ORCR_UNIQUE ), pOrderCreateInfo->fUnique );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_ORCR_BKEY ), pOrderCreateInfo->itmCobExpr );
   hb_itemCopy( hb_arrayGetItemPtr( pItem, UR_ORCR_CKEY ), pOrderCreateInfo->abExpr );

   return pItem;
}

static BOOL hb_usrItemToOrderCreateInfo( PHB_ITEM pItem,
                                         LPDBORDERCREATEINFO pOrderCreateInfo )
{
   if( pItem && hb_arrayLen( pItem ) == UR_ORCR_SIZE )
   {
      PHB_ITEM pCond = hb_arrayGetItemPtr( pItem, UR_ORCR_CONDINFO );

      if( hb_arrayLen( pCond ) > 0 )
      {
         LPDBORDERCONDINFO pOrderCondInfo;
         pOrderCondInfo = ( LPDBORDERCONDINFO ) hb_xgrab( sizeof( DBORDERCONDINFO ) );
         if( !hb_usrItemToOrderCondInfo( pCond, pOrderCondInfo ) )
         {
            hb_xfree( pOrderCondInfo );
            return FALSE;
         }
         pOrderCreateInfo->lpdbOrdCondInfo = pOrderCondInfo;
      }
      else
      {
         pOrderCreateInfo->lpdbOrdCondInfo = NULL;
      }
      pOrderCreateInfo->abBagName   = ( BYTE * ) hb_arrayGetC( pItem, UR_ORCR_BAGNAME );
      pOrderCreateInfo->atomBagName = ( BYTE * ) hb_arrayGetC( pItem, UR_ORCR_TAGNAME );
      pOrderCreateInfo->itmOrder    = hb_arrayGetItemPtr( pItem, UR_ORCR_ORDER );
      pOrderCreateInfo->fUnique     = hb_arrayGetL( pItem, UR_ORCR_UNIQUE );
      pOrderCreateInfo->itmCobExpr  = hb_arrayGetItemPtr( pItem, UR_ORCR_BKEY );
      pOrderCreateInfo->abExpr      = hb_arrayGetItemPtr( pItem, UR_ORCR_CKEY );

      return TRUE;
   }
   return FALSE;
}

static void hb_usrOrderCreateFree( LPDBORDERCREATEINFO pOrderCreateInfo )
{
   if( pOrderCreateInfo->lpdbOrdCondInfo )
      hb_xfree( pOrderCreateInfo->lpdbOrdCondInfo );
}


/*
 * -- USRRDD METHODS --
 */

static ERRCODE hb_usrInit( LPRDDNODE pRDD )
{
   ERRCODE errCode;
   LPUSRRDDNODE pNode;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrInit(%p)", pRDD));

   if( pRDD->rddID >= s_uiUsrNodes )
   {
      ULONG ulSize = ( pRDD->rddID + 1 ) * sizeof( LPUSRRDDNODE );
      if( s_uiUsrNodes )
         s_pUsrRddNodes = ( LPUSRRDDNODE * ) hb_xrealloc( s_pUsrRddNodes, ulSize );
      else
         s_pUsrRddNodes = ( LPUSRRDDNODE * ) hb_xgrab( ulSize );
      do
      {
         s_pUsrRddNodes[ s_uiUsrNodes ] = NULL;
      }
      while( ++s_uiUsrNodes <= pRDD->rddID );
   }

   s_pUsrRddNodes[ pRDD->rddID ] = pNode = ( LPUSRRDDNODE ) hb_xgrab( sizeof( USRRDDNODE ) );
   memset( pNode, 0, sizeof( USRRDDNODE ) );
   pNode->pSuperTable = &pRDD->pSuperTable;
   pNode->pMethods = ( PHB_ITEM ) pRDD->pTable.whoCares;
   pRDD->pTable.whoCares = pRDD->pSuperTable.whoCares;
   pNode->pItem = hb_itemNew( NULL );

   if( ISSUPER_INIT( pRDD ) )
      errCode = SUPER_INIT( pRDD );
   else
      errCode = SUCCESS;

   hb_usrEvalRddFunc( pNode->pMethods, UR_INIT, pRDD->rddID );

   return errCode;
}

static ERRCODE hb_usrExit( LPRDDNODE pRDD )
{
   ERRCODE errCode;
   LPUSRRDDNODE pNode;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrExit(%p)", pRDD));

   pNode = s_pUsrRddNodes[ pRDD->rddID ];
   hb_usrEvalRddFunc( pNode->pMethods, UR_EXIT, pRDD->rddID );
   if( pNode->pItem )
      hb_itemRelease( pNode->pItem );
   if( pNode->pMethods )
      hb_itemRelease( pNode->pMethods );
   hb_xfree( pNode );
   s_pUsrRddNodes[ pRDD->rddID ] = NULL;

   if( pRDD->rddID == s_uiUsrNodes - 1 )
   {
      while( --s_uiUsrNodes > 0 )
      {
         if( s_pUsrRddNodes[ s_uiUsrNodes - 1 ] != NULL )
            break;
      }

      if( s_uiUsrNodes )
      {
         s_pUsrRddNodes = ( LPUSRRDDNODE * ) hb_xrealloc( s_pUsrRddNodes,
                                       s_uiUsrNodes * sizeof( LPUSRRDDNODE ) );
      }
      else
      {
         hb_xfree( s_pUsrRddNodes );
         s_pUsrRddNodes = NULL;
      }
   }

   if( ISSUPER_EXIT( pRDD ) )
      errCode = SUPER_EXIT( pRDD );
   else
      errCode = SUCCESS;

   return errCode;
}

static ERRCODE hb_usrStructSize( AREAP pArea, USHORT * puiSize )
{
   ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrStrucSize(%p, %p)", pArea, puiSize));

   errCode = SUPER_STRUCTSIZE( pArea, puiSize );
   s_pUsrRddNodes[ pArea->rddID ]->uiDataOffset = *puiSize;
   *puiSize += sizeof( USRRDDDATA );

   return errCode;
}

static ERRCODE hb_usrSysName( AREAP pArea, BYTE * szSysName )
{
   LONG lOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrSysName(%p,%p)", pArea, szSysName));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_stackPush();
   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SYSNAME ) )
   {
      hb_stackPop();
      hb_strncpy( ( char * ) szSysName, SELF_RDDNODE( pArea )->szName,
                  HARBOUR_MAX_RDD_DRIVERNAME_LENGTH );
      return SUCCESS;
   }

   hb_vmPushInteger( pArea->uiArea );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 2 );

   hb_strncpy( ( char * ) szSysName, hb_itemGetCPtr( hb_stackItemFromBase( lOffset ) ),
               HARBOUR_MAX_RDD_DRIVERNAME_LENGTH );
   hb_stackPop();

   return hb_usrReturn();
}

static ERRCODE hb_usrNewArea( AREAP pArea )
{
   ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrNewArea(%p)", pArea));

   errCode = SUPER_NEW( pArea );

   if( errCode == SUCCESS )
   {
      SELF_USRDATA( pArea )->pItem = hb_itemNew( NULL );
      hb_usrEvalAreaFunc( SELF_USRNODE( pArea )->pMethods, UR_NEW, pArea );
   }

   return errCode;
}

static ERRCODE hb_usrRelease( AREAP pArea )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrRelease(%p)", pArea));

   hb_usrEvalAreaFunc( SELF_USRNODE( pArea )->pMethods, UR_RELEASE, pArea );

   pItem = SELF_USRDATA( pArea )->pItem;
   if( pItem )
      hb_itemRelease( pItem );

   return SUPER_RELEASE( pArea );
}

/*
 * methods which user can overload
 */


/*
 * Movement and positioning methods
 */

static ERRCODE hb_usrBof( AREAP pArea, BOOL * pBof )
{
   LONG lOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrBof(%p, %p)", pArea, pBof));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_xvmPushLogical( pArea->fBof );
   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_BOF ) )
   {
      hb_stackPop();
      return SUPER_BOF( pArea, pBof );
   }
   hb_vmPushInteger( pArea->uiArea );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 2 );

   if( hb_xvmPopLogical( pBof ) )
   {
      hb_ret();
      return FAILURE;
   }

   pArea->fBof = *pBof;
   return hb_usrReturn();
}

static ERRCODE hb_usrEof( AREAP pArea, BOOL * pEof )
{
   LONG lOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrEof(%p, %p)", pArea, pEof));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_xvmPushLogical( pArea->fEof );
   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_EOF ) )
   {
      hb_stackPop();
      return SUPER_EOF( pArea, pEof );
   }
   hb_vmPushInteger( pArea->uiArea );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 2 );

   if( hb_xvmPopLogical( pEof ) )
   {
      hb_ret();
      return FAILURE;
   }

   pArea->fEof = *pEof;
   return hb_usrReturn();
}

static ERRCODE hb_usrFound( AREAP pArea, BOOL * pFound )
{
   LONG lOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrFound(%p, %p)", pArea, pFound));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_xvmPushLogical( pArea->fFound );
   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_FOUND ) )
   {
      hb_stackPop();
      return SUPER_FOUND( pArea, pFound );
   }
   hb_vmPushInteger( pArea->uiArea );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 2 );

   if( hb_xvmPopLogical( pFound ) )
   {
      hb_ret();
      return FAILURE;
   }

   pArea->fFound = *pFound;
   return hb_usrReturn();
}

static ERRCODE hb_usrGoBottom( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrGoBottom(%p)", pArea));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_GOBOTTOM ) )
      return SUPER_GOBOTTOM( pArea );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmDo( 1 );

   return hb_usrReturn();
}

static ERRCODE hb_usrGoTop( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrGoTop(%p)", pArea));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_GOTOP ) )
      return SUPER_GOTOP( pArea );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmDo( 1 );

   return hb_usrReturn();
}

static ERRCODE hb_usrGoTo( AREAP pArea, ULONG ulRecNo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrGoTo(%p,%lu)", pArea, ulRecNo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_GOTO ) )
      return SUPER_GOTO( pArea, ulRecNo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushLong( ulRecNo );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrGoToId( AREAP pArea, PHB_ITEM pRecNo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrGoToId(%p,%p)", pArea, pRecNo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_GOTOID ) )
      return SUPER_GOTOID( pArea, pRecNo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pRecNo );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrSeek( AREAP pArea, BOOL fSoftSeek, PHB_ITEM pItem, BOOL fFindLast )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrSeek(%p,%d,%p,%d)", pArea, fSoftSeek, pItem, fFindLast));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SEEK ) )
      return SUPER_SEEK( pArea, fSoftSeek, pItem, fFindLast );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushLogical( fSoftSeek );
   hb_vmPush( pItem );
   hb_vmPushLogical( fFindLast );
   hb_vmDo( 4 );

   return hb_usrReturn();
}

static ERRCODE hb_usrSkip( AREAP pArea, LONG lRecords )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrSkip(%p,%ld)", pArea, lRecords));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SKIP ) )
      return SUPER_SKIP( pArea, lRecords );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushLong( lRecords );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrSkipFilter( AREAP pArea, LONG lDirect )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrSkipFilter(%p,%ld)", pArea, lDirect));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SKIPFILTER ) )
      return SUPER_SKIPFILTER( pArea, lDirect );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushLong( lDirect );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrSkipRaw( AREAP pArea, LONG lRecords )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrSkipRaw(%p,%ld)", pArea, lRecords));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SKIPRAW ) )
      return SUPER_SKIPRAW( pArea, lRecords );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushLong( lRecords );
   hb_vmDo( 2 );

   return hb_usrReturn();
}


/*
 * Data management
 */

static ERRCODE hb_usrDeleted( AREAP pArea, BOOL * pDeleted )
{
   LONG lOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrDeleted(%p, %p)", pArea, pDeleted));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_xvmPushLogical( FALSE );
   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_DELETED ) )
   {
      hb_stackPop();
      return SUPER_DELETED( pArea, pDeleted );
   }
   hb_vmPushInteger( pArea->uiArea );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 2 );

   if( hb_xvmPopLogical( pDeleted ) )
   {
      hb_ret();
      return FAILURE;
   }

   return hb_usrReturn();
}

static ERRCODE hb_usrAddField( AREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrAddField(%p, %p)", pArea, pFieldInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ADDFIELD ) )
      return SUPER_ADDFIELD( pArea, pFieldInfo );

   pItem = hb_usrFieldInfoToItem( pFieldInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_itemRelease( pItem );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrFieldDisplay( AREAP pArea, LPDBFIELDINFO pFieldInfo )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrFieldDisplay(%p, %p)", pArea, pFieldInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_FIELDDISPLAY ) )
      return SUPER_FIELDDISPLAY( pArea, pFieldInfo );

   pItem = hb_usrFieldInfoToItem( pFieldInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_itemRelease( pItem );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrFieldName( AREAP pArea, USHORT uiIndex, void * szName )
{
   LONG lOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrFieldName(%p,%hu,%p)", pArea, uiIndex, szName));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_stackPush();
   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_FIELDNAME ) )
   {
      hb_stackPop();
      return SUPER_FIELDNAME( pArea, uiIndex, szName );
   }

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushInteger( uiIndex );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 3 );

   hb_strncpy( ( char * ) szName, hb_itemGetCPtr( hb_stackItemFromBase( lOffset ) ),
               pArea->uiMaxFieldNameLength );
   hb_stackPop();

   return hb_usrReturn();
}

static ERRCODE hb_usrAppend( AREAP pArea, BOOL fUnLockAll )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrAppend(%p, %d)", pArea, fUnLockAll));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_APPEND ) )
      return SUPER_APPEND( pArea, fUnLockAll );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushLogical( fUnLockAll );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrDelete( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrDelete(%p)", pArea));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_DELETE ) )
      return SUPER_DELETE( pArea );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmDo( 1 );

   return hb_usrReturn();
}

static ERRCODE hb_usrRecall( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrRecall(%p)", pArea));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_RECALL ) )
      return SUPER_RECALL( pArea );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmDo( 1 );

   return hb_usrReturn();
}

static ERRCODE hb_usrFieldCount( AREAP pArea, USHORT * puiFields )
{
   LONG lOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrFieldCount(%p,%p)", pArea, puiFields));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_xvmPushInteger( 0 );
   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_FIELDCOUNT ) )
   {
      hb_stackPop();
      return SUPER_FIELDCOUNT( pArea, puiFields );
   }

   hb_vmPushInteger( pArea->uiArea );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 2 );

   * puiFields = hb_itemGetNI( hb_stackItemFromBase( lOffset ) );
   hb_stackPop();

   return hb_usrReturn();
}

static ERRCODE hb_usrFlush( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrFlush(%p)", pArea));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_FLUSH ) )
      return SUPER_FLUSH( pArea );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmDo( 1 );

   return hb_usrReturn();
}

static ERRCODE hb_usrGoCold( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrGoCold(%p)", pArea));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_GOCOLD ) )
      return SUPER_GOCOLD( pArea );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmDo( 1 );

   return hb_usrReturn();
}

static ERRCODE hb_usrGoHot( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrGoHot(%p)", pArea));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_GOHOT ) )
      return SUPER_GOHOT( pArea );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmDo( 1 );

   return hb_usrReturn();
}

static ERRCODE hb_usrPutRec( AREAP pArea, BYTE * pBuffer )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrPutRec(%p,%p)", pArea, pBuffer));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_PUTREC ) )
      return SUPER_PUTREC( pArea, pBuffer );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushPointer( pBuffer );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrGetRec( AREAP pArea, BYTE ** pBuffer )
{
   PHB_ITEM pItem;
   LONG lOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrGetRec(%p,%p)", pArea, pBuffer));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_stackPush();
   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_GETREC ) )
   {
      hb_stackPop();
      return SUPER_GETREC( pArea, pBuffer );
   }

   hb_vmPushInteger( pArea->uiArea );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 2 );

   pItem = hb_stackItemFromBase( lOffset );
   if( HB_IS_STRING( pItem ) )
      * pBuffer = ( BYTE * ) hb_itemGetCPtr( pItem );
   else
      * pBuffer = ( BYTE * ) hb_itemGetPtr( pItem );
   hb_stackPop();

   return hb_usrReturn();
}

static ERRCODE hb_usrGetValue( AREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   LONG lOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrGetValue(%p,%hu,%p)", pArea, uiIndex, pItem));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_stackPush();
   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_GETVALUE ) )
   {
      hb_stackPop();
      return SUPER_GETVALUE( pArea, uiIndex, pItem );
   }

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushInteger( uiIndex );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 3 );

   hb_itemCopy( pItem, hb_stackItemFromBase( lOffset ) );
   hb_stackPop();

   return hb_usrReturn();
}

static ERRCODE hb_usrPutValue( AREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrPutValue(%p,%hu,%p)", pArea, uiIndex, pItem));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_PUTVALUE ) )
      return SUPER_PUTVALUE( pArea, uiIndex, pItem );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushInteger( uiIndex );
   hb_vmPush( pItem );
   hb_vmDo( 3 );

   return hb_usrReturn();
}

static ERRCODE hb_usrGetVarLen( AREAP pArea, USHORT uiIndex, ULONG * pulLength )
{
   LONG lOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrGetVarLen(%p,%hu,%p)", pArea, uiIndex, pulLength));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_xvmPushInteger( 0 );
   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_GETVARLEN ) )
   {
      hb_stackPop();
      return SUPER_GETVARLEN( pArea, uiIndex, pulLength );
   }

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushInteger( uiIndex );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 3 );

   * pulLength = hb_itemGetNL( hb_stackItemFromBase( lOffset ) );
   hb_stackPop();

   return hb_usrReturn();
}

static ERRCODE hb_usrRecCount( AREAP pArea, ULONG * pulRecCount )
{
   LONG lOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrRecCount(%p,%p)", pArea, pulRecCount));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_xvmPushInteger( 0 );
   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_RECCOUNT ) )
   {
      hb_stackPop();
      return SUPER_RECCOUNT( pArea, pulRecCount );
   }

   hb_vmPushInteger( pArea->uiArea );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 2 );

   * pulRecCount = hb_itemGetNL( hb_stackItemFromBase( lOffset ) );
   hb_stackPop();

   return hb_usrReturn();
}

static ERRCODE hb_usrRecInfo( AREAP pArea, PHB_ITEM pRecID, USHORT uiInfoType, PHB_ITEM pInfo )
{
   LONG lOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrRecInfo(%p,%p,%hu,%p)", pArea, pRecID, uiInfoType, pInfo));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_vmPush( pInfo );
   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_RECINFO ) )
   {
      hb_stackPop();
      return SUPER_RECINFO( pArea, pRecID, uiInfoType, pInfo );
   }

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pRecID );
   hb_vmPushInteger( uiInfoType );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 4 );

   hb_itemCopy( pInfo, hb_stackItemFromBase( lOffset ) );
   hb_stackPop();

   return hb_usrReturn();
}

static ERRCODE hb_usrRecNo( AREAP pArea, ULONG * pulRecNo )
{
   LONG lOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrRecNo(%p,%p)", pArea, pulRecNo));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_xvmPushInteger( 0 );
   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_RECNO ) )
   {
      hb_stackPop();
      return SUPER_RECNO( pArea, pulRecNo );
   }

   hb_vmPushInteger( pArea->uiArea );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 2 );

   * pulRecNo = hb_itemGetNL( hb_stackItemFromBase( lOffset ) );
   hb_stackPop();

   return hb_usrReturn();
}

static ERRCODE hb_usrRecId( AREAP pArea, PHB_ITEM pRecId )
{
   LONG lOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrRecId(%p,%p)", pArea, pRecId));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_vmPush( pRecId );
   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_RECID ) )
   {
      hb_stackPop();
      return SUPER_RECID( pArea, pRecId );
   }

   hb_vmPushInteger( pArea->uiArea );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 2 );

   hb_itemCopy( pRecId, hb_stackItemFromBase( lOffset ) );
   hb_stackPop();

   return hb_usrReturn();
}

static ERRCODE hb_usrFieldInfo( AREAP pArea, USHORT uiIndex, USHORT uiInfoType, PHB_ITEM pInfo )
{
   LONG lOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrFieldInfo(%p,%hu,%hu,%p)", pArea, uiIndex, uiInfoType, pInfo));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_vmPush( pInfo );
   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_FIELDINFO ) )
   {
      hb_stackPop();
      return SUPER_FIELDINFO( pArea, uiIndex, uiInfoType, pInfo );
   }

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushInteger( uiIndex );
   hb_vmPushInteger( uiInfoType );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 4 );

   hb_itemCopy( pInfo, hb_stackItemFromBase( lOffset ) );
   hb_stackPop();

   return hb_usrReturn();
}

static ERRCODE hb_usrCreateFields( AREAP pArea, PHB_ITEM pStruct )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrCreateFields(%p,%p)", pArea, pStruct));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CREATEFIELDS ) )
      return SUPER_CREATEFIELDS( pArea, pStruct );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pStruct );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrSetFieldExtent( AREAP pArea, USHORT uiFieldExtent )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrSetFieldExtent(%p,%hu)", pArea, uiFieldExtent));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SETFIELDEXTENT ) )
      return SUPER_SETFIELDEXTENT( pArea, uiFieldExtent );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushInteger( uiFieldExtent );
   hb_vmDo( 2 );

   return hb_usrReturn();
}


/*
 * WorkArea/Database management
 */

static ERRCODE hb_usrAlias( AREAP pArea, BYTE * szAlias )
{
   LONG lOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrAlias(%p,%p)", pArea, szAlias));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_stackPush();
   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ALIAS ) )
   {
      hb_stackPop();
      return SUPER_ALIAS( pArea, szAlias );
   }

   hb_vmPushInteger( pArea->uiArea );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 2 );

   hb_strncpy( ( char * ) szAlias, hb_itemGetCPtr( hb_stackItemFromBase( lOffset ) ),
               HARBOUR_MAX_RDD_ALIAS_LENGTH );
   hb_stackPop();

   return hb_usrReturn();
}

static ERRCODE hb_usrClose( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrClose(%p)", pArea));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CLOSE ) )
      return SUPER_CLOSE( pArea );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmDo( 1 );

   return hb_usrReturn();
}

static ERRCODE hb_usrCreate( AREAP pArea, LPDBOPENINFO pOpenInfo )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrCreate(%p,%p)", pArea, pOpenInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CREATE ) )
      return SUPER_CREATE( pArea, pOpenInfo );

   pItem = hb_usrOpenInfoToItem( pOpenInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_itemRelease( pItem );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrOpen( AREAP pArea, LPDBOPENINFO pOpenInfo )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrOpen(%p,%p)", pArea, pOpenInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_OPEN ) )
      return SUPER_OPEN( pArea, pOpenInfo );

   pItem = hb_usrOpenInfoToItem( pOpenInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_itemRelease( pItem );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrInfo( AREAP pArea, USHORT uiInfoType, PHB_ITEM pInfo )
{
   LONG lOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrInfo(%p,%hu,%p)", pArea, uiInfoType, pInfo));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_vmPush( pInfo );
   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_INFO ) )
   {
      hb_stackPop();
      return SUPER_INFO( pArea, uiInfoType, pInfo );
   }

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushInteger( uiInfoType );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 3 );

   hb_itemCopy( pInfo, hb_stackItemFromBase( lOffset ) );
   hb_stackPop();

   return hb_usrReturn();
}

static ERRCODE hb_usrEval( AREAP pArea, LPDBEVALINFO pEvalInfo )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrEval(%p,%p)", pArea, pEvalInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_DBEVAL ) )
      return SUPER_DBEVAL( pArea, pEvalInfo );

   pItem = hb_usrEvalInfoToItem( pEvalInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_itemRelease( pItem );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrPack( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrPack(%p)", pArea));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_PACK ) )
      return SUPER_PACK( pArea );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmDo( 1 );

   return hb_usrReturn();
}

static ERRCODE hb_usrPackRec( AREAP pArea, ULONG ulRecNo, BOOL * pWritten )
{
   LONG lOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrPackRec(%p,%lu,%p)", pArea, ulRecNo, pWritten));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_xvmPushLogical( TRUE );
   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_PACKREC ) )
   {
      hb_stackPop();
      return SUPER_PACKREC( pArea, ulRecNo, pWritten );
   }

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushLong( ulRecNo );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 3 );

   if( hb_xvmPopLogical( pWritten ) )
   {
      hb_ret();
      return FAILURE;
   }

   return hb_usrReturn();
}

static ERRCODE hb_usrSort( AREAP pArea, LPDBSORTINFO pSortInfo )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrSort(%p,%p)", pArea, pSortInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SORT ) )
      return SUPER_SORT( pArea, pSortInfo );

   pItem = hb_usrSortInfoToItem( pSortInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_itemRelease( pItem );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrTrans( AREAP pArea, LPDBTRANSINFO pTransInfo )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrTrans(%p,%p)", pArea, pTransInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_TRANS ) )
      return SUPER_TRANS( pArea, pTransInfo );

   pItem = hb_usrTransInfoToItem( pTransInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_itemRelease( pItem );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrTransRec( AREAP pArea, LPDBTRANSINFO pTransInfo )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrTransRec(%p,%p)", pArea, pTransInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_TRANSREC ) )
      return SUPER_TRANSREC( pArea, pTransInfo );

   pItem = hb_usrTransInfoToItem( pTransInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_itemRelease( pItem );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrZap( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrZap(%p)", pArea));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ZAP ) )
      return SUPER_ZAP( pArea );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmDo( 1 );

   return hb_usrReturn();
}

/*
 * Relational Methods
 */

static ERRCODE hb_usrChildEnd( AREAP pArea, LPDBRELINFO pRelInfo )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrChildEnd(%p,%p)", pArea, pRelInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CHILDEND ) )
      return SUPER_CHILDEND( pArea, pRelInfo );

   pItem = hb_usrRelInfoToItem( pRelInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_itemRelease( pItem );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrChildStart( AREAP pArea, LPDBRELINFO pRelInfo )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrChildStart(%p,%p)", pArea, pRelInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CHILDSTART ) )
      return SUPER_CHILDSTART( pArea, pRelInfo );

   pItem = hb_usrRelInfoToItem( pRelInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_itemRelease( pItem );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrChildSync( AREAP pArea, LPDBRELINFO pRelInfo )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrChildSync(%p,%p)", pArea, pRelInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CHILDSYNC ) )
      return SUPER_CHILDSYNC( pArea, pRelInfo );

   pItem = hb_usrRelInfoToItem( pRelInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_itemRelease( pItem );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrSyncChildren( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrSyncChildren(%p)", pArea));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SYNCCHILDREN ) )
      return SUPER_SYNCCHILDREN( pArea );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmDo( 1 );

   return hb_usrReturn();
}

static ERRCODE hb_usrClearRel( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrClearRel(%p)", pArea));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CLEARREL ) )
      return SUPER_CLEARREL( pArea );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmDo( 1 );

   return hb_usrReturn();
}

static ERRCODE hb_usrForceRel( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrForceRel(%p)", pArea));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_FORCEREL ) )
      return SUPER_FORCEREL( pArea );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmDo( 1 );

   return hb_usrReturn();
}

static ERRCODE hb_usrRelArea( AREAP pArea, USHORT uiRelNo, void * pRelArea )
{
   LONG lOffset;
   USHORT * puiRelArea = ( USHORT * ) pRelArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrRelArea(%p,%hu,%p)", pArea, uiRelNo, pRelArea));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_vmPushInteger( 0 );
   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_RELAREA ) )
   {
      hb_stackPop();
      return SUPER_RELAREA( pArea, uiRelNo, pRelArea );
   }

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushInteger( uiRelNo );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 3 );

   * puiRelArea = hb_itemGetNI( hb_stackItemFromBase( lOffset ) );
   hb_stackPop();

   return hb_usrReturn();
}

static ERRCODE hb_usrRelEval( AREAP pArea, LPDBRELINFO pRelInfo )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrRelEval(%p,%p)", pArea, pRelInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_RELEVAL ) )
      return SUPER_RELEVAL( pArea, pRelInfo );

   pItem = hb_usrRelInfoToItem( pRelInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_itemRelease( pItem );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrRelText( AREAP pArea, USHORT uiRelNo, void * pExpr )
{
   LONG lOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrRelText(%p,%hu,%p)", pArea, uiRelNo, pExpr));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_stackPush();
   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_RELTEXT ) )
   {
      hb_stackPop();
      return SUPER_RELTEXT( pArea, uiRelNo, pExpr );
   }

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushInteger( uiRelNo );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 3 );

   hb_strncpy( ( char * ) pExpr, hb_itemGetCPtr( hb_stackItemFromBase( lOffset ) ),
               HARBOUR_MAX_RDD_RELTEXT_LENGTH );
   hb_stackPop();

   return hb_usrReturn();
}

static ERRCODE hb_usrSetRel( AREAP pArea, LPDBRELINFO pRelInfo )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrSetRel(%p,%p)", pArea, pRelInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SETREL ) )
      return SUPER_SETREL( pArea, pRelInfo );

   pItem = hb_usrRelInfoToItem( pRelInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_itemRelease( pItem );
   hb_vmDo( 2 );

   return hb_usrReturn();
}


/*
 * Order Management
 */

static ERRCODE hb_usrOrderListAdd( AREAP pArea, LPDBORDERINFO pOrderInfo )
{
   PHB_ITEM pItem, pResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrOrderListAdd(%p,%p)", pArea, pOrderInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ORDLSTADD ) )
      return SUPER_ORDLSTADD( pArea, pOrderInfo );

   pItem = hb_usrOrderInfoToItem( pOrderInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_vmDo( 2 );

   pResult = hb_arrayGetItemPtr( pItem, UR_ORI_RESULT );
   if( pResult && !HB_IS_NIL( pResult ) )
   {
      if( pOrderInfo->itmResult )
         hb_itemCopy( pOrderInfo->itmResult, pResult );
      else
         pOrderInfo->itmResult = hb_itemNew( pResult );
   }
   hb_itemRelease( pItem );

   return hb_usrReturn();
}

static ERRCODE hb_usrOrderListClear( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrOrderListClear(%p)", pArea));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ORDLSTCLEAR ) )
      return SUPER_ORDLSTCLEAR( pArea );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmDo( 1 );

   return hb_usrReturn();
}

static ERRCODE hb_usrOrderListDelete( AREAP pArea, LPDBORDERINFO pOrderInfo )
{
   PHB_ITEM pItem, pResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrOrderListDelete(%p,%p)", pArea, pOrderInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ORDLSTDELETE ) )
      return SUPER_ORDLSTDELETE( pArea, pOrderInfo );

   pItem = hb_usrOrderInfoToItem( pOrderInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_vmDo( 2 );

   pResult = hb_arrayGetItemPtr( pItem, UR_ORI_RESULT );
   if( pResult && !HB_IS_NIL( pResult ) )
   {
      if( pOrderInfo->itmResult )
         hb_itemCopy( pOrderInfo->itmResult, pResult );
      else
         pOrderInfo->itmResult = hb_itemNew( pResult );
   }
   hb_itemRelease( pItem );

   return hb_usrReturn();
}

static ERRCODE hb_usrOrderListFocus( AREAP pArea, LPDBORDERINFO pOrderInfo )
{
   PHB_ITEM pItem, pResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrOrderListFocus(%p,%p)", pArea, pOrderInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ORDLSTFOCUS ) )
      return SUPER_ORDLSTFOCUS( pArea, pOrderInfo );

   pItem = hb_usrOrderInfoToItem( pOrderInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_vmDo( 2 );

   pResult = hb_arrayGetItemPtr( pItem, UR_ORI_RESULT );
   if( pResult && !HB_IS_NIL( pResult ) )
   {
      if( pOrderInfo->itmResult )
         hb_itemCopy( pOrderInfo->itmResult, pResult );
      else
         pOrderInfo->itmResult = hb_itemNew( pResult );
   }
   hb_itemRelease( pItem );

   return hb_usrReturn();
}

static ERRCODE hb_usrOrderListRebuild( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrOrderListRebuild(%p)", pArea));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ORDLSTREBUILD ) )
      return SUPER_ORDLSTREBUILD( pArea );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmDo( 1 );

   return hb_usrReturn();
}

static ERRCODE hb_usrOrderCondition( AREAP pArea, LPDBORDERCONDINFO pOrderCondInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrOrderCondition(%p,%p)", pArea, pOrderCondInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ORDSETCOND ) )
      return SUPER_ORDSETCOND( pArea, pOrderCondInfo );


   hb_vmPushInteger( pArea->uiArea );
   if( pOrderCondInfo )
   {
      PHB_ITEM pItem = hb_usrOrderCondInfoToItem( pOrderCondInfo );
      hb_vmPush( pItem );
      hb_itemRelease( pItem );
   }
   else
   {
      hb_vmPushNil();
   }
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrOrderCreate( AREAP pArea, LPDBORDERCREATEINFO pOrderCreateInfo )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrOrderCreate(%p,%p)", pArea, pOrderCreateInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ORDCREATE ) )
      return SUPER_ORDCREATE( pArea, pOrderCreateInfo );

   pItem = hb_usrOrderCreateInfoToItem( pOrderCreateInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_itemRelease( pItem );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrOrderDestroy( AREAP pArea, LPDBORDERINFO pOrderInfo )
{
   PHB_ITEM pItem, pResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrOrderDestroy(%p,%p)", pArea, pOrderInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ORDDESTROY ) )
      return SUPER_ORDDESTROY( pArea, pOrderInfo );

   pItem = hb_usrOrderInfoToItem( pOrderInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_vmDo( 2 );

   pResult = hb_arrayGetItemPtr( pItem, UR_ORI_RESULT );
   if( pResult && !HB_IS_NIL( pResult ) )
   {
      if( pOrderInfo->itmResult )
         hb_itemCopy( pOrderInfo->itmResult, pResult );
      else
         pOrderInfo->itmResult = hb_itemNew( pResult );
   }
   hb_itemRelease( pItem );

   return hb_usrReturn();
}

static ERRCODE hb_usrOrderInfo( AREAP pArea, USHORT uiIndex, LPDBORDERINFO pOrderInfo )
{
   PHB_ITEM pItem, pResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrOrderInfo(%p,%hu,%p)", pArea, uiIndex, pOrderInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ORDINFO ) )
      return SUPER_ORDINFO( pArea, uiIndex, pOrderInfo );

   pItem = hb_usrOrderInfoToItem( pOrderInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_vmDo( 2 );

   pResult = hb_arrayGetItemPtr( pItem, UR_ORI_RESULT );
   if( pResult && !HB_IS_NIL( pResult ) )
   {
      if( pOrderInfo->itmResult )
         hb_itemCopy( pOrderInfo->itmResult, pResult );
      else
         pOrderInfo->itmResult = hb_itemNew( pResult );
   }
   hb_itemRelease( pItem );

   return hb_usrReturn();
}


/*
 * Filters and Scope Settings
 */

static ERRCODE hb_usrClearFilter( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrClearFilter(%p)", pArea));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CLEARFILTER ) )
      return SUPER_CLEARFILTER( pArea );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmDo( 1 );

   return hb_usrReturn();
}

static ERRCODE hb_usrClearLocate( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrClearLocate(%p)", pArea));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CLEARLOCATE ) )
      return SUPER_CLEARLOCATE( pArea );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmDo( 1 );

   return hb_usrReturn();
}

static ERRCODE hb_usrClearScope( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrClearScope(%p)", pArea));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CLEARSCOPE ) )
      return SUPER_CLEARSCOPE( pArea );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmDo( 1 );

   return hb_usrReturn();
}

static ERRCODE hb_usrFilterText( AREAP pArea, PHB_ITEM pFilter )
{
   LONG lOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrFilterText(%p,%hu,%p)", pArea, uiRelNo, pFilter));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_vmPush( pFilter );
   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_FILTERTEXT ) )
   {
      hb_stackPop();
      return SUPER_FILTERTEXT( pArea, pFilter );
   }

   hb_vmPushInteger( pArea->uiArea );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 2 );

   hb_itemCopy( pFilter, hb_stackItemFromBase( lOffset ) );
   hb_stackPop();

   return hb_usrReturn();
}

static ERRCODE hb_usrSetFilter( AREAP pArea, LPDBFILTERINFO pFilterInfo )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrSetFilter(%p,%p)", pArea, pFilterInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SETFILTER ) )
      return SUPER_SETFILTER( pArea, pFilterInfo );

   pItem = hb_usrFilterInfoToItem( pFilterInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_itemRelease( pItem );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrSetLocate( AREAP pArea, LPDBSCOPEINFO pScopeInfo )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrSetLocate(%p,%p)", pArea, pScopeInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_SETLOCATE ) )
      return SUPER_SETLOCATE( pArea, pScopeInfo );

   pItem = hb_usrScopeInfoToItem( pScopeInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_itemRelease( pItem );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrLocate( AREAP pArea, BOOL fContinue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrLocate(%p,%d)", pArea, fContinue));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_LOCATE ) )
      return SUPER_LOCATE( pArea, fContinue );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushLogical( fContinue );
   hb_vmDo( 2 );

   return hb_usrReturn();
}


/*
 * Miscellaneous
 */

static ERRCODE hb_usrCompile( AREAP pArea, BYTE * szExpr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrCompile(%p,%p)", pArea, szExpr));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_COMPILE ) )
      return SUPER_COMPILE( pArea, szExpr );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushString( ( char * ) szExpr, strlen( ( char * ) szExpr ) );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrError( AREAP pArea, PHB_ITEM pError )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrError(%p,%p)", pArea, pError));

   if( !pArea )
   {
      hb_errPutSeverity( pError, ES_ERROR );
      hb_errPutSubSystem( pError, "???DRIVER" );
      return hb_errLaunch( pError );
   }

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_ERROR ) )
      return SUPER_ERROR( pArea, pError );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pError );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrEvalBlock( AREAP pArea, PHB_ITEM pBlock )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrEvalBlock(%p,%p)", pArea, pBlock));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_EVALBLOCK ) )
      return SUPER_EVALBLOCK( pArea, pBlock );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pBlock );
   hb_vmDo( 2 );

   return hb_usrReturn();
}


/*
 * Network operations
 */
 
static ERRCODE hb_usrRawLock( AREAP pArea, USHORT uiAction, ULONG ulRecNo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrRawLock(%p,%hu,%lu)", pArea, uiAction, ulRecNo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_RAWLOCK ) )
      return SUPER_RAWLOCK( pArea, uiAction, ulRecNo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushInteger( uiAction );
   hb_vmPushLong( ulRecNo );
   hb_vmDo( 3 );

   return hb_usrReturn();
}

static ERRCODE hb_usrLock( AREAP pArea, LPDBLOCKINFO pLockInfo )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrLock(%p,%p)", pArea, pLockInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_LOCK ) )
      return SUPER_LOCK( pArea, pLockInfo );

   pItem = hb_usrLockInfoToItem( pLockInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_vmDo( 2 );

   pLockInfo->fResult = hb_arrayGetL( pItem, UR_LI_RESULT );
   hb_itemRelease( pItem );

   return hb_usrReturn();
}

static ERRCODE hb_usrUnLock( AREAP pArea, PHB_ITEM pRecNo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrUnLock(%p,%p)", pArea, pRecNo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_UNLOCK ) )
      return SUPER_UNLOCK( pArea, pRecNo );

   hb_vmPushInteger( pArea->uiArea );
   if( pRecNo )
      hb_vmPush( pRecNo );
   else
      hb_vmPushNil();
   hb_vmDo( 2 );

   return hb_usrReturn();
}


/*
 * Memofile functions
 */

static ERRCODE hb_usrCloseMemFile( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrCloseMemFile(%p)", pArea));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CLOSEMEMFILE ) )
      return SUPER_CLOSEMEMFILE( pArea );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmDo( 1 );

   return hb_usrReturn();
}

static ERRCODE hb_usrCreateMemFile( AREAP pArea, LPDBOPENINFO pOpenInfo )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrCreateMemFile(%p,%p)", pArea, pOpenInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_CREATEMEMFILE ) )
      return SUPER_CREATEMEMFILE( pArea, pOpenInfo );

   pItem = hb_usrOpenInfoToItem( pOpenInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_itemRelease( pItem );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrOpenMemFile( AREAP pArea, LPDBOPENINFO pOpenInfo )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrOpenMemFile(%p,%p)", pArea, pOpenInfo));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_OPENMEMFILE ) )
      return SUPER_OPENMEMFILE( pArea, pOpenInfo );

   pItem = hb_usrOpenInfoToItem( pOpenInfo );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPush( pItem );
   hb_itemRelease( pItem );
   hb_vmDo( 2 );

   return hb_usrReturn();
}

static ERRCODE hb_usrGetValueFile( AREAP pArea, USHORT uiIndex, void * szFile, USHORT uiMode )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrGetValueFile(%p,%hu,%p,%hu)", pArea, uiIndex, szFile, uiMode));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_GETVALUEFILE ) )
      return SUPER_GETVALUEFILE( pArea, uiIndex, szFile, uiMode );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushInteger( uiIndex );
   hb_vmPushString( ( char * ) szFile, strlen( ( char * ) szFile ) );
   hb_vmPushInteger( uiMode );
   hb_vmDo( 4 );

   return hb_usrReturn();
}

static ERRCODE hb_usrPutValueFile( AREAP pArea, USHORT uiIndex, void * szFile, USHORT uiMode )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrPutValueFile(%p,%hu,%p,%hu)", pArea, uiIndex, szFile, uiMode));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_PUTVALUEFILE ) )
      return SUPER_PUTVALUEFILE( pArea, uiIndex, szFile, uiMode );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmPushInteger( uiIndex );
   hb_vmPushString( ( char * ) szFile, strlen( ( char * ) szFile ) );
   hb_vmPushInteger( uiMode );
   hb_vmDo( 4 );

   return hb_usrReturn();
}


/*
 * Database file header handling
 */

static ERRCODE hb_usrReadDBHeader( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrReadDBHeader(%p)", pArea));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_READDBHEADER ) )
      return SUPER_READDBHEADER( pArea );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmDo( 1 );

   return hb_usrReturn();
}

static ERRCODE hb_usrWriteDBHeader( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrWriteDBHeader(%p)", pArea));

   if( !hb_usrPushMethod( SELF_USRNODE( pArea )->pMethods, UR_WRITEDBHEADER ) )
      return SUPER_WRITEDBHEADER( pArea );

   hb_vmPushInteger( pArea->uiArea );
   hb_vmDo( 1 );

   return hb_usrReturn();
}


/*
 * non WorkArea functions
 */

static ERRCODE hb_usrDrop( LPRDDNODE pRDD, PHB_ITEM pTable, PHB_ITEM pIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrDrop(%p,%p,%p)", pRDD, pTable, pIndex));

   if( !hb_usrPushMethod( SELF_USRNODE( pRDD )->pMethods, UR_DROP ) )
      return SUPER_DROP( pRDD, pTable, pIndex );

   hb_vmPushInteger( pRDD->rddID );
   hb_vmPush( pTable );
   hb_vmPush( pIndex );
   hb_vmDo( 3 );

   return hb_usrReturn();
}

static ERRCODE hb_usrExists( LPRDDNODE pRDD, PHB_ITEM pTable, PHB_ITEM pIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_usrExists(%p,%p,%p)", pRDD, pTable, pIndex));

   if( !hb_usrPushMethod( SELF_USRNODE( pRDD )->pMethods, UR_EXISTS ) )
      return SUPER_EXISTS( pRDD, pTable, pIndex );

   hb_vmPushInteger( pRDD->rddID );
   hb_vmPush( pTable );
   hb_vmPush( pIndex );
   hb_vmDo( 3 );

   return hb_usrReturn();
}

static ERRCODE hb_usrRddInfo( LPRDDNODE pRDD, USHORT uiInfoType, ULONG ulConnection, PHB_ITEM pInfo )
{
   LONG lOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_usrRddInfo(%p,%hu,%lu,%p)", pRDD, uiInfoType, ulConnection, pInfo));

   lOffset = hb_stackTopOffset() - hb_stackBaseOffset();
   hb_vmPush( pInfo );
   if( !hb_usrPushMethod( SELF_USRNODE( pRDD )->pMethods, UR_RDDINFO ) )
   {
      hb_stackPop();
      return SUPER_RDDINFO( pRDD, uiInfoType, ulConnection, pInfo );
   }

   hb_vmPushInteger( pRDD->rddID );
   hb_vmPushInteger( uiInfoType );
   hb_vmPushLong( ulConnection );
   hb_xvmPushLocalByRef( lOffset );
   hb_vmDo( 4 );

   hb_itemCopy( pInfo, hb_stackItemFromBase( lOffset ) );
   hb_stackPop();

   return hb_usrReturn();
}


static RDDFUNCS usrFuncTable =
{
   /* Movement and positioning methods */
   /* ( DBENTRYP_BP )   */ hb_usrBof,         /* Bof        */
   /* ( DBENTRYP_BP )   */ hb_usrEof,         /* Eof        */
   /* ( DBENTRYP_BP )   */ hb_usrFound,       /* Found      */
   /* ( DBENTRYP_V )    */ hb_usrGoBottom,    /* GoBottom   */
   /* ( DBENTRYP_UL )   */ hb_usrGoTo,        /* GoTo       */
   /* ( DBENTRYP_I )    */ hb_usrGoToId,      /* GoToId     */
   /* ( DBENTRYP_V )    */ hb_usrGoTop,       /* GoTop      */
   /* ( DBENTRYP_BIB )  */ hb_usrSeek,        /* Seek       */
   /* ( DBENTRYP_L )    */ hb_usrSkip,        /* Skip       */
   /* ( DBENTRYP_L )    */ hb_usrSkipFilter,  /* SkipFilter */
   /* ( DBENTRYP_L )    */ hb_usrSkipRaw,     /* SkipRaw    */

   /* Data management */
   /* ( DBENTRYP_VF )   */ hb_usrAddField,    /* AddField       */
   /* ( DBENTRYP_B )    */ hb_usrAppend,      /* Append         */
   /* ( DBENTRYP_I )    */ hb_usrCreateFields,/* CreateFields   */
   /* ( DBENTRYP_V )    */ hb_usrDelete,      /* DeleteRec      */
   /* ( DBENTRYP_BP )   */ hb_usrDeleted,     /* Deleted        */
   /* ( DBENTRYP_SP )   */ hb_usrFieldCount,  /* FieldCount     */
   /* ( DBENTRYP_VF )   */ hb_usrFieldDisplay,/* FieldDisplay   */
   /* ( DBENTRYP_SSI )  */ hb_usrFieldInfo,   /* FieldInfo      */
   /* ( DBENTRYP_SVP )  */ hb_usrFieldName,   /* FieldName      */
   /* ( DBENTRYP_V )    */ hb_usrFlush,       /* Flush          */
   /* ( DBENTRYP_PP )   */ hb_usrGetRec,      /* GetRec         */
   /* ( DBENTRYP_SI )   */ hb_usrGetValue,    /* GetValue       */
   /* ( DBENTRYP_SVL )  */ hb_usrGetVarLen,   /* GetVarLen      */
   /* ( DBENTRYP_V )    */ hb_usrGoCold,      /* GoCold         */
   /* ( DBENTRYP_V )    */ hb_usrGoHot,       /* GoHot          */
   /* ( DBENTRYP_P )    */ hb_usrPutRec,      /* PutRec         */
   /* ( DBENTRYP_SI )   */ hb_usrPutValue,    /* PutValue       */
   /* ( DBENTRYP_V )    */ hb_usrRecall,      /* Recall         */
   /* ( DBENTRYP_ULP )  */ hb_usrRecCount,    /* RecCount       */
   /* ( DBENTRYP_ISI )  */ hb_usrRecInfo,     /* RecInfo        */
   /* ( DBENTRYP_ULP )  */ hb_usrRecNo,       /* RecNo          */
   /* ( DBENTRYP_I )    */ hb_usrRecId,       /* RecId          */
   /* ( DBENTRYP_S )    */ hb_usrSetFieldExtent, /* SetFieldExtent */

   /* WorkArea/Database management */
   /* ( DBENTRYP_P )    */ hb_usrAlias,       /* Alias       */
   /* ( DBENTRYP_V )    */ hb_usrClose,       /* Close       */
   /* ( DBENTRYP_VP )   */ hb_usrCreate,      /* Create      */
   /* ( DBENTRYP_SI )   */ hb_usrInfo,        /* Info        */
   /* ( DBENTRYP_V )    */ NULL, /* RDD */    /* NewArea     */
   /* ( DBENTRYP_VP )   */ hb_usrOpen,        /* Open        */
   /* ( DBENTRYP_V )    */ NULL, /* RDD */    /* Release     */
   /* ( DBENTRYP_SP )   */ NULL, /* RDD */    /* StructSize  */
   /* ( DBENTRYP_P )    */ NULL, /* RDD */    /* SysName     */
   /* ( DBENTRYP_VEI )  */ hb_usrEval,        /* Eval        */
   /* ( DBENTRYP_V )    */ hb_usrPack,        /* Pack        */
   /* ( DBENTRYP_LSP )  */ hb_usrPackRec,     /* PackRec     */
   /* ( DBENTRYP_VS )   */ hb_usrSort,        /* Sort        */
   /* ( DBENTRYP_VT )   */ hb_usrTrans,       /* Trans       */
   /* ( DBENTRYP_VT )   */ hb_usrTransRec,    /* TransRec    */
   /* ( DBENTRYP_V )    */ hb_usrZap,         /* Zap         */

   /* Relational Methods */
   /* ( DBENTRYP_VR )   */ hb_usrChildEnd,    /* ChildEnd      */
   /* ( DBENTRYP_VR )   */ hb_usrChildStart,  /* ChildStart    */
   /* ( DBENTRYP_VR )   */ hb_usrChildSync,   /* ChildSync     */
   /* ( DBENTRYP_V )    */ hb_usrSyncChildren,/* SyncChildren  */
   /* ( DBENTRYP_V )    */ hb_usrClearRel,    /* ClearRel      */
   /* ( DBENTRYP_V )    */ hb_usrForceRel,    /* ForceRel      */
   /* ( DBENTRYP_SVP )  */ hb_usrRelArea,     /* RelArea       */
   /* ( DBENTRYP_VR )   */ hb_usrRelEval,     /* RelEval       */
   /* ( DBENTRYP_SVP )  */ hb_usrRelText,     /* RelText       */
   /* ( DBENTRYP_VR )   */ hb_usrSetRel,      /* SetRel        */

   /* Order Management */
   /* ( DBENTRYP_OI )   */ hb_usrOrderListAdd,     /* OrderListAdd      */
   /* ( DBENTRYP_V )    */ hb_usrOrderListClear,   /* OrderListClear    */
   /* ( DBENTRYP_OI )   */ hb_usrOrderListDelete,  /* OrderListDelete   */
   /* ( DBENTRYP_OI )   */ hb_usrOrderListFocus,   /* OrderListFocus    */
   /* ( DBENTRYP_V )    */ hb_usrOrderListRebuild, /* OrderListRebuild  */
   /* ( DBENTRYP_VOI )  */ hb_usrOrderCondition,   /* OrderCondition    */
   /* ( DBENTRYP_VOC )  */ hb_usrOrderCreate,      /* OrderCreate       */
   /* ( DBENTRYP_OI )   */ hb_usrOrderDestroy,     /* OrderDestroy      */
   /* ( DBENTRYP_OII )  */ hb_usrOrderInfo,        /* OrderInfo         */

   /* Filters and Scope Settings */
   /* ( DBENTRYP_V )    */ hb_usrClearFilter, /* ClearFilter  */
   /* ( DBENTRYP_V )    */ hb_usrClearLocate, /* ClearLocate  */
   /* ( DBENTRYP_V )    */ hb_usrClearScope,  /* ClearScope   */
   /* ( DBENTRYP_VPLP ) */ NULL,              /* CountScope   */
   /* ( DBENTRYP_I )    */ hb_usrFilterText,  /* FilterText   */
   /* ( DBENTRYP_SI )   */ NULL,              /* ScopeInfo    */
   /* ( DBENTRYP_VFI )  */ hb_usrSetFilter,   /* SetFilter    */
   /* ( DBENTRYP_VLO )  */ hb_usrSetLocate,   /* SetLocate    */
   /* ( DBENTRYP_VOS )  */ NULL,              /* SetScope     */
   /* ( DBENTRYP_VPL )  */ NULL,              /* SkipScope    */
   /* ( DBENTRYP_B )    */ hb_usrLocate,      /* Locate       */

   /* Miscellaneous */
   /* ( DBENTRYP_P )    */ hb_usrCompile,     /* Compile    */
   /* ( DBENTRYP_I )    */ hb_usrError,       /* Error      */
   /* ( DBENTRYP_I )    */ hb_usrEvalBlock,   /* EvalBlock  */

   /* Network operations */
   /* ( DBENTRYP_VSP )  */ hb_usrRawLock,     /* RawLock  */
   /* ( DBENTRYP_VL )   */ hb_usrLock,        /* Lock     */
   /* ( DBENTRYP_I )    */ hb_usrUnLock,      /* UnLock   */

   /* Memofile functions */
   /* ( DBENTRYP_V )    */ hb_usrCloseMemFile,  /* CloseMemFile   */
   /* ( DBENTRYP_VP )   */ hb_usrCreateMemFile, /* CreateMemFile  */
   /* ( DBENTRYP_SVPB ) */ hb_usrGetValueFile,  /* GetValueFile   */
   /* ( DBENTRYP_VP )   */ hb_usrOpenMemFile,   /* OpenMemFile    */
   /* ( DBENTRYP_SVPB ) */ hb_usrPutValueFile,  /* PutValueFile   */

   /* Database file header handling */
   /* ( DBENTRYP_V )    */ hb_usrReadDBHeader,  /* ReadDBHeader   */
   /* ( DBENTRYP_V )    */ hb_usrWriteDBHeader, /* WriteDBHeader  */

   /* non WorkArea functions */
   /* ( DBENTRYP_R )    */ NULL, /* RDD */    /* Init    */
   /* ( DBENTRYP_R )    */ NULL, /* RDD */    /* Exit    */
   /* ( DBENTRYP_RVV )  */ hb_usrDrop,        /* Drop    */
   /* ( DBENTRYP_RVV )  */ hb_usrExists,      /* Exists  */
   /* ( DBENTRYP_RSLV ) */ hb_usrRddInfo,     /* RddInfo */

   /* Special and reserved methods */
   /* ( DBENTRYP_SVP )  */ NULL               /* WhoCares */
};

static RDDFUNCS rddFuncTable =
{
   /* Movement and positioning methods */
   /* ( DBENTRYP_BP )   */ NULL,              /* Bof        */
   /* ( DBENTRYP_BP )   */ NULL,              /* Eof        */
   /* ( DBENTRYP_BP )   */ NULL,              /* Found      */
   /* ( DBENTRYP_V )    */ NULL,              /* GoBottom   */
   /* ( DBENTRYP_UL )   */ NULL,              /* GoTo       */
   /* ( DBENTRYP_I )    */ NULL,              /* GoToId     */
   /* ( DBENTRYP_V )    */ NULL,              /* GoTop      */
   /* ( DBENTRYP_BIB )  */ NULL,              /* Seek       */
   /* ( DBENTRYP_L )    */ NULL,              /* Skip       */
   /* ( DBENTRYP_L )    */ NULL,              /* SkipFilter */
   /* ( DBENTRYP_L )    */ NULL,              /* SkipRaw    */

   /* Data management */
   /* ( DBENTRYP_VF )   */ NULL,              /* AddField       */
   /* ( DBENTRYP_B )    */ NULL,              /* Append         */
   /* ( DBENTRYP_I )    */ NULL,              /* CreateFields   */
   /* ( DBENTRYP_V )    */ NULL,              /* DeleteRec      */
   /* ( DBENTRYP_BP )   */ NULL,              /* Deleted        */
   /* ( DBENTRYP_SP )   */ NULL,              /* FieldCount     */
   /* ( DBENTRYP_VF )   */ NULL,              /* FieldDisplay   */
   /* ( DBENTRYP_SSI )  */ NULL,              /* FieldInfo      */
   /* ( DBENTRYP_SVP )  */ NULL,              /* FieldName      */
   /* ( DBENTRYP_V )    */ NULL,              /* Flush          */
   /* ( DBENTRYP_PP )   */ NULL,              /* GetRec         */
   /* ( DBENTRYP_SI )   */ NULL,              /* GetValue       */
   /* ( DBENTRYP_SVL )  */ NULL,              /* GetVarLen      */
   /* ( DBENTRYP_V )    */ NULL,              /* GoCold         */
   /* ( DBENTRYP_V )    */ NULL,              /* GoHot          */
   /* ( DBENTRYP_P )    */ NULL,              /* PutRec         */
   /* ( DBENTRYP_SI )   */ NULL,              /* PutValue       */
   /* ( DBENTRYP_V )    */ NULL,              /* Recall         */
   /* ( DBENTRYP_ULP )  */ NULL,              /* RecCount       */
   /* ( DBENTRYP_ISI )  */ NULL,              /* RecInfo        */
   /* ( DBENTRYP_ULP )  */ NULL,              /* RecNo          */
   /* ( DBENTRYP_I )    */ NULL,              /* RecId          */
   /* ( DBENTRYP_S )    */ NULL,              /* SetFieldExtent */

   /* WorkArea/Database management */
   /* ( DBENTRYP_P )    */ NULL,              /* Alias       */
   /* ( DBENTRYP_V )    */ NULL,              /* Close       */
   /* ( DBENTRYP_VP )   */ NULL,              /* Create      */
   /* ( DBENTRYP_SI )   */ NULL,              /* Info        */
   /* ( DBENTRYP_V )    */ hb_usrNewArea,     /* NewArea     */
   /* ( DBENTRYP_VP )   */ NULL,              /* Open        */
   /* ( DBENTRYP_V )    */ hb_usrRelease,     /* Release     */
   /* ( DBENTRYP_SP )   */ hb_usrStructSize,  /* StructSize  */
   /* ( DBENTRYP_P )    */ hb_usrSysName,     /* SysName     */
   /* ( DBENTRYP_VEI )  */ NULL,              /* Eval        */
   /* ( DBENTRYP_V )    */ NULL,              /* Pack        */
   /* ( DBENTRYP_LSP )  */ NULL,              /* PackRec     */
   /* ( DBENTRYP_VS )   */ NULL,              /* Sort        */
   /* ( DBENTRYP_VT )   */ NULL,              /* Trans       */
   /* ( DBENTRYP_VT )   */ NULL,              /* TransRec    */
   /* ( DBENTRYP_V )    */ NULL,              /* Zap         */

   /* Relational Methods */
   /* ( DBENTRYP_VR )   */ NULL,              /* ChildEnd      */
   /* ( DBENTRYP_VR )   */ NULL,              /* ChildStart    */
   /* ( DBENTRYP_VR )   */ NULL,              /* ChildSync     */
   /* ( DBENTRYP_V )    */ NULL,              /* SyncChildren  */
   /* ( DBENTRYP_V )    */ NULL,              /* ClearRel      */
   /* ( DBENTRYP_V )    */ NULL,              /* ForceRel      */
   /* ( DBENTRYP_SVP )  */ NULL,              /* RelArea       */
   /* ( DBENTRYP_VR )   */ NULL,              /* RelEval       */
   /* ( DBENTRYP_SVP )  */ NULL,              /* RelText       */
   /* ( DBENTRYP_VR )   */ NULL,              /* SetRel        */

   /* Order Management */
   /* ( DBENTRYP_OI )   */ NULL,              /* OrderListAdd      */
   /* ( DBENTRYP_V )    */ NULL,              /* OrderListClear    */
   /* ( DBENTRYP_OI )   */ NULL,              /* OrderListDelete   */
   /* ( DBENTRYP_OI )   */ NULL,              /* OrderListFocus    */
   /* ( DBENTRYP_V )    */ NULL,              /* OrderListRebuild  */
   /* ( DBENTRYP_VOI )  */ NULL,              /* OrderCondition    */
   /* ( DBENTRYP_VOC )  */ NULL,              /* OrderCreate       */
   /* ( DBENTRYP_OI )   */ NULL,              /* OrderDestroy      */
   /* ( DBENTRYP_OII )  */ NULL,              /* OrderInfo         */

   /* Filters and Scope Settings */
   /* ( DBENTRYP_V )    */ NULL,              /* ClearFilter  */
   /* ( DBENTRYP_V )    */ NULL,              /* ClearLocate  */
   /* ( DBENTRYP_V )    */ NULL,              /* ClearScope   */
   /* ( DBENTRYP_VPLP ) */ NULL,              /* CountScope   */
   /* ( DBENTRYP_I )    */ NULL,              /* FilterText   */
   /* ( DBENTRYP_SI )   */ NULL,              /* ScopeInfo    */
   /* ( DBENTRYP_VFI )  */ NULL,              /* SetFilter    */
   /* ( DBENTRYP_VLO )  */ NULL,              /* SetLocate    */
   /* ( DBENTRYP_VOS )  */ NULL,              /* SetScope     */
   /* ( DBENTRYP_VPL )  */ NULL,              /* SkipScope    */
   /* ( DBENTRYP_B )    */ NULL,              /* Locate       */

   /* Miscellaneous */
   /* ( DBENTRYP_P )    */ NULL,              /* Compile    */
   /* ( DBENTRYP_I )    */ NULL,              /* Error      */
   /* ( DBENTRYP_I )    */ NULL,              /* EvalBlock  */

   /* Network operations */
   /* ( DBENTRYP_VSP )  */ NULL,              /* RawLock  */
   /* ( DBENTRYP_VL )   */ NULL,              /* Lock     */
   /* ( DBENTRYP_I )    */ NULL,              /* UnLock   */

   /* Memofile functions */
   /* ( DBENTRYP_V )    */ NULL,              /* CloseMemFile   */
   /* ( DBENTRYP_VP )   */ NULL,              /* CreateMemFile  */
   /* ( DBENTRYP_SVPB ) */ NULL,              /* GetValueFile   */
   /* ( DBENTRYP_VP )   */ NULL,              /* OpenMemFile    */
   /* ( DBENTRYP_SVPB ) */ NULL,              /* PutValueFile   */

   /* Database file header handling */
   /* ( DBENTRYP_V )    */ NULL,              /* ReadDBHeader   */
   /* ( DBENTRYP_V )    */ NULL,              /* WriteDBHeader  */

   /* non WorkArea functions */
   /* ( DBENTRYP_R )    */ hb_usrInit,        /* Init    */
   /* ( DBENTRYP_R )    */ hb_usrExit,        /* Exit    */
   /* ( DBENTRYP_RVV )  */ NULL,              /* Drop    */
   /* ( DBENTRYP_RVV )  */ NULL,              /* Exists  */
   /* ( DBENTRYP_RSLV ) */ NULL,              /* RddInfo */

   /* Special and reserved methods */
   /* ( DBENTRYP_SVP )  */ NULL               /* WhoCares */
};

HB_FUNC( USRRDD_GETFUNCTABLE )
{
   RDDFUNCS * pSelfTable, * pSuperTable;
   USHORT * puiCount, uiResult, uiCount, uiSize;
   char * szSuperRDD;
   PHB_ITEM pMethods;

   HB_TRACE(HB_TR_DEBUG, ("USRRDD_GETFUNCTABLE()"));

   puiCount = ( USHORT * ) hb_parptr( 1 );
   pSelfTable = ( RDDFUNCS * ) hb_parptr( 2 );
   pSuperTable = ( RDDFUNCS * ) hb_parptr( 3 );
   /* uiRddID = hb_parni( 4 ); */
   szSuperRDD = hb_parc( 5 );
   pMethods = hb_param( 6, HB_IT_ARRAY );

   if( puiCount && pSelfTable && pSuperTable && pMethods )
   {
      RDDFUNCS funcTable;
      DBENTRYP_V * pFunction, * pUsrFunction, * pRddFunction;

      * puiCount = RDDFUNCSCOUNT;
      uiSize = hb_arrayLen( pMethods );

      pUsrFunction = ( DBENTRYP_V * ) &usrFuncTable;
      pRddFunction = ( DBENTRYP_V * ) &rddFuncTable;
      pFunction = ( DBENTRYP_V * ) &funcTable;

      for( uiCount = 1; uiCount <= RDDFUNCSCOUNT; ++uiCount )
      {
         * pFunction = * pRddFunction;
         if( * pFunction == NULL && * pUsrFunction && uiCount <= uiSize &&
             hb_usrIsMethod( pMethods, uiCount ) )
         {
            * pFunction = * pUsrFunction;
         }
         ++pUsrFunction;
         ++pRddFunction;
         ++pFunction;
      }
      uiResult = hb_rddInherit( pSelfTable, &funcTable, pSuperTable, ( BYTE * ) szSuperRDD );
      if( uiResult == SUCCESS )
      {
         pSelfTable->whoCares = ( DBENTRYP_SVP ) hb_itemNew( pMethods );
      }
      hb_retni( uiResult );
   }
   else
      hb_retni( FAILURE );
}

HB_FUNC( USRRDD_RDDDATA )
{
   USHORT uiRddID = hb_parni( 1 );

   if( uiRddID < s_uiUsrNodes && s_pUsrRddNodes[ uiRddID ] )
   {
      PHB_ITEM pItem = s_pUsrRddNodes[ uiRddID ]->pItem;

      hb_itemReturn( pItem );
      if( hb_pcount() >= 2 )
      {
         hb_itemCopy( pItem, hb_param( 2, HB_IT_ANY ) );
      }
   }
}

HB_FUNC( USRRDD_ID )
{
   AREAP pArea;

   if( ISNUM( 1 ) )
      pArea = hb_usrGetAreaPointer( hb_parni( 1 ) );
   else
      pArea = ( AREAP ) hb_parptr( 1 );

   if( pArea && pArea->rddID < s_uiUsrNodes && SELF_USRNODE( pArea ) )
      hb_retni( pArea->rddID );
}

HB_FUNC( USRRDD_AREADATA )
{
   AREAP pArea;

   if( ISNUM( 1 ) )
      pArea = hb_usrGetAreaPointer( hb_parni( 1 ) );
   else
      pArea = ( AREAP ) hb_parptr( 1 );

   if( pArea && pArea->rddID < s_uiUsrNodes && SELF_USRNODE( pArea ) )
   {
      PHB_ITEM pItem = SELF_USRDATA( pArea )->pItem;

      hb_itemReturn( pItem );
      if( hb_pcount() >= 2 )
      {
         hb_itemCopy( pItem, hb_param( 2, HB_IT_ANY ) );
      }
   }
}

HB_FUNC( USRRDD_AREARESULT )
{
   AREAP pArea;

   if( ISNUM( 1 ) )
      pArea = hb_usrGetAreaPointer( hb_parni( 1 ) );
   else
      pArea = ( AREAP ) hb_parptr( 1 );

   if( pArea && pArea->rddID < s_uiUsrNodes && SELF_USRNODE( pArea ) )
   {
      if( !pArea->valResult )
         pArea->valResult = hb_itemNew( NULL );

      hb_itemReturn( pArea->valResult );
      if( hb_pcount() >= 2 )
      {
         hb_itemCopy( pArea->valResult, hb_param( 2, HB_IT_ANY ) );
      }
   }
}

static ERRCODE hb_usrErrorRT( AREAP pArea, USHORT uiGenCode, USHORT uiSubCode )
{
   PHB_ITEM pError;
   ERRCODE iRet = FAILURE;

   if( hb_vmRequestQuery() == 0 )
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, uiGenCode );
      hb_errPutSubCode( pError, uiSubCode );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( uiGenCode ) );
      iRet = SELF_ERROR( pArea, pError );
      hb_errRelease( pError );
   }
   return iRet;
}


static AREAP hb_usrGetAreaParam( int iParams )
{
   AREAP pArea = NULL;

   if( iParams <= hb_pcount() )
   {
      AREAP pArea;

      if( ISNUM( 1 ) )
         pArea = hb_usrGetAreaPointer( hb_parni( 1 ) );
      else
         pArea = ( AREAP ) hb_parptr( 1 );

      if( pArea && pArea->rddID < s_uiUsrNodes && SELF_USRNODE( pArea ) )
         return pArea;
   }

   if( pArea )
      hb_usrErrorRT( pArea, EG_UNSUPPORTED, 0 );
   else if( hb_pcount() > 0 )
      hb_usrErrorRT( pArea, EG_NOTABLE, EDBCMD_NOTABLE );
   else
      hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );

   hb_retni( FAILURE );

   return NULL;
}

static LPRDDNODE hb_usrGetNodeParam( int iParams )
{
   LPRDDNODE pRDD = NULL;
   USHORT uiNode = 0;

   if( iParams <= hb_pcount() )
   {
      uiNode = hb_parni( 1 );
      pRDD = hb_rddGetNode( uiNode );
      if( pRDD && uiNode < s_uiUsrNodes && s_pUsrRddNodes[ uiNode ] )
         return pRDD;
   }

   if( pRDD )
      hb_usrErrorRT( NULL, EG_UNSUPPORTED, 0 );
   else if( uiNode )
      hb_usrErrorRT( NULL, EG_NOTABLE, EDBCMD_NOTABLE );
   else
      hb_usrErrorRT( NULL, EG_ARG, EDBCMD_NOVAR );

   hb_retni( FAILURE );

   return NULL;
}


#define HB_FUNC_UR_SUPER( x )    HB_FUNC( UR_SUPER_##x )


HB_FUNC_UR_SUPER( BOF )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      BOOL fBof;

      hb_retni( SUPER_BOF( pArea, &fBof ) );
      hb_storl( fBof, 2 );
   }
}

HB_FUNC_UR_SUPER( EOF )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      BOOL fEof;

      hb_retni( SUPER_EOF( pArea, &fEof ) );
      hb_storl( fEof, 2 );
   }
}

HB_FUNC_UR_SUPER( FOUND )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      BOOL fFound;

      hb_retni( SUPER_FOUND( pArea, &fFound ) );
      hb_storl( fFound, 2 );
   }
}

HB_FUNC_UR_SUPER( GOBOTTOM )
{
   AREAP pArea = hb_usrGetAreaParam( 1 );

   if( pArea )
      hb_retni( SUPER_GOBOTTOM( pArea ) );
}

HB_FUNC_UR_SUPER( GOTOP )
{
   AREAP pArea = hb_usrGetAreaParam( 1 );

   if( pArea )
      hb_retni( SUPER_GOTOP( pArea ) );
}

HB_FUNC_UR_SUPER( GOTO )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
      hb_retni( SUPER_GOTO( pArea, hb_parnl( 2 ) ) );
}

HB_FUNC_UR_SUPER( GOTOID )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
      hb_retni( SUPER_GOTOID( pArea, hb_param( 2, HB_IT_ANY ) ) );
}

HB_FUNC_UR_SUPER( SEEK )
{
   AREAP pArea = hb_usrGetAreaParam( 4 );

   if( pArea )
      hb_retni( SUPER_SEEK( pArea, hb_parl( 2 ), hb_param( 3, HB_IT_ANY ),
                                   hb_parl( 4 ) ) );
}

HB_FUNC_UR_SUPER( SKIP )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
      hb_retni( SUPER_SKIP( pArea, hb_parnl( 2 ) ) );
}

HB_FUNC_UR_SUPER( SKIPFILTER )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
      hb_retni( SUPER_SKIPFILTER( pArea, hb_parnl( 2 ) ) );
}

HB_FUNC_UR_SUPER( SKIPRAW )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
      hb_retni( SUPER_SKIPRAW( pArea, hb_parnl( 2 ) ) );
}

HB_FUNC_UR_SUPER( DELETED )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      BOOL fDeleted;

      hb_retni( SUPER_DELETED( pArea, &fDeleted ) );
      hb_storl( fDeleted, 2 );
   }
}

HB_FUNC_UR_SUPER( ADDFIELD )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBFIELDINFO dbFieldInfo;

      if( hb_usrItemToFieldInfo( hb_param( 2, HB_IT_ARRAY ), &dbFieldInfo ) )
      {
         hb_retni( SUPER_ADDFIELD( pArea, &dbFieldInfo ) );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( FIELDDISPLAY )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBFIELDINFO dbFieldInfo;

      if( hb_usrItemToFieldInfo( hb_param( 2, HB_IT_ARRAY ), &dbFieldInfo ) )
      {
         hb_retni( SUPER_FIELDDISPLAY( pArea, &dbFieldInfo ) );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( FIELDNAME )
{
   AREAP pArea = hb_usrGetAreaParam( 3 );

   if( pArea )
   {
      char * szName = ( char * ) hb_xgrab( pArea->uiMaxFieldNameLength + 1 );

      hb_retni( SUPER_FIELDNAME( pArea, hb_parni( 2 ), szName ) );
      hb_storc( szName, 3 );
      hb_xfree( szName );
   }
}

HB_FUNC_UR_SUPER( APPEND )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
      hb_retni( SUPER_APPEND( pArea, hb_parl( 2 ) ) );
}

HB_FUNC_UR_SUPER( DELETE )
{
   AREAP pArea = hb_usrGetAreaParam( 1 );

   if( pArea )
      hb_retni( SUPER_DELETE( pArea ) );
}

HB_FUNC_UR_SUPER( RECALL )
{
   AREAP pArea = hb_usrGetAreaParam( 1 );

   if( pArea )
      hb_retni( SUPER_RECALL( pArea ) );
}

HB_FUNC_UR_SUPER( FIELDCOUNT )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      USHORT uiCount;

      hb_retni( SUPER_FIELDCOUNT( pArea, &uiCount ) );
      hb_storni( uiCount, 2 );
   }
}

HB_FUNC_UR_SUPER( FLUSH )
{
   AREAP pArea = hb_usrGetAreaParam( 1 );

   if( pArea )
      hb_retni( SUPER_FLUSH( pArea ) );
}

HB_FUNC_UR_SUPER( GOCOLD )
{
   AREAP pArea = hb_usrGetAreaParam( 1 );

   if( pArea )
      hb_retni( SUPER_GOCOLD( pArea ) );
}

HB_FUNC_UR_SUPER( GOHOT )
{
   AREAP pArea = hb_usrGetAreaParam( 1 );

   if( pArea )
      hb_retni( SUPER_GOHOT( pArea ) );
}

HB_FUNC_UR_SUPER( PUTREC )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      if( ISPOINTER( 2 ) )
      {
         hb_retni( SUPER_PUTREC( pArea, ( BYTE * ) hb_parptr( 2 ) ) );
      }
      else if( ISCHAR( 2 ) )
      {
         hb_retni( SUPER_PUTREC( pArea, ( BYTE * ) hb_parc( 2 ) ) );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( GETREC )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      BYTE * pBuffer;

      hb_retni( SUPER_GETREC( pArea, &pBuffer ) );
      hb_storptr( pBuffer, 2 );
   }
}

HB_FUNC_UR_SUPER( GETVALUE )
{
   AREAP pArea = hb_usrGetAreaParam( 3 );

   if( pArea )
      hb_retni( SUPER_GETVALUE( pArea, hb_parni( 2 ), hb_param( 3, HB_IT_ANY ) ) );
}

HB_FUNC_UR_SUPER( PUTVALUE )
{
   AREAP pArea = hb_usrGetAreaParam( 3 );

   if( pArea )
      hb_retni( SUPER_PUTVALUE( pArea, hb_parni( 2 ), hb_param( 3, HB_IT_ANY ) ) );
}

HB_FUNC_UR_SUPER( GETVARLEN )
{
   AREAP pArea = hb_usrGetAreaParam( 3 );

   if( pArea )
   {
      ULONG ulLength;

      hb_retni( SUPER_GETVARLEN( pArea, hb_parni( 2 ), &ulLength ) );
      hb_stornl( ulLength, 3 );
   }
}

HB_FUNC_UR_SUPER( RECCOUNT )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      ULONG ulRecCount;

      hb_retni( SUPER_RECCOUNT( pArea, &ulRecCount ) );
      hb_stornl( ulRecCount, 2 );
   }
}

HB_FUNC_UR_SUPER( RECINFO )
{
   AREAP pArea = hb_usrGetAreaParam( 4 );

   if( pArea )
      hb_retni( SUPER_RECINFO( pArea, hb_param( 2, HB_IT_ANY ), hb_parni( 3 ),
                                      hb_param( 4, HB_IT_ANY ) ) );
}

HB_FUNC_UR_SUPER( RECNO )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      ULONG ulRecNo;

      hb_retni( SUPER_RECNO( pArea, &ulRecNo ) );
      hb_stornl( ulRecNo, 2 );
   }
}

HB_FUNC_UR_SUPER( RECID )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
      hb_retni( SUPER_RECID( pArea, hb_param( 2, HB_IT_ANY ) ) );
}

HB_FUNC_UR_SUPER( FIELDINFO )
{
   AREAP pArea = hb_usrGetAreaParam( 4 );

   if( pArea )
      hb_retni( SUPER_FIELDINFO( pArea, hb_parni( 2 ), hb_parni( 3 ),
                                        hb_param( 4, HB_IT_ANY ) ) );
}

HB_FUNC_UR_SUPER( CREATEFIELDS )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
      hb_retni( SUPER_CREATEFIELDS( pArea, hb_param( 2, HB_IT_ANY ) ) );
}

HB_FUNC_UR_SUPER( SETFIELDEXTENT )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
      hb_retni( SUPER_SETFIELDEXTENT( pArea, hb_parni( 2 ) ) );
}

HB_FUNC_UR_SUPER( ALIAS )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      char szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];

      hb_retni( SUPER_ALIAS( pArea, ( BYTE * ) szAlias ) );
      hb_storc( szAlias, 2 );
   }
}

HB_FUNC_UR_SUPER( CLOSE )
{
   AREAP pArea = hb_usrGetAreaParam( 1 );

   if( pArea )
      hb_retni( SUPER_CLOSE( pArea ) );
}

HB_FUNC_UR_SUPER( CREATE )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBOPENINFO dbOpenInfo;

      if( hb_usrItemToOpenInfo( hb_param( 2, HB_IT_ARRAY ), &dbOpenInfo ) )
      {
         hb_retni( SUPER_CREATE( pArea, &dbOpenInfo ) );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( OPEN )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBOPENINFO dbOpenInfo;

      if( hb_usrItemToOpenInfo( hb_param( 2, HB_IT_ARRAY ), &dbOpenInfo ) )
      {
         hb_retni( SUPER_OPEN( pArea, &dbOpenInfo ) );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( INFO )
{
   AREAP pArea = hb_usrGetAreaParam( 3 );

   if( pArea )
      hb_retni( SUPER_INFO( pArea, hb_parni( 2 ), hb_param( 3, HB_IT_ANY ) ) );
}

HB_FUNC_UR_SUPER( DBEVAL )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBEVALINFO dbEvalInfo;

      if( hb_usrItemToEvalInfo( hb_param( 2, HB_IT_ARRAY ), &dbEvalInfo ) )
      {
         hb_retni( SUPER_DBEVAL( pArea, &dbEvalInfo ) );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( PACK )
{
   AREAP pArea = hb_usrGetAreaParam( 1 );

   if( pArea )
      hb_retni( SUPER_PACK( pArea ) );
}

HB_FUNC_UR_SUPER( PACKREC )
{
   AREAP pArea = hb_usrGetAreaParam( 3 );

   if( pArea )
   {
      BOOL fWritten;

      hb_retni( SUPER_PACKREC( pArea, hb_parnl( 2 ), &fWritten ) );
      hb_storl( fWritten, 3 );
   }
}

HB_FUNC_UR_SUPER( SORT )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBSORTINFO dbSortInfo;

      if( hb_usrItemToSortInfo( hb_param( 2, HB_IT_ARRAY ), &dbSortInfo ) )
      {
         hb_retni( SUPER_SORT( pArea, &dbSortInfo ) );
         hb_usrSortInfoFree( &dbSortInfo );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( TRANS )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBTRANSINFO dbTransInfo;

      if( hb_usrItemToTransInfo( hb_param( 2, HB_IT_ARRAY ), &dbTransInfo ) )
      {
         hb_retni( SUPER_TRANS( pArea, &dbTransInfo ) );
         hb_usrTransInfoFree( &dbTransInfo );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( TRANSREC )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBTRANSINFO dbTransInfo;

      if( hb_usrItemToTransInfo( hb_param( 2, HB_IT_ARRAY ), &dbTransInfo ) )
      {
         hb_retni( SUPER_TRANSREC( pArea, &dbTransInfo ) );
         hb_usrTransInfoFree( &dbTransInfo );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( ZAP )
{
   AREAP pArea = hb_usrGetAreaParam( 1 );

   if( pArea )
      hb_retni( SUPER_ZAP( pArea ) );
}

HB_FUNC_UR_SUPER( CHILDEND )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBRELINFO dbRelInfo;

      if( hb_usrItemToRelInfo( hb_param( 2, HB_IT_ARRAY ), &dbRelInfo ) )
      {
         hb_retni( SUPER_CHILDEND( pArea, &dbRelInfo ) );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( CHILDSTART )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBRELINFO dbRelInfo;

      if( hb_usrItemToRelInfo( hb_param( 2, HB_IT_ARRAY ), &dbRelInfo ) )
      {
         hb_retni( SUPER_CHILDSTART( pArea, &dbRelInfo ) );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( CHILDSYNC )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBRELINFO dbRelInfo;

      if( hb_usrItemToRelInfo( hb_param( 2, HB_IT_ARRAY ), &dbRelInfo ) )
      {
         hb_retni( SUPER_CHILDSYNC( pArea, &dbRelInfo ) );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( SYNCCHILDREN )
{
   AREAP pArea = hb_usrGetAreaParam( 1 );

   if( pArea )
      hb_retni( SUPER_SYNCCHILDREN( pArea ) );
}

HB_FUNC_UR_SUPER( CLEARREL )
{
   AREAP pArea = hb_usrGetAreaParam( 1 );

   if( pArea )
      hb_retni( SUPER_CLEARREL( pArea ) );
}

HB_FUNC_UR_SUPER( FORCEREL )
{
   AREAP pArea = hb_usrGetAreaParam( 1 );

   if( pArea )
      hb_retni( SUPER_FORCEREL( pArea ) );
}

HB_FUNC_UR_SUPER( RELAREA )
{
   AREAP pArea = hb_usrGetAreaParam( 3 );

   if( pArea )
   {
      USHORT uiRelArea;

      hb_retni( SUPER_RELAREA( pArea, hb_parni( 2 ), &uiRelArea ) );
      hb_storni( uiRelArea, 3 );
   }
}

HB_FUNC_UR_SUPER( RELEVAL )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBRELINFO dbRelInfo;

      if( hb_usrItemToRelInfo( hb_param( 2, HB_IT_ARRAY ), &dbRelInfo ) )
      {
         hb_retni( SUPER_RELEVAL( pArea, &dbRelInfo ) );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( RELTEXT )
{
   AREAP pArea = hb_usrGetAreaParam( 3 );

   if( pArea )
   {
      char szExpr[ HARBOUR_MAX_RDD_RELTEXT_LENGTH + 1 ];

      hb_retni( SUPER_RELTEXT( pArea, hb_parni( 2 ), szExpr ) );
      hb_storc( szExpr, 3 );
   }
}

HB_FUNC_UR_SUPER( SETREL )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBRELINFO dbRelInfo;

      if( hb_usrItemToRelInfo( hb_param( 2, HB_IT_ARRAY ), &dbRelInfo ) )
      {
         hb_retni( SUPER_SETREL( pArea, &dbRelInfo ) );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( ORDLSTADD )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBORDERINFO dbOrderInfo;
      PHB_ITEM pItem = hb_param( 2, HB_IT_ARRAY );

      if( hb_usrItemToOrderInfo( pItem, &dbOrderInfo ) )
      {
         hb_retni( SUPER_ORDLSTADD( pArea, &dbOrderInfo ) );
         hb_arraySet( pItem, UR_ORI_RESULT, dbOrderInfo.itmResult );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( ORDLSTCLEAR )
{
   AREAP pArea = hb_usrGetAreaParam( 1 );

   if( pArea )
      hb_retni( SUPER_ORDLSTCLEAR( pArea ) );
}

HB_FUNC_UR_SUPER( ORDLSTDELETE )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBORDERINFO dbOrderInfo;
      PHB_ITEM pItem = hb_param( 2, HB_IT_ARRAY );

      if( hb_usrItemToOrderInfo( pItem, &dbOrderInfo ) )
      {
         hb_retni( SUPER_ORDLSTDELETE( pArea, &dbOrderInfo ) );
         hb_arraySet( pItem, UR_ORI_RESULT, dbOrderInfo.itmResult );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( ORDLSTFOCUS )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBORDERINFO dbOrderInfo;
      PHB_ITEM pItem = hb_param( 2, HB_IT_ARRAY );

      if( hb_usrItemToOrderInfo( pItem, &dbOrderInfo ) )
      {
         hb_retni( SUPER_ORDLSTFOCUS( pArea, &dbOrderInfo ) );
         hb_arraySet( pItem, UR_ORI_RESULT, dbOrderInfo.itmResult );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( ORDLSTREBUILD )
{
   AREAP pArea = hb_usrGetAreaParam( 1 );

   if( pArea )
      hb_retni( SUPER_ORDLSTREBUILD( pArea ) );
}

HB_FUNC_UR_SUPER( ORDSETCOND )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBORDERCONDINFO dbOrderCondInfo;
      PHB_ITEM pItem = hb_param( 2, HB_IT_ARRAY );

      if( hb_usrItemToOrderCondInfo( pItem, &dbOrderCondInfo ) )
      {
         hb_retni( SUPER_ORDSETCOND( pArea, &dbOrderCondInfo ) );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( ORDCREATE )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBORDERCREATEINFO dbOrderCreateInfo;
      PHB_ITEM pItem = hb_param( 2, HB_IT_ARRAY );

      if( hb_usrItemToOrderCreateInfo( pItem, &dbOrderCreateInfo ) )
      {
         hb_retni( SUPER_ORDCREATE( pArea, &dbOrderCreateInfo ) );
         hb_usrOrderCreateFree( &dbOrderCreateInfo );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( ORDDESTROY )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBORDERINFO dbOrderInfo;
      PHB_ITEM pItem = hb_param( 2, HB_IT_ARRAY );

      if( hb_usrItemToOrderInfo( pItem, &dbOrderInfo ) )
      {
         hb_retni( SUPER_ORDDESTROY( pArea, &dbOrderInfo ) );
         hb_arraySet( pItem, UR_ORI_RESULT, dbOrderInfo.itmResult );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( ORDINFO )
{
   AREAP pArea = hb_usrGetAreaParam( 3 );

   if( pArea )
   {
      DBORDERINFO dbOrderInfo;
      PHB_ITEM pItem = hb_param( 3, HB_IT_ARRAY );

      if( hb_usrItemToOrderInfo( pItem, &dbOrderInfo ) )
      {
         hb_retni( SUPER_ORDINFO( pArea, hb_parni( 2 ), &dbOrderInfo ) );
         hb_arraySet( pItem, UR_ORI_RESULT, dbOrderInfo.itmResult );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( CLEARFILTER )
{
   AREAP pArea = hb_usrGetAreaParam( 1 );

   if( pArea )
      hb_retni( SUPER_CLEARFILTER( pArea ) );
}

HB_FUNC_UR_SUPER( CLEARLOCATE )
{
   AREAP pArea = hb_usrGetAreaParam( 1 );

   if( pArea )
      hb_retni( SUPER_CLEARLOCATE( pArea ) );
}

HB_FUNC_UR_SUPER( CLEARSCOPE )
{
   AREAP pArea = hb_usrGetAreaParam( 1 );

   if( pArea )
      hb_retni( SUPER_CLEARSCOPE( pArea ) );
}

HB_FUNC_UR_SUPER( FILTERTEXT )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
      hb_retni( SUPER_FILTERTEXT( pArea, hb_param( 2, HB_IT_ANY ) ) );
}

HB_FUNC_UR_SUPER( SETFILTER )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBFILTERINFO dbFilterInfo;

      if( hb_usrItemToFilterInfo( hb_param( 2, HB_IT_ARRAY ), &dbFilterInfo ) )
      {
         hb_retni( SUPER_SETFILTER( pArea, &dbFilterInfo ) );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( SETLOCATE )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBSCOPEINFO dbScopeInfo;

      if( hb_usrItemToScopeInfo( hb_param( 2, HB_IT_ARRAY ), &dbScopeInfo ) )
      {
         hb_retni( SUPER_SETLOCATE( pArea, &dbScopeInfo ) );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( LOCATE )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
      hb_retni( SUPER_LOCATE( pArea, hb_parl( 2 ) ) );
}

HB_FUNC_UR_SUPER( COMPILE )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      if( ISCHAR( 2 ) )
      {
         hb_retni( SUPER_COMPILE( pArea, ( BYTE * ) hb_parc( 2 ) ) );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( ERROR )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      PHB_ITEM pItem = hb_param( 2, HB_IT_OBJECT );

      if( pItem )
      {
         pItem = hb_itemNew( pItem );
         hb_retni( SUPER_ERROR( pArea, pItem ) );
         hb_itemRelease( pItem );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( EVALBLOCK )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      PHB_ITEM pItem = hb_param( 2, HB_IT_BLOCK );

      if( pItem )
      {
         hb_retni( SUPER_EVALBLOCK( pArea, pItem ) );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( RAWLOCK )
{
   AREAP pArea = hb_usrGetAreaParam( 3 );

   if( pArea )
      hb_retni( SUPER_RAWLOCK( pArea, hb_parni( 2 ), hb_parnl( 3 ) ) );
}

HB_FUNC_UR_SUPER( LOCK )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBLOCKINFO dbLockInfo;
      PHB_ITEM pItem = hb_param( 2, HB_IT_ARRAY );

      if( hb_usrItemToLockInfo( pItem, &dbLockInfo ) )
      {
         hb_retni( SUPER_LOCK( pArea, &dbLockInfo ) );
         hb_itemPutL( hb_arrayGetItemPtr( pItem, UR_LI_RESULT ),
                      dbLockInfo.fResult );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( UNLOCK )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
      hb_retni( SUPER_UNLOCK( pArea, hb_param( 2, HB_IT_ANY ) ) );
}

HB_FUNC_UR_SUPER( CLOSEMEMFILE )
{
   AREAP pArea = hb_usrGetAreaParam( 1 );

   if( pArea )
      hb_retni( SUPER_CLOSEMEMFILE( pArea ) );
}

HB_FUNC_UR_SUPER( CREATEMEMFILE )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBOPENINFO dbOpenInfo;

      if( hb_usrItemToOpenInfo( hb_param( 2, HB_IT_ARRAY ), &dbOpenInfo ) )
      {
         hb_retni( SUPER_CREATEMEMFILE( pArea, &dbOpenInfo ) );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( OPENMEMFILE )
{
   AREAP pArea = hb_usrGetAreaParam( 2 );

   if( pArea )
   {
      DBOPENINFO dbOpenInfo;

      if( hb_usrItemToOpenInfo( hb_param( 2, HB_IT_ARRAY ), &dbOpenInfo ) )
      {
         hb_retni( SUPER_OPENMEMFILE( pArea, &dbOpenInfo ) );
      }
      else
      {
         hb_usrErrorRT( pArea, EG_ARG, EDBCMD_NOVAR );
         hb_retni( FAILURE );
      }
   }
}

HB_FUNC_UR_SUPER( GETVALUEFILE )
{
   AREAP pArea = hb_usrGetAreaParam( 4 );

   if( pArea )
      hb_retni( SUPER_GETVALUEFILE( pArea, hb_parni( 2 ), hb_parc( 3 ),
                                           hb_parni( 4 ) ) );
}

HB_FUNC_UR_SUPER( PUTVALUEFILE )
{
   AREAP pArea = hb_usrGetAreaParam( 4 );

   if( pArea )
      hb_retni( SUPER_PUTVALUEFILE( pArea, hb_parni( 2 ), hb_parc( 3 ),
                                           hb_parni( 4 ) ) );
}

HB_FUNC_UR_SUPER( READDBHEADER )
{
   AREAP pArea = hb_usrGetAreaParam( 1 );

   if( pArea )
      hb_retni( SUPER_READDBHEADER( pArea ) );
}

HB_FUNC_UR_SUPER( WRITEBHEADER )
{
   AREAP pArea = hb_usrGetAreaParam( 1 );

   if( pArea )
      hb_retni( SUPER_WRITEDBHEADER( pArea ) );
}

HB_FUNC_UR_SUPER( DROP )
{
   LPRDDNODE pRDD = hb_usrGetNodeParam( 2 );

   if( pRDD )
      hb_retni( SUPER_DROP( pRDD, hb_param( 2, HB_IT_ANY ),
                                  hb_param( 3, HB_IT_ANY ) ) );
}

HB_FUNC_UR_SUPER( EXISTS )
{
   LPRDDNODE pRDD = hb_usrGetNodeParam( 2 );

   if( pRDD )
      hb_retni( SUPER_EXISTS( pRDD, hb_param( 2, HB_IT_ANY ),
                                    hb_param( 3, HB_IT_ANY ) ) );
}

HB_FUNC_UR_SUPER( RDDINFO )
{
   LPRDDNODE pRDD = hb_usrGetNodeParam( 4 );

   if( pRDD )
      hb_retni( SUPER_RDDINFO( pRDD, hb_parni( 2 ), hb_parnl( 3 ),
                                     hb_param( 4, HB_IT_ANY ) ) );
}

