/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * DBFDBT RDD
 *
 * Copyright 2003 Przemyslaw Czerpak <druzus@acn.waw.pl>
 * www - http://www.xharbour.org
 * Most of code taken from dbf1.c
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
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
#include "hbinit.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbset.h"
#include "hbdate.h"
#include "hbrdddbt.h"

#ifndef HB_CDP_SUPPORT_OFF
#  include "hbapicdp.h"
#endif

#define __PRG_SOURCE__ __FILE__
#ifndef __XHARBOUR__
#  define HB_VM_STACK hb_stack
#endif
#ifdef HB_PCODE_VER
#  undef HB_PRG_PCODE_VER
#  define HB_PRG_PCODE_VER HB_PCODE_VER
#endif

static RDDFUNCS dbtSuper;
static RDDFUNCS dbtTable =
{

   /* Movement and positioning methods */

   ( DBENTRYP_BP )    hb_dbtBof,
   ( DBENTRYP_BP )    hb_dbtEof,
   ( DBENTRYP_BP )    hb_dbtFound,
   ( DBENTRYP_V )     hb_dbtGoBottom,
   ( DBENTRYP_UL )    hb_dbtGoTo,
   ( DBENTRYP_I )     hb_dbtGoToId,
   ( DBENTRYP_V )     hb_dbtGoTop,
   ( DBENTRYP_BIB )   hb_dbtSeek,
   ( DBENTRYP_L )     hb_dbtSkip,
   ( DBENTRYP_L )     hb_dbtSkipFilter,
   ( DBENTRYP_L )     hb_dbtSkipRaw,


   /* Data management */

   ( DBENTRYP_VF )    hb_dbtAddField,
   ( DBENTRYP_B )     hb_dbtAppend,
   ( DBENTRYP_I )     hb_dbtCreateFields,
   ( DBENTRYP_V )     hb_dbtDeleteRec,
   ( DBENTRYP_BP )    hb_dbtDeleted,
   ( DBENTRYP_SP )    hb_dbtFieldCount,
   ( DBENTRYP_VF )    hb_dbtFieldDisplay,
   ( DBENTRYP_SSI )   hb_dbtFieldInfo,
   ( DBENTRYP_SVP )   hb_dbtFieldName,
   ( DBENTRYP_V )     hb_dbtFlush,
   ( DBENTRYP_PP )    hb_dbtGetRec,
   ( DBENTRYP_SI )    hb_dbtGetValue,
   ( DBENTRYP_SVL )   hb_dbtGetVarLen,
   ( DBENTRYP_V )     hb_dbtGoCold,
   ( DBENTRYP_V )     hb_dbtGoHot,
   ( DBENTRYP_P )     hb_dbtPutRec,
   ( DBENTRYP_SI )    hb_dbtPutValue,
   ( DBENTRYP_V )     hb_dbtRecall,
   ( DBENTRYP_ULP )   hb_dbtRecCount,
   ( DBENTRYP_ISI )   hb_dbtRecInfo,
   ( DBENTRYP_I )     hb_dbtRecNo,
   ( DBENTRYP_S )     hb_dbtSetFieldExtent,


   /* WorkArea/Database management */

   ( DBENTRYP_P )     hb_dbtAlias,
   ( DBENTRYP_V )     hb_dbtClose,
   ( DBENTRYP_VP )    hb_dbtCreate,
   ( DBENTRYP_SI )    hb_dbtInfo,
   ( DBENTRYP_V )     hb_dbtNewArea,
   ( DBENTRYP_VP )    hb_dbtOpen,
   ( DBENTRYP_V )     hb_dbtRelease,
   ( DBENTRYP_SP )    hb_dbtStructSize,
   ( DBENTRYP_P )     hb_dbtSysName,
   ( DBENTRYP_VEI )   hb_dbtEval,
   ( DBENTRYP_V )     hb_dbtPack,
   ( DBENTRYP_LSP )   hb_dbtPackRec,
   ( DBENTRYP_VS )    hb_dbtSort,
   ( DBENTRYP_VT )    hb_dbtTrans,
   ( DBENTRYP_VT )    hb_dbtTransRec,
   ( DBENTRYP_V )     hb_dbtZap,


   /* Relational Methods */

   ( DBENTRYP_VR )    hb_dbtChildEnd,
   ( DBENTRYP_VR )    hb_dbtChildStart,
   ( DBENTRYP_VR )    hb_dbtChildSync,
   ( DBENTRYP_V )     hb_dbtSyncChildren,
   ( DBENTRYP_V )     hb_dbtClearRel,
   ( DBENTRYP_V )     hb_dbtForceRel,
   ( DBENTRYP_SVP )   hb_dbtRelArea,
   ( DBENTRYP_VR )    hb_dbtRelEval,
   ( DBENTRYP_SVP )   hb_dbtRelText,
   ( DBENTRYP_VR )    hb_dbtSetRel,


   /* Order Management */

   ( DBENTRYP_OI )    hb_dbtOrderListAdd,
   ( DBENTRYP_V )     hb_dbtOrderListClear,
   ( DBENTRYP_VP )    hb_dbtOrderListDelete,
   ( DBENTRYP_OI )    hb_dbtOrderListFocus,
   ( DBENTRYP_V )     hb_dbtOrderListRebuild,
   ( DBENTRYP_VOI )   hb_dbtOrderCondition,
   ( DBENTRYP_VOC )   hb_dbtOrderCreate,
   ( DBENTRYP_OI )    hb_dbtOrderDestroy,
   ( DBENTRYP_OII )   hb_dbtOrderInfo,


   /* Filters and Scope Settings */

   ( DBENTRYP_V )     hb_dbtClearFilter,
   ( DBENTRYP_V )     hb_dbtClearLocate,
   ( DBENTRYP_V )     hb_dbtClearScope,
   ( DBENTRYP_VPLP )  hb_dbtCountScope,
   ( DBENTRYP_I )     hb_dbtFilterText,
   ( DBENTRYP_SI )    hb_dbtScopeInfo,
   ( DBENTRYP_VFI )   hb_dbtSetFilter,
   ( DBENTRYP_VLO )   hb_dbtSetLocate,
   ( DBENTRYP_VOS )   hb_dbtSetScope,
   ( DBENTRYP_VPL )   hb_dbtSkipScope,


   /* Miscellaneous */

   ( DBENTRYP_P )     hb_dbtCompile,
   ( DBENTRYP_I )     hb_dbtError,
   ( DBENTRYP_I )     hb_dbtEvalBlock,


   /* Network operations */

   ( DBENTRYP_VSP )   hb_dbtRawLock,
   ( DBENTRYP_VL )    hb_dbtLock,
   ( DBENTRYP_UL )    hb_dbtUnLock,


   /* Memofile functions */

   ( DBENTRYP_V )     hb_dbtCloseMemFile,
   ( DBENTRYP_VP )    hb_dbtCreateMemFile,
   ( DBENTRYP_SVPB )  hb_dbtGetValueFile,
   ( DBENTRYP_VP )    hb_dbtOpenMemFile,
   ( DBENTRYP_SVP )   hb_dbtPutValueFile,


   /* Database file header handling */

   ( DBENTRYP_V )     hb_dbtReadDBHeader,
   ( DBENTRYP_V )     hb_dbtWriteDBHeader,


   /* non WorkArea functions       */
   ( DBENTRYP_I0 )    hb_dbtExit,
   ( DBENTRYP_I1 )    hb_dbtDrop,
   ( DBENTRYP_I2 )    hb_dbtExists,

   /* Special and reserved methods */

   ( DBENTRYP_SVP )   hb_dbtWhoCares
};


HB_FUNC( _DBFDBT ) {;}

HB_FUNC( DBFDBT_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;

   uiCount = ( USHORT * ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   pTable = ( RDDFUNCS * ) hb_itemGetPtr( hb_param( 2, HB_IT_POINTER ) );

   HB_TRACE(HB_TR_DEBUG, ("DBFDBT_GETFUNCTABLE(%i, %p)", uiCount, pTable));

   if( pTable )
   {
      if ( uiCount )
         * uiCount = RDDFUNCSCOUNT;
      hb_retni( hb_rddInherit( pTable, &dbtTable, &dbtSuper, ( BYTE * ) "DBF" ) );
   }
   else
      hb_retni( FAILURE );
}


HB_INIT_SYMBOLS_BEGIN( dbfdbt1__InitSymbols )
{ "_DBFDBT",             HB_FS_PUBLIC, {HB_FUNCNAME( _DBFDBT )}, NULL },
{ "DBFDBT_GETFUNCTABLE", HB_FS_PUBLIC, {HB_FUNCNAME( DBFDBT_GETFUNCTABLE )}, NULL }
HB_INIT_SYMBOLS_END( dbfdbt1__InitSymbols )

#if defined(HB_STATIC_STARTUP)
#  pragma startup dbfdbt1__InitSymbols
#elif defined(_MSC_VER)
#  if _MSC_VER >= 1010
#     pragma data_seg( ".CRT$XIY" )
#     pragma comment( linker, "/Merge:.CRT=.data" )
#  else
#     pragma data_seg( "XIY" )
#  endif
   static HB_$INITSYM hb_vm_auto_dbfdbt1__InitSymbols = dbfdbt1__InitSymbols;
#  pragma data_seg()
#elif ! defined(__GNUC__)
#  pragma startup dbfdbt1__InitSymbols
#endif

/*
 * Exclusive lock memo file.
 */
static BOOL hb_dbtFileLockEx( DBTAREAP pArea )
{
   BOOL fRet;

   if ( !pArea->fShared )
   {
      fRet = TRUE;
   }
   else
   {
      do
      {
         fRet = hb_fsLock( pArea->hMemoFile, DBT_LOCKPOS, DBT_LOCKSIZE,
                           FL_LOCK | FLX_EXCLUSIVE | FLX_WAIT );
      } while ( !fRet );
   }
   return fRet;
}

/*
 * Shared lock memo file.
 */
static BOOL hb_dbtFileLockSh( DBTAREAP pArea )
{
   BOOL fRet;

   if ( !pArea->fShared )
   {
      fRet = TRUE;
   }
   else
   {
      do
      {
         fRet = hb_fsLock( pArea->hMemoFile, DBT_LOCKPOS, DBT_LOCKSIZE,
                           FL_LOCK | FLX_SHARED | FLX_WAIT );
      } while ( !fRet );
   }
   return fRet;
}

/*
 * Unlock memo file.
 */
static BOOL hb_dbtFileUnLock( DBTAREAP pArea )
{
   return !pArea->fShared || hb_fsLock( pArea->hMemoFile, DBT_LOCKPOS, DBT_LOCKSIZE, FL_UNLOCK );
}

/*
 * Return the size of memo.
 */
static ULONG hb_dbtGetMemoLen( DBTAREAP pArea, USHORT uiIndex )
{
   ULONG ulBlock;
   BYTE pBlock[ DBT_BLOCKSIZE ];

   HB_TRACE(HB_TR_DEBUG, ("hb_dbtGetMemoLen(%p, %hu)", pArea, uiIndex));

   ulBlock = hb_dbfGetMemoBlock( ( DBFAREAP ) pArea, uiIndex );
   if( ulBlock == 0 )
      return 0;
   hb_fsSeek( pArea->hMemoFile, ulBlock * DBT_BLOCKSIZE, FS_SET );
   ulBlock = 0;
   do
   {
      uiIndex = 0;
      if ( hb_fsRead( pArea->hMemoFile, pBlock, DBT_BLOCKSIZE ) == DBT_BLOCKSIZE )
      {
         while( uiIndex < DBT_BLOCKSIZE && pBlock[ uiIndex ] != 0x1A )
            uiIndex++;
         ulBlock += uiIndex;
      }
   } while( uiIndex == DBT_BLOCKSIZE );
   return ulBlock;
}

/*
 * Read memo data.
 */
static void hb_dbtGetMemo( DBTAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   ULONG ulSize, ulBlock;
   BYTE * pBuffer;

   HB_TRACE(HB_TR_DEBUG, ("hb_dbtGetMemo(%p, %hu, %p)", pArea, uiIndex, pItem));

   ulSize = hb_dbtGetMemoLen( pArea, uiIndex );

   pBuffer = ( BYTE * ) hb_xgrab( ulSize + 1 );
   if( ulSize > 0 )
   {
      ulBlock = hb_dbfGetMemoBlock( ( DBFAREAP ) pArea, uiIndex );
      hb_fsSeek( pArea->hMemoFile, ulBlock * DBT_BLOCKSIZE, FS_SET );
      hb_fsReadLarge( pArea->hMemoFile, pBuffer, ulSize );
   }
   else
      *pBuffer = '\0';

   hb_itemPutCPtr( pItem, ( char * ) pBuffer, ulSize );
#ifndef HB_CDP_SUPPORT_OFF
   hb_cdpnTranslate( pItem->item.asString.value, pArea->cdPage, hb_cdp_page, ulSize );
#endif
   hb_itemSetCMemo( pItem );
}

/*
 * Write memo data.
 */
static void hb_dbtWriteMemo( DBTAREAP pArea, ULONG ulBlock, PHB_ITEM pItem, ULONG ulLen,
                             ULONG * ulStoredBlock )
{
   BYTE pBlock[ DBT_BLOCKSIZE ], pBuff[4];
   BOOL bNewBlock;
   ULONG ulNewBlock, ulNextBlock = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_dbtWriteMemo(%p, %lu, %p, %lu, %p)", pArea, ulBlock, pItem,
                           ulLen, ulStoredBlock));

   memset( pBlock, 0x1A, DBT_BLOCKSIZE );
   bNewBlock = !( ulBlock && ulLen < DBT_BLOCKSIZE - 1 );
   if( bNewBlock )
   {
      /* Get next block from header */
      hb_fsSeek( pArea->hMemoFile, 0, FS_SET );
      hb_fsRead( pArea->hMemoFile, pBuff, 4 );
      ulNewBlock = HB_GET_LE_UINT32( pBuff );
      ulNextBlock = ulNewBlock * DBT_BLOCKSIZE;
      hb_fsSeek( pArea->hMemoFile, ulNextBlock, FS_SET );
   }
   else
   {
      hb_fsSeek( pArea->hMemoFile, ulBlock * DBT_BLOCKSIZE, FS_SET );
      ulNewBlock = ulBlock;
   }
   * ulStoredBlock = ulNewBlock;

#ifndef HB_CDP_SUPPORT_OFF
   hb_cdpnTranslate( pItem->item.asString.value, hb_cdp_page, pArea->cdPage, ulLen );
#endif
   /* Write memo data and eof mark */
   hb_fsWriteLarge( pArea->hMemoFile, ( BYTE * ) pItem->item.asString.value, ulLen );
   hb_fsWrite( pArea->hMemoFile, pBlock, ( DBT_BLOCKSIZE - ( USHORT ) ( ulLen % DBT_BLOCKSIZE ) ) );
   pArea->fMemoFlush = TRUE;
#ifndef HB_CDP_SUPPORT_OFF
   hb_cdpnTranslate( pItem->item.asString.value, pArea->cdPage, hb_cdp_page, ulLen );
#endif

   if( bNewBlock )
   {
      ulNextBlock += ulLen + 1;
      ulNextBlock += ( DBT_BLOCKSIZE - ulNextBlock % DBT_BLOCKSIZE );
      ulNextBlock /= DBT_BLOCKSIZE;
      HB_PUT_LE_UINT32( pBuff, ulNextBlock );
      hb_fsSeek( pArea->hMemoFile, 0, FS_SET );
      hb_fsWrite( pArea->hMemoFile, pBuff, 4 );
      pArea->fMemoFlush = TRUE;
   }
}

/*
 * Assign a value to the specified memo field.
 */
static BOOL hb_dbtPutMemo( DBTAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   ULONG ulLen, ulBlock;

   HB_TRACE(HB_TR_DEBUG, ("hb_dbtPutMemo(%p, %hu, %p)", pArea, uiIndex, pItem));

   ulLen = pItem->item.asString.length;
   if( ulLen > 0 )
   {
      ulBlock = hb_dbfGetMemoBlock( ( DBFAREAP ) pArea, uiIndex );
      hb_dbtWriteMemo( pArea, ulBlock, pItem, ulLen, &ulBlock );
   }
   else
      ulBlock = 0;
   hb_dbfPutMemoBlock( ( DBFAREAP ) pArea, uiIndex, ulBlock );
   return TRUE;
}


/* DBT METHODS */

/*
 * Obtain the length of a field value.
 * ( DBENTRYP_SVL )   hb_dbtGetVarLen
 */
static ERRCODE hb_dbtGetVarLen( DBTAREAP pArea, USHORT uiIndex, ULONG * pLength )
{
   BOOL bDeleted;

   HB_TRACE(HB_TR_DEBUG, ("hb_dbtGetVarLen(%p, %hu, %p)", pArea, uiIndex, pLength));

   if( pArea->fHasMemo && pArea->hMemoFile != FS_ERROR )
   {
      /* Force read record */
      if( SELF_DELETED( ( AREAP ) pArea, &bDeleted ) == FAILURE )
         return FAILURE;

      if( pArea->lpFields[ uiIndex - 1 ].uiType == HB_IT_MEMO )
      {
         if( hb_dbtFileLockSh( pArea ) )
         {
            * pLength = hb_dbtGetMemoLen( pArea, uiIndex - 1 );
            hb_dbtFileUnLock( pArea );
         }
         else
         {
            * pLength = 0;
         }
         return SUCCESS;
      }
   }

   return SUPER_GETVARLEN( ( AREAP ) pArea, uiIndex, pLength );
}

/*
 * Retrieve information about the current driver.
 * ( DBENTRYP_SI )    hb_dbtInfo
 */
static ERRCODE hb_dbtInfo( DBTAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dbtInfo(%p, %hu, %p)", pArea, uiIndex, pItem));

   switch( uiIndex )
   {
      case DBI_MEMOEXT:
         hb_itemPutC( pItem, DBT_MEMOEXT );
         break;

      /* case DBI_RDD_VERSION */

      default:
         return SUPER_INFO( ( AREAP ) pArea, uiIndex, pItem );
   }

   return SUCCESS;
}

/*
 * Retrieve the size of the WorkArea structure.
 * ( DBENTRYP_SP )    hb_dbtStructSize
 */
static ERRCODE hb_dbtStructSize( DBTAREAP pArea, USHORT * uiSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dbtStrucSize(%p, %p)", pArea, uiSize));
   HB_SYMBOL_UNUSED( pArea );

   * uiSize = sizeof( DBTAREA );
   return SUCCESS;
}

/*
 * Obtain the name of replaceable database driver (RDD) subsystem.
 * ( DBENTRYP_P )     hb_dbtSysName
 */
static ERRCODE hb_dbtSysName( DBTAREAP pArea, BYTE * pBuffer )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dbtSysName(%p, %p)", pArea, pBuffer));
   HB_SYMBOL_UNUSED( pArea );

   strncpy( ( char * ) pBuffer, "DBFDBT", 7  /* HARBOUR_MAX_RDD_DRIVERNAME_LENGTH */ );
   return SUCCESS;
}

/*
 * Obtain the current value of a field.
 * ( DBENTRYP_SI )    hb_dbtGetValue
 */
static ERRCODE hb_dbtGetValue( DBTAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   BOOL bDeleted;

   HB_TRACE(HB_TR_DEBUG, ("hb_dbtGetValue(%p, %hu, %p)", pArea, uiIndex, pItem));

   if( pArea->fHasMemo && pArea->hMemoFile != FS_ERROR &&
       pArea->lpFields[ uiIndex - 1 ].uiType == HB_IT_MEMO )
   {
      /* Force read record */
      if( SELF_DELETED( ( AREAP ) pArea, &bDeleted ) == FAILURE )
         return FAILURE;

      if( hb_dbtFileLockSh( pArea ) )
      {
         hb_dbtGetMemo( pArea, uiIndex - 1, pItem );
         hb_dbtFileUnLock( pArea );
      }
      else
      {
         PHB_ITEM pError = hb_errNew();

         hb_errPutGenCode( pError, EG_LOCK );
         hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_LOCK ) );
         hb_errPutSubCode( pError, EDBF_LOCK );
         hb_errPutFlags( pError, EF_CANDEFAULT );
         SELF_ERROR( ( AREAP ) pArea, pError );
         hb_itemRelease( pError );
         return FAILURE;
      }
      return SUCCESS;
   }
   else
      return SUPER_GETVALUE( ( AREAP ) pArea, uiIndex, pItem );
}

/*
 * Assign a value to a field.
 * ( DBENTRYP_SI )    hb_dbtPutValue
 */
static ERRCODE hb_dbtPutValue( DBTAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   BOOL bDeleted;
   PHB_ITEM pError;
   ERRCODE uiError, uiErrorG;

   HB_TRACE(HB_TR_DEBUG, ("hb_dbtPutValue(%p, %hu, %p)", pArea, uiIndex, pItem));

   if( pArea->fHasMemo && pArea->hMemoFile != FS_ERROR &&
       pArea->lpFields[ uiIndex - 1 ].uiType == HB_IT_MEMO )
   {
      if( HB_IS_MEMO( pItem ) || HB_IS_STRING( pItem ) )
      {
         /* Force read record */
         if( SELF_DELETED( ( AREAP ) pArea, &bDeleted ) == FAILURE )
            return FAILURE;

         if( !pArea->fPositioned )
            return SUCCESS;

         /* Buffer is hot? */
         if( !pArea->fRecordChanged && SELF_GOHOT( ( AREAP ) pArea ) == FAILURE )
            return FAILURE;

         if( hb_dbtFileLockEx( pArea ) )
         {
            uiError = hb_dbtPutMemo( pArea, uiIndex -1, pItem ) ? SUCCESS : EDBF_DATAWIDTH;
            hb_dbtFileUnLock( pArea );
         }
         else
         {
            uiError = EDBF_LOCK;
         }
         /* Update deleted flag */
         pArea->pRecord[ 0 ] = (BYTE) (pArea->fDeleted ? '*' : ' ');
      }
      else
         uiError = EDBF_DATATYPE;

      if( uiError != SUCCESS )
      {
         uiErrorG = uiError == EDBF_DATAWIDTH ? EG_DATAWIDTH :
                  ( uiError == EDBF_LOCK      ? EG_LOCK : EG_DATATYPE );
         pError = hb_errNew();
         hb_errPutGenCode( pError, uiErrorG );
         hb_errPutDescription( pError, hb_langDGetErrorDesc( uiErrorG ) );
         hb_errPutSubCode( pError, uiError );
         hb_errPutFlags( pError, EF_CANDEFAULT );
         SELF_ERROR( ( AREAP ) pArea, pError );
         hb_itemRelease( pError );
         return FAILURE;
      }
      return SUCCESS;
   }
   return SUPER_PUTVALUE( ( AREAP ) pArea, uiIndex, pItem);
}


/* ( DBENTRYP_V )     hb_dbtCloseMemFile    : NULL */

/*
 * Create a memo file in the WorkArea.
 * ( DBENTRYP_VP )    hb_dbtCreateMemFile
 */
static ERRCODE hb_dbtCreateMemFile( DBTAREAP pArea, LPDBOPENINFO pCreateInfo )
{
   BYTE pBlock[ DBT_BLOCKSIZE ];
   BOOL bRetry;
   PHB_ITEM pError;

   HB_TRACE(HB_TR_DEBUG, ("hb_dbtCreateMemFile(%p, %p)", pArea, pCreateInfo));

   pArea->uiMemoBlockSize = DBT_BLOCKSIZE;

   if( pCreateInfo )
   {
      pError = NULL;
      /* Try create */
      do
      {
         pArea->hMemoFile = hb_spCreate( pCreateInfo->abName, FC_NORMAL );
         if( pArea->hMemoFile == FS_ERROR )
         {
            if( !pError )
            {
               pError = hb_errNew();
               hb_errPutGenCode( pError, EG_CREATE );
               hb_errPutSubCode( pError, EDBF_CREATE_DBF );
               hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_CREATE ) );
               hb_errPutFileName( pError, ( char * ) pCreateInfo->abName );
               hb_errPutFlags( pError, EF_CANRETRY );
            }
            bRetry = ( SELF_ERROR( ( AREAP ) pArea, pError ) == E_RETRY );
         }
         else
            bRetry = FALSE;
      } while( bRetry );
      if( pError )
         hb_itemRelease( pError );

      if( pArea->hMemoFile == FS_ERROR )
         return FAILURE;
   }
   else /* For zap file */
      hb_fsSeek( pArea->hMemoFile, 0, FS_SET );

   memset( pBlock, 0, DBT_BLOCKSIZE );
   * ( ( LONG * ) pBlock ) = 1;
   if( hb_fsWrite( pArea->hMemoFile, pBlock, DBT_BLOCKSIZE ) != DBT_BLOCKSIZE )
      return FAILURE;
   hb_fsWrite( pArea->hMemoFile, NULL, 0 );
   pArea->fMemoFlush = TRUE;

   return SUCCESS;
}


/* ( DBENTRYP_SVPB )  hb_dbtGetValueFile    : NULL */

/*
 * Open a memo file in the specified WorkArea.
 * ( DBENTRYP_VP )    hb_dbtOpenMemFile
 */
static ERRCODE hb_dbtOpenMemFile( DBTAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   USHORT uiFlags;
   BOOL bRetry;
   PHB_ITEM pError = NULL;

   HB_TRACE(HB_TR_DEBUG, ("hb_dbtOpenMemFile(%p, %p)", pArea, pOpenInfo));

   uiFlags = (pOpenInfo->fReadonly ? FO_READ : FO_READWRITE) |
             (pOpenInfo->fShared ? FO_DENYNONE : FO_EXCLUSIVE);

   /* Try open */
   do
   {
      pArea->hMemoFile = hb_spOpen( pOpenInfo->abName, uiFlags );
      if( pArea->hMemoFile == FS_ERROR )
      {
         if( !pError )
         {
            pError = hb_errNew();
            hb_errPutGenCode( pError, EG_OPEN );
            hb_errPutSubCode( pError, EDBF_OPEN_DBF );
            hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_OPEN ) );
            hb_errPutFileName( pError, ( char * ) pOpenInfo->abName );
            hb_errPutFlags( pError, EF_CANRETRY | EF_CANDEFAULT );
         }
         bRetry = ( SELF_ERROR( ( AREAP ) pArea, pError ) == E_RETRY );
      }
      else
         bRetry = FALSE;
   } while( bRetry );

   if( pError )
   {
      hb_itemRelease( pError );
   }
   pArea->uiMemoBlockSize = DBT_BLOCKSIZE;

   return ( pArea->hMemoFile == FS_ERROR ? FAILURE : SUCCESS );
}

/* ( DBENTRYP_SVP )   hb_dbtPutValueFile    : NULL */

/*
 * Read the database file header record in the WorkArea.
 * ( DBENTRYP_V )     hb_dbtReadDBHeader
 */
static ERRCODE hb_dbtReadDBHeader( DBTAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dbtReadHeader(%p)", pArea));

   if( SUPER_READDBHEADER( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;
// Set in SUPER() now 3/05/2004
//   pArea->fHasMemo = ( pArea->bVersion == 0x83 );

   return SUCCESS;
}

/*
 * Write the database file header record in the WorkArea.
 * ( DBENTRYP_V )     hb_dbtWriteDBHeader
 */
static ERRCODE hb_dbtWriteDBHeader( DBTAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dbtWriteDBHeader(%p)", pArea));

   if ( pArea->fHasMemo && pArea->bVersion != 0x30 && pArea->bVersion != 0x31 )
   {
      pArea->bVersion = 0x83;
   }
   return SUPER_WRITEDBHEADER( ( AREAP ) pArea );
}
