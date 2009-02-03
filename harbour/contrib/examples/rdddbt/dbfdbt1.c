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
#include "hbapiitm.h"
#include "hbinit.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbset.h"
#include "hbvm.h"
#include "hbdate.h"
#include "hbrdddbt.h"
#include "rddsys.ch"

#ifndef HB_CDP_SUPPORT_OFF
#  include "hbapicdp.h"
#endif

static RDDFUNCS dbtSuper;
static const RDDFUNCS dbtTable =
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
   ( DBENTRYP_ULP )   hb_dbtRecNo,
   ( DBENTRYP_I )     hb_dbtRecId,
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
   ( DBENTRYP_SI )    hb_dbtRelText,
   ( DBENTRYP_VR )    hb_dbtSetRel,


   /* Order Management */

   ( DBENTRYP_OI )    hb_dbtOrderListAdd,
   ( DBENTRYP_V )     hb_dbtOrderListClear,
   ( DBENTRYP_OI )    hb_dbtOrderListDelete,
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
   ( DBENTRYP_B )     hb_dbtLocate,


   /* Miscellaneous */

   ( DBENTRYP_P )     hb_dbtCompile,
   ( DBENTRYP_I )     hb_dbtError,
   ( DBENTRYP_I )     hb_dbtEvalBlock,


   /* Network operations */

   ( DBENTRYP_VSP )   hb_dbtRawLock,
   ( DBENTRYP_VL )    hb_dbtLock,
   ( DBENTRYP_I )     hb_dbtUnLock,


   /* Memofile functions */

   ( DBENTRYP_V )     hb_dbtCloseMemFile,
   ( DBENTRYP_VP )    hb_dbtCreateMemFile,
   ( DBENTRYP_SVPB )  hb_dbtGetValueFile,
   ( DBENTRYP_VP )    hb_dbtOpenMemFile,
   ( DBENTRYP_SVPB )  hb_dbtPutValueFile,


   /* Database file header handling */

   ( DBENTRYP_V )     hb_dbtReadDBHeader,
   ( DBENTRYP_V )     hb_dbtWriteDBHeader,


   /* non WorkArea functions       */
   ( DBENTRYP_R )     hb_dbtInit,
   ( DBENTRYP_R )     hb_dbtExit,
   ( DBENTRYP_RVV )   hb_dbtDrop,
   ( DBENTRYP_RVV )   hb_dbtExists,
   ( DBENTRYP_RSLV )  hb_dbtRddInfo,

   /* Special and reserved methods */

   ( DBENTRYP_SVP )   hb_dbtWhoCares
};


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
   USHORT uiLen;

   HB_TRACE(HB_TR_DEBUG, ("hb_dbtGetMemoLen(%p, %hu)", pArea, uiIndex));

   ulBlock = hb_dbfGetMemoBlock( ( DBFAREAP ) pArea, uiIndex );
   if( ulBlock == 0 )
      return 0;
   hb_fsSeekLarge( pArea->hMemoFile, ( HB_FOFFSET ) ulBlock * DBT_BLOCKSIZE, FS_SET );
   ulBlock = 0;
   do
   {
      uiIndex = 0;
      uiLen = hb_fsRead( pArea->hMemoFile, pBlock, DBT_BLOCKSIZE );
      if ( uiLen == ( USHORT ) FS_ERROR )
         break;
      while( uiIndex < uiLen && pBlock[ uiIndex ] != 0x1A )
         uiIndex++;
      ulBlock += uiIndex;
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
      hb_fsSeekLarge( pArea->hMemoFile, ( HB_FOFFSET ) ulBlock * DBT_BLOCKSIZE, FS_SET );
      hb_fsReadLarge( pArea->hMemoFile, pBuffer, ulSize );
#ifndef HB_CDP_SUPPORT_OFF
      hb_cdpnTranslate( ( char * ) pBuffer, pArea->cdPage, hb_vmCDP(), ulSize );
#endif
   }
   else
      *pBuffer = '\0';

   hb_itemPutCLPtr( pItem, ( char * ) pBuffer, ulSize );
   hb_itemSetCMemo( pItem );
}

/*
 * Write memo data.
 */
static void hb_dbtWriteMemo( DBTAREAP pArea, ULONG ulBlock, PHB_ITEM pItem, ULONG ulLen,
                             ULONG * ulStoredBlock )
{
   BYTE pAddr[4];
   BOOL bNewBlock;

   HB_TRACE(HB_TR_DEBUG, ("hb_dbtWriteMemo(%p, %lu, %p, %lu, %p)", pArea, ulBlock, pItem,
                           ulLen, ulStoredBlock));

   bNewBlock = !( ulBlock && ulLen < DBT_BLOCKSIZE - 1 );
   if( bNewBlock )
   {
      /* Get next block from header */
      hb_fsSeek( pArea->hMemoFile, 0, FS_SET );
      hb_fsRead( pArea->hMemoFile, pAddr, 4 );
      ulBlock = HB_GET_LE_UINT32( pAddr );
   }
   * ulStoredBlock = ulBlock;

   hb_fsSeekLarge( pArea->hMemoFile, ( HB_FOFFSET ) ulBlock * DBT_BLOCKSIZE, FS_SET );
#ifndef HB_CDP_SUPPORT_OFF
   if( hb_vmCDP() != pArea->cdPage )
   {
      BYTE * pBuff = ( BYTE * ) hb_xalloc( ulLen + 1 );

      if( pBuff )
      {
         memcpy( pBuff, hb_itemGetCPtr( pItem ), ulLen );
         pBuff[ ulLen ] = 0x1A;
         hb_cdpnTranslate( ( char * ) pBuff, hb_vmCDP(), pArea->cdPage, ulLen );
         hb_fsWriteLarge( pArea->hMemoFile, pBuff, ulLen + 1 );
         hb_xfree( pBuff );
      }
      else
      {
         BYTE pBlock[ DBT_BLOCKSIZE ], *pSrc = ( BYTE * ) hb_itemGetCPtr( pItem );
         ULONG ulWritten = 0, ulRest;

         do
         {
            ulRest = HB_MIN( ulLen - ulWritten, DBT_BLOCKSIZE );
            memcpy( pBlock, pSrc + ulWritten, ulRest );
            memset( pBlock + ulRest, 0x1A, DBT_BLOCKSIZE - ulRest );
            hb_cdpnTranslate( ( char * ) pBlock, hb_vmCDP(), pArea->cdPage, ulRest );
            hb_fsWrite( pArea->hMemoFile, pBlock, DBT_BLOCKSIZE );
            ulWritten += DBT_BLOCKSIZE;
         }
         while ( ulWritten <= ulLen );
      }
   }
   else
#endif
   {
      BYTE pBlock[ DBT_BLOCKSIZE ];
      memset( pBlock, 0x1A, DBT_BLOCKSIZE );
      hb_fsWriteLarge( pArea->hMemoFile, ( BYTE * ) hb_itemGetCPtr( pItem ), ulLen );
      hb_fsWrite( pArea->hMemoFile, pBlock, ( DBT_BLOCKSIZE - ( USHORT ) ( ulLen % DBT_BLOCKSIZE ) ) );
   }
   pArea->fMemoFlush = TRUE;

   if( bNewBlock )
   {
      ulBlock += ( ulLen / DBT_BLOCKSIZE ) + 1;
      HB_PUT_LE_UINT32( pAddr, ulBlock );
      hb_fsSeek( pArea->hMemoFile, 0, FS_SET );
      hb_fsWrite( pArea->hMemoFile, pAddr, 4 );
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

   ulLen = hb_itemGetCLen( pItem );
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
static HB_ERRCODE hb_dbtGetVarLen( DBTAREAP pArea, USHORT uiIndex, ULONG * pLength )
{
   BOOL bDeleted;

   HB_TRACE(HB_TR_DEBUG, ("hb_dbtGetVarLen(%p, %hu, %p)", pArea, uiIndex, pLength));

   if( pArea->fHasMemo && pArea->hMemoFile != FS_ERROR )
   {
      /* Force read record */
      if( SELF_DELETED( ( AREAP ) pArea, &bDeleted ) == HB_FAILURE )
         return HB_FAILURE;

      if( pArea->lpFields[ uiIndex - 1 ].uiType == HB_FT_MEMO )
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
         return HB_SUCCESS;
      }
   }

   return SUPER_GETVARLEN( ( AREAP ) pArea, uiIndex, pLength );
}

/*
 * Retrieve information about the current driver.
 * ( DBENTRYP_SI )    hb_dbtInfo
 */
static HB_ERRCODE hb_dbtInfo( DBTAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dbtInfo(%p, %hu, %p)", pArea, uiIndex, pItem));

   switch( uiIndex )
   {
      case DBI_MEMOEXT:
         if( pArea->fHasMemo && pArea->hMemoFile != FS_ERROR )
         {
            PHB_FNAME pFileName;

            pFileName = hb_fsFNameSplit( ( char * ) pArea->szMemoFileName );
            hb_itemPutC( pItem, pFileName->szExtension );
            hb_xfree( pFileName );
         }
         else
         {
            hb_itemClear( pItem );
            return SELF_RDDINFO( SELF_RDDNODE( pArea ), RDDI_MEMOEXT, 0, pItem );
         }
         break;

      case DBI_MEMOBLOCKSIZE:
         if( pArea->fHasMemo && pArea->hMemoFile != FS_ERROR )
            hb_itemPutNI( pItem, pArea->uiMemoBlockSize );
         else
            hb_itemPutNI( pItem, DBT_BLOCKSIZE );
         break;

      case DBI_MEMOTYPE:
         if( pArea->fHasMemo && pArea->hMemoFile != FS_ERROR )
            hb_itemPutNI( pItem, pArea->bMemoType );
         else
            hb_itemPutNI( pItem, DB_MEMO_DBT );
         break;

      /* case DBI_RDD_VERSION */

      default:
         return SUPER_INFO( ( AREAP ) pArea, uiIndex, pItem );
   }

   return HB_SUCCESS;
}

/*
 * Retrieve the size of the WorkArea structure.
 * ( DBENTRYP_SP )    hb_dbtStructSize
 */
static HB_ERRCODE hb_dbtStructSize( DBTAREAP pArea, USHORT * uiSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dbtStrucSize(%p, %p)", pArea, uiSize));
   HB_SYMBOL_UNUSED( pArea );

   * uiSize = sizeof( DBTAREA );
   return HB_SUCCESS;
}

/*
 * Obtain the current value of a field.
 * ( DBENTRYP_SI )    hb_dbtGetValue
 */
static HB_ERRCODE hb_dbtGetValue( DBTAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   BOOL bDeleted;

   HB_TRACE(HB_TR_DEBUG, ("hb_dbtGetValue(%p, %hu, %p)", pArea, uiIndex, pItem));

   if( pArea->fHasMemo && pArea->hMemoFile != FS_ERROR &&
       pArea->lpFields[ uiIndex - 1 ].uiType == HB_FT_MEMO )
   {
      /* Force read record */
      if( SELF_DELETED( ( AREAP ) pArea, &bDeleted ) == HB_FAILURE )
         return HB_FAILURE;

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
         return HB_FAILURE;
      }
      return HB_SUCCESS;
   }
   else
      return SUPER_GETVALUE( ( AREAP ) pArea, uiIndex, pItem );
}

/*
 * Assign a value to a field.
 * ( DBENTRYP_SI )    hb_dbtPutValue
 */
static HB_ERRCODE hb_dbtPutValue( DBTAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   BOOL bDeleted;
   PHB_ITEM pError;
   HB_ERRCODE uiError, uiErrorG;

   HB_TRACE(HB_TR_DEBUG, ("hb_dbtPutValue(%p, %hu, %p)", pArea, uiIndex, pItem));

   if( pArea->fHasMemo && pArea->hMemoFile != FS_ERROR &&
       pArea->lpFields[ uiIndex - 1 ].uiType == HB_FT_MEMO )
   {
      if( HB_IS_MEMO( pItem ) || HB_IS_STRING( pItem ) )
      {
         /* Force read record */
         if( SELF_DELETED( ( AREAP ) pArea, &bDeleted ) == HB_FAILURE )
            return HB_FAILURE;

         if( !pArea->fPositioned )
            return HB_SUCCESS;

         /* Buffer is hot? */
         if( !pArea->fRecordChanged && SELF_GOHOT( ( AREAP ) pArea ) == HB_FAILURE )
            return HB_FAILURE;

         if( hb_dbtFileLockEx( pArea ) )
         {
            uiError = hb_dbtPutMemo( pArea, uiIndex -1, pItem ) ? HB_SUCCESS : EDBF_DATAWIDTH;
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

      if( uiError != HB_SUCCESS )
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
         return HB_FAILURE;
      }
      return HB_SUCCESS;
   }
   return SUPER_PUTVALUE( ( AREAP ) pArea, uiIndex, pItem);
}


/* ( DBENTRYP_V )     hb_dbtCloseMemFile    : NULL */

/*
 * Create a memo file in the WorkArea.
 * ( DBENTRYP_VP )    hb_dbtCreateMemFile
 */
static HB_ERRCODE hb_dbtCreateMemFile( DBTAREAP pArea, LPDBOPENINFO pCreateInfo )
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
         pArea->hMemoFile = hb_fsExtOpen( pCreateInfo->abName, NULL,
                                          FO_READWRITE | FO_EXCLUSIVE | FXO_TRUNCATE |
                                          FXO_DEFAULTS | FXO_SHARELOCK,
                                          NULL, pError );
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
         return HB_FAILURE;
   }
   else /* For zap file */
      hb_fsSeek( pArea->hMemoFile, 0, FS_SET );

   memset( pBlock, 0, DBT_BLOCKSIZE );
   * ( ( LONG * ) pBlock ) = 1;
   if( hb_fsWrite( pArea->hMemoFile, pBlock, DBT_BLOCKSIZE ) != DBT_BLOCKSIZE )
      return HB_FAILURE;
   hb_fsWrite( pArea->hMemoFile, NULL, 0 );
   pArea->fMemoFlush = TRUE;

   return HB_SUCCESS;
}


/* ( DBENTRYP_SVPB )  hb_dbtGetValueFile    : NULL */

/*
 * Open a memo file in the specified WorkArea.
 * ( DBENTRYP_VP )    hb_dbtOpenMemFile
 */
static HB_ERRCODE hb_dbtOpenMemFile( DBTAREAP pArea, LPDBOPENINFO pOpenInfo )
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
      pArea->hMemoFile = hb_fsExtOpen( pOpenInfo->abName, NULL, uiFlags |
                                       FXO_DEFAULTS | FXO_SHARELOCK,
                                       NULL, pError );
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

   return ( pArea->hMemoFile == FS_ERROR ? HB_FAILURE : HB_SUCCESS );
}

/* ( DBENTRYP_SVPB )  hb_dbtPutValueFile    : NULL */

/*
 * Retrieve (set) information about RDD
 * ( DBENTRYP_RSLV )   hb_dbtFieldInfo
 */
static HB_ERRCODE hb_dbtRddInfo( LPRDDNODE pRDD, USHORT uiIndex, ULONG ulConnect, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dbtRddInfo(%p, %hu, %lu, %p)", pRDD, uiIndex, ulConnect, pItem));

   switch( uiIndex )
   {
      case RDDI_MEMOEXT:
      {
         LPDBFDATA pData = ( LPDBFDATA ) pRDD->lpvCargo;
         char *szNew = hb_itemGetCPtr( pItem );

         if( szNew[0] == '.' && szNew[1] )
            szNew = hb_strdup( szNew );
         else
            szNew = NULL;

         hb_itemPutC( pItem, pData->szMemoExt[ 0 ] ? pData->szMemoExt : DBT_MEMOEXT );
         if( szNew )
         {
            hb_strncpy( pData->szMemoExt, szNew, HB_MAX_FILE_EXT );
            hb_xfree( szNew );
         }
         break;
      }
      case RDDI_MEMOBLOCKSIZE:
         hb_itemPutNI( pItem, DBT_BLOCKSIZE );
         break;

      case RDDI_MEMOTYPE:
         hb_itemPutNI( pItem, DB_MEMO_DBT );
         break;

      case RDDI_MEMOGCTYPE:
         hb_itemPutNI( pItem, 0 );
         break;

      case RDDI_MEMOREADLOCK:
      case RDDI_MEMOREUSE:
         hb_itemPutL( pItem, FALSE );
         break;

      default:
         return SUPER_RDDINFO( pRDD, uiIndex, ulConnect, pItem );
   }

   return HB_SUCCESS;
}


HB_FUNC( DBFDBT ) {;}

HB_FUNC( DBFDBT_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;

   uiCount = ( USHORT * ) hb_parptr( 1 );
   pTable = ( RDDFUNCS * ) hb_parptr( 2 );

   HB_TRACE(HB_TR_DEBUG, ("DBFDBT_GETFUNCTABLE(%p, %p)", uiCount, pTable));

   if( pTable )
   {
      if ( uiCount )
         * uiCount = RDDFUNCSCOUNT;
      hb_retni( hb_rddInherit( pTable, &dbtTable, &dbtSuper, "DBF" ) );
   }
   else
      hb_retni( HB_FAILURE );
}


#define __PRG_SOURCE__ __FILE__

#ifdef HB_PCODE_VER
#  undef HB_PRG_PCODE_VER
#  define HB_PRG_PCODE_VER HB_PCODE_VER
#endif

HB_FUNC_EXTERN( _DBF );

static void hb_dbfdbtRddInit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( hb_rddRegister( "DBF",    RDT_FULL ) > 1 ||
       hb_rddRegister( "DBFDBT", RDT_FULL ) > 1 )
   {
      hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );

      /* not executed, only to force DBF RDD linking */
      HB_FUNC_EXEC( _DBF );
   }
}

HB_INIT_SYMBOLS_BEGIN( dbfdbt1__InitSymbols )
{ "DBFDBT",              {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( DBFDBT )}, NULL },
{ "DBFDBT_GETFUNCTABLE", {HB_FS_PUBLIC|HB_FS_LOCAL}, {HB_FUNCNAME( DBFDBT_GETFUNCTABLE )}, NULL }
HB_INIT_SYMBOLS_END( dbfdbt1__InitSymbols )

HB_CALL_ON_STARTUP_BEGIN( _hb_dbfdbt_rdd_init_ )
   hb_vmAtInit( hb_dbfdbtRddInit, NULL );
HB_CALL_ON_STARTUP_END( _hb_dbfdbt_rdd_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup dbfdbt1__InitSymbols
   #pragma startup _hb_dbfdbt_rdd_init_
#elif defined( HB_MSC_STARTUP )
   #if defined( HB_OS_WIN_64 )
      #pragma section( HB_MSC_START_SEGMENT, long, read )
   #endif
   #pragma data_seg( HB_MSC_START_SEGMENT )
   static HB_$INITSYM hb_vm_auto_dbfdbt1__InitSymbols = dbfdbt1__InitSymbols;
   static HB_$INITSYM hb_vm_auto_dbfdbt_rdd_init = _hb_dbfdbt_rdd_init_;
   #pragma data_seg()
#endif
