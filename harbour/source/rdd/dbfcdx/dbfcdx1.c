/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DBFCDX RDD
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#include "hbapi.h"
#include "hbinit.h"
#include "hbvm.h"
#include "hbapiitm.h"
#include "hbrddcdx.h"
#include "hbdbf.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbset.h"
#include "hbdate.h"
#include "error.ch"

extern HB_FUNC( _DBFCDX );
extern HB_FUNC( DBFCDX_GETFUNCTABLE );

HB_INIT_SYMBOLS_BEGIN( dbfcdx1__InitSymbols )
{ "_DBFCDX",             HB_FS_PUBLIC, HB_FUNCNAME( _DBFCDX ), NULL },
{ "DBFCDX_GETFUNCTABLE", HB_FS_PUBLIC, HB_FUNCNAME( DBFCDX_GETFUNCTABLE ), NULL }
HB_INIT_SYMBOLS_END( dbfcdx1__InitSymbols )
#if defined(_MSC_VER)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto_dbfcdx1__InitSymbols = dbfcdx1__InitSymbols;
   #pragma data_seg()
#elif ! defined(__GNUC__)
   #pragma startup dbfcdx1__InitSymbols
#endif

static USHORT s_uiMemoBlockSize = 64;        /* Default block memo size */

static RDDFUNCS cdxSuper = { NULL };

static RDDFUNCS cdxTable = { ( DBENTRYP_BP ) hb_cdxBof,
                             ( DBENTRYP_BP ) hb_cdxEof,
                             ( DBENTRYP_BP ) hb_cdxFound,
                             ( DBENTRYP_V ) hb_cdxGoBottom,
                             ( DBENTRYP_UL ) hb_cdxGoTo,
                             ( DBENTRYP_I ) hb_cdxGoToId,
                             ( DBENTRYP_V ) hb_cdxGoTop,
                             ( DBENTRYP_BIB ) hb_cdxSeek,
                             ( DBENTRYP_L ) hb_cdxSkip,
                             ( DBENTRYP_L ) hb_cdxSkipFilter,
                             ( DBENTRYP_L ) hb_cdxSkipRaw,
                             ( DBENTRYP_VF ) hb_cdxAddField,
                             ( DBENTRYP_B ) hb_cdxAppend,
                             ( DBENTRYP_I ) hb_cdxCreateFields,
                             ( DBENTRYP_V ) hb_cdxDeleteRec,
                             ( DBENTRYP_BP ) hb_cdxDeleted,
                             ( DBENTRYP_SP ) hb_cdxFieldCount,
                             ( DBENTRYP_VF ) hb_cdxFieldDisplay,
                             ( DBENTRYP_SSI ) hb_cdxFieldInfo,
                             ( DBENTRYP_SVP ) hb_cdxFieldName,
                             ( DBENTRYP_V ) hb_cdxFlush,
                             ( DBENTRYP_PP ) hb_cdxGetRec,
                             ( DBENTRYP_SI ) hb_cdxGetValue,
                             ( DBENTRYP_SVL ) hb_cdxGetVarLen,
                             ( DBENTRYP_V ) hb_cdxGoCold,
                             ( DBENTRYP_V ) hb_cdxGoHot,
                             ( DBENTRYP_P ) hb_cdxPutRec,
                             ( DBENTRYP_SI ) hb_cdxPutValue,
                             ( DBENTRYP_V ) hb_cdxRecAll,
                             ( DBENTRYP_ULP ) hb_cdxRecCount,
                             ( DBENTRYP_ISI ) hb_cdxRecInfo,
                             ( DBENTRYP_I ) hb_cdxRecNo,
                             ( DBENTRYP_S ) hb_cdxSetFieldExtent,
                             ( DBENTRYP_P ) hb_cdxAlias,
                             ( DBENTRYP_V ) hb_cdxClose,
                             ( DBENTRYP_VP ) hb_cdxCreate,
                             ( DBENTRYP_SI ) hb_cdxInfo,
                             ( DBENTRYP_V ) hb_cdxNewArea,
                             ( DBENTRYP_VP ) hb_cdxOpen,
                             ( DBENTRYP_V ) hb_cdxRelease,
                             ( DBENTRYP_SP ) hb_cdxStructSize,
                             ( DBENTRYP_P ) hb_cdxSysName,
                             ( DBENTRYP_VEI ) hb_cdxEval,
                             ( DBENTRYP_V ) hb_cdxPack,
                             ( DBENTRYP_LSP ) hb_cdxPackRec,
                             ( DBENTRYP_VS ) hb_cdxSort,
                             ( DBENTRYP_VT ) hb_cdxTrans,
                             ( DBENTRYP_VT ) hb_cdxTransRec,
                             ( DBENTRYP_V ) hb_cdxZap,
                             ( DBENTRYP_VR ) hb_cdxChildEnd,
                             ( DBENTRYP_VR ) hb_cdxChildStart,
                             ( DBENTRYP_VR ) hb_cdxChildSync,
                             ( DBENTRYP_V ) hb_cdxSyncChildren,
                             ( DBENTRYP_V ) hb_cdxClearRel,
                             ( DBENTRYP_V ) hb_cdxForceRel,
                             ( DBENTRYP_SVP ) hb_cdxRelArea,
                             ( DBENTRYP_VR ) hb_cdxRelEval,
                             ( DBENTRYP_SVP ) hb_cdxRelText,
                             ( DBENTRYP_VR ) hb_cdxSetRel,
                             ( DBENTRYP_OI ) hb_cdxOrderListAdd,
                             ( DBENTRYP_V ) hb_cdxOrderListClear,
                             ( DBENTRYP_VP ) hb_cdxOrderListDelete,
                             ( DBENTRYP_OI ) hb_cdxOrderListFocus,
                             ( DBENTRYP_V ) hb_cdxOrderListRebuild,
                             ( DBENTRYP_VOI ) hb_cdxOrderCondition,
                             ( DBENTRYP_VOC ) hb_cdxOrderCreate,
                             ( DBENTRYP_OI ) hb_cdxOrderDestroy,
                             ( DBENTRYP_OII ) hb_cdxOrderInfo,
                             ( DBENTRYP_V ) hb_cdxClearFilter,
                             ( DBENTRYP_V ) hb_cdxClearLocate,
                             ( DBENTRYP_V ) hb_cdxClearScope,
                             ( DBENTRYP_VPLP ) hb_cdxCountScope,
                             ( DBENTRYP_I ) hb_cdxFilterText,
                             ( DBENTRYP_SI ) hb_cdxScopeInfo,
                             ( DBENTRYP_VFI ) hb_cdxSetFilter,
                             ( DBENTRYP_VLO ) hb_cdxSetLocate,
                             ( DBENTRYP_VP ) hb_cdxSetScope,
                             ( DBENTRYP_VPL ) hb_cdxSkipScope,
                             ( DBENTRYP_P ) hb_cdxCompile,
                             ( DBENTRYP_I ) hb_cdxError,
                             ( DBENTRYP_I ) hb_cdxEvalBlock,
                             ( DBENTRYP_VSP ) hb_cdxRawLock,
                             ( DBENTRYP_VL ) hb_cdxLock,
                             ( DBENTRYP_UL ) hb_cdxUnLock,
                             ( DBENTRYP_V ) hb_cdxCloseMemFile,
                             ( DBENTRYP_VP ) hb_cdxCreateMemFile,
                             ( DBENTRYP_SVPB ) hb_cdxGetValueFile,
                             ( DBENTRYP_VP ) hb_cdxOpenMemFile,
                             ( DBENTRYP_SVP ) hb_cdxPutValueFile,
                             ( DBENTRYP_V ) hb_cdxReadDBHeader,
                             ( DBENTRYP_V ) hb_cdxWriteDBHeader,
                             ( DBENTRYP_SVP ) hb_cdxWhoCares
                           };

/*
 * Common functions.
 */

/*
 * Swap bytes.
 */
ULONG hb_cdxSwapBytes( ULONG ulLong )
{
   BYTE * pLong, bByte;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxSwapBytes(%lu)", ulLong));

   pLong = ( BYTE * ) &ulLong;
   bByte = pLong[ 0 ];
   pLong[ 0 ] = pLong[ 3 ];
   pLong[ 3 ] = bByte;
   bByte = pLong[ 1 ];
   pLong[ 1 ] = pLong[ 2 ];
   pLong[ 2 ] = bByte;
   return ulLong;
}

/*
 * Converts memo block offset into ASCII.
 */
static ULONG hb_cdxGetMemoBlock( CDXAREAP pArea, USHORT uiIndex )
{
   USHORT uiCount;
   BYTE bByte;
   ULONG ulBlock;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxGetMemoBlock(%p, %hu)", pArea, uiIndex));

   ulBlock = 0;
   for( uiCount = 0; uiCount < 10; uiCount++ )
   {
      bByte = pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] + uiCount ];
      if( bByte >= '0' && bByte <= '9' )
         ulBlock = ulBlock * 10 + ( bByte - '0' );
   }
   return ulBlock;
}

/*
 * Converts ASCII data into memo block offset.
 */
static void hb_cdxPutMemoBlock( CDXAREAP pArea, USHORT uiIndex, ULONG ulBlock )
{
   SHORT iCount;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxPutMemoBlock(%p, %hu, %lu)", pArea, uiIndex, ulBlock));

   for( iCount = 9; iCount >= 0; iCount-- )
   {
      if( ulBlock > 0 )
      {
         pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] + iCount ] = ( BYTE )( ulBlock % 10 ) + '0';
         ulBlock /= 10;
      }
      else
         pArea->pRecord[ pArea->pFieldOffset[ uiIndex ] + iCount ] = ' ';
   }
}

/*
 * Return the size of memo.
 */
static ULONG hb_cdxGetMemoLen( CDXAREAP pArea, USHORT uiIndex )
{
   ULONG ulBlock;
   MEMOBLOCK mbBlock;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxGetMemoLen(%p, %hu)", pArea, uiIndex));

   ulBlock = hb_cdxGetMemoBlock( pArea, uiIndex );
   if( ulBlock == 0 )
      return 0;
   hb_fsSeek( pArea->hMemoFile, ulBlock * pArea->uiMemoBlockSize, FS_SET );
   if( hb_fsRead( pArea->hMemoFile, ( BYTE * ) &mbBlock, sizeof( MEMOBLOCK ) ) !=
       sizeof( MEMOBLOCK ) )
      return 0;
   return hb_cdxSwapBytes( mbBlock.ulSize );
}

/*
 * Read memo data.
 */
static void hb_cdxGetMemo( CDXAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   ULONG ulBlock, ulSize;
   BYTE * pBuffer;
   MEMOBLOCK mbBlock;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxGetMemo(%p, %hu, %p)", pArea, uiIndex, pItem));

   ulBlock = hb_cdxGetMemoBlock( pArea, uiIndex );
   if( ulBlock > 0 )
   {
      hb_fsSeek( pArea->hMemoFile, ulBlock * pArea->uiMemoBlockSize, FS_SET );
      if( hb_fsRead( pArea->hMemoFile, ( BYTE * ) &mbBlock, sizeof( MEMOBLOCK ) ) !=
          sizeof( MEMOBLOCK ) )
         ulSize = 0;
      else
         ulSize = hb_cdxSwapBytes( mbBlock.ulSize );
      if( ulSize > 0 && ulSize < 0xFFFF )
      {
         pBuffer = ( BYTE * ) hb_xgrab( ulSize + 1 );
         hb_fsReadLarge( pArea->hMemoFile, pBuffer, ulSize );
         hb_itemPutCPtr( pItem, ( char * ) pBuffer, ulSize );
         hb_itemSetCMemo( pItem );
      }
   }
}

/*
 * Append blocks to free memo blocks list.
 */
static void hb_cdxAddFreeBlocks( CDXAREAP pArea, ULONG ulBlock, USHORT uiBlocks )
{
   SHORT iCount;
   BOOL bFound;
   LPMEMOFREEBLOCK pFreeBlock;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxAddFreeBlocks(%p, %lu, %hu)", pArea, ulBlock, uiBlocks));

   bFound = FALSE;
   for( iCount = pArea->pMemoRoot->uiListLen - 1; iCount >= 0; iCount-- )
   {
      pFreeBlock = ( LPMEMOFREEBLOCK ) ( pArea->pMemoRoot->pFreeList + iCount * SIZEOFMEMOFREEBLOCK );
      if( pFreeBlock->ulBlock < ulBlock )
      {
         /* Can grow current free block? */
         if( pFreeBlock->ulBlock + pFreeBlock->uiBlocks == uiBlocks )
         {
            pFreeBlock->uiBlocks += uiBlocks;
            ulBlock = pFreeBlock->ulBlock;
            uiBlocks = pFreeBlock->uiBlocks;
         }
         /* Can append to next free block? */
         else if( iCount < pArea->pMemoRoot->uiListLen - 1 &&
                  ulBlock + uiBlocks == ( pFreeBlock + 1 )->ulBlock )
         {
            pFreeBlock++;
            iCount++;
            pFreeBlock->ulBlock = ulBlock;
            pFreeBlock->uiBlocks += uiBlocks;
         }
         /* Append to next free block */
         else
         {
            pFreeBlock++;
            iCount++;
            memmove( pFreeBlock + 1, pFreeBlock,
                     ( HB_MIN( MAXFREEBLOCKS, pArea->pMemoRoot->uiListLen ) - iCount ) *
                     sizeof( SIZEOFMEMOFREEBLOCK ) );
            pFreeBlock->ulBlock = ulBlock;
            pFreeBlock->uiBlocks = uiBlocks;
            pArea->pMemoRoot->uiListLen = HB_MIN( MAXFREEBLOCKS, pArea->pMemoRoot->uiListLen + 1 );
         }
         pArea->pMemoRoot->fChanged = TRUE;
         bFound = TRUE;
         break;
      }
   }

   /* If is the last block in memo file truncate it */
   if( ulBlock + uiBlocks == pArea->pMemoRoot->ulNextBlock )
   {
      pArea->pMemoRoot->ulNextBlock = ulBlock;
      pArea->pMemoRoot->fChanged = TRUE;
      if( bFound )
      {
         memmove( pFreeBlock, pFreeBlock + 1,
                  ( HB_MIN( MAXFREEBLOCKS, pArea->pMemoRoot->uiListLen ) - iCount ) *
                  sizeof( SIZEOFMEMOFREEBLOCK ) );
         pArea->pMemoRoot->uiListLen--;
      }
      hb_fsSeek( pArea->hMemoFile, ulBlock * pArea->uiMemoBlockSize, FS_SET );
      hb_fsWrite( pArea->hMemoFile, NULL, 0 );
      return;
   }

   /* Insert free block */
   if( !bFound && pArea->pMemoRoot->uiListLen < MAXFREEBLOCKS )
   {
      pFreeBlock = ( LPMEMOFREEBLOCK ) pArea->pMemoRoot->pFreeList;
      memmove( pFreeBlock + 1, pFreeBlock,
               HB_MIN( MAXFREEBLOCKS, pArea->pMemoRoot->uiListLen ) * sizeof( SIZEOFMEMOFREEBLOCK ) );
      pFreeBlock->ulBlock = ulBlock;
      pFreeBlock->uiBlocks = uiBlocks;
      pArea->pMemoRoot->uiListLen = HB_MIN( MAXFREEBLOCKS, pArea->pMemoRoot->uiListLen + 1 );
      pArea->pMemoRoot->fChanged = TRUE;
   }
}

/*
 * Try get free memo blocks from list.
 */
static BOOL hb_cdxCompleteFromFreeBlocks( CDXAREAP pArea, ULONG ulBlock, USHORT uiBlocks )
{
   USHORT uiCount;
   LPMEMOFREEBLOCK pFreeBlock;
   MEMOBLOCK mbBlock;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxCompleteFromFreeBlocks(%p, %lu, %hu)", pArea, ulBlock, uiBlocks));

   for( uiCount = 0; uiCount < pArea->pMemoRoot->uiListLen; uiCount++ )
   {
      pFreeBlock = ( LPMEMOFREEBLOCK ) ( pArea->pMemoRoot->pFreeList + uiCount * SIZEOFMEMOFREEBLOCK );
      if( pFreeBlock->ulBlock == ulBlock )
      {
         if( pFreeBlock->uiBlocks >= uiBlocks )
         {
            /* Same size, remove it */
            if( pFreeBlock->uiBlocks == uiBlocks )
            {
               memmove( pFreeBlock, pFreeBlock + SIZEOFMEMOFREEBLOCK,
                        ( pArea->pMemoRoot->uiListLen - uiCount ) * SIZEOFMEMOFREEBLOCK );
               memset( pArea->pMemoRoot->pFreeList + ( pArea->pMemoRoot->uiListLen - 1 ) *
                       SIZEOFMEMOFREEBLOCK, 0, SIZEOFMEMOFREEBLOCK );
               pArea->pMemoRoot->uiListLen--;
            }
            else /* Adjust new free size */
            {
               pFreeBlock->ulBlock += uiBlocks;
               pFreeBlock->uiBlocks -= uiBlocks;
               mbBlock.ulType = mbBlock.ulSize = 0;
               hb_fsSeek( pArea->hMemoFile, pFreeBlock->ulBlock * pArea->uiMemoBlockSize, FS_SET );
               hb_fsWrite( pArea->hMemoFile, ( BYTE * ) &mbBlock, sizeof( MEMOBLOCK ) );
            }
            pArea->pMemoRoot->fChanged = TRUE;
            return TRUE;
         }
         else
            break;
      }
   }
   return FALSE;
}

/*
 * Get free memo blocks from list or return a new block.
 */
static void hb_cdxGetFreeBlocks( CDXAREAP pArea, USHORT uiBlocks, ULONG * ulBlock )
{
   USHORT uiCount;
   LPMEMOFREEBLOCK pFreeBlock;
   MEMOBLOCK mbBlock;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxGetFreeBlocks(%p, %hu, %p)", pArea, uiBlocks, ulBlock));

   for( uiCount = 0; uiCount < pArea->pMemoRoot->uiListLen; uiCount++ )
   {
      pFreeBlock = ( LPMEMOFREEBLOCK ) ( pArea->pMemoRoot->pFreeList + uiCount * SIZEOFMEMOFREEBLOCK );
      if( uiBlocks <= pFreeBlock->uiBlocks )
      {
         * ulBlock = pFreeBlock->ulBlock;

         /* Same size, remove it */
         if( pFreeBlock->uiBlocks == uiBlocks )
         {
            memmove( pFreeBlock, pFreeBlock + SIZEOFMEMOFREEBLOCK,
                     ( pArea->pMemoRoot->uiListLen - uiCount ) * SIZEOFMEMOFREEBLOCK );
            memset( pArea->pMemoRoot->pFreeList + ( pArea->pMemoRoot->uiListLen - 1 ) *
                    SIZEOFMEMOFREEBLOCK, 0, SIZEOFMEMOFREEBLOCK );
            pArea->pMemoRoot->uiListLen--;
         }
         else /* Adjust new free size */
         {
            pFreeBlock->ulBlock += uiBlocks;
            pFreeBlock->uiBlocks -= uiBlocks;
            mbBlock.ulType = mbBlock.ulSize = 0;
            hb_fsSeek( pArea->hMemoFile, pFreeBlock->ulBlock * pArea->uiMemoBlockSize, FS_SET );
            hb_fsWrite( pArea->hMemoFile, ( BYTE * ) &mbBlock, sizeof( MEMOBLOCK ) );
         }
         pArea->pMemoRoot->fChanged = TRUE;
         return;
      }
   }

   /* Not found a free block */
   * ulBlock = pArea->pMemoRoot->ulNextBlock;
   pArea->pMemoRoot->ulNextBlock += uiBlocks;
   pArea->pMemoRoot->fChanged = TRUE;
}

/*
 * Write memo data.
 */
static void hb_cdxWriteMemo( CDXAREAP pArea, ULONG ulBlock, PHB_ITEM pItem, ULONG ulLen,
                             ULONG * ulStoredBlock, USHORT uiType )
{
   USHORT uiBloksRequired, uiBlocksUsed;
   MEMOBLOCK mbBlock;
   BOOL bWriteBlocks;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxWriteMemo(%p, %lu, %p, %lu, %p, %hu)", pArea, ulBlock,
                           pItem, ulLen, ulNewBlock, uiType));

   uiBloksRequired = ( USHORT ) ( ulLen + sizeof( MEMOBLOCK ) + pArea->uiMemoBlockSize - 1 ) /
                                  pArea->uiMemoBlockSize;
   if( ulBlock > 0 )
   {
      hb_fsSeek( pArea->hMemoFile, ulBlock * pArea->uiMemoBlockSize, FS_SET );
      hb_fsRead( pArea->hMemoFile, ( BYTE * ) &mbBlock, sizeof( MEMOBLOCK ) );
      uiBlocksUsed = ( USHORT ) ( hb_cdxSwapBytes( mbBlock.ulSize ) + sizeof( MEMOBLOCK ) +
                                  pArea->uiMemoBlockSize - 1 ) / pArea->uiMemoBlockSize;
   }

   bWriteBlocks = FALSE;
   /* Use same space */
   if( ulBlock > 0 && uiBlocksUsed >= uiBloksRequired )
   {
      * ulStoredBlock = ulBlock;
      bWriteBlocks = TRUE;

      /* Free space */
      if( uiBlocksUsed > uiBloksRequired )
         hb_cdxAddFreeBlocks( pArea, ulBlock + uiBloksRequired, uiBlocksUsed - uiBloksRequired );
   }
   else /* Need more space */
   {
      if( ulBlock > 0 )
      {
         if( hb_cdxCompleteFromFreeBlocks( pArea, ulBlock + uiBlocksUsed,
                                           uiBloksRequired - uiBlocksUsed ) )
            bWriteBlocks = TRUE;
         else /* Free all blocks */
            hb_cdxAddFreeBlocks( pArea, ulBlock, uiBlocksUsed );
      }

      if( !bWriteBlocks )
      {
         hb_cdxGetFreeBlocks( pArea, uiBloksRequired, ulStoredBlock );
         bWriteBlocks = TRUE;
      }
   }

   /* Write memo header and data */
   if( bWriteBlocks )
   {
      mbBlock.ulType = hb_cdxSwapBytes( uiType );
      mbBlock.ulSize = hb_cdxSwapBytes( ulLen );
      hb_fsSeek( pArea->hMemoFile, * ulStoredBlock * pArea->uiMemoBlockSize, FS_SET );
      hb_fsWrite( pArea->hMemoFile, ( BYTE * ) &mbBlock, sizeof( MEMOBLOCK ) );
      hb_fsWriteLarge( pArea->hMemoFile, ( BYTE * ) hb_itemGetCPtr( pItem ), ulLen );
   }
}

/*
 * Assign a value to the specified memo field.
 */
static BOOL hb_cdxPutMemo( CDXAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   USHORT uiBlocksUsed;
   ULONG ulLen, ulBlock;
   MEMOBLOCK mbBlock;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxPutMemo(%p, %hu, %p)", pArea, uiIndex, pItem));

   ulLen = hb_itemGetCLen( pItem );
   ulBlock = hb_cdxGetMemoBlock( pArea, uiIndex );
   if( ulLen > 0 )
      hb_cdxWriteMemo( pArea, ulBlock, pItem, ulLen, &ulBlock, 1 );
   else
   {
      /* Free blocks */
      if( ulBlock > 0 )
      {
         hb_fsSeek( pArea->hMemoFile, ulBlock * pArea->uiMemoBlockSize, FS_SET );
         hb_fsRead( pArea->hMemoFile, ( BYTE * ) &mbBlock, sizeof( MEMOBLOCK ) );
         uiBlocksUsed = ( USHORT ) ( hb_cdxSwapBytes( mbBlock.ulSize ) +
                                     pArea->uiMemoBlockSize - 1 ) / pArea->uiMemoBlockSize;
         hb_cdxAddFreeBlocks( pArea, ulBlock, uiBlocksUsed );
      }
      ulBlock = 0;
   }
   if( pArea->fShared && pArea->pMemoRoot->fChanged )
   {
      pArea->pMemoRoot->fChanged = FALSE;
      hb_fsSeek( pArea->hMemoFile, 0, FS_SET );
      pArea->pMemoRoot->ulNextBlock = hb_cdxSwapBytes( pArea->pMemoRoot->ulNextBlock );
      pArea->pMemoRoot->ulBlockSize = hb_cdxSwapBytes( pArea->pMemoRoot->ulBlockSize );
      hb_fsWrite( pArea->hMemoFile, ( BYTE * ) pArea->pMemoRoot, sizeof( MEMOROOT ) );
      pArea->pMemoRoot->ulNextBlock = hb_cdxSwapBytes( pArea->pMemoRoot->ulNextBlock );
      pArea->pMemoRoot->ulBlockSize = hb_cdxSwapBytes( pArea->pMemoRoot->ulBlockSize );
   }
   hb_cdxPutMemoBlock( pArea, uiIndex, ulBlock );
   return TRUE;
}

/*
 * -- DBFCDX METHODS --
 */

/*
 * Obtain the current value of a field.
 */
ERRCODE hb_cdxGetValue( CDXAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   BOOL bDeleted;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxGetValue(%p, %hu, %p)", pArea, uiIndex, pItem));

   if( pArea->lpFields[ uiIndex - 1 ].uiType == HB_IT_MEMO )
   {
      /* Force read record */
      if( SELF_DELETED( ( AREAP ) pArea, &bDeleted ) == FAILURE )
         return FAILURE;

      hb_cdxGetMemo( pArea, uiIndex - 1, pItem );
      return SUCCESS;
   }
   else
      return SUPER_GETVALUE( ( AREAP ) pArea, uiIndex, pItem );
}

/*
 * Obtain the length of a field value.
 */
ERRCODE hb_cdxGetVarLen( CDXAREAP pArea, USHORT uiIndex, ULONG * pLength )
{
   BOOL bDeleted;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxGetVarLen(%p, %hu, %p)", pArea, uiIndex, pLength));

   /* Force read record */
   if( SELF_DELETED( ( AREAP ) pArea, &bDeleted ) == FAILURE )
      return FAILURE;

   if( pArea->fHasMemo )
      * pLength = hb_cdxGetMemoLen( pArea, uiIndex - 1 );
   else
      * pLength = 0;

   return SUCCESS;
}

/*
 * Assign a value to a field.
 */
ERRCODE hb_cdxPutValue( CDXAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   BOOL bDeleted;
   PHB_ITEM pError;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxPutValue(%p, %hu, %p)", pArea, uiIndex, pItem));

   if( pArea->lpFields[ uiIndex - 1 ].uiType == HB_IT_MEMO )
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
         
         if( !hb_cdxPutMemo( pArea, uiIndex - 1, pItem ) )
         {
            pError = hb_errNew();
            hb_errPutGenCode( pError, EG_DATAWIDTH );
            hb_errPutDescription( pError, hb_langDGetErrorDesc( EDBF_DATAWIDTH ) );
            hb_errPutSubCode( pError, EDBF_DATAWIDTH );
            SELF_ERROR( ( AREAP ) pArea, pError );
            hb_errRelease( pError );
            return FAILURE;
         }

         /* Update deleted flag */
         pArea->pRecord[ 0 ] = pArea->fDeleted ? '*' : ' ';
         return SUCCESS;
      }
   }
   return SUPER_PUTVALUE( ( AREAP ) pArea, uiIndex, pItem);
}

/*
 * Close the table in the WorkArea.
 */
ERRCODE hb_cdxClose( CDXAREAP pArea )
{
   ERRCODE uiError;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxClose(%p)", pArea));

/*   SELF_ORDLSTCLEAR( ( AREAP ) pArea ); */

   /* Free Indexes array */
   if( pArea->lpIndexes )
   {
      hb_xfree( pArea->lpIndexes );
      pArea->lpIndexes = NULL;
   }
   uiError = SUPER_CLOSE( ( AREAP ) pArea );

   /* Free root memo block */
   if( pArea->fHasMemo && pArea->pMemoRoot )
   {
      if( pArea->pMemoRoot->fChanged )
      {
         pArea->pMemoRoot->fChanged = FALSE;
         hb_fsSeek( pArea->hMemoFile, 0, FS_SET );
         pArea->pMemoRoot->ulNextBlock = hb_cdxSwapBytes( pArea->pMemoRoot->ulNextBlock );
         pArea->pMemoRoot->ulBlockSize = hb_cdxSwapBytes( pArea->pMemoRoot->ulBlockSize );
         hb_fsWrite( pArea->hMemoFile, ( BYTE * ) pArea->pMemoRoot, sizeof( MEMOROOT ) );
      }
      hb_xfree( pArea->pMemoRoot );
      pArea->pMemoRoot = NULL;
   }
   return uiError;
}

/*
 * Retrieve information about the current driver.
 */
ERRCODE hb_cdxInfo( CDXAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxInfo(%p, %hu, %p)", pArea, uiIndex, pItem));

   switch( uiIndex )
   {
      case DBI_MEMOEXT:
         hb_itemPutC( pItem, CDX_MEMOEXT );
         break;

      case DBI_MEMOBLOCKSIZE:
         hb_itemPutNI( pItem, pArea->uiMemoBlockSize );
         break;

      default:
         return SUPER_INFO( ( AREAP ) pArea, uiIndex, pItem );
   }

   return SUCCESS;
}

/*
 * Open a data store in the WorkArea.
 */
ERRCODE hb_cdxOpen( CDXAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxOpen(%p, %p)", pArea, pOpenInfo));


   /* Force exclusive mode */
   if( hb_set.HB_SET_AUTOSHARE == 2 )
      pOpenInfo->fShared = FALSE;

   if( SUPER_OPEN( ( AREAP ) pArea, pOpenInfo ) == FAILURE )
      return FAILURE;

   /* Alloc root memo block and read data */
   if( pArea->fHasMemo )
   {
      pArea->pMemoRoot = ( LPMEMOROOT ) hb_xgrab( sizeof( MEMOROOT ) );
      memset( pArea->pMemoRoot, 0, sizeof( MEMOROOT ) );
      hb_fsSeek( pArea->hMemoFile, 0, FS_SET );
      if( hb_fsRead( pArea->hMemoFile, ( BYTE * ) pArea->pMemoRoot, sizeof( MEMOROOT ) ) !=
          sizeof( MEMOROOT ) )
         return FAILURE;
      if( pArea->pMemoRoot->szSignature[ 0 ] == 0 )
      {
         strcpy( ( char * ) pArea->pMemoRoot->szSignature, "Harbour" );
         hb_fsSeek( pArea->hMemoFile, 0, FS_SET );
         hb_fsWrite( pArea->hMemoFile, ( BYTE * ) pArea->pMemoRoot, sizeof( MEMOROOT ) );
      }
      pArea->pMemoRoot->ulNextBlock = hb_cdxSwapBytes( pArea->pMemoRoot->ulNextBlock );
      pArea->pMemoRoot->ulBlockSize = hb_cdxSwapBytes( pArea->pMemoRoot->ulBlockSize );
   }

   /* If SET_AUTOPEN open index */
   if( pArea->fHasTags && hb_set.HB_SET_AUTOPEN )
   {
      printf("TODO: hb_cdxOpen()\n");
   }
   return SUCCESS;
}

/*
 * Retrieve the size of the WorkArea structure.
 */
ERRCODE hb_cdxStructSize( CDXAREAP pArea, USHORT * uiSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxStrucSize(%p, %p)", pArea, uiSize));
   HB_SYMBOL_UNUSED( pArea );

   * uiSize = sizeof( CDXAREA );
   return SUCCESS;
}

/*
 * Obtain the name of replaceable database driver (RDD) subsystem.
 */
ERRCODE hb_cdxSysName( CDXAREAP pArea, BYTE * pBuffer )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxSysName(%p, %p)", pArea, pBuffer));
   HB_SYMBOL_UNUSED( pArea );

   strncpy( ( char * ) pBuffer, "DBFCDX", HARBOUR_MAX_RDD_DRIVERNAME_LENGTH );
   return SUCCESS;
}

/*
 * Create a memo file in the WorkArea.
 */
ERRCODE hb_cdxCreateMemFile( CDXAREAP pArea, LPDBOPENINFO pCreateInfo )
{
   MEMOHEADER fptHeader;
   BYTE pBlock[ 512 ];
   BOOL bRetry;
   PHB_ITEM pError;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxCreateMemFile(%p, %p)", pArea, pCreateInfo));

   if( pCreateInfo )
   {
      pError = NULL;
      /* Try create */
      do
      {
         pArea->hMemoFile = hb_fsCreate( pCreateInfo->abName, FC_NORMAL );
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
         hb_errRelease( pError );

      if( pArea->hMemoFile == FS_ERROR )
         return FAILURE;
   }
   else /* For zap file */
      hb_fsSeek( pArea->hMemoFile, 0, FS_SET );

   pArea->uiMemoBlockSize = s_uiMemoBlockSize;
   fptHeader.ulNextBlock = hb_cdxSwapBytes( s_uiMemoBlockSize > 512 ? 1 :
                                            512 / s_uiMemoBlockSize  );
   fptHeader.ulBlockSize = hb_cdxSwapBytes( s_uiMemoBlockSize );
   if( hb_fsWrite( pArea->hMemoFile, ( BYTE * ) &fptHeader, sizeof( MEMOHEADER ) ) !=
       sizeof( MEMOHEADER ) )
      return FAILURE;
   memset( pBlock, 0, 512 );
   if( hb_fsWrite( pArea->hMemoFile, pBlock, 512 - sizeof( MEMOHEADER ) ) !=
       512 - sizeof( MEMOHEADER ) )
      return FAILURE;
   hb_fsWrite( pArea->hMemoFile, NULL, 0 );
   return SUCCESS;
}

/*
 * Open a memo file in the specified WorkArea.
 */
ERRCODE hb_cdxOpenMemFile( CDXAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   USHORT uiFlags;
   BOOL bRetry;
   MEMOHEADER fptHeader;
   PHB_ITEM pError;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxOpenMemFile(%p, %p)", pArea, pOpenInfo));

   uiFlags = pOpenInfo->fReadonly ? FO_READ : FO_READWRITE;
   uiFlags |= pOpenInfo->fShared ? FO_DENYNONE : FO_EXCLUSIVE;
   pError = NULL;

   /* Try open */
   do
   {
      pArea->hMemoFile = hb_fsOpen( pOpenInfo->abName, uiFlags );
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
      hb_errRelease( pError );
      pError = NULL;
   }
   if( pArea->hMemoFile == FS_ERROR )
      return FAILURE;

   hb_fsSeek( pArea->hMemoFile, 0, FS_SET );
   if( hb_fsRead( pArea->hMemoFile, ( BYTE * ) &fptHeader, sizeof( MEMOHEADER ) ) !=
       sizeof( MEMOHEADER ) )
      return FAILURE;
   pArea->uiMemoBlockSize = ( USHORT ) hb_cdxSwapBytes( fptHeader.ulBlockSize );
   return SUCCESS;
}

/*
 * Read the database file header record in the WorkArea.
 */
ERRCODE hb_cdxReadDBHeader( CDXAREAP pArea )
{
   DBFHEADER dbHeader;
   BOOL bRetry, bError;
   PHB_ITEM pError;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxReadHeader(%p)", pArea));

   pError = NULL;
   /* Try read */
   do
   {
      hb_fsSeek( pArea->hDataFile, 0, FS_SET );
      if( hb_fsRead( pArea->hDataFile, ( BYTE * ) &dbHeader, sizeof( DBFHEADER ) ) !=
          sizeof( DBFHEADER ) || ( dbHeader.bVersion != 0x03 && dbHeader.bVersion != 0xF5 ) )
      {
         bError = TRUE;
         if( !pError )
         {
            pError = hb_errNew();
            hb_errPutGenCode( pError, EG_CORRUPTION );
            hb_errPutSubCode( pError, EDBF_CORRUPT );
            hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_CORRUPTION ) );
            hb_errPutFileName( pError, pArea->szDataFileName );
            hb_errPutFlags( pError, EF_CANRETRY | EF_CANDEFAULT );
         }
         bRetry = ( SELF_ERROR( ( AREAP ) pArea, pError ) == E_RETRY );
      }
      else
         bRetry = bError = FALSE;
   } while( bRetry );

   if( pError )
      hb_errRelease( pError );

   /* Read error? */
   if( bError )
      return FAILURE;

   pArea->bDay = dbHeader.bDay;
   pArea->bMonth = dbHeader.bMonth;
   pArea->bYear = dbHeader.bYear;
   pArea->uiHeaderLen = dbHeader.uiHeaderLen;
   pArea->ulRecCount = dbHeader.ulRecCount;
   pArea->fHasMemo = ( dbHeader.bVersion == 0xF5 );
   pArea->fHasTags = dbHeader.bHasTags;
   return SUCCESS;
}

/*
 * Write the database file header record in the WorkArea.
 */
ERRCODE hb_cdxWriteDBHeader( CDXAREAP pArea )
{
   DBFHEADER dbfHeader;
   ULONG ulRecCount;
   long lYear, lMonth, lDay;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxWriteDBHeader(%p)", pArea));

   memset( &dbfHeader, 0, sizeof( DBFHEADER ) );
   dbfHeader.bVersion = pArea->fHasMemo ? 0xF5 : 0x03;
   hb_dateToday( &lYear, &lMonth, &lDay );
   dbfHeader.bYear = ( BYTE ) ( lYear - 1900 );
   dbfHeader.bMonth = ( BYTE ) lMonth;
   dbfHeader.bDay = ( BYTE ) lDay;
   dbfHeader.bHasTags = ( BYTE ) pArea->fHasTags;

   /* Update record count */
   SELF_RECCOUNT( ( AREAP ) pArea, &ulRecCount );

   dbfHeader.ulRecCount = ulRecCount;
   dbfHeader.uiHeaderLen = pArea->uiHeaderLen;
   dbfHeader.uiRecordLen = pArea->uiRecordLen;
   hb_fsSeek( pArea->hDataFile, 0, FS_SET );
   hb_fsWrite( pArea->hDataFile, ( BYTE * ) &dbfHeader, sizeof( DBFHEADER ) );
   pArea->fUpdateHeader = FALSE;
   return SUCCESS;
}





HB_FUNC( _DBFCDX )
{
}

HB_FUNC( DBFCDX_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;

   uiCount = ( USHORT * ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   * uiCount = RDDFUNCSCOUNT;
   pTable = ( RDDFUNCS * ) hb_itemGetPtr( hb_param( 2, HB_IT_POINTER ) );

   HB_TRACE(HB_TR_DEBUG, ("DBFCDX_GETFUNCTABLE(%i, %p)", uiCount, pTable));

   if( pTable )
      hb_retni( hb_rddInherit( pTable, &cdxTable, &cdxSuper, ( BYTE * ) "DBF" ) );
   else
      hb_retni( FAILURE );
}