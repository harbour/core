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
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbdbf.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbset.h"
#include "hbdate.h"
#include "error.ch"

#include "dbfcdx1.h"
#include "hbrddcdx.h"
#include "dbfcdx2.h"


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
                             ( DBENTRYP_V ) hb_cdxRecall,
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
                             ( DBENTRYP_VOS ) hb_cdxSetScope,
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
                             ( DBENTRYP_V ) hb_cdxWriteDBHeader /*, */
                             /*( DBENTRYP_SVP ) hb_cdxWhoCares  */
                           };

/*
 * Common functions.
 */

static LONG hb_cdxMakeMask( BYTE bByte )
{
   BYTE bCount;
   LONG lMask;

   for( lMask = 0, bCount = 1; bCount <= bByte; bCount++,
   lMask = ( lMask << 1 ) + 1 );
   return lMask;
}

static void hb_cdxDNtoSort( double dValue, BYTE * szBuffer )
{
   int i;
   BYTE *pTemp;

/*   HB_TRACE(HB_TR_DEBUG, ("hb_cdxDNtoSort(%ld, %p)", lValue, szBuffer)); */

   pTemp = (BYTE*) &dValue;
   if (dValue < 0)
   {
      for ( i = 7 ; i >= 0 ; i--, pTemp++)
         szBuffer[i] = (*pTemp) ^ 0xFF;
   }
   else
   {
      for ( i = 7 ; i >= 0 ; i--, pTemp++)
         szBuffer[i] = (*pTemp);
      szBuffer[0] ^= 0x80;
   }
   return;
}

static double hb_cdxSorttoND( BYTE * szBuffer, USHORT uiLen )
{
   double dNumber;
   int    i;
   BYTE   * pTemp;

   pTemp = (BYTE*) &dNumber;
   pTemp += 7;
   if ( szBuffer[0] & 0x80 )        /* >0 */
   {
      for ( i = 0 ; i < 8 ; i++, pTemp-- )
         /* if( i < 8 - uiLen ) */
         if( i >= uiLen )
            (*pTemp) = 0;
         else
            (*pTemp) = szBuffer[i];

       pTemp += 8;
       (*pTemp) ^= 0x80;
   }
   else
   {
       for ( i = 0 ; i < 8 ; i++, pTemp-- )
          /* if( i < 8 - uiLen ) */
           if( i >= uiLen )
             (*pTemp) = 0;
          else
             (*pTemp) = szBuffer[i] ^ 0xFF;

   }

   return dNumber;
}

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
   else
   {
      hb_itemPutC( pItem, "");
      hb_itemSetCMemo( pItem );
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
         if( (pFreeBlock->ulBlock + pFreeBlock->uiBlocks) == ulBlock )
         {
            /* The new blocks are directly after the current free block */
            pFreeBlock->uiBlocks += uiBlocks;
            ulBlock = pFreeBlock->ulBlock;
            uiBlocks = pFreeBlock->uiBlocks;
         }
         /* Can append to next free block? */
         else if( iCount < pArea->pMemoRoot->uiListLen - 1 &&
                  (ulBlock + uiBlocks) == ( pFreeBlock + 1 )->ulBlock )
         {
            /* The new blocks are directly before the next free block */
            pFreeBlock++;
            iCount++;
            pFreeBlock->ulBlock = ulBlock;
            pFreeBlock->uiBlocks += uiBlocks;
            uiBlocks = pFreeBlock->uiBlocks;
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

   /* If it is the last block in memo file truncate it */
   if( (ulBlock + uiBlocks) == pArea->pMemoRoot->ulNextBlock )
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
                           pItem, ulLen, ulStoredBlock, uiType));

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

   /* Close all index */
   SELF_ORDLSTCLEAR( ( AREAP ) pArea );

   /* Free Indexes array * / review this change later
   if( pAreaCdx->lpIndexes )
   {
      hb_xfree( pAreaCdx->lpIndexes );
      pAreaCdx->lpIndexes = NULL;
   }
   */
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
   char * szFileName;
   DBORDERINFO pExtInfo;
   PHB_FNAME pFileName;
   DBORDERINFO pOrderInfo;
   USHORT uiFlags;
   FHANDLE hFile;

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
      /* printf("TODO: hb_cdxOpen()\n"); */
      pFileName = hb_fsFNameSplit( pArea->szDataFileName );
      szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 3 );
      szFileName[ 0 ] = '\0';
      strcpy ( szFileName, pFileName->szPath );
      strncat( szFileName, pFileName->szName, _POSIX_PATH_MAX -
               strlen( szFileName ) );

      pExtInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) pArea, DBOI_BAGEXT, &pExtInfo );
      strncat( szFileName, pExtInfo.itmResult->item.asString.value, _POSIX_PATH_MAX -
               strlen( szFileName ) );
      hb_itemRelease( pExtInfo.itmResult );
      hb_xfree( pFileName );

      /* ---------- */

      uiFlags =  pArea->fReadonly  ? FO_READ : FO_READWRITE;
      uiFlags |= pArea->fShared ? FO_DENYNONE : FO_EXCLUSIVE;
      hFile = hb_fsOpen( ( BYTE * ) szFileName, uiFlags );

      if( hFile != FS_ERROR ) {
         hb_fsClose( hFile );

         pOrderInfo.itmResult = hb_itemPutNI( NULL, 0 );
         pOrderInfo.atomBagName = hb_itemPutC( NULL, szFileName );
         pOrderInfo.itmOrder  = NULL;
         SELF_ORDLSTADD( ( AREAP ) pArea, &pOrderInfo );

         pOrderInfo.itmOrder  = hb_itemPutNI( NULL, 1 );
         SELF_ORDLSTFOCUS( ( AREAP ) pArea, &pOrderInfo );
         hb_itemRelease( pOrderInfo.itmOrder );

         SELF_GOTOP( ( AREAP ) pArea );

         hb_itemRelease( pOrderInfo.atomBagName );
         hb_itemRelease( pOrderInfo.itmResult );
      }
      hb_xfree( szFileName );
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
 * Clear the current order list.
 */
/*
ERRCODE hb_cdxOrderListClear( CDXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_cdxOrderListClear(%p)", pArea));
   HB_SYMBOL_UNUSED( pArea );

   return SUCCESS;
}
*/

/*
 * Provides information about order management.
 */
ERRCODE hb_cdxOrderInfo( CDXAREAP pArea, USHORT uiIndex, LPDBORDERINFO pOrderInfo )
{
   USHORT uiTag;
   LPCDXTAG pTag;
   USHORT uiAux;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxOrderInfo(%p, %hu, %p)", pArea, uiIndex, pOrderInfo));
   HB_SYMBOL_UNUSED( pArea );

   switch( uiIndex )
   {
      case DBOI_BAGEXT:
         pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, CDX_INDEXEXT );
         break;

      case DBOI_BAGNAME:
         if ( ! pOrderInfo->itmOrder )
            pTag = hb_cdxGetActiveTag( pArea->lpIndexes );

         else
         {
               uiTag = hb_cdxFindTag( pArea, pOrderInfo );
               if ( uiTag )
               {
                  pTag = hb_cdxGetTagByNumber(pArea,  uiTag );
               }
         }
         if ( pTag )
            /* hb_itemPutC( pOrderInfo->itmResult, pTag->pIndex->pCompound->szName ); */
            hb_itemPutC( pOrderInfo->itmResult, pTag->pIndex->szFileName );
         break;


      case DBOI_CONDITION:
         if ( ! pOrderInfo->itmOrder )
            pTag = hb_cdxGetActiveTag( pArea->lpIndexes );

         else
         {
               uiTag = hb_cdxFindTag( pArea, pOrderInfo );
               if ( uiTag )
               {
                  pTag = hb_cdxGetTagByNumber(pArea,  uiTag );
               }
         }
         if ( pTag )
            pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, pTag->ForExpr );
         break;

      case DBOI_EXPRESSION:

         if ( ! pOrderInfo->itmOrder )
            pTag = hb_cdxGetActiveTag( pArea->lpIndexes );

         else
         {
               uiTag = hb_cdxFindTag( pArea, pOrderInfo );
               if ( uiTag )
               {
                  pTag = pArea->lpIndexes->TagList;
                  --uiTag;
                  while( uiTag && pTag )
                  {
                     pTag = pTag->pNext;
                     --uiTag;
                   }
               }
         }
         if ( pTag )
            pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, pTag->KeyExpr );

         break;

      case DBOI_UNIQUE:

         if ( ! pOrderInfo->itmOrder )
            pTag = hb_cdxGetActiveTag( pArea->lpIndexes );

         else
         {
               uiTag = hb_cdxFindTag( pArea, pOrderInfo );
               if ( uiTag )
               {
                  pTag = pArea->lpIndexes->TagList;
                  --uiTag;
                  while( uiTag && pTag )
                  {
                     pTag = pTag->pNext;
                     --uiTag;
                   }
               }
         }
         if ( pTag )
            pOrderInfo->itmResult = hb_itemPutL( pOrderInfo->itmResult, pTag->UniqueKey );

         break;

      case DBOI_NAME:

         if ( ! pOrderInfo->itmOrder )
            pTag = hb_cdxGetActiveTag( pArea->lpIndexes );

         else
         {
               uiTag = hb_itemGetNI( pOrderInfo->itmOrder );

               if ( uiTag )
               {
                  pTag = pArea->lpIndexes->TagList;
                  --uiTag;
                  while( uiTag && pTag )
                  {
                     pTag = pTag->pNext;
                     --uiTag;
                   }
               }
         }
         if ( pTag )
            pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, pTag->szName );
         break;

      case DBOI_NUMBER:
         if (pArea->lpIndexes)
            hb_itemPutNI( pOrderInfo->itmResult, pArea->lpIndexes->uiTag );
         else
            hb_itemPutNI( pOrderInfo->itmResult, 0 );
         break;

      case DBOI_ORDERCOUNT:
         uiTag = 0;
         if (pArea->lpIndexes)
         {
            pTag = pArea->lpIndexes->TagList;
            while( pTag )
            {
               uiTag++;
               pTag = pTag->pNext;
            }
         }
         hb_itemPutNI( pOrderInfo->itmResult, uiTag );
         break;

      case DBOI_KEYVAL:

         pTag = hb_cdxGetActiveTag( pArea->lpIndexes );
         if ( pTag && !pArea->fEof && pTag->CurKeyInfo && ((ULONG) pTag->CurKeyInfo->Tag == pArea->ulRecNo) )
         {
            switch ( pTag->uiType )
            {
               case 'N' :
                  uiAux = HB_IT_DOUBLE; break;
               case 'D' :
                  uiAux = HB_IT_DATE; break;
               case 'L' :
                  uiAux = HB_IT_LOGICAL; break;
               case 'C' :
                  uiAux = HB_IT_STRING; break;
               default:
                  uiAux = HB_IT_NIL;
            }
            if ( uiAux != HB_IT_NIL )
              hb_cdxKeyGetItem( pTag->CurKeyInfo, pOrderInfo->itmResult, uiAux );

         }
         break;

      /*
      default:
         return SUPER_ORDINFO( ( AREAP ) pArea, uiIndex, pOrderInfo );
      */

   }
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
      hb_errRelease( pError );
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

/* hb_cdxkeyxxx */
/* #include "cdxkey.c" */
static LPKEYINFO hb_cdxKeyNew()
{
   LPKEYINFO pKey;

   pKey = ( LPKEYINFO ) hb_xgrab( sizeof( KEYINFO ) );
   pKey->Value = NULL;
   pKey->length = 0;
   pKey->fString = FALSE;
   pKey->Tag = pKey->Xtra = 0;
   pKey->pNext = NULL;
   return pKey;
}

static void hb_cdxKeyFree( LPKEYINFO pKey )
{
   if( pKey )
   {
      if( pKey->Value )
          hb_xfree( pKey->Value );

      hb_xfree( pKey );
   }
}

static int hb_cdxKeyCompare( LPKEYINFO pKey1, LPKEYINFO pKey2, USHORT * EndPos, BOOL Exact )
{
   int iLimit, iResult;

   * EndPos = 0;
   if( pKey2 == NULL || pKey2->length == 0 )
      return 1;
   if( pKey1 == NULL || pKey1->length == 0 )
      return ( pKey2->length == 0 ) ? 0: -1;

   iLimit = (pKey1->length > pKey2->length) ? pKey2->length : pKey1->length;

   if( pKey1->fString && pKey2->fString)
   {
      do
      {
          /*
           for nation sorting support
          iResult = hb_charcmp( pKey1->value[ * EndPos ],
                                pKey2->value[ * EndPos ] )
          */
          iResult = ( (unsigned char) pKey1->Value[ * EndPos ]) -
                    ( (unsigned char) pKey2->Value[ * EndPos ]);
          * EndPos += 1;
      } while( iResult == 0 && * EndPos < iLimit );
   }
   else
   {
      do
      {
          iResult = ( (unsigned char) pKey1->Value[ * EndPos ]) -
                    ( (unsigned char) pKey2->Value[ * EndPos ]);
          * EndPos += 1;
      } while( iResult == 0 && * EndPos < iLimit );
   }

   if( iResult == 0 )
   {
      * EndPos += 1;
      iResult = pKey1->length - pKey2->length;
   }
   if( (iResult < 0) && ((ULONG)* EndPos > pKey1->length) && !Exact )
      iResult = 0;

   if( iResult < 0 )
      return -1;
   else if( iResult > 0 )
      return 1;
   else
      return 0;
}

static LPKEYINFO hb_cdxKeyPutItem( LPKEYINFO pKey, PHB_ITEM pItem )
{
   BYTE cTemp[8];
   int i;

   if( !pKey )
      pKey = hb_cdxKeyNew();
   if( pKey->Value )
   {
      hb_xfree( pKey->Value );
      pKey->Value = NULL;
   }

   switch( hb_itemType( pItem ) )
   {
        case HB_IT_STRING:

           pKey->Value = hb_itemGetC( pItem );
           pKey->length = pItem->item.asString.length;
           pKey->fString = TRUE;

           while( pKey->length > 0 &&
                  pKey->Value[ pKey->length - 1 ] == ' ' )
                  /*pKey->Value[ pItem->item.asString.length - 1 ] == ' ' )*/
              pKey->length--;
           pKey->Value[ pKey->length ] = 0;

        break;

        case HB_IT_INTEGER:
        case HB_IT_LONG:
        case HB_IT_DOUBLE:
           hb_cdxDNtoSort( hb_itemGetND( pItem ), &cTemp[0] );

           i = 7;
           while( !cTemp[i] )
              i--;

           i++;
           pKey->length = i;
           pKey->Value = (char *) hb_xgrab( i + 1 );
           pKey->Value[i] = 0;
           memcpy( pKey->Value, cTemp, i );
           pKey->fString = FALSE;

           break;

        case HB_IT_DATE:
           hb_cdxDNtoSort( (double) hb_itemGetDL( pItem ), &cTemp[0] );

           i = 7;
           while( !cTemp[i] )
              i--;

           i++;
           pKey->length = i;
           pKey->Value = (char *) hb_xgrab( i + 1 );
           pKey->Value[i] = 0;
           memcpy( pKey->Value, cTemp, i );
           pKey->fString = FALSE;

           break;

        case HB_IT_LOGICAL:
           pKey->length = 1;
           pKey->Value = (char *) hb_xgrab( 2 );
           pKey->fString = TRUE;

           pKey->Value[0] = ( hb_itemGetL( pItem ) ? 'T' : 'F' );
           pKey->Value[1] = 0;

           break;

        default:
           printf( "hb_cdxKeyPutItem( )" );
   }

   return pKey;
}

static PHB_ITEM hb_cdxKeyGetItem( LPKEYINFO pKey, PHB_ITEM pItem, USHORT uiType )
{

   if( pKey && pKey->Value )
   {
      switch( uiType )
      {
         case HB_IT_STRING:
            pItem = hb_itemPutC( pItem, pKey->Value );
            break;

         case HB_IT_INTEGER:
         case HB_IT_LONG:
         case HB_IT_DOUBLE:
            pItem = (PHB_ITEM) hb_itemPutND( pItem,
                        (long) hb_cdxSorttoND( (BYTE*) pKey->Value, (USHORT) pKey->length ) );
            break;

         case HB_IT_DATE:
            pItem = (PHB_ITEM) hb_itemPutDL( pItem,
                  (long) hb_cdxSorttoND( (BYTE*) pKey->Value, (USHORT) pKey->length ) );
            break;

         case HB_IT_LOGICAL:
            pItem = hb_itemPutL( pItem,
                        (pKey->Value[0] == 'T' ? TRUE : FALSE ) );
            break;
        default:
            pItem = hb_itemNew( pItem );
            printf( "hb_cdxKeyGetItem() ???" );
      }
   }
   else
      pItem = hb_itemNew( pItem );

   return pItem;
}

static LPKEYINFO hb_cdxKeyCopy( LPKEYINFO pKeyDest, LPKEYINFO pKey )
{
   if ( !pKeyDest )
   {
      pKeyDest = hb_cdxKeyNew();
   }
   if ( pKeyDest->Value )
   {
      hb_xfree( pKeyDest->Value );
      pKeyDest->Value = NULL;
      pKeyDest->length = 0;
   }

   if ( pKey )
   {
       pKeyDest->Value = (char *) hb_xgrab( pKey->length + 1 );
       memcpy( pKeyDest->Value, pKey->Value, pKey->length );
       pKeyDest->length = pKey->length;
       pKeyDest->Value[ pKeyDest->length ] = 0;
       pKeyDest->fString = pKey->fString;
       pKeyDest->Tag = pKey->Tag;
       pKeyDest->Xtra = pKey->Xtra;
   }
   return pKeyDest;
}

static LPKEYINFO hb_cdxKeyPut( LPKEYINFO pKey, BYTE * szText, USHORT uiLen )
{
   if( !pKey )
      pKey = hb_cdxKeyNew();

   if( pKey->Value )
   {
      hb_xfree( pKey->Value );
      pKey->Value = NULL;
   }

   if( szText == NULL )
   {
      pKey->length = 0;
      pKey->fString = FALSE;
   }
   else
   {
      pKey->length = uiLen;
      pKey->Value = ( char * ) hb_xgrab( uiLen + 1 );
      memcpy( pKey->Value, szText, uiLen );
      pKey->fString = FALSE;
   }

   return pKey;
}

static LPKEYINFO hb_cdxKeyPutC( LPKEYINFO pKey, char * szText )
{
   if( !pKey )
      pKey = hb_cdxKeyNew();

   if( szText == NULL )
      szText = "";

   if( pKey->Value )
   {
      hb_xfree( pKey->Value );
      pKey->Value = NULL;
   }

   pKey->length = strlen( szText );
   pKey->Value = ( char * ) hb_xgrab( pKey->length + 1 );
   strcpy( pKey->Value, szText );
   pKey->fString = TRUE;

   return pKey;
}

/* end hb_cdxkeyxxx */

/* hb_cdxTagxxx */
/* #include "cdxtag.c" */
/* Creates a new structure with a tag information
 *
 * PIF = pointer to a parent index structure
 * ITN = tag name
 * TagHdr = number of index page where a tag header is stored or -1 if
 *          allocate a new tag page
*/
static LPCDXTAG hb_cdxTagNew( LPCDXINDEX PIF, char * ITN, LONG TagHdr )
{
   LPCDXTAG pTag;

   pTag = ( LPCDXTAG ) hb_xgrab( sizeof( CDXTAG ) );
   memset( pTag, 0, sizeof( CDXTAG ) );
   pTag->szName = ( char * ) hb_xgrab( CDX_MAXTAGNAMELEN + 1 );
   hb_strncpyUpper( pTag->szName, ITN, CDX_MAXTAGNAMELEN );
   pTag->pIndex = PIF;
   pTag->AscendKey = TRUE;
   pTag->uiType = 'C';
   pTag->CurKeyInfo = hb_cdxKeyNew();
   if( TagHdr == -1 )
   {
      /* this tag is not stored in the file yet - allocate a space for it  */
      pTag->TagBlock = hb_cdxIndexGetAvailPage( PIF );
      hb_cdxIndexGetAvailPage( PIF );
      pTag->TagChanged = TRUE;
   }
   else
   {
      pTag->TagBlock = TagHdr;
      hb_cdxTagTagLoad( pTag );
   }
   pTag->OptFlags = 0x60;
   return pTag;
}

static void hb_cdxTagFree( LPCDXTAG pTag )
{
   if( pTag->RootPage != NULL )
   {
      hb_cdxPageFree( pTag->RootPage );
      pTag->RootPage = NULL;
   }
   if( pTag->TagChanged )
      hb_cdxTagTagStore( pTag );
   if( pTag->szName != NULL )
      hb_xfree( pTag->szName );
   if( pTag->KeyExpr != NULL )
      hb_xfree( pTag->KeyExpr );
   if( pTag->pKeyItem != NULL )
   {
      if( hb_itemType( pTag->pKeyItem ) != HB_IT_BLOCK )
         hb_macroDelete( ( HB_MACRO_PTR ) hb_itemGetPtr( pTag->pKeyItem ) );
      hb_itemRelease( pTag->pKeyItem );
   }
   if( pTag->ForExpr != NULL )
      hb_xfree( pTag->ForExpr );
   if( pTag->pForItem != NULL )
   {
      if( hb_itemType( pTag->pForItem ) != HB_IT_BLOCK )
         hb_macroDelete( ( HB_MACRO_PTR ) hb_itemGetPtr( pTag->pForItem ) );
      hb_itemRelease( pTag->pForItem );
   }
   hb_cdxKeyFree( pTag->CurKeyInfo );
   hb_xfree( pTag );
}

static void hb_cdxTagIndexTagNew( LPCDXTAG pTag, char * KeyExp, PHB_ITEM pKeyItem,
                                  BYTE bType, USHORT uiLen, char * ForExp,
                                  PHB_ITEM pForItem, BOOL Ascnd, BOOL Uniq )
{
   CDXTAGHEADER pHeader;

   if( KeyExp != NULL )
   {
      pTag->KeyExpr = ( char * ) hb_xgrab( CDX_MAXKEY + 1 );
      hb_strncpyUpper( pTag->KeyExpr, KeyExp, CDX_MAXKEY );
   }
   pTag->pKeyItem = pKeyItem;
   if( ForExp != NULL )
   {
      pTag->ForExpr = ( char * ) hb_xgrab( CDX_MAXKEY + 1 );
      hb_strncpyUpper( pTag->ForExpr, ForExp, CDX_MAXKEY );
   }
   pTag->pForItem = pForItem;
   pTag->AscendKey = Ascnd;
   pTag->UniqueKey = Uniq;
   pTag->uiType = bType;
   pTag->uiLen = uiLen;
   pTag->MaxKeys = ( CDX_PAGELEN - 12 ) / ( uiLen + 8 );
   memset( &pHeader, 0, sizeof( CDXTAGHEADER ) );
   hb_cdxIndexPageWrite( pTag->pIndex, pTag->TagBlock, &pHeader, sizeof( CDXTAGHEADER ) );
   hb_cdxTagDoIndex( pTag );
}

static void hb_cdxTagDoIndex( LPCDXTAG pTag )
{
   ULONG ulRecNo;
   BOOL bWhileOk;
   LPSORTINFO pSort;
   PHB_ITEM pItem;
   HB_MACRO_PTR pMacro;
   BYTE cTemp[8];
   if( pTag->OptFlags & 0x80 )
      hb_cdxTagEmptyIndex( pTag );
   else
   {
      pSort = hb_cdxSortNew( pTag, pTag->UniqueKey );
      pItem = hb_itemNew( NULL );
      for( ulRecNo = 1; ulRecNo <= pTag->pIndex->pArea->ulRecCount; ulRecNo++ )
      {
         hb_fsSeek( pTag->pIndex->pArea->hDataFile,
                    pTag->pIndex->pArea->uiHeaderLen +
                    ( ulRecNo - 1 ) * pTag->pIndex->pArea->uiRecordLen,
                    FS_SET );
         hb_fsRead( pTag->pIndex->pArea->hDataFile,
                    pTag->pIndex->pArea->pRecord,
                    pTag->pIndex->pArea->uiRecordLen );
         if( pTag->pForItem != NULL ) {
            /* TODO: test for expression */
            /* now is working but not tested */
            /* bWhileOk = TRUE; */
            if( hb_itemType( pTag->pForItem ) == HB_IT_BLOCK )
            {
               hb_vmPushSymbol( &hb_symEval );
               hb_vmPush( pTag->pForItem );
               hb_vmDo( 0 );
               hb_itemCopy( pItem, &hb_stack.Return );
            }
            else
            {
               pMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pTag->pForItem );
               hb_macroRun( pMacro );
               hb_itemCopy( pItem, hb_stackItemFromTop( - 1 ) );
               hb_stackPop();
            }
            bWhileOk = hb_itemGetL( pItem );
         } else
            bWhileOk = TRUE;
         if( bWhileOk )
         {
            if( hb_itemType( pTag->pKeyItem ) == HB_IT_BLOCK )
            {

               hb_vmPushSymbol( &hb_symEval );
               hb_vmPush( pTag->pKeyItem );
               hb_vmDo( 0 );
               hb_itemCopy( pItem, &hb_stack.Return );
            }
            else
            {
               pMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pTag->pKeyItem );
               hb_macroRun( pMacro );
               hb_itemCopy( pItem, hb_stackItemFromTop( - 1 ) );
               hb_stackPop();
            }

            switch( hb_itemType( pItem ) )
            {
               case HB_IT_STRING:
                  hb_cdxSortInsertWord( pSort, (long) ulRecNo, (char*) pItem->item.asString.value,
                                                        (USHORT) pItem->item.asString.length );
                  break;

               case HB_IT_INTEGER:
               case HB_IT_LONG:
               case HB_IT_DOUBLE:
                  hb_cdxDNtoSort( hb_itemGetND( pItem ), &cTemp[0] );
                  hb_cdxSortInsertWord( pSort, ulRecNo, (char *) cTemp, 8 );
                  break;

               case HB_IT_LOGICAL:
                  cTemp[0] = (hb_itemGetL( pItem ) ? 'T' : 'F');
                  hb_cdxSortInsertWord( pSort, ulRecNo, (char *) cTemp, 1 );
                  break;

               case HB_IT_DATE:
                  hb_cdxDNtoSort( (double) hb_itemGetDL( pItem ), &cTemp[0] );
                  hb_cdxSortInsertWord( pSort, ulRecNo, (char *) cTemp, 8 );
                  break;

               default:
                  printf( "hb_cdxTagDoIndex( LPCDXTAG pTag )" );
            }
         }
      }
      if( pSort->WordCount > 0 )
         hb_cdxSortDisplayWord( pSort );
      else
         hb_cdxTagEmptyIndex( pTag );
      hb_cdxSortFree( pSort );
      hb_itemRelease( pItem );
   }
   pTag->TagChanged = TRUE;
   hb_cdxTagTagStore( pTag );
}

static void hb_cdxTagEmptyIndex( LPCDXTAG pTag )
{
   USHORT uiKeyLength, uiBitCount;
   CDXDATA pData;

   pTag->RootBlock = hb_cdxIndexGetAvailPage( pTag->pIndex );
   memset( &pData, 0, sizeof( CDXDATA ) );
   pData.Node_Atr = 3;
   pData.Left_Ptr = pData.Rght_Ptr = -1;
   uiKeyLength = pTag->uiLen;
   for( uiBitCount = 0; uiKeyLength; uiBitCount++, uiKeyLength >>= 1 );
   pData.cdxu.External.ShortBytes = 3;
   pData.cdxu.External.RecNumBits = 24 - uiBitCount * 2;
   pData.cdxu.External.RecNumMask = hb_cdxMakeMask( pData.cdxu.External.RecNumBits );
   pData.cdxu.External.FreeSpace = CDX_EXTERNAL_SPACE;
   pData.cdxu.External.DupCntBits = pData.cdxu.External.TrlCntBits = (BYTE) uiBitCount;
   pData.cdxu.External.DupCntMask = (BYTE) hb_cdxMakeMask( pData.cdxu.External.DupCntBits );
   pData.cdxu.External.TrlCntMask = (BYTE) hb_cdxMakeMask( pData.cdxu.External.TrlCntBits );
   hb_cdxIndexPageWrite( pTag->pIndex, pTag->RootBlock, &pData, sizeof( CDXDATA ) );
}

static void hb_cdxTagTagStore( LPCDXTAG pTag )
{
   USHORT uiForLen;
   CDXTAGHEADER pHeader;

   if( !pTag->TagChanged )
      return;
   pTag->TagChanged = FALSE;
   if( pTag->UniqueKey )
      pTag->OptFlags |= 0x01;
   if( pTag->pForItem != NULL )
      pTag->OptFlags |= 0x08;
   memset( &pHeader, 0, sizeof( CDXTAGHEADER ) );
   pHeader.lRoot = pTag->RootBlock;
   pHeader.uiKeySize = pTag->uiLen;
   pHeader.bType = pTag->OptFlags;
   pHeader.bSignature = 1;
   if( !pTag->AscendKey )
      pHeader.iDescending = 1;
   pHeader.iFilterPos = 1 + ( pTag->KeyExpr == NULL ? 0 : strlen( pTag->KeyExpr ) );
   pHeader.iExprLen = pHeader.iFilterPos;
   if( pTag->KeyExpr != NULL )
      strcpy( ( char * ) pHeader.KeyPool, pTag->KeyExpr );
   uiForLen = pTag->ForExpr == NULL ? 0 : strlen( pTag->ForExpr );
   if( uiForLen > 0 )
   {
      pHeader.iFilterLen = uiForLen + 1;
      strcpy( ( char * ) pHeader.KeyPool + pHeader.iExprLen, pTag->ForExpr );
   }
   else
      pHeader.iFilterLen = 1;
   hb_cdxIndexPageWrite( pTag->pIndex, pTag->TagBlock, &pHeader, sizeof( CDXTAGHEADER ) );
}

static void hb_cdxTagTagOpen( LPCDXTAG pTag, BYTE bCode )
{
   hb_cdxTagTagClose( pTag );
   pTag->RootBlock = hb_cdxTagNewRoot( pTag );
   pTag->RootPage = hb_cdxPageNew( pTag, NULL, pTag->RootBlock );
   if( bCode == 2 )
      return;
   if( bCode == 0 )
      hb_cdxTagKeyRead( pTag, TOP_RECORD );
   else
      printf( "hb_cdxTagTagOpen()" );
}

static void hb_cdxTagTagClose( LPCDXTAG pTag )
{
   if( pTag->RootPage != NULL )
   {
      hb_cdxPageFree( pTag->RootPage );
      pTag->RootPage = NULL;
   }
   if( pTag->TagChanged )
      hb_cdxTagTagStore( pTag );
}

static LONG hb_cdxTagNewRoot( LPCDXTAG pTag )
{
   CDXTAGHEADER pHeader;

   hb_cdxIndexPageRead( pTag->pIndex, pTag->TagBlock, &pHeader, sizeof( CDXTAGHEADER ) );
   return pHeader.lRoot;
}

static void hb_cdxTagPageLoad( LPCDXTAG pTag, LPPAGEINFO pPage )
{
   CDXDATA pData;

   hb_cdxIndexPageRead( pTag->pIndex, pPage->Page, &pData, sizeof( CDXDATA ) );
   if( pData.Node_Atr > 1 )
      pPage->PageType = PAGE_LEAF;
   else
      pPage->PageType = PAGE_NODE;
   pPage->Left = pData.Left_Ptr;
   pPage->Right = pData.Rght_Ptr;
   if( pData.Entry_Ct > 0 )
   {
      if( pPage->PageType == PAGE_LEAF )
         hb_cdxTagExtNodeBuild( pTag, &pData, pPage );
      else
         hb_cdxTagIntNodeBuild( pTag, &pData, pPage );
   }
   pPage->Changed = FALSE;
}

static void hb_cdxTagKeyRead( LPCDXTAG pTag, BYTE bTypRead )
{
   pTag->CurKeyInfo->Tag = 0;
   if( pTag->RootPage == NULL )
      return;
   if( !pTag->AscendKey )
   {
      switch( bTypRead )
      {
         case TOP_RECORD:
            bTypRead = BTTM_RECORD;
            break;

         case BTTM_RECORD:
            bTypRead = TOP_RECORD;
            break;

         case NEXT_RECORD:
            bTypRead = PREV_RECORD;
            break;

         case PREV_RECORD:
            bTypRead = NEXT_RECORD;
            break;
      }
   }
   pTag->TagBOF = pTag->TagEOF = FALSE;
   switch( bTypRead )
   {
      case TOP_RECORD:
         hb_cdxTagTagOpen( pTag, 2 );
         /* -----------------10/10/2001 19:04-----------------
           Disable this test for now.
          --------------------------------------------------
         if( pTag->pForItem != NULL )
            printf( "hb_cdxTagKeyRead()" );
         else
            pTag->TagBOF = !hb_cdxPageReadTopKey( pTag->RootPage );
          */
         pTag->TagBOF = !hb_cdxPageReadTopKey( pTag->RootPage );
         /* -----------------10/10/2001 19:04-----------------
           Disable this test for now.
          --------------------------------------------------
         if( pTag->pForItem != NULL )
            printf( "hb_cdxTagTestRange()" );
          */
         if( pTag->TagEOF )
            pTag->TagBOF = TRUE;
         pTag->TagEOF = pTag->TagBOF;
         break;

      case BTTM_RECORD:
         hb_cdxTagTagOpen( pTag, 2 );
         /* -----------------10/10/2001 19:04-----------------
           Disable this test for now.
          --------------------------------------------------
         if( pTag->pForItem != NULL )
            printf( "hb_cdxTagKeyRead()" );
         else
            pTag->TagEOF = !hb_cdxPageReadBottomKey( pTag->RootPage );
         */
         pTag->TagEOF = !hb_cdxPageReadBottomKey( pTag->RootPage );
         /* -----------------10/10/2001 19:04-----------------
           Disable this test for now.
          --------------------------------------------------
         if( pTag->pForItem != NULL )
         {
            printf( "hb_cdxTagTestRange()" );
         }
          */
         if( pTag->TagBOF )
            pTag->TagEOF = TRUE;
         pTag->TagBOF = pTag->TagEOF;
         break;

      case NEXT_RECORD:
         /* -----------------10/10/2001 19:04-----------------
           Disable this test for now.
          --------------------------------------------------
         while( TRUE )
         {
            pTag->TagEOF = !hb_cdxPageReadNextKey( pTag->RootPage );
            if( pTag->pForItem != NULL )
               printf( "hb_cdxTagKeyRead()" );
            else
               break;
         }
         */
         pTag->TagEOF = !hb_cdxPageReadNextKey( pTag->RootPage );
         break;

      case PREV_RECORD:
         /* -----------------10/10/2001 19:04-----------------
           Disable this test for now.
          --------------------------------------------------
         while( TRUE )
         {
            pTag->TagBOF = !hb_cdxPageReadPrevKey( pTag->RootPage );
            if( pTag->pForItem != NULL )
               printf( "hb_cdxTagKeyRead()" );
            else
               break;
         }
         break;
         */
         pTag->TagBOF = !hb_cdxPageReadPrevKey( pTag->RootPage );

   }
   if( pTag->TagBOF || pTag->TagEOF )
   {
      if( !pTag->AscendKey )
      {
         pTag->CurKeyInfo->Tag = pTag->TagEOF;
         pTag->TagEOF = pTag->TagBOF;
         pTag->TagBOF = ( BOOL ) pTag->CurKeyInfo->Tag;
      }
      pTag->CurKeyInfo->Tag = 0;
   }
}

static void hb_cdxTagKeyAdd( LPCDXTAG pTag, LPKEYINFO pKey )
{
   int iSeek;
   LONG lOldXtra;
   LPKEYINFO pNewKey;

   lOldXtra = pKey->Xtra;
   pKey->Xtra = pKey->Tag;
   hb_cdxTagTagOpen( pTag, 0 );
   pNewKey = hb_cdxKeyNew();
   pNewKey = hb_cdxKeyCopy( pNewKey, pKey );

   pTag->TagBOF = pTag->TagEOF = FALSE;
   iSeek = hb_cdxPageSeekKey( pTag->RootPage,
          pTag->UniqueKey ? CDX_IGNORE_REC_NUM : pNewKey->Tag, pNewKey, TRUE );
   if( pTag->UniqueKey && iSeek == 0 )
   {
   }
   else if( iSeek < 1 )
   {
      hb_cdxPageInsertKey( pTag->RootPage, pNewKey, FALSE );
      hb_cdxTagTagOpen( pTag, 0 );
      hb_cdxPageSeekKey( pTag->RootPage, pNewKey->Tag, pNewKey, FALSE );
      iSeek = hb_cdxPageRetrieveKey( pTag->RootPage, &pTag->CurKeyInfo );
      if( !pTag->AscendKey )
      {
         pTag->TagBOF = ( iSeek & 2 );
         pTag->TagEOF = ( iSeek & 1 );
      }
      else
      {
         pTag->TagBOF = ( iSeek & 1 );
         pTag->TagEOF = ( iSeek & 2 );
      }
   }
   else
   {
      if( !pTag->AscendKey )
         printf( "hb_cdxTagKeyAdd()\n");
      else
         hb_cdxTagKeyRead( pTag, BTTM_RECORD );
      hb_cdxPageInsertKey( pTag->RootPage, pNewKey, TRUE );
      hb_cdxTagTagOpen( pTag, 0 );
      if( !pTag->AscendKey )
      {
         printf( "hb_cdxTagKeyAdd()\n");
         pTag->TagBOF = TRUE;
      }
      else
      {
         hb_cdxTagKeyRead( pTag, BTTM_RECORD );
         pTag->TagEOF = TRUE;
      }
   }
   hb_cdxKeyFree( pNewKey );
   pKey->Xtra = lOldXtra;
}

static void hb_cdxTagPageStore( LPCDXTAG pTag, LPPAGEINFO PIK )
{
   CDXDATA pData;

   memset( &pData, 0, sizeof( CDXDATA ) );
   pData.Node_Atr = ( PIK->PageType < PAGE_LEAF ) ? 0 : 2;
   if( PIK->Owner == NULL )
      pData.Node_Atr++;
   pData.Left_Ptr = PIK->Left;
   pData.Rght_Ptr = PIK->Right;
   if( pData.Node_Atr < 2 )
      hb_cdxTagIntNodeWrite( pTag, PIK->Page, &pData, PIK );
   else
      hb_cdxTagExtNodeWrite( pTag, PIK->Page, &pData, PIK );
}

static void hb_cdxTagExtNodeWrite( LPCDXTAG pTag, LONG PN, LPCDXDATA pData,
                                   LPPAGEINFO PIK )
{
   USHORT uiKeyLength, uiBitCount, cd, kcnt, lm, uiCount, ck, na;
   LONG sr, rp, lp, NPN, TmpTag;
   LPKEYINFO p, q;

   if( pTag->OptFlags & 0x80 )
      sr = hb_fsSeek( pTag->pIndex->hFile, 0, FS_END );
   else
      sr = pTag->pIndex->pArea->ulRecCount;
   uiKeyLength = pTag->uiLen;
   for( uiBitCount = 0; uiKeyLength; uiBitCount++, uiKeyLength >>= 1 );
   PIK->ReqByte = 3;
   PIK->RNBits = 24 - uiBitCount * 2;
   PIK->RNMask = hb_cdxMakeMask( PIK->RNBits );
   while( sr > PIK->RNMask )
   {
      PIK->ReqByte++;
      PIK->RNBits += 8;
      PIK->RNMask = ( PIK->RNMask << 8 ) | 0xFF;
      if( PIK->RNMask < 0 )
      {
         PIK->RNMask = 0x7FFFFFFF;
         PIK->RNBits = 31;
      }
   }
   PIK->Space = CDX_EXTERNAL_SPACE;
   PIK->DCBits = PIK->TCBits = (BYTE) uiBitCount;
   PIK->DCMask = (BYTE) hb_cdxMakeMask( PIK->DCBits );
   PIK->TCMask = (BYTE) hb_cdxMakeMask( PIK->TCBits );
   sr = cd = kcnt = 0;
   lm = sizeof( pData->cdxu.Internal.IntData ) / 2;
   q = NULL;
   for( uiCount = 0; uiCount < PIK->uiKeys; uiCount++ )
   {
      p = hb_cdxPageGetKey( PIK, uiCount );
      if( q != NULL )
      {
         hb_cdxKeyCompare( p, q, &cd, TRUE );
         if( cd > 0 )
            cd--;
      }
      q = p;
      /* Comprobar que las Keys son de tipo car cter. */
      /* cd = p->pItem->item.asString.length - cd; */
      cd = (USHORT) ( p->length - cd );
      sr += cd + PIK->ReqByte;
      if( sr < lm )
         kcnt++;
   }
   if( sr < CDX_EXTERNAL_SPACE )
      kcnt = PIK->uiKeys;
   ck = 0;
   pData->Entry_Ct = 0;
   if( kcnt < PIK->uiKeys )
   {
      ck = hb_cdxTagFillExternalNode( pTag, pData, PIK, kcnt, ck, &p );
      if( pData->Node_Atr % 2 > 0 )
         pData->Node_Atr--;
      na = pData->Node_Atr;
      rp = pData->Rght_Ptr;
      lp = pData->Left_Ptr;
      pData->Rght_Ptr = PN;
      pData->cdxu.External.FreeSpace = PIK->Space;
      NPN = hb_cdxIndexGetAvailPage( pTag->pIndex );
      TmpTag = p->Tag;
      p->Tag = NPN;
      hb_cdxPageAddNodeKey( PIK, p );
      p->Tag = TmpTag;
      if( PIK->PageType == PAGE_ROOT )
         PIK->PageType = PAGE_NODE;
      hb_cdxIndexPageWrite( pTag->pIndex, NPN, pData, CDX_PAGELEN );
      if( lp > 0 )
      {
         hb_cdxIndexPageRead( pTag->pIndex, lp, pData, CDX_PAGELEN );
         pData->Rght_Ptr = NPN;
         hb_cdxIndexPageWrite( pTag->pIndex, lp, pData, CDX_PAGELEN );
      }
      memset( pData, 0, CDX_PAGELEN );
      pData->Node_Atr = na;
      pData->Rght_Ptr = rp;
      pData->Left_Ptr = NPN;
      pData->Entry_Ct = 0;
      kcnt = PIK->uiKeys;
   }
   hb_cdxTagFillExternalNode( pTag, pData, PIK, kcnt, ck, &p );
   pData->cdxu.External.FreeSpace = PIK->Space;
   hb_cdxIndexPageWrite( pTag->pIndex, PN, pData, sizeof( CDXDATA ) );
}

static USHORT hb_cdxTagFillExternalNode( LPCDXTAG pTag, LPCDXDATA pData,
                                         LPPAGEINFO PIK, USHORT kcnt,
                                         USHORT ck, LPKEYINFO * p )
{
   USHORT i, k, ct, cd, v, c;
   LONG m, r;
   LPKEYINFO q;

   memset( pData->cdxu.External.ExtData, 0, sizeof( pData->cdxu.External.ExtData ) );
   pData->cdxu.External.FreeSpace = PIK->Space;
   pData->cdxu.External.RecNumMask = PIK->RNMask;
   pData->cdxu.External.DupCntMask = PIK->DCMask;
   pData->cdxu.External.TrlCntMask = PIK->TCMask;
   pData->cdxu.External.RecNumBits = PIK->RNBits;
   pData->cdxu.External.DupCntBits = PIK->DCBits;
   pData->cdxu.External.TrlCntBits = PIK->TCBits;
   pData->cdxu.External.ShortBytes = PIK->ReqByte;
   m = ~PIK->RNMask;
   PIK->Space = CDX_EXTERNAL_SPACE;
   k = CDX_EXTERNAL_SPACE;
   q = NULL;
   i = 0;
   while( i < kcnt && ck < PIK->uiKeys )
   {
      * p = hb_cdxPageGetKey( PIK, ck );
      ct =  (USHORT) ( pTag->uiLen - ( * p )->length ) ;
      if( q != NULL )
      {
         hb_cdxKeyCompare( * p,  q, &cd, TRUE );
         if( cd > 0 )
            cd--;
      }
      else
         cd = 0;
      q = * p;
      PIK->Space -= pTag->uiLen + PIK->ReqByte - cd - ct;
      v = i * PIK->ReqByte;
      c = ( ct << ( 16 - PIK->TCBits ) ) | ( cd << ( 16 - PIK->TCBits - PIK->DCBits ) );
      memcpy( &pData->cdxu.External.ExtData[ v + PIK->ReqByte - 2 ], &c, 2 );
      memcpy( &r, &pData->cdxu.External.ExtData[ v ], 4 );
      r &= m;
      r |= ( * p )->Tag;
      memcpy( &pData->cdxu.External.ExtData[ v ], &r, 4 );
      k -= pTag->uiLen - cd - ct;
      if( pTag->uiLen - cd - ct > 0 )
         memcpy( &pData->cdxu.External.ExtData[ k ], ( * p )->Value + cd,
                 pTag->uiLen - cd - ct );
      i++;
      ck++;
      pData->Entry_Ct++;
   }
   return ck;
}

static void hb_cdxTagExtNodeBuild( LPCDXTAG pTag, LPCDXDATA pData, LPPAGEINFO PIK )
{
   USHORT k, i, v, c, t, d;
   LONG r;
   LPKEYINFO pKey, pLastKey;
   static char szBuffer[ CDX_MAXKEY + 1 ];

   k = CDX_EXTERNAL_SPACE;
   PIK->Space  = pData->cdxu.External.FreeSpace;
   PIK->RNMask = pData->cdxu.External.RecNumMask;
   PIK->DCMask = pData->cdxu.External.DupCntMask;
   PIK->TCMask = pData->cdxu.External.TrlCntMask;
   PIK->RNBits = pData->cdxu.External.RecNumBits;
   PIK->DCBits = pData->cdxu.External.DupCntBits;
   PIK->TCBits = pData->cdxu.External.TrlCntBits;
   PIK->ReqByte = pData->cdxu.External.ShortBytes;
   i = 0;
   while( i < pData->Entry_Ct )
   {
      v = i * PIK->ReqByte;
      memcpy( &c, &pData->cdxu.External.ExtData[ v + PIK->ReqByte - 2 ], 2 );
      t = ( c >> ( 16 - PIK->TCBits ) ) & PIK->TCMask;
      d = ( c >> ( 16 - PIK->TCBits - PIK->DCBits ) ) & PIK->DCMask;
      memcpy( &r, &pData->cdxu.External.ExtData[ v ], 4 );
      r &= PIK->RNMask;
      k -= pTag->uiLen - d - t;
      if( pTag->uiLen - d - t > 0 )
         memcpy( &szBuffer[ d ], &pData->cdxu.External.ExtData[ k ],
                 pTag->uiLen - d - t );
      szBuffer[ pTag->uiLen - t ] = 0;
      pKey = hb_cdxKeyNew();
      pKey->Tag = r;
      pKey->Xtra = r;
      pKey->length = pTag->uiLen - t;
      pKey->Value = (char *) hb_xgrab( pKey->length + 1 );
      memcpy( pKey->Value, szBuffer, pKey->length );
      pKey->Value[ pKey->length ] = 0;

      if( PIK->pKeys == NULL )
         PIK->pKeys = pKey;
      else
      {
         pLastKey = PIK->pKeys;
         while( pLastKey->pNext )
            pLastKey = pLastKey->pNext;
         pLastKey->pNext = pKey;
      }
      PIK->uiKeys++;
      i++;
   }
}

/* Read a tag definition from the index file
 *
 * pTag = structure with a tag information
 */
static void hb_cdxTagTagLoad( LPCDXTAG pTag )
{
   CDXTAGHEADER pHeader;
   HB_MACRO_PTR pMacro;

   /* read the page from a file */
   hb_cdxIndexPageRead( pTag->pIndex, pTag->TagBlock, &pHeader, sizeof( CDXTAGHEADER ) );
   pTag->RootBlock = pHeader.lRoot;
   /* Return if:
    * no root page allocated
    * invalid root page offset (position inside an index file)
    * invalid key value length
    */
   if( pTag->RootBlock == 0 || pTag->RootBlock % CDX_PAGELEN > 0 ||
       (ULONG) pTag->RootBlock > hb_fsSeek( pTag->pIndex->hFile, 0, FS_END ) ||
       pHeader.uiKeySize > CDX_MAXKEY )
      return;

   pTag->uiLen = pHeader.uiKeySize;
   pTag->MaxKeys = ( CDX_PAGELEN - 12 ) / ( pTag->uiLen + 8 );
   pTag->OptFlags = pHeader.bType;
   pTag->UniqueKey = ( pTag->OptFlags & 0x01 );
   pTag->AscendKey = ( pHeader.iDescending == 0 );
   pTag->KeyExpr = ( char * ) hb_xgrab( CDX_MAXKEY + 1 );
   /* QUESTION: Is UPPER a valid operation here?
    * This will break expressions like:
    * somefield+'smallcaps'+otherfield
   */
   hb_strncpyUpper( pTag->KeyExpr, ( char * ) pHeader.KeyPool, CDX_MAXKEY );

   if( pTag->OptFlags < 0x80 && pTag->KeyExpr[ 0 ] == 0 )
      return;
   if( pTag->OptFlags & 0x80 )
      return;

   SELF_COMPILE( (AREAP) pTag->pIndex->pArea, ( BYTE * ) pTag->KeyExpr );
   pTag->pKeyItem = pTag->pIndex->pArea->valResult;
   pTag->pIndex->pArea->valResult = NULL;
   pMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pTag->pKeyItem );
   hb_macroRun( pMacro );
   /*
   This line must be replace the next one when hb_macroRun is fixed.
   By now it do the work as only string keys are supported.

   switch( hb_itemType( &hb_stack.Return ) )
    */
   switch( hb_itemType( hb_stackItemFromTop( -1 ) ) )
   {
      case HB_IT_INTEGER:
      case HB_IT_LONG:
      case HB_IT_DOUBLE:
         pTag->uiType = 'N';
         /* pTag->uiLen = 10; */
         pTag->uiLen = 8;
        break;

      case HB_IT_DATE:
         pTag->uiType = 'D';
         pTag->uiLen = 8;
         break;

      case HB_IT_LOGICAL:
         pTag->uiType = 'L';
         pTag->uiLen = 1;
         break;

      case HB_IT_STRING:
         pTag->uiType = 'C';
         /*
         pTag->uiLen = hb_stack.Return.item.asString.length > CDX_MAXKEY ? CDX_MAXKEY :
                           hb_stack.Return.item.asString.length;
          */
         pTag->uiLen = ( hb_stackItemFromTop( -1 ) )->item.asString.length > CDX_MAXKEY ? (BYTE) CDX_MAXKEY :
                           (BYTE) ( hb_stackItemFromTop( -1 ) )->item.asString.length;
         break;
   }
   hb_stackPop();    /* pop macro evaluated value */

   /* Check if there is a FOR expression */
   if( pHeader.KeyPool[ strlen( pTag->KeyExpr ) + 1 ] == 0 )
      return;
   pTag->ForExpr = ( char * ) hb_xgrab( CDX_MAXKEY + 1 );
   hb_strncpyUpper( pTag->ForExpr, ( const char * ) pHeader.KeyPool +
                    strlen( pTag->KeyExpr ) + 1, CDX_MAXKEY );
   SELF_COMPILE( (AREAP) pTag->pIndex->pArea, ( BYTE * ) pTag->ForExpr );
   pTag->pForItem = pTag->pIndex->pArea->valResult;
   pTag->pIndex->pArea->valResult = NULL;
   pMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pTag->pForItem );
   hb_macroRun( pMacro );
   if( hb_itemType( hb_stackItemFromTop( -1 ) ) != HB_IT_LOGICAL )
   {
      hb_macroDelete( pMacro );
      hb_itemRelease( pTag->pForItem );
      pTag->pForItem = NULL;
      hb_xfree( pTag->ForExpr );
      pTag->ForExpr = NULL;
   }
   hb_stackPop();
}

static void hb_cdxTagSetRoot( LPCDXTAG pTag, LPPAGEINFO PIK )
{
   LONG NRN, TmpTag;
   LPKEYINFO p, TmpStr;

   PIK->Owner = hb_cdxPageNew( pTag, NULL, 0 );
   NRN = hb_cdxIndexGetAvailPage( pTag->pIndex );
   PIK->Owner->Page = NRN;
   PIK->Owner->PageType = PAGE_ROOT;
   if( PIK->uiKeys > 0 )
   {
      p = hb_cdxPageGetKey( PIK, PIK->uiKeys - 1 );
      TmpTag = p->Tag;
      p->Tag = PIK->Page;
      hb_cdxPageInsertKey( PIK->Owner, p, TRUE );
      p->Tag = TmpTag;
   }
   else
   {
      TmpStr = hb_cdxKeyNew();
      TmpStr->Tag = PIK->Page;
      hb_cdxPageInsertKey( PIK->Owner, TmpStr, TRUE );
      hb_cdxKeyFree( TmpStr );
   }
   PIK->Owner->Child = PIK;
   pTag->RootPage = PIK->Owner;
   pTag->RootBlock = NRN;
   pTag->TagChanged = TRUE;
}

static void hb_cdxTagIntNodeWrite( LPCDXTAG pTag, LONG PN, LPCDXDATA pData, LPPAGEINFO PIK )
{
   USHORT Cnt, ck, kcnt, na;
   LONG rp, lp, NPN, TmpTag;
   LPKEYINFO p;

   if( PIK->uiKeys > pTag->MaxKeys )
   {
      Cnt = PIK->uiKeys / 2;
      Cnt = PIK->uiKeys - Cnt;
   }
   else
      Cnt = PIK->uiKeys;
   ck = 0;
   kcnt = Cnt;
   if( PIK->uiKeys > 0 )
   {
      pData->Entry_Ct = 0;
      if( kcnt < PIK->uiKeys )
      {
         ck = hb_cdxTagFillInternalNode( pTag, pData, PIK, kcnt, ck, &p );
         if( pData->Node_Atr % 2 > 0 )
            pData->Node_Atr--;
         na = pData->Node_Atr;
         rp = pData->Rght_Ptr;
         lp = pData->Left_Ptr;
         pData->Rght_Ptr = PN;
         NPN = hb_cdxIndexGetAvailPage( pTag->pIndex );
         TmpTag = p->Tag;
         p->Tag = NPN;
         hb_cdxPageAddNodeKey( PIK, p );
         p->Tag = TmpTag;
         if( PIK->PageType == PAGE_ROOT )
            PIK->PageType = PAGE_NODE;
         hb_cdxIndexPageWrite( pTag->pIndex, NPN, pData, CDX_PAGELEN );
         if( lp > 0 )
         {
            hb_cdxIndexPageRead( pTag->pIndex, lp, pData, CDX_PAGELEN );
            pData->Rght_Ptr = NPN;
            hb_cdxIndexPageWrite( pTag->pIndex, lp, pData, CDX_PAGELEN );
         }
         memset( pData, 32, CDX_PAGELEN );
         pData->Node_Atr = na;
         pData->Rght_Ptr = rp;
         pData->Left_Ptr = NPN;
         pData->Entry_Ct = 0;
         kcnt = pTag->MaxKeys;
      }
      hb_cdxTagFillInternalNode( pTag, pData, PIK, kcnt, ck, &p );
   }
   hb_cdxIndexPageWrite( pTag->pIndex, PN, pData, CDX_PAGELEN );
}

static USHORT hb_cdxTagFillInternalNode( LPCDXTAG pTag, LPCDXDATA pData,
                                         LPPAGEINFO PIK, USHORT kcnt,
                                         USHORT ck, LPKEYINFO * p )
{
   USHORT i, v;
   LONG r;

   i = 0;
   memset( pData->cdxu.Internal.IntData, ( pTag->uiType == 'C' ) ? 32 : 0,
           sizeof( pData->cdxu.Internal.IntData ) );
   while( i < kcnt && ck < PIK->uiKeys )
   {
      * p = hb_cdxPageGetKey( PIK, ck );
      v = i * ( pTag->uiLen + 8 );
      memcpy( &pData->cdxu.Internal.IntData[ v ],
              ( * p )->Value,
              ( * p )->length );
      v += pTag->uiLen;
      r = hb_cdxSwapBytes( ( * p )->Xtra );
      memcpy( &pData->cdxu.Internal.IntData[ v ], &r, 4 );
      r = hb_cdxSwapBytes( ( * p )->Tag );
      memcpy( &pData->cdxu.Internal.IntData[ v + 4 ], &r, 4 );
      i++;
      ck++;
      pData->Entry_Ct++;
   }
   return ck;
}

static void hb_cdxTagIntNodeBuild( LPCDXTAG pTag, LPCDXDATA pData, LPPAGEINFO pPage )
{
   USHORT i, v;
   LONG r, n;
   LPKEYINFO pKey, pLastKey;
   static char szBuffer[ CDX_MAXKEY + 1 ];

   i = 0;
   while( i < pData->Entry_Ct )
   {
      v = i * ( pTag->uiLen + 8 );
      memmove( szBuffer, pData->cdxu.Internal.IntData + v, pTag->uiLen );
      szBuffer[ pTag->uiLen ] = 0;
      v += pTag->uiLen;
      memcpy( &r, &pData->cdxu.Internal.IntData[ v ], 4 );
      r = hb_cdxSwapBytes( r );
      memcpy( &n, &pData->cdxu.Internal.IntData[ v + 4 ], 4 );
      n = hb_cdxSwapBytes( n );
      pKey = hb_cdxKeyNew();
      if( pTag->uiType == 'C' )
      {
         v = strlen( szBuffer );
         while( v > 0 && szBuffer[ v - 1 ] == ' ' )
            v--;
         szBuffer[ v ] = 0;
      }
      else
      {
         v = pTag->uiLen;
         while( v > 0 && szBuffer[ v - 1 ] == 0 )
            v--;
         szBuffer[ v ] = 0;
      }
      pKey->length = v;
      pKey->Value = (char *) hb_xgrab( v + 1 );
      memcpy( pKey->Value, szBuffer, v );
      pKey->Value[v] = 0;
      pKey->fString = ( pTag->uiType == 'C' ? TRUE : FALSE);

      pKey->Tag = n;
      pKey->Xtra = r;
      if( pPage->pKeys == NULL )
         pPage->pKeys = pKey;
      else
      {
         pLastKey = pPage->pKeys;
         while( pLastKey->pNext != NULL )
            pLastKey = pLastKey->pNext;
         pLastKey->pNext = pKey;
      }
      pPage->uiKeys++;
      i++;
   }
}

static LONG hb_cdxTagKeyFind( LPCDXTAG pTag, LPKEYINFO pKey )
{
   int K;

   pTag->CurKeyInfo->Tag = 0;
   hb_cdxTagTagOpen( pTag, 0 );
   if( pTag->RootPage == NULL )
      return 0;
   pTag->TagBOF = pTag->TagEOF = FALSE;
   K = hb_cdxPageSeekKey( pTag->RootPage, pKey->Tag, pKey, FALSE );
   if( K == 0 )
   {
      hb_cdxPageRetrieveKey( pTag->RootPage, &pTag->CurKeyInfo );
      if( pTag->pForItem == NULL )
         return pTag->CurKeyInfo->Tag;
      else
         /* TODO: test for expression */
         pTag->TagEOF = TRUE;
   }
   else if( K < 0 )
   {
      hb_cdxPageRetrieveKey( pTag->RootPage, &pTag->CurKeyInfo );
      if( pTag->pForItem != NULL )
         /* TODO: test for expression */
         pTag->TagEOF = TRUE;
   }
   else
      pTag->TagEOF = TRUE;
   return 0;
}

static LPCDXTAG hb_cdxGetActiveTag( LPCDXINDEX PIF )
{
   LPCDXTAG pTag;
   USHORT   uiTag;

   if( ! PIF )
      return NULL;

   uiTag = PIF->uiTag;
   if ( ! uiTag )
      return NULL;

   pTag = PIF->TagList;
   --uiTag;
   while( uiTag && pTag )
   {
      pTag = pTag->pNext;
      --uiTag;
   }

   if( !pTag )
      PIF->uiTag = 0;

   return pTag;
}

static LPCDXTAG hb_cdxGetTagByNumber(CDXAREAP pArea,  USHORT uiTag )
{
   LPCDXTAG pTag;

   if ( ! uiTag )
      return NULL;

   pTag = pArea->lpIndexes->TagList;
   --uiTag;
   while( uiTag && pTag )
   {
      pTag = pTag->pNext;
      --uiTag;
   }

   return pTag;
}



/* end hb_cdxTagxxx */

/* hb_cdxPagexxx */
/* #include "cdxpage.c" */
static LPPAGEINFO hb_cdxPageNew( LPCDXTAG PIT, LPPAGEINFO PIK, LONG FilePosn )
{
   LPPAGEINFO pPage;

   pPage = ( LPPAGEINFO ) hb_xgrab( sizeof( HB_PAGEINFO ) );
   memset( pPage, 0, sizeof( HB_PAGEINFO ) );
   pPage->Page = FilePosn;
   pPage->Left = pPage->Right = -1;
   pPage->Owner = PIK;
   pPage->TagParent = PIT;
   pPage->CurKey = -1;
   if( FilePosn > 0 )
      hb_cdxPagePageLoad( pPage );
   return pPage;
}

static void hb_cdxPageFree( LPPAGEINFO pPage )
{
   LPKEYINFO pKey;

   if( pPage->Child != NULL )
   {
      hb_cdxPageFree( pPage->Child );
      pPage->Child = NULL;
   }

   if( pPage->Changed )
      hb_cdxPagePageStore( pPage );

   if( pPage->NewRoot && pPage->Owner != NULL )
   {
      pPage->Owner->Child = NULL;
      hb_cdxPageFree( pPage->Owner );
      pPage->Owner = NULL;
   }
   pPage->NewRoot = FALSE;

   if( pPage->Owner != NULL )
      pPage->Owner->Child = NULL;
   pPage->Owner = NULL;

   /* Free all keys */
   while( pPage->pKeys != NULL )
   {
      pKey = pPage->pKeys;
      pPage->pKeys = pKey->pNext;
      hb_cdxKeyFree( pKey );
   }
   hb_xfree( pPage );
}

static BOOL hb_cdxPageReadTopKey( LPPAGEINFO pPage )
{
   LPKEYINFO pKey;

   if( pPage->uiKeys == 0 )
      return FALSE;
   pPage->CurKey = 0;
   pKey = pPage->pKeys;
   if( pPage->PageType < PAGE_LEAF )
   {
      if( hb_cdxPageGetChild( pPage, pKey->Tag ) )
         return hb_cdxPageReadTopKey( pPage->Child );
      else
         return FALSE;
   }
   else
   {
      pPage->TagParent->CurKeyInfo =
                        hb_cdxKeyCopy( pPage->TagParent->CurKeyInfo, pKey );
      return TRUE;
   }
}

static BOOL hb_cdxPageReadBottomKey( LPPAGEINFO pPage )
{
   LPKEYINFO pKey;

   if( pPage->uiKeys == 0 )
      return FALSE;
   pPage->CurKey = pPage->uiKeys - 1;
   pKey = hb_cdxPageGetKey( pPage, pPage->CurKey );
   if( pPage->PageType < PAGE_LEAF )
   {
      /*
      printf( "hb_cdxPageReadBottomKey()" );
      return TRUE;
       */
      if( hb_cdxPageGetChild( pPage, pKey->Tag ) )
         return hb_cdxPageReadBottomKey( pPage->Child );
      else
         return FALSE;
   }
   else
   {
      pPage->TagParent->CurKeyInfo =
                        hb_cdxKeyCopy( pPage->TagParent->CurKeyInfo, pKey );
      return TRUE;
   }
}

static int hb_cdxPageSeekKey( LPPAGEINFO pPage, LONG lBlock, LPKEYINFO pKey, BOOL bExact )
{
   int k;
   USHORT dav;
   LPKEYINFO p;

   k = 1;
   bExact = ( bExact || pPage->TagParent->uiType != 'C' );
   pPage->CurKey = 0;
   if( pPage->uiKeys > 0 )
   {
      while( k > 0 && pPage->CurKey < pPage->uiKeys )
      {
         p = hb_cdxPageGetKey( pPage, pPage->CurKey );
         k = hb_cdxKeyCompare( pKey, p, &dav, bExact );
         if( !pPage->TagParent->AscendKey )
            k = -k;
         if( k == 0 && lBlock == CDX_MAX_REC_NUM )
            k = 1;
         if( k == 0 && lBlock != CDX_IGNORE_REC_NUM )
         {
            if( lBlock > p->Xtra )
               k = 1;
            else if( lBlock < p->Xtra )
               k = -1;
         }
         if( k <= 0 || pPage->CurKey == pPage->uiKeys - 1 )
         {
            if( pPage->PageType < PAGE_LEAF )
            {
               hb_cdxPageGetChild( pPage, p->Tag );
               k = hb_cdxPageSeekKey( pPage->Child, lBlock, pKey, bExact );
            }
            else if( k == 0 && lBlock != CDX_IGNORE_REC_NUM )
            {
               if( lBlock > p->Tag )
                  k = 1;
               else if( lBlock < p->Tag )
                  k = -1;
            }
         }
         if( k > 0 )
            pPage->CurKey++;
      }
   }
   return k;
}

static void hb_cdxPageInsertKey( LPPAGEINFO pPage, LPKEYINFO pKey, BOOL bAddAfter )
{
   USHORT uiCount;
   LPKEYINFO pNewKey, pLastKey;

   if( pPage->Child != NULL )
      hb_cdxPageInsertKey( pPage->Child, pKey, bAddAfter );
   else
   {
      pNewKey = hb_cdxKeyNew();
      pNewKey = hb_cdxKeyCopy( pNewKey, pKey );
      if( bAddAfter )
         pPage->CurKey++;
      if( pPage->CurKey > pPage->uiKeys )
         pPage->CurKey = pPage->uiKeys;
      if( pPage->CurKey < 0 )
         pPage->CurKey = 0;
      if( pPage->Owner != NULL && pPage->CurKey >= pPage->uiKeys )
         printf( "hb_cdxPageInsertKey()" );
      if( pPage->pKeys == NULL )
         pPage->pKeys = pNewKey;
      else
      {
         if( pPage->CurKey == 0 )
         {
            pNewKey->pNext = pPage->pKeys;
            pPage->pKeys = pNewKey;
         }
         else
         {
            uiCount = pPage->CurKey;
            pLastKey = pPage->pKeys;
            while( uiCount > 1 && pLastKey->pNext )
            {
               pLastKey = pLastKey->pNext;
               uiCount--;
            }
            pNewKey->pNext = pLastKey->pNext;
            pLastKey->pNext = pNewKey;
         }
      }
      pPage->uiKeys++;
      pPage->Changed = TRUE;
   }
}

static void hb_cdxPagePageStore( LPPAGEINFO pPage )
{
   if( pPage->Page == 0 )
      pPage->Page = hb_cdxIndexGetAvailPage( pPage->TagParent->pIndex );
   hb_cdxTagPageStore( pPage->TagParent, pPage );
   pPage->Changed = FALSE;
}

static BOOL hb_cdxPageReadNextKey( LPPAGEINFO pPage )
{
   BOOL b;
   LPKEYINFO p;

   if( pPage->uiKeys == 0 )
      return FALSE;
   if( pPage->PageType < PAGE_LEAF )
   {
      b = FALSE;
      while( ! b && pPage->CurKey < pPage->uiKeys )
      {
         p = hb_cdxPageGetKey( pPage, pPage->CurKey );
         if( hb_cdxPageGetChild( pPage, p->Tag ) )
         {
            if( pPage->Child->CurKey == -1 )
            {
               if( pPage->Child->PageType < PAGE_LEAF )
                  pPage->Child->CurKey = 0;
            }
            b = hb_cdxPageReadNextKey( pPage->Child );
         }
         else
            b = FALSE;
         if( !b )
            pPage->CurKey++;
      }
      if( !b )
         pPage->CurKey = pPage->uiKeys;
      return b;
   }
   else
   {
      pPage->CurKey++;
      if( pPage->CurKey < pPage->uiKeys )
      {
         p = hb_cdxPageGetKey( pPage, pPage->CurKey );
         pPage->TagParent->CurKeyInfo =
                        hb_cdxKeyCopy( pPage->TagParent->CurKeyInfo, p );
         return TRUE;
      }
      else
      {
         pPage->CurKey = pPage->uiKeys;
         return FALSE;
      }
   }
}

static BOOL hb_cdxPageReadPrevKey( LPPAGEINFO pPage )
{
   BOOL b;
   LPKEYINFO p;

   if( pPage->uiKeys == 0 )
      return FALSE;
   if( pPage->PageType < PAGE_LEAF )
   {
      b = FALSE;
      while( ! b && pPage->CurKey >= 0 )
      {
         p = hb_cdxPageGetKey( pPage, pPage->CurKey );
         if( hb_cdxPageGetChild( pPage, p->Tag ) )
         {
            if( pPage->Child->CurKey == -1 )
            {
               /* if( pPage->Child->PageType < PAGE_LEAF ) */
               pPage->Child->CurKey = pPage->Child->uiKeys;
            }
            b = hb_cdxPageReadPrevKey( pPage->Child );
         }
         else
            b = FALSE;
         if( !b )
            pPage->CurKey--;
      }
      if( !b )
         pPage->CurKey = -1;
      return b;
   }
   else
   {
      pPage->CurKey--;
      if( pPage->CurKey >= 0 )
      {
         p = hb_cdxPageGetKey( pPage, pPage->CurKey );
         pPage->TagParent->CurKeyInfo =
                           hb_cdxKeyCopy( pPage->TagParent->CurKeyInfo, p );
         return TRUE;
      }
      else
      {
         pPage->CurKey = -1;
         return FALSE;
      }
   }
}


static LPKEYINFO hb_cdxPageGetKey( LPPAGEINFO pPage, USHORT uiKey )
{
   LPKEYINFO pKey;

   pKey = pPage->pKeys;
   while( uiKey > 0 && pKey->pNext != NULL )
   {
      pKey = pKey->pNext;
      uiKey--;
   }
   return pKey;
}

static void hb_cdxPagePageLoad( LPPAGEINFO pPage )
{
   LPKEYINFO pKey;

   while( pPage->pKeys != NULL )
   {
      pKey = pPage->pKeys;
      pPage->pKeys = pPage->pKeys->pNext;
      hb_cdxKeyFree( pKey );
   }
   pPage->uiKeys = 0;
   hb_cdxTagPageLoad( pPage->TagParent, pPage );
   pPage->Changed = FALSE;
}

static int hb_cdxPageRetrieveKey( LPPAGEINFO pPage, LPKEYINFO *pKey )
{
   int iCheck;
   LPKEYINFO pNewKey;

   if( pPage->Owner == NULL )
      pPage->ChkBOF = pPage->ChkEOF = TRUE;
   pPage->ChkBOF = ( pPage->ChkBOF && pPage->CurKey == 0 );
   pPage->ChkEOF = ( pPage->ChkEOF && pPage->CurKey == pPage->uiKeys - 1 );
   if( pPage->Child != NULL )
      return hb_cdxPageRetrieveKey( pPage->Child, pKey );
   else
   {
      pNewKey = hb_cdxPageGetKey( pPage, pPage->CurKey );
      *pKey = hb_cdxKeyCopy( *pKey, pNewKey );
      iCheck = 0;
      if( pPage->ChkBOF )
         iCheck = 1;
      if( pPage->ChkEOF )
         iCheck += 2;
      return iCheck;
   }
}

static void hb_cdxPageAddNodeKey( LPPAGEINFO pPage, LPKEYINFO pKey )
{
   if( pPage->Owner == NULL )
   {
      hb_cdxTagSetRoot( pPage->TagParent, pPage );
      pPage->NewRoot = TRUE;
   }
   pPage->Owner->CurKey = hb_cdxPageSeekNodeTag( pPage->Owner, pPage->Page );
   pPage->Owner->Child = NULL;
   hb_cdxPageInsertKey( pPage->Owner, pKey, FALSE );
   pPage->Owner->CurKey++;
   pPage->Owner->Child = pPage;
   pPage->Reload = TRUE;
}

static int hb_cdxPageSeekNodeTag( LPPAGEINFO pPage, LONG Tag )
{
   int iSeek;
   USHORT i;
   LPKEYINFO p;

   if( pPage->uiKeys == 0 )
      return -1;
   else
   {
      iSeek = -1;
      i = 0;
      while( iSeek < 0 && i < pPage->uiKeys )
      {
         p = hb_cdxPageGetKey( pPage, i );
         if( p->Tag == Tag )
            iSeek = i;
         i++;
      }
      return iSeek;
   }
}

static BOOL hb_cdxPageGetChild( LPPAGEINFO pPage, LONG Tag )
{
   if( Tag == 0 )
      return FALSE;
   if( pPage->TagParent->pIndex->NextAvail > 0 &&
       Tag > pPage->TagParent->pIndex->NextAvail )
      return FALSE;
   if( pPage->PageType < PAGE_LEAF )
   {
      if( pPage->Child != NULL )
      {
         if( pPage->Child->Page != Tag || pPage->Child->Reload )
         {
            hb_cdxPageFree( pPage->Child );
            pPage->Child = NULL;
         }
      }
      if( pPage->Child == NULL )
         pPage->Child = hb_cdxPageNew( pPage->TagParent, pPage, Tag );
   }
   return ( pPage->Child != NULL );
}

static void hb_cdxPageDeleteKey( LPPAGEINFO pPage )
{
   BOOL lastone;
   LONG TempTag;
   LPKEYINFO p, pPrevKey;

   pPage->Changed = TRUE;
   if( pPage->Child != NULL )
      hb_cdxPageDeleteKey( pPage->Child );
   else
   {
      lastone = ( pPage->CurKey >= pPage->uiKeys - 1 );
      p = hb_cdxPageGetKey( pPage, pPage->CurKey );
      if( pPage->pKeys == p )
         pPage->pKeys = p->pNext;
      else
      {
         pPrevKey = hb_cdxPageGetKey( pPage, pPage->CurKey - 1 );
         pPrevKey->pNext = p->pNext;
      }
      pPage->uiKeys--;
      hb_cdxKeyFree( p );
      if( pPage->CurKey >= pPage->uiKeys )
         pPage->CurKey = pPage->uiKeys - 1;
      if( pPage->CurKey < 0 )
         pPage->CurKey = 0;
      if( lastone && pPage->Owner != NULL )
      {
         if( pPage->uiKeys > 0 )
         {
            p = hb_cdxPageGetKey( pPage, pPage->CurKey );
            TempTag = p->Tag;
            p->Tag = pPage->Page;
            /* printf( "ReplaceNodeKey();" );  */
            hb_cdxPageReplaceNodeKey( pPage->Owner, p );
            p->Tag = TempTag;
         }
         else
            /* printf( "DeleteNodeKey();" ); */
            hb_cdxPageDeleteNodeKey( pPage->Owner );
      }
   }
}

static void hb_cdxPageReplaceNodeKey( LPPAGEINFO pPage, LPKEYINFO pKey )
{
   LPKEYINFO p, pPrevKey;
   LONG TempTag;

   p = hb_cdxPageGetKey( pPage, pPage->CurKey );
   p = hb_cdxKeyCopy( p, pKey );
   if( !pPage->CurKey )
      pPage->pKeys = p;
   else
   {
      pPrevKey = hb_cdxPageGetKey( pPage, pPage->CurKey - 1 );
      pPrevKey->pNext = p;
   }
   if ( pPage->Owner != NULL && pPage->CurKey >= pPage->uiKeys - 1 )
   {
      TempTag = p->Tag;
      p->Tag = pPage->Page;
      hb_cdxPageReplaceNodeKey( pPage->Owner, pKey );
      p->Tag = TempTag;
   }

   return;
}

static void hb_cdxPageDeleteNodeKey( LPPAGEINFO pPage )
{
   BOOL lastone;
   LONG TempTag;
   LPKEYINFO p, pPrevKey;

   lastone = ( pPage->CurKey >= pPage->uiKeys - 1 );
   p = hb_cdxPageGetKey( pPage, pPage->CurKey );
   if( pPage->pKeys == p )
      pPage->pKeys = p->pNext;
   else
   {
      pPrevKey = hb_cdxPageGetKey( pPage, pPage->CurKey - 1 );
      pPrevKey->pNext = p->pNext;
   }
   pPage->uiKeys--;
   hb_cdxKeyFree( p );
   if( pPage->CurKey >= pPage->uiKeys )
      pPage->CurKey = pPage->uiKeys - 1;
   if( pPage->CurKey < 0 )
      pPage->CurKey = 0;
   if( lastone && pPage->Owner != NULL )
   {
      if( pPage->uiKeys > 0 )
      {
         p = hb_cdxPageGetKey( pPage, pPage->CurKey );
         TempTag = p->Tag;
         p->Tag = pPage->Page;
         hb_cdxPageReplaceNodeKey( pPage->Owner, p );
         p->Tag = TempTag;
      }
      else
         hb_cdxPageDeleteNodeKey( pPage->Owner );
   }
}

/* end hb_cdxPagexxx */

/* hb_cdxIndexxxx */
/* #include "cdxindex.c" */
static LPCDXINDEX hb_cdxIndexNew( AREAP pArea )
{
   LPCDXINDEX pIndex;

   pIndex = ( LPCDXINDEX ) hb_xgrab( sizeof( CDXINDEX ) );
   memset( pIndex, 0, sizeof( CDXINDEX ) );
   pIndex->hFile = FS_ERROR;
   pIndex->pArea = (LPCDXAREA) pArea;
   pIndex->NextAvail = -1;
   return pIndex;
}

static void hb_cdxIndexFree( LPCDXINDEX pIndex )
{
   LPCDXTAG pTag;

   /* Free Compound tag */
   if( pIndex->pCompound != NULL )
   {
      hb_cdxTagTagClose( pIndex->pCompound );
      hb_cdxTagFree( pIndex->pCompound );
   }

   /* Free all tags */
   while( pIndex->TagList )
   {
      pTag = pIndex->TagList;
      pIndex->TagList = pTag->pNext;
      hb_cdxTagFree( pTag );
   }

   /* Close file */
   if( pIndex->hFile != FS_ERROR )
      hb_fsClose( pIndex->hFile );

   if ( pIndex->szFileName != NULL )
      hb_xfree( pIndex->szFileName );
   hb_xfree( pIndex );
}

static LONG hb_cdxIndexGetAvailPage( LPCDXINDEX pIndex )
{
   if( pIndex->NextAvail == -1 )
      hb_cdxIndexResetAvailPage( pIndex );
   pIndex->NextAvail += CDX_PAGELEN;
   return pIndex->NextAvail - CDX_PAGELEN;
}

static void hb_cdxIndexResetAvailPage( LPCDXINDEX pIndex )
{
   pIndex->NextAvail = hb_fsSeek( pIndex->hFile, 0, FS_END );
}

static void hb_cdxIndexPageRead( LPCDXINDEX pIndex, LONG lPos, void * pBuffer,
                                 USHORT uiSize )
{
   if( hb_fsSeek( pIndex->hFile, lPos, FS_SET ) == (ULONG) lPos )
      hb_fsRead( pIndex->hFile, ( BYTE * ) pBuffer, uiSize );
}

static void hb_cdxIndexPageWrite( LPCDXINDEX pIndex, LONG lPos, void * pBuffer,
                                  USHORT uiSize )
{
   if( hb_fsSeek( pIndex->hFile, lPos, FS_SET ) == (ULONG) lPos )
      hb_fsWrite( pIndex->hFile, ( BYTE * ) pBuffer, uiSize );
}

static void hb_cdxIndexAddTag( LPCDXINDEX pIndex, char * szTagName, char * szKeyExp,
                               PHB_ITEM pKeyItem, BYTE bType, USHORT uiLen, char * szForExp,
                               PHB_ITEM pForItem, BOOL bAscending, BOOL bUnique )
{
   LPCDXTAG pTag, pLastTag;
   LPKEYINFO pKey;

   hb_cdxTagTagOpen( pIndex->pCompound, 0 );
   pKey = hb_cdxKeyNew();
   pKey = hb_cdxKeyPutC( pKey, szTagName );
   pTag = pIndex->TagList;
   pLastTag = NULL;
   while( pTag != NULL )
   {
      if( hb_stricmp( pTag->szName, szTagName ) == 0 )
      {
         pKey->Tag = pTag->TagBlock;
         if( hb_cdxTagKeyFind( pIndex->pCompound, pKey ) > 0 )
            hb_cdxPageDeleteKey( pIndex->pCompound->RootPage );
         if( pLastTag == NULL )
            pIndex->TagList = pTag->pNext;
         else
            pLastTag->pNext = pTag->pNext;
         hb_cdxTagFree( pTag );
         break;
      }
      pLastTag = pTag;
      pTag = pTag->pNext;
   }

   /* Create new tag an add to tag list */
   pTag = hb_cdxTagNew( pIndex, szTagName, -1 );
   if( pIndex->TagList == NULL )
      pIndex->TagList = pTag;
   else
   {
      pLastTag = pIndex->TagList;
      while( pLastTag->pNext )
         pLastTag = pLastTag->pNext;
      pLastTag->pNext = pTag;
   }
   hb_cdxTagIndexTagNew( pTag, szKeyExp, pKeyItem, bType, uiLen, szForExp,
                         pForItem, bAscending, bUnique );
   pKey = hb_cdxKeyPutC( pKey, pTag->szName );
   pKey->Tag = pTag->TagBlock;
   hb_cdxTagKeyAdd( pIndex->pCompound, pKey );
   hb_cdxKeyFree( pKey );
   pIndex->pCompound->RootPage->Changed = TRUE;
   hb_cdxTagTagClose( pIndex->pCompound );
}
/* end hb_cdxIndexxxx */

/* hb_cdxSortxxx */
/* #include "cdxsort.c" */
static LPSORTINFO hb_cdxSortNew( LPCDXTAG pTag, BOOL bUnique )
{
   BYTE * P;
   LPSORTINFO pSort;

   pSort = ( LPSORTINFO ) hb_xgrab( sizeof( SORTINFO ) );
   memset( pSort, 0, sizeof( SORTINFO ) );
   pSort->SortChunk = SORT_CHUNK_LIMIT;
   pSort->NodeLimit = pSort->SortChunk / sizeof( SORTDATA );
   pSort->NodeMask = pSort->NodeShift = pSort->NodeCur = 1;
   pSort->ChunkLimit = 0x8000;
   while( pSort->NodeMask < pSort->NodeLimit - 1 )
   {
      pSort->NodeMask = ( pSort->NodeMask << 1 ) + 1;
      pSort->ChunkLimit >>= 1;
      pSort->NodeShift++;
   }
   pSort->ChunkSize = pSort->ChunkLimit;
   pSort->ChunkList = ( long * ) hb_xgrab( pSort->ChunkSize * sizeof( LONG ) );
   memset( pSort->ChunkList, 0, pSort->ChunkSize * sizeof( LONG ) );
   P = ( BYTE * ) hb_xgrab( pSort->SortChunk * sizeof( BYTE ) );
   memset( P, 0, pSort->SortChunk * sizeof( BYTE ) );
   pSort->ChunkList[ 0 ] = ( LONG ) P;
   hb_cdxSortLinkNew( pSort, &pSort->RootLink );
   pSort->Unique = bUnique;
   pSort->Ascend = TRUE;
   pSort->CurTag = pTag;
   pSort->KeyTot = pTag->pIndex->pArea->ulRecCount;
   pSort->KeyWork = hb_cdxKeyNew();
   pSort->LastKey = hb_cdxKeyNew();
   return pSort;
}

static void hb_cdxSortFree( LPSORTINFO pSort )
{
   USHORT usCount;
   LONG pa;

   pSort->Closing = TRUE;
   for( usCount = 0; usCount <= 30; usCount++ )
   {
      if( pSort->NodeList[ usCount ] != NULL )
      {
         pa = pSort->NodeList[ usCount ]->Rght_Ptr;
         pSort->NodeList[ usCount ]->Rght_Ptr = -1;
         if( pSort->NodeList[ usCount ]->Entry_Ct > 0 )
         {
            if( pSort->NodeList[ usCount + 1 ] == NULL )
            {
               pSort->CurTag->RootBlock = pa;
               pSort->NodeList[ usCount ]->Node_Atr++;
            }
            hb_cdxIndexPageWrite( pSort->CurTag->pIndex, pa, pSort->NodeList[ usCount ],
                                  sizeof( CDXDATA ) );
            if( pSort->NodeList[ usCount + 1 ] != NULL )
               hb_cdxSortAddToNode( pSort, ( USHORT ) ( usCount + 1 ), pa,
                                    pSort->LastTag, pSort->KeyWork );
         }
         hb_xfree( ( LPCDXDATA ) pSort->NodeList[ usCount ] );
      }
   }

   if( pSort->ChunkList != NULL )
   {
      for( usCount = 0; usCount < pSort->ChunkLimit; usCount++ )
      {
         if( pSort->ChunkList[ usCount ] != 0 )
            hb_xfree( ( BYTE * ) pSort->ChunkList[ usCount ] );
      }
      hb_xfree( pSort->ChunkList );
   }
   hb_cdxKeyFree( pSort->KeyWork );
   hb_cdxKeyFree( pSort->LastKey );

   hb_xfree( pSort );
}

static void hb_cdxSortLinkNew( LPSORTINFO pSort, LONG * NewLink )
{
   if( pSort->NodeCur >= pSort->NodeLimit )
      hb_cdxSortGetNewChunk( pSort );
   * NewLink = ( pSort->ChunkCur << pSort->NodeShift ) + pSort->NodeCur;
   pSort->NodeCur++;
}

static void hb_cdxSortGetNewChunk( LPSORTINFO pSort )
{
   BYTE * P;

   pSort->ChunkCur++;
   if( pSort->ChunkCur == pSort->ChunkLimit )
   {
      printf( "FlushChunks();" );
      return;
   }
   P = ( BYTE * ) pSort->ChunkList[ pSort->ChunkCur ];
   if( P == NULL )
      P = ( BYTE * ) hb_xgrab( pSort->SortChunk * sizeof( BYTE ) );
   if( pSort->ChunkCur != 0 )
   {
      memset( P, 0, pSort->SortChunk * sizeof( BYTE ) );
      pSort->ChunkList[ pSort->ChunkCur ] = ( LONG ) P;
      pSort->NodeCur = 0;
   }
}

static void hb_cdxSortInsertWord( LPSORTINFO pSort, LONG Tag, char * Value,
                                  USHORT uiLen )
{
   char s[ 8 ];
   USHORT cc, nc;
   LPSORTDATA wx;

   hb_cdxDNtoSort( ( double ) Tag, (BYTE *) &s[0] );

   if( pSort->NodeLimit - pSort->NodeCur < uiLen + 8 )
   {
      cc = pSort->ChunkCur;
      nc = pSort->NodeCur;
      hb_cdxSortGetNewChunk( pSort );
      if( pSort->ChunkCur > 0 )
      {
         pSort->ChunkCur = cc;
         pSort->NodeCur = nc;
      }
   }
   pSort->WordCount++;
   memcpy( pSort->WPch+1, Value, uiLen );
   pSort->WPch[0] = (BYTE) uiLen;
   if( pSort->WPch[0] > 0 )
   {
      if( pSort->WPch[0] > pSort->KeySize )
         pSort->KeySize = pSort->WPch[0];
      /* pSort->WPch[0]--; lose last byte! */
      /*
         Patch for empty fields (> 1)
      */
      while( pSort->WPch[0] > 1 && pSort->WPch[ pSort->WPch[0] ] ==
                                   ( pSort->CurTag->uiType == 'C' ? ' ' : 0 ) )
      {
         pSort->WPch[0]--;
      }
   }
   pSort->LevelPtr = pSort->RootLink;
   pSort->PriorPtr = 0;
   pSort->WCur = 0;
   do
      hb_cdxSortStuffKey( pSort, &wx, FALSE );
   while( pSort->WCur != pSort->WPch[0] );
   if( pSort->Unique )
   {
      if( SORT_GET_NUSE(wx->sortu.A.NUse) == SORT_END_OF_KEY )
         return;
      SORT_SET_NUSE( wx->sortu.A.NUse, SORT_END_OF_KEY);
   }

   memcpy( &pSort->WPch[ pSort->WPch[0] + 1 ], s, 8 );
   pSort->WPch[0] += 8;
   do
      hb_cdxSortStuffKey( pSort, &wx, TRUE );
   while( pSort->WCur != pSort->WPch[0] );

   SORT_SET_NUSE(wx->sortu.A.NUse, SORT_END_OF_WORD);
}

static void hb_cdxSortStuffKey( LPSORTINFO pSort, LPSORTDATA * wx, BOOL fTag )
{
   SHORT v;
   LONG p1;
   LPSORTDATA x;

   hb_cdxSortGetNode( pSort, pSort->WPch[ pSort->WCur + 1 ], &p1, fTag );
   * wx = hb_cdxSortLinkGet( pSort, p1 );
   pSort->WCur++;
   if( pSort->LevelPtr == 0 )
   {
      if( pSort->PriorPtr > 0 )
      {
         x = hb_cdxSortLinkGet( pSort, pSort->PriorPtr );
         x->sortu.A.WordArray = (BYTE) p1;
      }
      v = pSort->WPch[0] - pSort->WCur - 1;
      if( v > 0 )
      {
         if( v > 4 )
            v = 4;
         memcpy( ( * wx )->sortu.B.ChrStack, &pSort->WPch[ pSort->WCur+1 ], v );
         SORT_SET_NUSE( ( * wx )->sortu.A.NUse, SORT_STACK_OF_CHAR );
         SORT_SET_STACK_LEN( ( * wx )->sortu.A.NUse, v-1 );
         pSort->WCur += v;
      }
   }
   pSort->PriorPtr = p1;
   pSort->LevelPtr = ( * wx )->sortu.A.WordArray;
}

static void hb_cdxSortGetNode( LPSORTINFO pSort, BYTE Character,
                                                LONG * NewLink, BOOL fTag )
{
   char c;
   int df;
   LONG p, q, r;
   LPSORTDATA px, qx, rx;

   if( pSort->LevelPtr == 0 )
   {
      hb_cdxSortLinkNew( pSort, NewLink );
      px = hb_cdxSortLinkGet( pSort, * NewLink );
      px->sortu.A.Character = Character;
      if( fTag )
         px->sortu.A.NUse |= SORT_NOT_KEY;
      return;
   }
   p = pSort->LevelPtr;
   px = hb_cdxSortLinkGet( pSort, pSort->LevelPtr );
   q = pSort->PriorPtr;

   if( SORT_GET_NUSE(px->sortu.A.NUse) == SORT_STACK_OF_CHAR )
   {
      hb_cdxSortLinkNew( pSort, &r );
      c = px->sortu.A.Character;
      qx = hb_cdxSortLinkGet( pSort, q );
      qx->sortu.A.WordArray = (BYTE) r;
      rx = hb_cdxSortLinkGet( pSort, r );
      rx->sortu.A.Character = c;
      rx->sortu.A.WordArray = (BYTE) p;
      px = hb_cdxSortLinkGet( pSort, p );
      px->sortu.A.Character = px->sortu.B.ChrStack[ 0 ];
      memmove( &px->sortu.B.ChrStack[ 0 ], &px->sortu.B.ChrStack[ 1 ], 3 );
      px->sortu.B.ChrStack[ 3 ] = 0;
      if( px->sortu.A.NUse & SORT_NOT_KEY )
         rx->sortu.A.NUse |= SORT_NOT_KEY;
      if ( SORT_GET_STACK_LEN( px->sortu.A.NUse ) )
      {
         SORT_SET_STACK_LEN( px->sortu.A.NUse,
                      SORT_GET_STACK_LEN( px->sortu.A.NUse ) - 1 );
         SORT_SET_NUSE( px->sortu.A.NUse, SORT_STACK_OF_CHAR );
      }
      else
         SORT_SET_NUSE( px->sortu.A.NUse, SORT_ACTIVE_LIST );

      p = r;
      px = hb_cdxSortLinkGet( pSort, p );
   }
   if ( (px->sortu.A.NUse & SORT_NOT_KEY) && !fTag)
      df = 1;
   else if ( fTag && !(px->sortu.A.NUse & SORT_NOT_KEY) )
      df = -1;

   /*        nation support
   else if ( !fTag && pSort->CurTag->uiType == 'C' )
      df = hb_charcmp( Character, px->sortu.A.Character );
   */

   else if( Character > px->sortu.A.Character )
      df = 1;
   else if( Character < px->sortu.A.Character )
      df = -1;
   else
      df = 0;

   if( !pSort->Ascend )
      df = -df;
   while( px->sortu.A.LevelLink != 0 && df > 0 )
   {
      q = p;
      p = px->sortu.A.LevelLink;
      px = hb_cdxSortLinkGet( pSort, p );

      if( SORT_GET_NUSE(px->sortu.A.NUse) == SORT_STACK_OF_CHAR )
      {
         hb_cdxSortLinkNew( pSort, &r );
         c = px->sortu.A.Character;
         qx = hb_cdxSortLinkGet( pSort, q );
         qx->sortu.A.WordArray = (BYTE) r;
         rx = hb_cdxSortLinkGet( pSort, r );
         rx->sortu.A.Character = c;
         rx->sortu.A.WordArray = (BYTE) p;
         px = hb_cdxSortLinkGet( pSort, p );
         px->sortu.A.Character = px->sortu.B.ChrStack[ 0 ];
         memmove( &px->sortu.B.ChrStack[ 0 ], &px->sortu.B.ChrStack[ 1 ], 3 );
         px->sortu.B.ChrStack[ 3 ] = 0;
         if( px->sortu.A.NUse & SORT_NOT_KEY )
            rx->sortu.A.NUse |= SORT_NOT_KEY;
         if ( SORT_GET_STACK_LEN( px->sortu.A.NUse ) )
         {
            SORT_SET_STACK_LEN( px->sortu.A.NUse,
                         SORT_GET_STACK_LEN( px->sortu.A.NUse ) - 1 );
            SORT_SET_NUSE( px->sortu.A.NUse, SORT_STACK_OF_CHAR );
         }
         else
            SORT_SET_NUSE( px->sortu.A.NUse, SORT_ACTIVE_LIST );
         p = r;
         px = hb_cdxSortLinkGet( pSort, p );
      }
      if ( (px->sortu.A.NUse & SORT_NOT_KEY) && !fTag)
         df = 1;
      else if ( fTag && !(px->sortu.A.NUse & SORT_NOT_KEY) )
         df = -1;

      /*        nation support
      else if ( !fTag && pSort->CurTag->uiType == 'C' )
         df = hb_charcmp( Character, px->sortu.A.Character );
      */

      else if( Character > px->sortu.A.Character )
         df = 1;
      else if( Character < px->sortu.A.Character )
         df = -1;
      else
         df = 0;

      if( !pSort->Ascend )
         df = -df;
   }
   if( df == 0 )
      * NewLink = p;
   else
   {
      hb_cdxSortLinkNew( pSort, &r );
      if( df == -1 )
      {
         qx = hb_cdxSortLinkGet( pSort, q );
         if( q == pSort->PriorPtr )
            qx->sortu.A.WordArray = (BYTE) r;
         else
            qx->sortu.A.LevelLink = (BYTE) r;
      }
      else
      {
         p = px->sortu.A.LevelLink;
         px->sortu.A.LevelLink = (BYTE) r;
      }
      rx = hb_cdxSortLinkGet( pSort, r );
      rx->sortu.A.LevelLink = (BYTE) p;
      rx->sortu.A.Character = Character;
      if( fTag )
         rx->sortu.A.NUse |= SORT_NOT_KEY;
      * NewLink = r;
   }
}

static LPSORTDATA hb_cdxSortLinkGet( LPSORTINFO pSort, LONG Value )
{
   LPSORTDATA P;

   if( Value > 0 )
   {
      P = ( LPSORTDATA ) pSort->ChunkList[ Value >> pSort->NodeShift ];
      if( P != NULL )
         return &P[ Value & pSort->NodeMask ];
      else
         return NULL;
   }
   return NULL;
}

static void hb_cdxSortDisplayWord( LPSORTINFO pSort )
{
   pSort->WPch[ 0 ] = 0;
   hb_cdxSortRecurseDict( pSort, pSort->RootLink, 0 );
}

static void hb_cdxSortRecurseDict( LPSORTINFO pSort, LONG WPtr, LONG WBgn )
{
   USHORT WCnt;

   if( WPtr == 0 )
      return;
   WCnt = pSort->WPch[ 0 ];
   pSort->WAdr = hb_cdxSortLinkGet( pSort, WPtr );
   pSort->WPch[ WCnt + 1 ] = pSort->WAdr->sortu.A.Character;
   pSort->WPch[ 0 ]++;
   if( SORT_GET_NUSE( pSort->WAdr->sortu.A.NUse ) == SORT_STACK_OF_CHAR )
   {
      memcpy( &pSort->WPch[ pSort->WPch[ 0 ] + 1 ],
                                        pSort->WAdr->sortu.B.ChrStack, 4 );
      pSort->WPch[ 0 ] += SORT_GET_STACK_LEN( pSort->WAdr->sortu.A.NUse ) + 1;
   }
   if( SORT_GET_NUSE(pSort->WAdr->sortu.A.NUse) == SORT_END_OF_WORD )
      hb_cdxSortSendWord( pSort, pSort->WPch );
   else
   {
      if( pSort->WAdr->sortu.A.WordArray != 0 )
         hb_cdxSortRecurseDict( pSort, pSort->WAdr->sortu.A.WordArray, WBgn );
      pSort->WAdr = hb_cdxSortLinkGet( pSort, WPtr );
   }
   pSort->WPch[ 0 ] = (BYTE) WCnt;
   if( pSort->WAdr->sortu.A.LevelLink != 0 &&
          SORT_GET_NUSE( pSort->WAdr->sortu.A.NUse) != SORT_STACK_OF_CHAR )
      hb_cdxSortRecurseDict( pSort, pSort->WAdr->sortu.A.LevelLink, WCnt );
}

static void hb_cdxSortSendWord( LPSORTINFO pSort, BYTE * Value )
{
   LONG Tag;
   char * pce;
   USHORT uiLen;

   uiLen = ( USHORT ) Value[0];
   Value++;
   pce = (char *) (Value + uiLen - 8) ;
   Tag = ( long ) hb_cdxSorttoND( (BYTE *) pce, 8 );
   hb_cdxSortOutputWord( pSort, Tag, Value, uiLen-8 );
}

static void hb_cdxSortOutputWord( LPSORTINFO pSort, LONG Tag, BYTE * Value,
                                  USHORT uiLen )
{
   pSort->KeyCnt++;
   /*
    Patch for empty fields
    */
   while( uiLen > 0 && Value[uiLen-1] ==
      ( pSort->CurTag->uiType == 'C' ? ' ' : 0 ) )
   {
      uiLen--;
   }
   hb_cdxKeyPut( pSort->KeyWork, Value, uiLen );
   hb_cdxSortAddToNode( pSort, 0, Tag, Tag, pSort->KeyWork );
   pSort->LastTag = Tag;
   hb_cdxKeyCopy( pSort->LastKey, pSort->KeyWork );
}

static void hb_cdxSortAddToNode( LPSORTINFO pSort, USHORT Lvl, LONG Tag,
                                 LONG Link, LPKEYINFO Value )
{
   USHORT i, bitcnt;
   LONG sr;

   if( pSort->NodeList[ Lvl ] == NULL )
   {
      pSort->NodeList[ Lvl ] = ( LPCDXDATA ) hb_xgrab( sizeof( CDXDATA ) );
      memset( pSort->NodeList[ Lvl ], 0, sizeof( CDXDATA ) );
      if( Lvl == 0 )
      {
         sr = pSort->KeyTot;
         i = pSort->CurTag->uiLen;
         for( bitcnt = 0; i; bitcnt++, i >>= 1 );
         pSort->NodeList[ 0 ]->cdxu.External.ShortBytes = 3;
         pSort->NodeList[ 0 ]->cdxu.External.RecNumBits = 24 - bitcnt * 2;
         pSort->NodeList[ 0 ]->cdxu.External.RecNumMask =
            hb_cdxMakeMask( pSort->NodeList[ 0 ]->cdxu.External.RecNumBits );
         while( sr > pSort->NodeList[ 0 ]->cdxu.External.RecNumMask )
         {
            pSort->NodeList[ 0 ]->cdxu.External.ShortBytes++;
            pSort->NodeList[ 0 ]->cdxu.External.RecNumBits += 8;
            pSort->NodeList[ 0 ]->cdxu.External.RecNumMask =
               ( pSort->NodeList[ 0 ]->cdxu.External.RecNumMask << 8 ) | 0xFF;
         }
         pSort->NodeList[ 0 ]->cdxu.External.FreeSpace = CDX_EXTERNAL_SPACE;
         pSort->NodeList[ 0 ]->cdxu.External.DupCntBits =
            pSort->NodeList[ 0 ]->cdxu.External.TrlCntBits = (BYTE) bitcnt;
         pSort->NodeList[ 0 ]->cdxu.External.DupCntMask =
            (BYTE) hb_cdxMakeMask( pSort->NodeList[ 0 ]->cdxu.External.DupCntBits );
         pSort->NodeList[ 0 ]->cdxu.External.TrlCntMask =
            (BYTE) hb_cdxMakeMask( pSort->NodeList[ 0 ]->cdxu.External.TrlCntBits );
      }
      pSort->NodeList[ Lvl ]->Left_Ptr = -1;
      pSort->NodeList[ Lvl ]->Rght_Ptr = hb_cdxIndexGetAvailPage( pSort->CurTag->pIndex );
      pSort->NodeList[ Lvl ]->Node_Atr = ( Lvl == 0 ) ? 2 : 0;
   }
   if( Lvl == 0 )
      hb_cdxSortAddExternal( pSort, Lvl, Tag, Link, Value );
   else
      hb_cdxSortAddInternal( pSort, Lvl, Tag, Link, Value );
}

static void hb_cdxSortAddExternal( LPSORTINFO pSort, USHORT Lvl, LONG Tag, LONG Link,
                                   LPKEYINFO Value )
{
   USHORT k, ct, cd, v, c;
   LONG m, r, pa;

   if( pSort->NodeList[ Lvl ]->Entry_Ct == 0 )
   {
      memset( pSort->NodeList[ Lvl ]->cdxu.External.ExtData, 0,
              sizeof( pSort->NodeList[ Lvl ]->cdxu.External.ExtData ) );
      pSort->NodeList[ Lvl ]->cdxu.External.FreeSpace = CDX_EXTERNAL_SPACE;
      hb_cdxKeyPutC( pSort->LastKey, "" );
   }
   m = ~pSort->NodeList[ Lvl ]->cdxu.External.RecNumMask;
   ct = ( USHORT ) ( pSort->CurTag->uiLen - Value->length );
   hb_cdxKeyCompare( Value, pSort->LastKey, &cd, TRUE );
   if( cd > 0 )
      cd -= ( USHORT ) 1;
   v = ( USHORT ) ( pSort->NodeList[ Lvl ]->Entry_Ct *
       pSort->NodeList[ Lvl ]->cdxu.External.ShortBytes );
   k = ( USHORT ) ( pSort->NodeList[ Lvl ]->cdxu.External.FreeSpace + v );
   pSort->NodeList[ Lvl ]->cdxu.External.FreeSpace -=
      ( USHORT ) ( pSort->CurTag->uiLen +
      pSort->NodeList[ Lvl ]->cdxu.External.ShortBytes - cd - ct );
   c = ( USHORT ) ( ( ct << ( 16 - pSort->NodeList[ Lvl ]->cdxu.External.TrlCntBits ) ) |
       ( cd << ( 16 - pSort->NodeList[ Lvl ]->cdxu.External.TrlCntBits -
       pSort->NodeList[ Lvl ]->cdxu.External.DupCntBits ) ) );
   memcpy( &pSort->NodeList[ Lvl ]->cdxu.External.ExtData[ v +
      pSort->NodeList[ Lvl ]->cdxu.External.ShortBytes - 2 ], &c, 2 );
   memcpy( &r, &pSort->NodeList[ Lvl ]->cdxu.External.ExtData[ v ], 4 );
   r &= m;
   r |= Tag;
   memcpy( &pSort->NodeList[ Lvl ]->cdxu.External.ExtData[ v ], &r, 4 );
   k -= ( USHORT ) ( pSort->CurTag->uiLen - cd - ct );
   if( pSort->CurTag->uiLen - cd - ct > 0 )
      memcpy( &pSort->NodeList[ Lvl ]->cdxu.External.ExtData[ k ],
              Value->Value + cd,
              pSort->CurTag->uiLen - cd - ct );
   pSort->NodeList[ Lvl ]->Entry_Ct++;
   if( pSort->NodeList[ Lvl ]->cdxu.External.FreeSpace <
       ( pSort->CurTag->uiLen + 8 +
       pSort->NodeList[ Lvl ]->cdxu.External.ShortBytes ) * 2 )
   {
      pa = pSort->NodeList[ Lvl ]->Rght_Ptr;
      if( pSort->KeyCnt < pSort->KeyTot )
         pSort->NodeList[ Lvl ]->Rght_Ptr = hb_cdxIndexGetAvailPage( pSort->CurTag->pIndex );
      else
         pSort->NodeList[ Lvl ]->Rght_Ptr = -1;
      pSort->NodeList[ Lvl ]->Node_Atr = 2;
      hb_cdxIndexPageWrite( pSort->CurTag->pIndex, pa, pSort->NodeList[ Lvl ],
                            sizeof( CDXDATA ) );
      pSort->NodeList[ Lvl ]->Left_Ptr = pa;
      hb_cdxSortAddToNode( pSort, ( USHORT ) ( Lvl + 1 ), pa, Link, Value );
      pSort->NodeList[ Lvl ]->Entry_Ct = 0;
   }
}

static void hb_cdxSortAddInternal( LPSORTINFO pSort, USHORT Lvl, LONG Tag, LONG Link,
                                   LPKEYINFO Value )
{
   USHORT v;
   LONG r, pa;

   if( pSort->NodeList[ Lvl ]->Entry_Ct == 0 )
      memset( pSort->NodeList[ Lvl ]->cdxu.Internal.IntData,
              pSort->CurTag->uiType == 'C' ? 32 : 0,
              sizeof( pSort->NodeList[ Lvl ]->cdxu.Internal.IntData ) );
   v = ( USHORT ) ( pSort->NodeList[ Lvl ]->Entry_Ct *
       ( pSort->CurTag->uiLen + 8 ) );
   memcpy( &pSort->NodeList[ Lvl ]->cdxu.Internal.IntData[ v ],
           Value->Value, Value->length );
   v += pSort->CurTag->uiLen;
   r = hb_cdxSwapBytes( Link );
   memcpy( &pSort->NodeList[ Lvl ]->cdxu.Internal.IntData[ v ], &r, 4 );
   r = hb_cdxSwapBytes( Tag );
   memcpy( &pSort->NodeList[ Lvl ]->cdxu.Internal.IntData[ v + 4 ], &r, 4 );
   pSort->NodeList[ Lvl ]->Entry_Ct++;
   if( pSort->NodeList[ Lvl ]->Entry_Ct >= pSort->CurTag->MaxKeys )
   {
      pa = pSort->NodeList[ Lvl ]->Rght_Ptr;
      if( !pSort->Closing )
         pSort->NodeList[ Lvl ]->Rght_Ptr = hb_cdxIndexGetAvailPage( pSort->CurTag->pIndex );
      else
         pSort->NodeList[ Lvl ]->Rght_Ptr = -1;
      pSort->NodeList[ Lvl ]->Node_Atr = 0;
      hb_cdxIndexPageWrite( pSort->CurTag->pIndex, pa, pSort->NodeList[ Lvl ],
                            sizeof( CDXDATA ) );
      pSort->NodeList[ Lvl ]->Left_Ptr = pa;
      hb_cdxSortAddToNode( pSort, ( USHORT ) ( Lvl + 1 ), pa, Link, Value );
      pSort->NodeList[ Lvl ]->Entry_Ct = 0;
   }
}
/* end hb_cdxSortxxx */

static USHORT hb_cdxFindTag( CDXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   USHORT uiTag;

   if ( pOrderInfo->itmOrder )
   {
      if ( hb_itemType( pOrderInfo->itmOrder ) != HB_IT_STRING &&
                hb_itemGetNI( pOrderInfo->itmOrder ) == 0 )
         uiTag = 0;
      else
      {
         if ( hb_itemType( pOrderInfo->itmOrder ) == HB_IT_STRING )
         {
            LPCDXTAG pTag;

            pTag = pArea->lpIndexes->TagList;
            for( uiTag = 0; pTag; uiTag++ )
            {
               /* if ( !strcmp( pTag->szName, hb_itemGetCPtr( pOrderInfo->itmOrder ) ) ) */
               if ( !hb_stricmp( pTag->szName, hb_itemGetCPtr( pOrderInfo->itmOrder ) ) )
                  break;
               pTag = pTag->pNext;
            }
            if ( pTag )
               ++uiTag;

            else
               uiTag = 0;

         }
         else
         {
            uiTag = hb_itemGetNI( pOrderInfo->itmOrder );
         }
      }
   }
   else
      uiTag = 0;

   return uiTag;
}

ERRCODE hb_cdxOrderCreate( CDXAREAP pAreaCdx, LPDBORDERCREATEINFO pOrderInfo )
{
   PHB_ITEM pExpr, pKeyExp, pForExp, pResult, pError;
   HB_MACRO_PTR pExpMacro, pForMacro;
   USHORT uiType, uiLen, uiCount;
   char * szFileName, * szTagName;
   PHB_FNAME pFileName;
   DBORDERINFO pExtInfo;
   LPCDXINDEX pIndex;
   LPCDXTAG pTag, pLastTag;
   DBFHEADER pHeader;
   BYTE bType;
   BOOL bNewFile;
   AREAP pArea = (AREAP) pAreaCdx;
   /*  this is for testing when doing changes or testing a new platform/compiler
   if ( sizeof(CDXINTERNAL) != 500 )
     printf("cdxOrdCreate: Error, size of CDXINTERNAL: %i\n", sizeof(CDXINTERNAL));
   if ( sizeof(CDXDATA) != 512 )
     printf("cdxOrdCreate: Error, size of CDXDATA: %i\n", sizeof(CDXDATA));
   */
   HB_TRACE(HB_TR_DEBUG, ("cdxOrderCreate(%p, %p)", pArea, pOrderInfo));

   if( SELF_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   /* If we have a codeblock for the expression, use it */
   if( pOrderInfo->itmCobExpr )
      pExpr = pOrderInfo->itmCobExpr;
   else /* Otherwise, try compiling the key expression string */
   {
      if( SELF_COMPILE( (AREAP) pArea, ( BYTE * ) pOrderInfo->abExpr->item.asString.value ) == FAILURE )
         return FAILURE;
      pExpr = pArea->valResult;
      pArea->valResult = NULL;
   }

   /* Save for later use */
   pKeyExp = hb_itemNew( NULL );
   hb_itemCopy( pKeyExp, pExpr );

   /* Get a blank record before testing expression */
   SELF_GOBOTTOM( ( AREAP ) pArea );
   SELF_SKIP( ( AREAP ) pArea, 1 );
   pExpMacro = pForMacro = NULL;
   if( hb_itemType( pExpr ) == HB_IT_BLOCK )
   {
      if( SELF_EVALBLOCK( ( AREAP ) pArea, pExpr ) == FAILURE )
      {
         hb_itemRelease( pKeyExp );
         return FAILURE;
      }
      pResult = pArea->valResult;
      pArea->valResult = NULL;
   }
   else
   {
      pExpMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pExpr );
      hb_macroRun( pExpMacro );
      pResult = pExpr;
      hb_itemCopy( pResult, hb_stackItemFromTop( - 1 ) );
   }

   uiType = hb_itemType( pResult );
   uiLen = 0;

   switch( uiType )
   {
      case HB_IT_INTEGER:
      case HB_IT_LONG:
      case HB_IT_DOUBLE:
         bType = 'N';
         uiLen = 8;
         break;

      case HB_IT_DATE:
         bType = 'D';
         uiLen = 8;
         break;

      case HB_IT_LOGICAL:
         bType = 'L';
         uiLen = 1;
         break;

      case HB_IT_STRING:
         bType = 'C';
         uiLen = pResult->item.asString.length > CDX_MAXKEY ? (USHORT) CDX_MAXKEY :
                 (USHORT) pResult->item.asString.length;
         break;

      default:
         bType = '\0';
   }

   hb_itemRelease( pResult );

   /* Make sure uiLen is not 0 */
   if( uiLen == 0 )
   {
      hb_itemRelease( pKeyExp );
      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_DATAWIDTH );
      hb_errPutSubCode( pError, 1026 );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_DATAWIDTH ) );
      SELF_ERROR( ( AREAP ) pArea, pError );
      hb_errRelease( pError );
      if( pExpMacro != NULL )
         hb_macroDelete( pExpMacro );
      return FAILURE;
   }

   /* Check conditional expression */
   pExpr = pForExp = NULL;
   if( pArea->lpdbOrdCondInfo )
   {
      /* If we have a codeblock for the conditional expression, use it */
      if( pArea->lpdbOrdCondInfo->itmCobFor )
         pExpr = pArea->lpdbOrdCondInfo->itmCobFor;
      else /* Otherwise, try compiling the conditional expression string */
      {
        if( pArea->lpdbOrdCondInfo->abFor ) {
          if( SELF_COMPILE( (AREAP) pArea, pArea->lpdbOrdCondInfo->abFor ) == FAILURE )
          {
            hb_itemRelease( pKeyExp );
            if( pExpMacro != NULL )
               hb_macroDelete( pExpMacro );
            return FAILURE;
          }
          pExpr = pArea->valResult;
          pArea->valResult = NULL;
        }
      }
      /* Save for later use */
      if ( pExpr )
      {
        pForExp = hb_itemNew( NULL );
        hb_itemCopy( pForExp, pExpr );
      }
   }

   /* Test conditional expression */
   if( pExpr )
   {
      if( hb_itemType( pExpr ) == HB_IT_BLOCK )
      {
         if( SELF_EVALBLOCK( ( AREAP ) pArea, pExpr ) == FAILURE )
         {
            hb_itemRelease( pKeyExp );
            hb_itemRelease( pForExp );
            if( pExpMacro != NULL )
               hb_macroDelete( pExpMacro );
            return FAILURE;
         }
         pResult = pArea->valResult;
         pArea->valResult = NULL;
      }
      else
      {
         pForMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pExpr );
         hb_macroRun( pForMacro );
         pResult = pExpr;
         hb_itemCopy( pResult, hb_stackItemFromTop( - 1 ) );
      }
      uiType = hb_itemType( pResult );
      hb_itemRelease( pResult );
      if( uiType != HB_IT_LOGICAL )
      {
         hb_itemRelease( pKeyExp );
         hb_itemRelease( pForExp );
         if( pExpMacro != NULL )
            hb_macroDelete( pExpMacro );
         if( pForMacro != NULL )
            hb_macroDelete( pForMacro );
         return FAILURE;
      }
   }

   /* Check file name */
   szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 3 );
   szFileName[ 0 ] = '\0';
   if( strlen( ( char * ) pOrderInfo->abBagName ) == 0 )
   {
      pFileName = hb_fsFNameSplit( pAreaCdx->szDataFileName );
      /*
      if( pFileName->szDrive )
         strcat( szFileName, pFileName->szDrive );
      */
      if( pFileName->szPath )
         strcat( szFileName, pFileName->szPath );
      strcat( szFileName, pFileName->szName );
      pExtInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) pArea, DBOI_BAGEXT, &pExtInfo );
      strcat( szFileName, pExtInfo.itmResult->item.asString.value );
      hb_itemRelease( pExtInfo.itmResult );
   }
   else
   {
      strcpy( szFileName, ( char * ) pOrderInfo->abBagName );
      pFileName = hb_fsFNameSplit( szFileName );
      if( !pFileName->szExtension )
      {
         pExtInfo.itmResult = hb_itemPutC( NULL, "" );
         SELF_ORDINFO( ( AREAP ) pArea, DBOI_BAGEXT, &pExtInfo );
         strcat( szFileName, pExtInfo.itmResult->item.asString.value );
         hb_itemRelease( pExtInfo.itmResult );
      }
   }
   szTagName = ( char * ) hb_xgrab( CDX_MAXTAGNAMELEN + 1 );
   hb_strncpyUpper( szTagName, pFileName->szName, CDX_MAXTAGNAMELEN );
   hb_xfree( pFileName );

   /* Close all index */
   hb_cdxOrderListClear( (CDXAREAP) pArea );

   pIndex = hb_cdxIndexNew( pArea );
   pAreaCdx->lpIndexes = pIndex;

   /* New file? */
   if( !hb_fsFile( ( BYTE * ) szFileName ) )
   {
      pIndex->hFile = hb_fsCreate( ( BYTE * ) szFileName, FC_NORMAL );
      bNewFile = TRUE;
   }
   else
   {
      pIndex->hFile = hb_fsOpen( ( BYTE * ) szFileName, FO_READWRITE |
                                    ( pAreaCdx->fShared ?
                                      FO_DENYNONE : FO_EXCLUSIVE ) );
      bNewFile = FALSE;
   }

   if( pIndex->hFile == FS_ERROR )
   {
      hb_cdxOrderListClear( (CDXAREAP)  pArea );
      hb_xfree( szFileName );
      hb_xfree( szTagName );
      hb_itemRelease( pKeyExp );
      if( pForExp != NULL )
         hb_itemRelease( pForExp );
      if( pExpMacro != NULL )
         hb_macroDelete( pExpMacro );
      if( pForMacro != NULL )
         hb_macroDelete( pForMacro );
      return FAILURE;
   }

   /* Corrupted? */
   if( !bNewFile )
   {
      bNewFile = ( hb_fsSeek( pIndex->hFile, 0, FS_END ) <= CDX_PAGELEN );
      hb_fsSeek( pIndex->hFile, 0, FS_SET );
   }

   if( bNewFile )
   {
      pIndex->NextAvail = 0;
      pIndex->pCompound = hb_cdxTagNew( pIndex, szTagName, -1 );
      pIndex->pCompound->OptFlags = 0xE0;
      hb_cdxTagIndexTagNew( pIndex->pCompound, NULL, NULL, 'C', 10, NULL, NULL,
                            TRUE, FALSE );
      hb_cdxTagTagOpen( pIndex->pCompound, 0 );
   }
   else
   {
      pIndex->pCompound = hb_cdxTagNew( pIndex, szTagName, 0 );
      pIndex->pCompound->OptFlags = 0xE0;
      hb_cdxIndexResetAvailPage( pIndex );
      hb_cdxTagTagOpen( pIndex->pCompound, 0 );
      while( !pIndex->pCompound->TagEOF )
      {
         pTag = hb_cdxTagNew( pIndex,
                              pIndex->pCompound->CurKeyInfo->Value,
                              pIndex->pCompound->CurKeyInfo->Tag );
         if( pIndex->TagList == NULL )
            pIndex->TagList = pTag;
         else
         {
            pLastTag = pIndex->TagList;
            while( pLastTag->pNext )
               pLastTag = pLastTag->pNext;
            pLastTag->pNext = pTag;
         }
         hb_cdxTagKeyRead( pIndex->pCompound, NEXT_RECORD );
      }
   }

   /* Update DBF header */
   if( !pAreaCdx->fHasTags )
   {
      pFileName = hb_fsFNameSplit( pAreaCdx->szDataFileName );
      hb_strncpyUpper( szFileName, pFileName->szName, CDX_MAXTAGNAMELEN );
      hb_xfree( pFileName );
      hb_fsSeek( pAreaCdx->hDataFile, 0, FS_SET );
      if( strcmp( szFileName, szTagName ) == 0 && hb_fsRead( pAreaCdx->hDataFile,
          ( BYTE * ) &pHeader, sizeof( DBFHEADER ) ) == sizeof( DBFHEADER ) )
      {
         pHeader.bHasTags = 1;
         hb_fsSeek( pAreaCdx->hDataFile, 0, FS_SET );
         hb_fsWrite( pAreaCdx->hDataFile, ( BYTE * ) &pHeader, sizeof( DBFHEADER ) );
      }
   }

   hb_xfree( szFileName );

   hb_strncpyUpper( szTagName, ( const char * ) pOrderInfo->atomBagName, CDX_MAXTAGNAMELEN );
   uiCount = strlen( szTagName );
   while( uiCount > 0 && szTagName[ uiCount - 1 ] == ' ' )
      uiCount--;
   szTagName[ uiCount ] = 0;
   hb_cdxIndexAddTag( pIndex, szTagName, pOrderInfo->abExpr->item.asString.value,
                      pKeyExp, bType, uiLen, ( char * ) ( pArea->lpdbOrdCondInfo ? pArea->lpdbOrdCondInfo->abFor :
                      NULL ), pForExp, pArea->lpdbOrdCondInfo ?
                      !pArea->lpdbOrdCondInfo->fDescending : TRUE , pOrderInfo->fUnique );

   hb_xfree( szTagName );

   /* Clear pArea->lpdbOrdCondInfo */
   SELF_ORDSETCOND( ( AREAP ) pArea, NULL );
   return SELF_GOTOP( ( AREAP ) pArea );
}


ERRCODE hb_cdxOrderListAdd( CDXAREAP pAreaCdx, LPDBORDERINFO pOrderInfo )
{
   USHORT uiFlags;
   char * szFileName, * szFileNameDbfPath = NULL;
   AREAP pArea = (AREAP) pAreaCdx;
   LPCDXINDEX pIndex;
   DBORDERINFO pExtInfo;
   PHB_FNAME pFileName;
   PHB_ITEM pError = NULL;
   BOOL bRetry;
   LPCDXTAG pTag, pLastTag;
   LPCDXTAG TagList, pTag1, pTag2, pTagTmp;


   HB_TRACE(HB_TR_DEBUG, ("cdxOrderListAdd(%p, %p)", pArea, pOrderInfo));

   if( SELF_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   /* Close all index for now (it must be fixed!)*/
   hb_cdxOrderListClear((CDXAREAP)  pArea );

   /* File exists? */
   /* Check file name */
   szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 3 );
   szFileName[ 0 ] = '\0';
   pIndex = hb_cdxIndexNew( pArea );
   pAreaCdx->lpIndexes = pIndex;

   strcpy( szFileName, hb_itemGetCPtr( pOrderInfo->atomBagName ) );

   if( strlen( szFileName ) == 0 )
   {
      hb_cdxOrderListClear( (CDXAREAP) pArea );
      hb_xfree( szFileName );
      return FAILURE;
   }
   else
   {
      pFileName = hb_fsFNameSplit( szFileName );
      if( !pFileName->szExtension )
      {
         pExtInfo.itmResult = hb_itemPutC( NULL, "" );
         SELF_ORDINFO( ( AREAP ) pArea, DBOI_BAGEXT, &pExtInfo );
         strcat( szFileName, pExtInfo.itmResult->item.asString.value );
         hb_itemRelease( pExtInfo.itmResult );
      }
      if ( !pFileName->szPath )
      {
         hb_xfree( pFileName );
         pFileName = hb_fsFNameSplit( pAreaCdx->szDataFileName );
         if ( pFileName->szPath )
         {
            szFileNameDbfPath = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 3 );
            szFileNameDbfPath[ 0 ] = '\0';
            strcpy( szFileNameDbfPath, pFileName->szPath );
            strncat( szFileNameDbfPath, szFileName, _POSIX_PATH_MAX -
               strlen( szFileNameDbfPath ) );
         }
      //pArea->szDataFileName = (char *) hb_xgrab( strlen( (char * ) pOpenInfo->abName)+1 );
      }
      hb_xfree( pFileName );
   }
   uiFlags =  pAreaCdx->fReadonly  ? FO_READ : FO_READWRITE;
   uiFlags |= pAreaCdx->fShared ? FO_DENYNONE : FO_EXCLUSIVE;

   do
   {
      pIndex->hFile = FS_ERROR;
      if ( szFileNameDbfPath )
         pIndex->hFile = hb_fsOpen( ( BYTE * ) szFileNameDbfPath, uiFlags );
      if ( pIndex->hFile != FS_ERROR ) {
         pIndex->szFileName = ( char * ) hb_xgrab( strlen( szFileNameDbfPath ) + 1 );
         strcpy( pIndex->szFileName, szFileNameDbfPath);
      } else {
         pIndex->hFile = hb_fsOpen( ( BYTE * ) szFileName, uiFlags );
         if ( pIndex->hFile != FS_ERROR ) {
            pIndex->szFileName = ( char * ) hb_xgrab( strlen( szFileName ) + 1 );
            strcpy( pIndex->szFileName, szFileName);
         }
      }
     if( pIndex->hFile == FS_ERROR )
     {
       if( !pError )
       {
         pError = hb_errNew();
         hb_errPutGenCode( pError, EG_OPEN );
         hb_errPutSubCode( pError, 1003 );
         hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_OPEN ) );
         hb_errPutFileName( pError, szFileName );
         hb_errPutFlags( pError, EF_CANRETRY | EF_CANDEFAULT );
       }
       bRetry = ( SELF_ERROR( ( AREAP ) pArea, pError ) == E_RETRY );
     }
     else
       bRetry = FALSE;
   } while( bRetry );
   if( pError )
      hb_errRelease( pError );

   if( pIndex->hFile == FS_ERROR )
   {
      hb_cdxOrderListClear( (CDXAREAP)  pArea );
      if ( szFileNameDbfPath != NULL )
         hb_xfree( szFileNameDbfPath );
      hb_xfree( szFileName );
      return FAILURE;
   }

   /* Corrupted? */
   /*mising test !*/
   /* load the tags*/
   pIndex->pCompound = hb_cdxTagNew( pIndex, szFileName, 0 );
   pIndex->pCompound->OptFlags = 0xE0;
   hb_cdxIndexResetAvailPage( pIndex );
   hb_cdxTagTagOpen( pIndex->pCompound, 0 );
   TagList = NULL;
   while( !pIndex->pCompound->TagEOF )
   {
      pTag = hb_cdxTagNew( pIndex,
              pIndex->pCompound->CurKeyInfo->Value,
              pIndex->pCompound->CurKeyInfo->Tag );
      if( TagList == NULL )
        TagList = pTag;
      else
      {
        pLastTag = TagList;
        while( pLastTag->pNext )
          pLastTag = pLastTag->pNext;
        pLastTag->pNext = pTag;
      }
      hb_cdxTagKeyRead( pIndex->pCompound, NEXT_RECORD );
   }
   /* Reorder the Tag list to be compatible with Clipper */
   pTag1 = TagList;
   while (pTag1->pNext)
   {
      if (pTag1->TagBlock < pTag1->pNext->TagBlock)
         pTag1 = pTag1->pNext;
      else
      {
         if ( TagList->TagBlock > pTag1->pNext->TagBlock)
         {
            pTagTmp = TagList;
            TagList = pTag1->pNext;
            pTag1->pNext = pTag1->pNext->pNext;
            TagList->pNext = pTagTmp;
         }
         else
         {
            pTag2 = TagList;
            while (pTag2->pNext && (pTag2->pNext->TagBlock < pTag1->pNext->TagBlock) )
               pTag2 = pTag2->pNext;
            pTagTmp = pTag2->pNext;
            pTag2->pNext = pTag1->pNext;
            pTag1->pNext = pTag1->pNext->pNext;
            pTag2->pNext->pNext = pTagTmp;
         }
      }
   }
   if( pIndex->TagList == NULL )
      pIndex->TagList = TagList;
   else
   {
      pLastTag = pIndex->TagList;
      while( pLastTag->pNext )
         pLastTag = pLastTag->pNext;
      pLastTag->pNext = TagList;
   }

   /*missing: ordSetFocus(1) if there was no other open index in the area*/
   pIndex->uiTag = 1;
   SELF_GOTOP( ( AREAP ) pArea );

   if ( szFileNameDbfPath != NULL )
      hb_xfree( szFileNameDbfPath );
   hb_xfree( szFileName );
   return SUCCESS;
}

extern ERRCODE hb_cdxOrderListClear( CDXAREAP pArea )
{
   LPCDXINDEX pIndex;

   HB_TRACE(HB_TR_DEBUG, ("cdxOrderListClear(%p)", pArea));
   /* Commit changes first */
   if( SELF_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   while( pArea->lpIndexes )
   {
      pIndex = pArea->lpIndexes;
      pArea->lpIndexes = pArea->lpIndexes->pNext;
      hb_cdxIndexFree( pIndex );
   }
   pArea->lpIndexes = NULL;
   return SUCCESS;
}

ERRCODE hb_cdxOrderListFocus( CDXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   LPCDXTAG pTag = hb_cdxGetActiveTag( pArea->lpIndexes );

   HB_TRACE(HB_TR_DEBUG, ("cdxOrderListFocus(%p, %p)", pArea, pOrderInfo));
   /*
   HB_SYMBOL_UNUSED( pArea );
   HB_SYMBOL_UNUSED( pOrderInfo );
   */
   if ( ! pArea->lpIndexes )
      return SUCCESS;

   if ( pTag )
      pOrderInfo->itmResult = hb_itemPutC( pOrderInfo->itmResult, pTag->szName );

   if ( pOrderInfo->itmOrder )
   {
      pArea->lpIndexes->uiTag = hb_cdxFindTag( pArea, pOrderInfo );

      if( pArea->lpIndexes->uiTag )
          SELF_GOTO( ( AREAP ) pArea, pArea->ulRecNo );
   }

   return SUCCESS;
}

ERRCODE hb_cdxGoTop( CDXAREAP pArea )
{
   /*must change to follow ordSetFocus()*/
   LPCDXTAG pTag = hb_cdxGetActiveTag( pArea->lpIndexes );

   HB_TRACE(HB_TR_DEBUG, ("cdxGoTop(%p)", pArea));

   if ( ! pTag )
      SUPER_GOTOP( ( AREAP ) pArea );
   else
   {
      hb_cdxTagTagOpen( pTag, 0 );
      hb_cdxTagKeyRead( pTag, TOP_RECORD );
      SELF_GOTO( ( AREAP ) pArea, pTag->CurKeyInfo->Tag );
   }
   return SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
}

ERRCODE hb_cdxGoBottom( CDXAREAP pArea )
{
   /*must change to follow ordSetFocus()*/
   LPCDXTAG pTag = hb_cdxGetActiveTag( pArea->lpIndexes );

   HB_TRACE(HB_TR_DEBUG, ("cdxGoBottom(%p)", pArea));

   if ( ! pTag )
     SUPER_GOBOTTOM( ( AREAP ) pArea );
   else
   {
     hb_cdxTagTagOpen( pTag, 0 );
     hb_cdxTagKeyRead( pTag, BTTM_RECORD );
     SELF_GOTO( ( AREAP ) pArea, pTag->CurKeyInfo->Tag );
   }
   return SELF_SKIPFILTER( ( AREAP ) pArea, -1 );
}

ERRCODE hb_cdxSkipRaw( CDXAREAP pArea, LONG lToSkip )
{
   /*must change to follow ordSetFocus()*/
   LPCDXTAG pTag = hb_cdxGetActiveTag( pArea->lpIndexes );

   HB_TRACE(HB_TR_DEBUG, ("cdxSkipRaw(%p, %ld)", pArea, lToSkip));

   if ( ! pTag )
     SUPER_SKIPRAW( ( AREAP ) pArea, lToSkip );
   else
   {

     if ( pArea->fBof )
        SELF_GOTOP( ( AREAP ) pArea );

     if ( lToSkip == 0 )
     {
       SUPER_SKIPRAW( ( AREAP ) pArea, 0 );
     }

     else if ( lToSkip > 0 )
     {
       if ( !pArea->fEof )
       {
          while ( !pTag->TagEOF && lToSkip-- > 0 )
            hb_cdxTagKeyRead( pTag, NEXT_RECORD );
          if ( !pTag->TagEOF )
            SELF_GOTO( ( AREAP ) pArea, pTag->CurKeyInfo->Tag );
          else
          {
            /*BOOL fTop;        */            /* TRUE if "top" */
            /*BOOL fBottom;     */            /* TRUE if "bottom" */
            SUPER_GOBOTTOM( ( AREAP ) pArea );
            SUPER_SKIPRAW( ( AREAP ) pArea, 1 );
            pArea->fEof = pTag->TagEOF = TRUE;
          }
       }
     }
     else /* ( lToSkip < 0 ) */
     {
       if ( pArea->fEof )
       {
         SELF_GOBOTTOM( ( AREAP ) pArea );
         lToSkip++;
       }
       pTag->TagBOF = FALSE;
       while ( !pTag->TagBOF && lToSkip++ < 0 )
         hb_cdxTagKeyRead( pTag, PREV_RECORD );
       if ( !pTag->TagBOF )
         SELF_GOTO( ( AREAP ) pArea, pTag->CurKeyInfo->Tag );
       else
       {
         /*BOOL fTop;       */             /* TRUE if "top" */
         /*BOOL fBottom;    */             /* TRUE if "bottom" */
         pTag->TagBOF = FALSE;
         SELF_GOTOP( ( AREAP ) pArea );
         pArea->fBof = pTag->TagBOF = TRUE;
       }
     }
   }
   /* return SELF_SKIPFILTER( ( AREAP ) pArea, -1 ); */
   return SUCCESS;
}

ERRCODE hb_cdxSeek( CDXAREAP pArea, BOOL bSoftSeek, PHB_ITEM pKey, BOOL bFindLast )
{
   PHB_ITEM pError;
   ERRCODE retvalue;
   LPCDXTAG pTag = hb_cdxGetActiveTag( pArea->lpIndexes );

   HB_TRACE(HB_TR_DEBUG, ("cdxSeek(%p, %d, %p, %d)", pArea, bSoftSeek, pKey, bFindLast));
   /*HB_SYMBOL_UNUSED( pArea );       */
   /*HB_SYMBOL_UNUSED( bSoftSeek );   */
   /*HB_SYMBOL_UNUSED( pKey );        */
   /*HB_SYMBOL_UNUSED( bFindLast );   */

   if ( ! pTag )
   {
     pError = hb_errNew();
     hb_errPutGenCode( pError, EG_NOORDER );
     hb_errPutSubCode( pError, 1020 );
     hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_NOORDER ) );
     hb_errPutFlags( pError, EF_CANDEFAULT );
     SELF_ERROR( ( AREAP ) pArea, pError );
     hb_errRelease( pError );
     return FAILURE;
   }
   else
   {
     LONG lRecno;
     LPKEYINFO pKey2;

     pKey2 = hb_cdxKeyNew();
     hb_cdxKeyPutItem( pKey2, pKey );
     if ( bFindLast )
       pKey2->Tag = CDX_MAX_REC_NUM;
     else
       pKey2->Tag = CDX_IGNORE_REC_NUM;
     pKey2->Xtra = 0;

     lRecno = hb_cdxTagKeyFind( pTag, pKey2 );
     pArea->fEof = pTag->TagEOF;
     pArea->fBof = pTag->TagBOF;
     hb_cdxKeyFree( pKey2 );
     if ( lRecno > 0 )
     {
       retvalue = SELF_GOTO( ( AREAP ) pArea, pTag->CurKeyInfo->Tag );
       pArea->fFound = TRUE;
       return retvalue;
     }
     else
     {
       pArea->fFound = FALSE;
       if ( bSoftSeek && !pTag->TagEOF )
       {
         return SELF_GOTO( ( AREAP ) pArea, pTag->CurKeyInfo->Tag );
       }
       else
       {
         SELF_GOBOTTOM( ( AREAP ) pArea );
         return SELF_SKIPRAW( ( AREAP ) pArea, 1 );
         /*pArea->fEof = pTag->TagEOF = TRUE; */
       }
     }
   }
   /*return SUCCESS; */
}

/*
 * Position cursor at a specific physical record.
 */
ERRCODE hb_cdxGoTo( CDXAREAP pArea, ULONG ulRecNo )
{
   LPKEYINFO pKey;
   HB_MACRO_PTR pMacro;
   LONG lRecno;
   LPCDXTAG pTag;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxGoTo(%p, %lu)", pArea, ulRecNo));

   if( SUPER_GOTO( ( AREAP ) pArea, ulRecNo ) == FAILURE )
      return FAILURE;

   if ( pArea->fEof )
      /* return FAILURE; */
      return SUCCESS;

   pTag = hb_cdxGetActiveTag( pArea->lpIndexes );
   if ( ! pTag )
      return SUCCESS;

   if( !pTag->CurKeyInfo || (ULONG) pTag->CurKeyInfo->Tag != ulRecNo )
   {

      pKey = hb_cdxKeyNew();

      if( hb_itemType( pTag->pKeyItem ) == HB_IT_BLOCK )
      {
          hb_vmPushSymbol( &hb_symEval );
          hb_vmPush( pTag->pKeyItem );
          hb_vmDo( 0 );
          hb_cdxKeyPutItem( pKey, &hb_stack.Return );
      }
      else
      {
          pMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pTag->pKeyItem );
          hb_macroRun( pMacro );
          hb_cdxKeyPutItem( pKey, hb_stackItemFromTop( - 1 ) );
          hb_stackPop();
      }

      lRecno = hb_cdxTagKeyFind( pTag, pKey );
      pArea->fEof = pTag->TagEOF;
      pArea->fBof = pTag->TagBOF;

      if ( lRecno > 0 )
      {
         if ( (ULONG) lRecno == pArea->ulRecNo )
            return SUCCESS;
         else
         {
            /* if ( !pTag->UniqueKey && !pTag->TagEOF ) */
            if ( !pTag->TagEOF )
            {
               USHORT  dav;
               while(!pTag->TagEOF && pTag->CurKeyInfo->Tag != lRecno &&
                     !hb_cdxKeyCompare( pKey, pTag->CurKeyInfo, &dav, TRUE ) )
                  hb_cdxTagKeyRead( pTag, NEXT_RECORD );

               pArea->fEof = pTag->TagEOF;
               pArea->fBof = pTag->TagBOF;
            }
         }
      }
      else
      {
          /* Need fixed !!!!!! */
          /* Error Index */
      }
      hb_cdxKeyFree( pKey );
   }

   return SUCCESS;      /* ???? m.b FAILURE */
}

/*
 * Position the cursor to a specific, physical identity.
 */
/*
ERRCODE hb_cdxGoToId( CDXAREAP pArea, PHB_ITEM pItem )
{
   PHB_ITEM pError;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxGoToId(%p, %p)", pArea, pItem));

   if( HB_IS_NUMERIC( pItem ) )
      return SELF_GOTO( ( AREAP ) pArea, hb_itemGetNL( pItem ) );
   else
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_DATATYPE );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_DATATYPE ) );
      hb_errPutSubCode( pError, EDBF_DATATYPE );
      SELF_ERROR( ( AREAP ) pArea, pError );
      hb_errRelease( pError );
      return FAILURE;
   }
}
*/
/*
 * Mark the WorkArea data buffer as hot.
 */
ERRCODE hb_cdxGoHot( CDXAREAP pArea )
{
   LPCDXTAG     pTag;
   /* USHORT       uiTag; */
   LPKEYINFO    pKey;
   HB_MACRO_PTR pMacro;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxGoHot(%p)", pArea));

   if( SUPER_GOHOT( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   if( pArea->lpIndexes && pArea->lpIndexes->TagList )
   {
      pTag = pArea->lpIndexes->TagList;
      /* uiTag = 1; */
      while( pTag )
      {
            pKey = hb_cdxKeyNew();
            if( hb_itemType( pTag->pKeyItem ) == HB_IT_BLOCK )
            {
               hb_vmPushSymbol( &hb_symEval );
               hb_vmPush( pTag->pKeyItem );
               hb_vmDo( 0 );
               hb_cdxKeyPutItem( pKey, &hb_stack.Return );
            }
            else
            {
               pMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pTag->pKeyItem );
               hb_macroRun( pMacro );
               hb_cdxKeyPutItem( pKey, hb_stackItemFromTop( - 1 ) );
               hb_stackPop();
            }
            pKey->Tag = pArea->ulRecNo;

            pTag->HotKey = pKey;
            pTag = pTag->pNext;
            /* ++uiTag; */
      }
   }

   return SUCCESS;
}
/*
 * Perform a write of WorkArea memory to the data store.
 */
ERRCODE hb_cdxGoCold( CDXAREAP pArea )
{
   LPCDXTAG     pTag;
   LPKEYINFO    pKey;
   HB_MACRO_PTR pMacro;
   USHORT       dav;
   USHORT       uiTag;
   BOOL         bForOk;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxGoCold(%p)", pArea));

   if( pArea->lpIndexes && pArea->lpIndexes->TagList )
   {
     pTag = pArea->lpIndexes->TagList;

     if( pArea->fRecordChanged )
     {
        uiTag = 1;
        while( pTag )
        {
          /* test for expresion, working but not tested */
          if( pTag->pForItem != NULL ) {
            if( hb_itemType( pTag->pForItem ) == HB_IT_BLOCK )
            {
               hb_vmPushSymbol( &hb_symEval );
               hb_vmPush( pTag->pForItem );
               hb_vmDo( 0 );
               bForOk = hb_itemGetL( &hb_stack.Return );
            }
            else
            {
               pMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pTag->pForItem );
               hb_macroRun( pMacro );
               bForOk = hb_itemGetL( hb_stackItemFromTop( - 1 ) );
               hb_stackPop();
            }
          } else
            bForOk = TRUE;
          pKey = hb_cdxKeyNew();
          if( hb_itemType( pTag->pKeyItem ) == HB_IT_BLOCK )
          {
              hb_vmPushSymbol( &hb_symEval );
              hb_vmPush( pTag->pKeyItem );
              hb_vmDo( 0 );
              hb_cdxKeyPutItem( pKey, &hb_stack.Return );
          }
          else
          {
              pMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pTag->pKeyItem );
              hb_macroRun( pMacro );
              hb_cdxKeyPutItem( pKey, hb_stackItemFromTop( - 1 ) );
              hb_stackPop();
          }
          pKey->Tag = pArea->ulRecNo;

          if( pArea->fAppend )
          {
             if ( bForOk )
             {
                hb_cdxTagKeyAdd( pTag, pKey );
                pTag->RootPage->Changed = TRUE;
                if( uiTag == pArea->lpIndexes->uiTag)
                   hb_cdxTagTagStore( pTag );
                else
                   hb_cdxTagTagClose( pTag );
             }
          }
          else
          {
             if( hb_cdxKeyCompare( pKey, pTag->HotKey, &dav, TRUE ) )
             {
                if( uiTag == pArea->lpIndexes->uiTag ||
                           hb_cdxTagKeyFind( pTag, pTag->HotKey ) > 0 )
                   hb_cdxPageDeleteKey( pTag->RootPage );
                if ( bForOk )
                  hb_cdxTagKeyAdd( pTag, pKey );
                pTag->RootPage->Changed = TRUE;

                if( uiTag == pArea->lpIndexes->uiTag)
                   hb_cdxTagTagStore( pTag );
                else
                   hb_cdxTagTagClose( pTag );
             }
          }

          hb_cdxKeyFree( pKey );
          if( pTag->HotKey )
          {
             hb_cdxKeyFree( pTag->HotKey );
             pTag->HotKey = NULL;
          }
          pTag = pTag->pNext;
          ++uiTag;

        }
     }
   }

   if( SUPER_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;
   return SUCCESS;
}