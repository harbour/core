/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * DBFFPT RDD
 *
 * Copyright 2003 Przemyslaw Czerpak <druzus@acn.waw.pl>
 * www - http://www.xharbour.org
 *
 * The SIX memo conversion algorithms and some piece of code taken from
 * DBFCDX and DBFFPT
 *    Copyright 1999-2002 Bruno Cantero <bruno@issnet.net>
 *    Copyright 2000-2003 Horacio Roldan <harbour_ar@yahoo.com.ar> (portions)
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
#include "hbrddfpt.h"

#ifndef HB_CDP_SUPPORT_OFF
#  include "hbapicdp.h"
   extern PHB_CODEPAGE hb_cdp_page;
#endif

#define __PRG_SOURCE__ __FILE__
#ifndef __XHARBOUR__
#  define HB_VM_STACK hb_stack
#endif
#ifdef HB_PCODE_VER
#  undef HB_PRG_PCODE_VER
#  define HB_PRG_PCODE_VER HB_PCODE_VER
#endif

static RDDFUNCS fptSuper;
static RDDFUNCS fptTable =
{

   /* Movement and positioning methods */

   ( DBENTRYP_BP )    hb_fptBof,
   ( DBENTRYP_BP )    hb_fptEof,
   ( DBENTRYP_BP )    hb_fptFound,
   ( DBENTRYP_V )     hb_fptGoBottom,
   ( DBENTRYP_UL )    hb_fptGoTo,
   ( DBENTRYP_I )     hb_fptGoToId,
   ( DBENTRYP_V )     hb_fptGoTop,
   ( DBENTRYP_BIB )   hb_fptSeek,
   ( DBENTRYP_L )     hb_fptSkip,
   ( DBENTRYP_L )     hb_fptSkipFilter,
   ( DBENTRYP_L )     hb_fptSkipRaw,


   /* Data management */

   ( DBENTRYP_VF )    hb_fptAddField,
   ( DBENTRYP_B )     hb_fptAppend,
   ( DBENTRYP_I )     hb_fptCreateFields,
   ( DBENTRYP_V )     hb_fptDeleteRec,
   ( DBENTRYP_BP )    hb_fptDeleted,
   ( DBENTRYP_SP )    hb_fptFieldCount,
   ( DBENTRYP_VF )    hb_fptFieldDisplay,
   ( DBENTRYP_SSI )   hb_fptFieldInfo,
   ( DBENTRYP_SVP )   hb_fptFieldName,
   ( DBENTRYP_V )     hb_fptFlush,
   ( DBENTRYP_PP )    hb_fptGetRec,
   ( DBENTRYP_SI )    hb_fptGetValue,
   ( DBENTRYP_SVL )   hb_fptGetVarLen,
   ( DBENTRYP_V )     hb_fptGoCold,
   ( DBENTRYP_V )     hb_fptGoHot,
   ( DBENTRYP_P )     hb_fptPutRec,
   ( DBENTRYP_SI )    hb_fptPutValue,
   ( DBENTRYP_V )     hb_fptRecall,
   ( DBENTRYP_ULP )   hb_fptRecCount,
   ( DBENTRYP_ISI )   hb_fptRecInfo,
   ( DBENTRYP_I )     hb_fptRecNo,
   ( DBENTRYP_S )     hb_fptSetFieldExtent,


   /* WorkArea/Database management */

   ( DBENTRYP_P )     hb_fptAlias,
   ( DBENTRYP_V )     hb_fptClose,
   ( DBENTRYP_VP )    hb_fptCreate,
   ( DBENTRYP_SI )    hb_fptInfo,
   ( DBENTRYP_V )     hb_fptNewArea,
   ( DBENTRYP_VP )    hb_fptOpen,
   ( DBENTRYP_V )     hb_fptRelease,
   ( DBENTRYP_SP )    hb_fptStructSize,
   ( DBENTRYP_P )     hb_fptSysName,
   ( DBENTRYP_VEI )   hb_fptEval,
   ( DBENTRYP_V )     hb_fptPack,
   ( DBENTRYP_LSP )   hb_fptPackRec,
   ( DBENTRYP_VS )    hb_fptSort,
   ( DBENTRYP_VT )    hb_fptTrans,
   ( DBENTRYP_VT )    hb_fptTransRec,
   ( DBENTRYP_V )     hb_fptZap,


   /* Relational Methods */

   ( DBENTRYP_VR )    hb_fptChildEnd,
   ( DBENTRYP_VR )    hb_fptChildStart,
   ( DBENTRYP_VR )    hb_fptChildSync,
   ( DBENTRYP_V )     hb_fptSyncChildren,
   ( DBENTRYP_V )     hb_fptClearRel,
   ( DBENTRYP_V )     hb_fptForceRel,
   ( DBENTRYP_SVP )   hb_fptRelArea,
   ( DBENTRYP_VR )    hb_fptRelEval,
   ( DBENTRYP_SVP )   hb_fptRelText,
   ( DBENTRYP_VR )    hb_fptSetRel,


   /* Order Management */

   ( DBENTRYP_OI )    hb_fptOrderListAdd,
   ( DBENTRYP_V )     hb_fptOrderListClear,
   ( DBENTRYP_VP )    hb_fptOrderListDelete,
   ( DBENTRYP_OI )    hb_fptOrderListFocus,
   ( DBENTRYP_V )     hb_fptOrderListRebuild,
   ( DBENTRYP_VOI )   hb_fptOrderCondition,
   ( DBENTRYP_VOC )   hb_fptOrderCreate,
   ( DBENTRYP_OI )    hb_fptOrderDestroy,
   ( DBENTRYP_OII )   hb_fptOrderInfo,


   /* Filters and Scope Settings */

   ( DBENTRYP_V )     hb_fptClearFilter,
   ( DBENTRYP_V )     hb_fptClearLocate,
   ( DBENTRYP_V )     hb_fptClearScope,
   ( DBENTRYP_VPLP )  hb_fptCountScope,
   ( DBENTRYP_I )     hb_fptFilterText,
   ( DBENTRYP_SI )    hb_fptScopeInfo,
   ( DBENTRYP_VFI )   hb_fptSetFilter,
   ( DBENTRYP_VLO )   hb_fptSetLocate,
   ( DBENTRYP_VOS )   hb_fptSetScope,
   ( DBENTRYP_VPL )   hb_fptSkipScope,


   /* Miscellaneous */

   ( DBENTRYP_P )     hb_fptCompile,
   ( DBENTRYP_I )     hb_fptError,
   ( DBENTRYP_I )     hb_fptEvalBlock,


   /* Network operations */

   ( DBENTRYP_VSP )   hb_fptRawLock,
   ( DBENTRYP_VL )    hb_fptLock,
   ( DBENTRYP_UL )    hb_fptUnLock,


   /* Memofile functions */

   ( DBENTRYP_V )     hb_fptCloseMemFile,
   ( DBENTRYP_VP )    hb_fptCreateMemFile,
   ( DBENTRYP_SVPB )  hb_fptGetValueFile,
   ( DBENTRYP_VP )    hb_fptOpenMemFile,
   ( DBENTRYP_SVP )   hb_fptPutValueFile,


   /* Database file header handling */

   ( DBENTRYP_V )     hb_fptReadDBHeader,
   ( DBENTRYP_V )     hb_fptWriteDBHeader,


   /* non WorkArea functions       */
   ( DBENTRYP_I0 )    hb_fptExit,
   ( DBENTRYP_I1 )    hb_fptDrop,
   ( DBENTRYP_I2 )    hb_fptExists,

   /* Special and reserved methods */

   ( DBENTRYP_SVP )   hb_fptWhoCares
};


HB_FUNC( _DBFFPT ) {;}

HB_FUNC( DBFFPT_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;

   uiCount = ( USHORT * ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   pTable = ( RDDFUNCS * ) hb_itemGetPtr( hb_param( 2, HB_IT_POINTER ) );

   HB_TRACE(HB_TR_DEBUG, ("DBFFPT_GETFUNCTABLE(%i, %p)", uiCount, pTable));

   if( pTable )
   {
      if ( uiCount )
         * uiCount = RDDFUNCSCOUNT;
      hb_retni( hb_rddInherit( pTable, &fptTable, &fptSuper, ( BYTE * ) "DBF" ) );
   }
   else
      hb_retni( FAILURE );
}


HB_INIT_SYMBOLS_BEGIN( dbffpt1__InitSymbols )
{ "_DBFFPT",             HB_FS_PUBLIC, {HB_FUNCNAME( _DBFFPT )}, NULL },
{ "DBFFPT_GETFUNCTABLE", HB_FS_PUBLIC, {HB_FUNCNAME( DBFFPT_GETFUNCTABLE )}, NULL }
HB_INIT_SYMBOLS_END( dbffpt1__InitSymbols )

#if defined(HB_STATIC_STARTUP)
#  pragma startup dbffpt1__InitSymbols
#elif defined(_MSC_VER)
#  if _MSC_VER >= 1010
#     pragma data_seg( ".CRT$XIY" )
#     pragma comment( linker, "/Merge:.CRT=.data" )
#  else
#     pragma data_seg( "XIY" )
#  endif
   static HB_$INITSYM hb_vm_auto_dbffpt1__InitSymbols = dbffpt1__InitSymbols;
#  pragma data_seg()
#elif ! defined(__GNUC__)
#  pragma startup dbffpt1__InitSymbols
#endif


/*
 * Exclusive lock memo file.
 */
static BOOL hb_fptFileLockEx( FPTAREAP pArea )
{
   BOOL fRet;

	 do
	 {
			fRet = hb_fsLock( pArea->hMemoFile, FPT_LOCKPOS, FPT_LOCKSIZE,
                        FL_LOCK | FLX_EXCLUSIVE | FLX_WAIT );
	 } while ( !fRet );

	 return fRet;
}

/*
 * Shared lock memo file.
 */
static BOOL hb_fptFileLockSh( FPTAREAP pArea )
{
   BOOL fRet;

	 do
	 {
			fRet = hb_fsLock( pArea->hMemoFile, FPT_LOCKPOS, FPT_LOCKSIZE,
                        FL_LOCK | FLX_SHARED | FLX_WAIT );
	 } while ( !fRet );

   return fRet;
}

/*
 * Unlock memo file.
 */
static BOOL hb_fptFileUnLock( FPTAREAP pArea )
{
   return hb_fsLock( pArea->hMemoFile, FPT_LOCKPOS, FPT_LOCKSIZE, FL_UNLOCK );
}

/*
   GARBAGE COLLECTOR:
   I don't have any documentation about it. All I know is reverse engineering
   or analyzes of other sources. If any one can tell me sth more about it then
   I will be really glad. I use method one for SixMemo and method 2 for FLEX
   memos.

  Method 1.
   FPTHEADER->reserved2[492]     is a list of free pages,
                                 6 bytes for each page
                                    size[2]  (size in blocks) (little endian)
                                    block[4] (block number) (little endian)
                                 signature1[12] has to be cutted down to
                                 10 bytes. The last 2 bytes becomes the
                                 number of entries in freeblock list (max 82)

 Method 2.
   FPTHEADER->flexDir[4]         is a little endian offset to page
                                 (1024 bytes size) where header is:
                                    type[4] = 1000 (big endian)
                                    size[4] = 1010 (big endian)
                                 then
                                    nItem[2] numeber of item (little endian)
                                 then 1008 bytes with free blocks list
                                 (max 126 entries) in format:
                                    offset[4]   (litle endian)
                                    size[4]     (litle endian)
                                 nItem is allways odd and after read we have
                                 to recalculate it:
                                    nItem = ( nItem - 3 ) / 4
		if FPTHEADER->flexDir = 0 then we can create it by allocating
            two 1024 bytes pages ofr flexRev and flexDir page.
               FPTHEADER->flexRev[4] 1024 bytes in next free block
               FPTHEADER->flexDir[4] next 1024 bytes
            flexRev page is copy of flexDir page but the items are stored
            in reversed form size[4] first then offset[4]
               size[4]     (litle endian)
               offset[4]   (litle endian)
            before writting GC pages (dir and rev, both has to be synced)
            we should first sort the entries moving the shortest blocks
            to the begining so when we where looking for free block we
            can scan the list from the begining finding the first one
            large enough. unused bytes in GC pgae should be fill with 0xAD
            when we free fpt block we should set in its header:
               type[4] = 1001 (big endian)
               size[4] = rest of block size (block size - 8) (big endian)

   TODO: Clipper 5.3 can use more then one GC page. I don't have any
   documentation for that and don't have time for farther hacking
   binary files to find the algorithm. If you have any documentation
   about it, please send it to me.
 */

/*
 * Sort GC free memo block list by size.
 */
static void hb_fptSortGCitems( LPMEMOGCTABLE pGCtable )
{
   ULONG ulOffset, ulSize;
   BOOL fMoved = TRUE;
   int i, j, l;

   /* this table should be allready quite good sorted so this simple
      algorithms will be the most efficient one.
      It will need only one or two passes */
   l = pGCtable->usItems - 1;
   while ( fMoved )
   {
      fMoved = FALSE;
      j = l;
      for( i = 0; i < j; i++ )
      {
         if ( pGCtable->pGCitems[i].ulSize > pGCtable->pGCitems[i+1].ulSize )
         {
            ulOffset = pGCtable->pGCitems[i+1].ulOffset;
            ulSize = pGCtable->pGCitems[i+1].ulSize;
            pGCtable->pGCitems[i+1].ulSize   = pGCtable->pGCitems[i].ulSize;
            pGCtable->pGCitems[i+1].ulOffset = pGCtable->pGCitems[i].ulOffset;
            pGCtable->pGCitems[ i ].ulSize   = ulSize;
            pGCtable->pGCitems[ i ].ulOffset = ulOffset;
            fMoved = TRUE;
            pGCtable->bChanged |= 2;
            l = i;
         }
      }
   }
}

/*
 * Pack GC free memo block list - try to join free blocks.
 */
static void hb_fptPackGCitems( LPMEMOGCTABLE pGCtable )
{
   ULONG ulEnd;
   int i, j;

   /* TODO: better alogrithm this primitve one can be too slow for big
      free block list table */
   for( i = 0; i < pGCtable->usItems; i++ )
   {
      if ( pGCtable->pGCitems[i].ulOffset != 0 &&
           pGCtable->pGCitems[i].ulSize != 0 )
      {
         ulEnd = pGCtable->pGCitems[i].ulOffset + pGCtable->pGCitems[i].ulSize;
         if ( ulEnd == pGCtable->ulNextBlock )
         {
            pGCtable->ulNextBlock -= pGCtable->pGCitems[i].ulSize;
            pGCtable->pGCitems[i].ulOffset = pGCtable->pGCitems[i].ulSize = 0;
            pGCtable->bChanged |= 2;
            i = -1;
         }
         else
         {
            for( j = i + 1; j < pGCtable->usItems; j++ )
            {
               if ( ulEnd == pGCtable->pGCitems[j].ulOffset )
               {
                  pGCtable->pGCitems[i].ulSize += pGCtable->pGCitems[j].ulSize;
                  pGCtable->pGCitems[j].ulOffset = pGCtable->pGCitems[j].ulSize = 0;
                  pGCtable->bChanged |= 2;
                  i = -1;
                  break;
               }
            }
         }
      }
   }

   /* remove empty items */
   for( i = j = 0; i < pGCtable->usItems; i++ )
   {
      if ( pGCtable->pGCitems[i].ulOffset != 0 &&
           pGCtable->pGCitems[i].ulSize != 0 )
      {
         if ( i > j )
         {
            pGCtable->pGCitems[j].ulOffset = pGCtable->pGCitems[i].ulOffset;
            pGCtable->pGCitems[j].ulSize   = pGCtable->pGCitems[i].ulSize;
         }
         j++;
      }
   }
   pGCtable->usItems = j;
}

/*
 * Write proper header into modified GC free memo blocks.
 */
static ERRCODE hb_fptWriteGCitems( FPTAREAP pArea, LPMEMOGCTABLE pGCtable, USHORT usItem )
{
   FPTBLOCK fptBlock;
   ERRCODE errCode = SUCCESS;
   int i /* ,iStart, iStop */ ;

   HB_SYMBOL_UNUSED( usItem ) ;

/*
   if ( usItem == 0 )
   {
      iStart = 0;
      iStop = pGCtable->usItems;
   }
   else
   {
      iStart = usItem;
      iStop = usItem + 1;
   }
*/

   for( i = 0; i < pGCtable->usItems; i++ )
   {
      if ( pGCtable->pGCitems[i].fChanged )
      {
         HB_PUT_BE_ULONG( fptBlock.type, FPTIT_FLEX_UNUSED );
         HB_PUT_BE_ULONG( fptBlock.size, pArea->uiMemoBlockSize *
                          pGCtable->pGCitems[i].ulSize - sizeof( FPTBLOCK ) );
         hb_fsSeek( pArea->hMemoFile, pGCtable->pGCitems[i].ulOffset *
                                      pArea->uiMemoBlockSize, FS_SET );
         if ( hb_fsWrite( pArea->hMemoFile, ( BYTE * ) &fptBlock,
                          sizeof( FPTBLOCK ) ) != sizeof( FPTBLOCK ) )
         {
            errCode = EDBF_WRITE;
         }
         pGCtable->pGCitems[i].fChanged = FALSE;
      }
   }
   return errCode;
}

/*
 * Add new block to GC free memo blocks list.
 */
static ERRCODE hb_fptGCfreeBlock( FPTAREAP pArea, LPMEMOGCTABLE pGCtable,
                                  ULONG ulOffset, ULONG ulByteSize )
{
   ERRCODE errCode = SUCCESS;
   ULONG ulSize;

   if ( ulByteSize == 0 )
   {
      FPTBLOCK fptBlock;

      hb_fsSeek( pArea->hMemoFile, ulOffset * pArea->uiMemoBlockSize, FS_SET );
      if( hb_fsRead( pArea->hMemoFile, ( BYTE * ) &fptBlock,
                                 sizeof( FPTBLOCK ) ) == sizeof( FPTBLOCK ) )
      {
         ulByteSize = HB_GET_BE_ULONG( fptBlock.size ) + sizeof( FPTBLOCK );
      }
   }
   else
   {
      ulByteSize += sizeof( FPTBLOCK );
   }

   ulSize = ( ulByteSize + pArea->uiMemoBlockSize - 1 ) / pArea->uiMemoBlockSize;

   if ( ulByteSize == 0 )
   {
      return EDBF_CORRUPT;
   }

   if ( ulOffset + ulSize == pGCtable->ulNextBlock )
   {
      pGCtable->ulNextBlock -= ulSize;
      pGCtable->bChanged |= 1;
      hb_fptPackGCitems( pGCtable );
   }
   else
   {
      BOOL fChanged = FALSE;
      int i;

      for( i = 0; i < pGCtable->usItems; i++ )
      {
         if ( pGCtable->pGCitems[i].ulOffset + pGCtable->pGCitems[i].ulSize == ulOffset )
         {
            ulOffset = pGCtable->pGCitems[i].ulOffset;
            ulSize   = pGCtable->pGCitems[i].ulSize += ulSize;
            fChanged = pGCtable->pGCitems[i].fChanged = TRUE;
            break;
         }
         if ( pGCtable->pGCitems[i].ulOffset == ulOffset + ulSize )
         {
            pGCtable->pGCitems[i].ulOffset = ulOffset;
            ulSize   = pGCtable->pGCitems[i].ulSize += ulSize;
            fChanged = pGCtable->pGCitems[i].fChanged = TRUE;
            break;
         }
      }
      if ( !fChanged )
      {
         if ( pGCtable->usItems <= pGCtable->usMaxItem )
         {
            if ( pGCtable->pGCitems == NULL )
            {
               pGCtable->pGCitems = ( LPMEMOGCITEM ) hb_xgrab( sizeof( MEMOGCITEM ) * ( pGCtable->usMaxItem + 1 ) );
            }
            pGCtable->pGCitems[ pGCtable->usItems ].ulOffset = ulOffset;
            pGCtable->pGCitems[ pGCtable->usItems ].ulSize = ulSize;
            pGCtable->pGCitems[ pGCtable->usItems ].fChanged = fChanged = TRUE;
            pGCtable->usItems++;
         }
         else if ( pGCtable->pGCitems[ 1 ].ulSize < ulSize )
         {
            if ( pGCtable->ulNextBlock == pGCtable->pGCitems[ 1 ].ulOffset +
                                          pGCtable->pGCitems[ 1 ].ulSize )
            {
               pGCtable->ulNextBlock -= pGCtable->pGCitems[ 1 ].ulSize;
            }
            else if ( pGCtable->pGCitems[ 1 ].fChanged )
            {
               errCode = hb_fptWriteGCitems( pArea, pGCtable, 1 );
            }
            pGCtable->pGCitems[ 1 ].ulOffset = ulOffset;
            pGCtable->pGCitems[ 1 ].ulSize = ulSize;
            pGCtable->pGCitems[ 1 ].fChanged = fChanged = TRUE;
         }
      }

      if ( fChanged )
      {
         pGCtable->bChanged |= 2;
         hb_fptPackGCitems( pGCtable );
         hb_fptSortGCitems( pGCtable );
      }
   }

   return errCode;
}

/*
 * Get free memo block from GC free memo blocks list or allocate new one.
 */
static ERRCODE hb_fptGCgetFreeBlock( FPTAREAP pArea, LPMEMOGCTABLE pGCtable,
                                     ULONG * ulOffset, ULONG ulByteSize )
{
   BOOL fAlloc = FALSE;
   ULONG ulSize;
   int i;

   ulSize = ( ulByteSize + sizeof( FPTBLOCK ) + pArea->uiMemoBlockSize - 1 ) /
            pArea->uiMemoBlockSize;

   for( i = 0; i < pGCtable->usItems; i++ )
   {
      if ( pGCtable->pGCitems[i].ulSize >= ulSize )
      {
         *ulOffset = pGCtable->pGCitems[i].ulOffset;
         pGCtable->pGCitems[i].ulOffset += ulSize;
         pGCtable->pGCitems[i].ulSize -= ulSize;
         if ( pGCtable->pGCitems[i].ulSize == 0 )
         {
            while ( ++i < pGCtable->usItems )
            {
               pGCtable->pGCitems[i-1].ulOffset = pGCtable->pGCitems[i].ulOffset;
               pGCtable->pGCitems[i-1].ulSize   = pGCtable->pGCitems[i].ulSize;
            }
            pGCtable->usItems--;
         }
         else
         {
            pGCtable->pGCitems[i].fChanged = TRUE;
            hb_fptSortGCitems( pGCtable );
         }
         pGCtable->bChanged |= 2;
         fAlloc = TRUE;
         break;
      }
   }
   if ( !fAlloc )
   {
      *ulOffset = pGCtable->ulNextBlock;
      pGCtable->ulNextBlock += ulSize;
      pGCtable->bChanged |= 1;
   }
   return SUCCESS;
}

/*
 * Init GC table free memo blok list.
 */
static void hb_fptInitGCdata( LPMEMOGCTABLE pGCtable )
{
   memset( pGCtable, 0, sizeof(MEMOGCTABLE) );
}

/*
 * Clean GC table free memo blok list.
 */
static void hb_fptDestroyGCdata( LPMEMOGCTABLE pGCtable )
{
   if ( pGCtable->pGCitems != NULL )
   {
      hb_xfree( pGCtable->pGCitems );
      pGCtable->pGCitems = NULL;
      pGCtable->usItems = 0;
   }
   pGCtable->bChanged = 0;
}

/*
 * Read GC table from memo file.
 */
static ERRCODE hb_fptReadGCdata( FPTAREAP pArea, LPMEMOGCTABLE pGCtable )
{
   int i;

   hb_fptDestroyGCdata( pGCtable );
   memset( &pGCtable->fptHeader, 0, sizeof( FPTHEADER ) );

   hb_fsSeek( pArea->hMemoFile, 0, FS_SET );
   if ( hb_fsRead( pArea->hMemoFile, ( BYTE * ) &pGCtable->fptHeader, sizeof( FPTHEADER ) ) >= 512 )
   {
      pGCtable->ulNextBlock = HB_GET_BE_ULONG( pGCtable->fptHeader.nextBlock );

      if ( pArea->bMemoType == MEMO_FPT_SIX ||
           pArea->bMemoType == MEMO_FPT_SIXHB )
      {
         pGCtable->bType = MEMO_FPT_SIX;
         pGCtable->usMaxItem = MAX_SIXFREEBLOCKS;
         pGCtable->usItems = HB_GET_LE_USHORT( pGCtable->fptHeader.nGCitems );
         if ( pGCtable->usItems > pGCtable->usMaxItem )
         {
            return EDBF_CORRUPT;
         }

         pGCtable->pGCitems = ( LPMEMOGCITEM ) hb_xgrab( sizeof( MEMOGCITEM ) * ( pGCtable->usMaxItem + 1 ) );

         for( i = 0; i < pGCtable->usItems; i++ )
         {
            pGCtable->pGCitems[i].ulSize = HB_GET_LE_USHORT( &pGCtable->fptHeader.reserved2[ i * 6 ] );
            pGCtable->pGCitems[i].ulOffset = HB_GET_LE_ULONG( &pGCtable->fptHeader.reserved2[ i * 6 + 2 ] );
            pGCtable->pGCitems[i].fChanged = FALSE;
         }
      }
      else if ( pArea->bMemoType == MEMO_FPT_FLEX ||
                pArea->bMemoType == MEMO_FPT_HB   ||
                pArea->bMemoType == MEMO_FPT_CLIP )
      {
         FPTBLOCK fptBlock;
         BYTE *bPageBuf;

         pGCtable->bType = MEMO_FPT_FLEX;
         pGCtable->usMaxItem = MAX_FLEXFREEBLOCKS;
         pGCtable->ulRevPage = HB_GET_LE_ULONG( pGCtable->fptHeader.flexRev );
         pGCtable->ulDirPage = HB_GET_LE_ULONG( pGCtable->fptHeader.flexDir );
         pGCtable->ulCounter = HB_GET_LE_ULONG( pGCtable->fptHeader.counter );
         if ( pGCtable->ulDirPage )
         {
            hb_fsSeek( pArea->hMemoFile, pGCtable->ulDirPage, FS_SET );
            if ( hb_fsRead( pArea->hMemoFile, ( BYTE * ) &fptBlock,
                                sizeof( FPTBLOCK ) ) != sizeof( FPTBLOCK ) ||
                 HB_GET_BE_ULONG( fptBlock.type ) != FPTIT_FLEX_GC )
            {
               return EDBF_CORRUPT;
            }
            pGCtable->ulSize = HB_GET_BE_ULONG( fptBlock.size );
            bPageBuf = ( BYTE * ) hb_xgrab( pGCtable->ulSize );
            if ( hb_fsRead( pArea->hMemoFile, bPageBuf, ( USHORT ) pGCtable->ulSize ) !=
                                                            ( USHORT ) pGCtable->ulSize )
            {
               hb_xfree( bPageBuf );
               return EDBF_CORRUPT;
            }
            pGCtable->usMaxItem = (USHORT ) ( ( pGCtable->ulSize - 2 ) >> 3 );
            pGCtable->usItems = ( HB_GET_LE_USHORT( bPageBuf ) - 3 ) >> 2;

            pGCtable->pGCitems = ( LPMEMOGCITEM ) hb_xgrab( sizeof( MEMOGCITEM ) *
                     ( HB_MIN( pGCtable->usItems, pGCtable->usMaxItem ) + 1 ) );

            for( i = 0; i < pGCtable->usItems; i++ )
            {
               pGCtable->pGCitems[i].ulOffset = HB_GET_LE_ULONG( &bPageBuf[ i * 8 + 2 ] ) /
                                                      pArea->uiMemoBlockSize;
               pGCtable->pGCitems[i].ulSize = HB_GET_LE_ULONG( &bPageBuf[ i * 8 + 6 ] ) /
                                                      pArea->uiMemoBlockSize;
               pGCtable->pGCitems[i].fChanged = FALSE;
            }
            hb_xfree( bPageBuf );
         }
      }

      if ( pGCtable->pGCitems )
      {
         hb_fptSortGCitems( pGCtable );
      }

      return SUCCESS;
   }
   return EDBF_READ;
}

/*
 * Write GC table into memo file.
 */
static ERRCODE hb_fptWriteGCdata( FPTAREAP pArea, LPMEMOGCTABLE pGCtable )
{
   ERRCODE errCode = SUCCESS;
   ULONG ulHdrSize = 512;
   int i, j;

   if ( pGCtable->bChanged > 0 )
   {
      if ( pGCtable->bType == MEMO_FPT_SIX )
      {
         HB_PUT_LE_USHORT( pGCtable->fptHeader.nGCitems, pGCtable->usItems );
         memset( pGCtable->fptHeader.reserved2, 0, sizeof( pGCtable->fptHeader.reserved2 ) );
         j = ( pGCtable->usItems > pGCtable->usMaxItem ) ?
               pGCtable->usItems - pGCtable->usMaxItem : 0;
         for( i = j ; i < pGCtable->usItems; i++ )
         {
            HB_PUT_LE_USHORT( &pGCtable->fptHeader.reserved2[ ( i - j ) * 6 ],
                              (( USHORT ) pGCtable->pGCitems[i].ulSize ) );
            HB_PUT_LE_ULONG( &pGCtable->fptHeader.reserved2[ ( i - j ) * 6 + 2 ],
                              pGCtable->pGCitems[i].ulOffset );
         }
      }
      else if ( pGCtable->bType == MEMO_FPT_FLEX )
      {
         ulHdrSize = sizeof( FPTHEADER );
         pGCtable->ulCounter++;
         if ( pGCtable->usItems == 0 && pGCtable->ulDirPage )
         {
            ULONG ulOffset = pGCtable->ulDirPage;
            ULONG ulSize = ( pGCtable->ulSize + pArea->uiMemoBlockSize - 1 ) /
                           pArea->uiMemoBlockSize;
            if ( pGCtable->ulRevPage )
            {
               ulSize <<= 1;
               if ( pGCtable->ulDirPage > pGCtable->ulRevPage )
               {
                  ulOffset = pGCtable->ulRevPage;
               }
            }
            ulOffset /= pArea->uiMemoBlockSize;
            if ( ulOffset + ulSize == pGCtable->ulNextBlock )
            {
               pGCtable->ulDirPage = pGCtable->ulRevPage = 0;
               pGCtable->ulNextBlock -= ulSize;
            }
         }
         else if ( pGCtable->usItems > 0 && ! pGCtable->ulDirPage )
         {
            pGCtable->ulSize = FLEXGCPAGE_SIZE;
            errCode = hb_fptGCgetFreeBlock( pArea, pGCtable,
                                &pGCtable->ulDirPage, pGCtable->ulSize );
            if ( errCode == SUCCESS )
            {
               pGCtable->ulDirPage *= pArea->uiMemoBlockSize;
               errCode = hb_fptGCgetFreeBlock( pArea, pGCtable,
                                &pGCtable->ulRevPage, pGCtable->ulSize );
               pGCtable->ulRevPage *= pArea->uiMemoBlockSize;
            }
            pGCtable->bChanged |= 2;
         }
         if ( pGCtable->ulDirPage && pGCtable->bChanged > 1 )
         {
            FPTBLOCK fptBlock;
            BYTE *bPageBuf;

            HB_PUT_BE_ULONG( fptBlock.type, FPTIT_FLEX_GC );
            HB_PUT_BE_ULONG( fptBlock.size, pGCtable->ulSize );
            bPageBuf = ( BYTE * ) hb_xgrab( pGCtable->ulSize );
            memset( bPageBuf, 0xAD, pGCtable->ulSize );
            HB_PUT_LE_USHORT( bPageBuf, ( (USHORT) pGCtable->usItems << 2 ) + 3 );
            j = ( pGCtable->usItems > pGCtable->usMaxItem ) ?
                  pGCtable->usItems - pGCtable->usMaxItem : 0;
            for( i = j ; i < pGCtable->usItems; i++ )
            {
               HB_PUT_LE_ULONG( &bPageBuf[ ( i - j ) * 8 + 2 ],
                                pGCtable->pGCitems[i].ulOffset * pArea->uiMemoBlockSize );
               HB_PUT_LE_ULONG( &bPageBuf[ ( i - j ) * 8 + 6 ],
                                pGCtable->pGCitems[i].ulSize * pArea->uiMemoBlockSize );
            }
            hb_fsSeek( pArea->hMemoFile, pGCtable->ulDirPage, FS_SET );
            if ( hb_fsWrite( pArea->hMemoFile, ( BYTE * ) &fptBlock,
                             sizeof( FPTBLOCK ) ) != sizeof( FPTBLOCK ) ||
                 hb_fsWrite( pArea->hMemoFile, bPageBuf,
                             ( USHORT ) pGCtable->ulSize ) != ( USHORT ) pGCtable->ulSize )
            {
               errCode = EDBF_WRITE;
            }
            else if ( pGCtable->ulRevPage )
            {
               for( i = j; i < pGCtable->usItems; i++ )
               {
                  HB_PUT_LE_ULONG( &bPageBuf[ ( i - j ) * 8 + 2 ],
                                   ( ( USHORT ) pGCtable->pGCitems[i].ulSize * pArea->uiMemoBlockSize ) );
                  HB_PUT_LE_ULONG( &bPageBuf[ ( i - j ) * 8 + 6 ],
                                   pGCtable->pGCitems[i].ulOffset * pArea->uiMemoBlockSize );
               }
               hb_fsSeek( pArea->hMemoFile, pGCtable->ulRevPage, FS_SET );
               if ( hb_fsWrite( pArea->hMemoFile, ( BYTE * ) &fptBlock,
                                sizeof( FPTBLOCK ) ) != sizeof( FPTBLOCK ) ||
                    hb_fsWrite( pArea->hMemoFile, bPageBuf,
                                ( USHORT )pGCtable->ulSize ) != ( USHORT ) pGCtable->ulSize )
               {
                  errCode = EDBF_WRITE;
               }
            }
            hb_xfree( bPageBuf );
         }
         HB_PUT_LE_ULONG( pGCtable->fptHeader.flexRev, pGCtable->ulRevPage );
         HB_PUT_LE_ULONG( pGCtable->fptHeader.flexDir, pGCtable->ulDirPage );
         HB_PUT_LE_ULONG( pGCtable->fptHeader.counter, pGCtable->ulCounter );
      }

      if ( pGCtable->bChanged > 1 && errCode == SUCCESS )
      {
         errCode = hb_fptWriteGCitems( pArea, pGCtable, 0 );
      }
      if ( errCode == SUCCESS )
      {
         HB_PUT_BE_ULONG( pGCtable->fptHeader.nextBlock, pGCtable->ulNextBlock );
         hb_fsSeek( pArea->hMemoFile, 0, FS_SET );
         if ( hb_fsWrite( pArea->hMemoFile, ( BYTE * ) &pGCtable->fptHeader, ( USHORT )ulHdrSize ) != ( USHORT ) ulHdrSize )
         {
            errCode = EDBF_WRITE;
         }
         else
         {
            /* trunc file */
            hb_fsSeek( pArea->hMemoFile, pGCtable->ulNextBlock * pArea->uiMemoBlockSize, FS_SET );
            hb_fsWrite( pArea->hMemoFile, NULL, 0 );
         }
      }
      pGCtable->bChanged = 0;
   }
   return errCode;
}

/*
 * Return the size of memo.
 */
static ULONG hb_fptGetMemoLen( FPTAREAP pArea, USHORT uiIndex )
{
   ULONG ulBlock;
   FPTBLOCK fptBlock;

   HB_TRACE(HB_TR_DEBUG, ("hb_fptGetMemoLen(%p, %hu)", pArea, uiIndex));

   ulBlock = hb_dbfGetMemoBlock( (DBFAREAP) pArea, uiIndex );

   if( ulBlock == 0 )
      return 0;

   hb_fsSeek( pArea->hMemoFile, ulBlock * pArea->uiMemoBlockSize, FS_SET );

   if( hb_fsRead( pArea->hMemoFile, ( BYTE * ) &fptBlock,
                              sizeof( FPTBLOCK ) ) != sizeof( FPTBLOCK ) )
      return 0;

   return HB_GET_BE_ULONG( fptBlock.size );
}

/*
 * Read SIX item from memo.
 */
static ERRCODE hb_fptReadSixItem( FPTAREAP pArea, BYTE ** pbMemoBuf, BYTE * bBufEnd, PHB_ITEM pItem )
{
   USHORT usType;
   ULONG ulLen, i;
   PHB_ITEM pNewItem;
   ERRCODE errCode = SUCCESS;

   ulLen = SIX_ITEM_BUFSIZE;
   if ( bBufEnd - (*pbMemoBuf) >= ( LONG ) ulLen )
   {
      usType = HB_GET_LE_USHORT( &(*pbMemoBuf)[0] );
      switch ( usType )
      {
         case FPTIT_SIX_LNUM:
            hb_itemPutNL( pItem, ( LONG ) HB_GET_LE_ULONG( &(*pbMemoBuf)[6] ) );
            break;

         case FPTIT_SIX_DNUM:
            hb_itemPutNDLen( pItem, HB_GET_LE_DOUBLE( &(*pbMemoBuf)[6] ),
                                    HB_GET_LE_USHORT( &(*pbMemoBuf)[2] ),
                                    HB_GET_LE_USHORT( &(*pbMemoBuf)[4] ) );
            break;

         case FPTIT_SIX_LDATE:
            hb_itemPutDL( pItem, ( LONG ) HB_GET_LE_ULONG( &(*pbMemoBuf)[6] ) );
            break;

         case FPTIT_SIX_LOG:
            hb_itemPutL( pItem, (&(*pbMemoBuf)[6]) != 0 );
            break;

         case FPTIT_SIX_CHAR:
            ulLen = HB_GET_LE_ULONG( &(*pbMemoBuf)[2] );
            if ( pArea->bMemoType == MEMO_FPT_SIX )
            {
              ulLen &= 0xFFFF; /* only 2 bytes (SHORT) for SIX compatibility */
            }
            (*pbMemoBuf) += SIX_ITEM_BUFSIZE;
            if ( bBufEnd - (*pbMemoBuf) >= ( LONG ) ulLen )
            {
               hb_itemPutCL( pItem, ( char *) (*pbMemoBuf), ulLen );
#ifndef HB_CDP_SUPPORT_OFF
               hb_cdpnTranslate( pItem->item.asString.value, pArea->cdPage, hb_cdp_page, ulLen );
#endif
            }
            else
            {
               errCode = EDBF_CORRUPT;
            }
            break;

/*         case FPTIT_SIX_BLOCK: */
/*         case FPTIT_SIX_VREF:  */
/*         case FPTIT_SIX_MREF:  */

         case FPTIT_SIX_ARRAY:
            ulLen = HB_GET_LE_ULONG( &(*pbMemoBuf)[2] );
            if ( pArea->bMemoType == MEMO_FPT_SIX )
            {
                 ulLen &= 0xFFFF; /* only 2 bytes (SHORT) for SIX compatibility */
            }
            (*pbMemoBuf) += SIX_ITEM_BUFSIZE;
            hb_arrayNew( pItem, ulLen );
            pNewItem = hb_itemNew( NULL );
            for ( i = 1 ; i <= ulLen ; i++ )
            {
               errCode = hb_fptReadSixItem( pArea, pbMemoBuf, bBufEnd, pNewItem );
               if ( errCode != SUCCESS )
               {
                  break;
               }
               hb_itemArrayPut( pItem, i, pNewItem );
               hb_itemClear( pNewItem );
            }
            hb_itemRelease( pNewItem );
            ulLen = 0;
            break;
         default:
            hb_itemClear( pItem );
            break;
      }
      *pbMemoBuf += ulLen;
   }
   else
   {
      errCode = EDBF_CORRUPT;
   }

   return errCode;
}

/*
 * Read FLEX item from memo.
 */
static ERRCODE hb_fptReadFlexItem( FPTAREAP pArea, BYTE ** pbMemoBuf, BYTE * bBufEnd, PHB_ITEM pItem, BOOL bRoot )
{
   BYTE usType;
   ULONG ulLen, i;
   PHB_ITEM pNewItem;
   ERRCODE errCode = SUCCESS;

   if ( bRoot )
   {
      usType = FPTIT_FLEXAR_ARAY;
   }
   else if ( bBufEnd - (*pbMemoBuf) > 0 )
   {
      usType = *(*pbMemoBuf)++;
   }
   else
   {
      return EDBF_CORRUPT;
   }
   switch ( usType )
   {
      case FPTIT_FLEXAR_NIL:
         hb_itemClear( pItem );
         break;
      case FPTIT_FLEXAR_TRUE:
         hb_itemPutL( pItem, TRUE );
         break;
      case FPTIT_FLEXAR_FALSE:
         hb_itemPutL( pItem, FALSE );
         break;
      case FPTIT_FLEXAR_DATE:
         if ( bBufEnd - (*pbMemoBuf) >= 4 )
         {
            hb_itemPutDL( pItem, (LONG) HB_GET_LE_ULONG( *pbMemoBuf ) );
            *pbMemoBuf += 4;
         }
         else
         {
            errCode = EDBF_CORRUPT;
         }
         break;
      case FPTIT_FLEXAR_BYTE1:
      case FPTIT_FLEXAR_BYTE:
         if ( bBufEnd - (*pbMemoBuf) >= 2 )
         {
            hb_itemPutNI( pItem, **pbMemoBuf );
            *pbMemoBuf += 2;
         }
         else
         {
            errCode = EDBF_CORRUPT;
         }
         break;
      case FPTIT_FLEXAR_BYTE2:
         if ( bBufEnd - (*pbMemoBuf) >= 3 )
         {
            hb_itemPutNI( pItem, ( BYTE ) **pbMemoBuf );
            *pbMemoBuf += 3;
         }
         else
         {
            errCode = EDBF_CORRUPT;
         }
         break;
      case FPTIT_FLEXAR_SHORT:
         if ( bBufEnd - (*pbMemoBuf) >= 3 )
         {
            hb_itemPutNI( pItem, (SHORT) HB_GET_LE_USHORT( *pbMemoBuf ) );
            *pbMemoBuf += 3;
         }
         else
         {
            errCode = EDBF_CORRUPT;
         }
         break;
      case FPTIT_FLEXAR_SHORT2:
         if ( bBufEnd - (*pbMemoBuf) >= 4 )
         {
            hb_itemPutNI( pItem, (SHORT) HB_GET_LE_USHORT( *pbMemoBuf ) );
            *pbMemoBuf += 4;
         }
         else
         {
            errCode = EDBF_CORRUPT;
         }
         break;
      case FPTIT_FLEXAR_LONG:
         if ( bBufEnd - (*pbMemoBuf) >= 6 )
         {
            hb_itemPutNL( pItem, (LONG) HB_GET_LE_ULONG( *pbMemoBuf ) );
            *pbMemoBuf += 6;
         }
         else
         {
            errCode = EDBF_CORRUPT;
         }
         break;
      case FPTIT_FLEXAR_DBL:
         if ( bBufEnd - (*pbMemoBuf) >= 10 )
         {
            hb_itemPutNDLen( pItem, HB_GET_LE_DOUBLE( *pbMemoBuf + 2 ),
                                    **pbMemoBuf, (*pbMemoBuf)[1] );
            *pbMemoBuf += 10;
         }
         else
         {
            errCode = EDBF_CORRUPT;
         }
         break;
      case FPTIT_FLEXAR_NUL:
         hb_itemPutCL( pItem, NULL, 0);
         break;

      case FPTIT_FLEXAR_STR:
         if ( bBufEnd - (*pbMemoBuf) >= 2 )
         {
            ulLen = HB_GET_LE_USHORT( *pbMemoBuf );
            *pbMemoBuf += 2;
            if ( bBufEnd - (*pbMemoBuf) >= ( LONG ) ulLen )
            {
               hb_itemPutCL( pItem, ( char *) *pbMemoBuf, ulLen );
               *pbMemoBuf += ulLen;
#ifndef HB_CDP_SUPPORT_OFF
               hb_cdpnTranslate( pItem->item.asString.value, pArea->cdPage, hb_cdp_page, ulLen );
#endif
            }
            else
            {
               errCode = EDBF_CORRUPT;
            }
         }
         else
         {
            errCode = EDBF_CORRUPT;
         }
         break;

      case FPTIT_FLEXAR_ARAY:
         if ( bBufEnd - (*pbMemoBuf) >= 2 )
         {
            ulLen = HB_GET_LE_USHORT( *pbMemoBuf );
            *pbMemoBuf += 2;
            if ( bBufEnd - (*pbMemoBuf) >= ( LONG ) ulLen )
            {
               hb_arrayNew( pItem, ulLen );
               pNewItem = hb_itemNew( NULL );
               for ( i = 1 ; i <= ulLen ; i++ )
               {
                  errCode = hb_fptReadFlexItem( pArea, pbMemoBuf, bBufEnd, pNewItem, FALSE );
                  if ( errCode != SUCCESS )
                  {
                     break;
                  }
                  hb_itemArrayPut( pItem, i, pNewItem );
                  hb_itemClear( pNewItem );
               }
               hb_itemRelease( pNewItem );
            }
            else
            {
               errCode = EDBF_CORRUPT;
            }
         }
         else
         {
            errCode = EDBF_CORRUPT;
         }
         break;
      default:
         hb_itemClear( pItem );
         break;
   }
   return errCode;
}

/*
 * Read fpt vartype memos.
 */
static ERRCODE hb_fptGetMemo( FPTAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   ERRCODE errCode = SUCCESS;
   ULONG ulBlock, ulSize, ulType;
   BYTE * pBuffer, * bMemoBuf;
   FPTBLOCK fptBlock;

   HB_TRACE(HB_TR_DEBUG, ("hb_fptGetMemo(%p, %hu, %p)", pArea, uiIndex, pItem));

   ulBlock = hb_dbfGetMemoBlock( (DBFAREAP) pArea, uiIndex );

   if( ulBlock > 0 )
   {
      hb_fsSeek( pArea->hMemoFile, ulBlock * pArea->uiMemoBlockSize, FS_SET );

      if( hb_fsRead( pArea->hMemoFile, ( BYTE * ) &fptBlock,
                                 sizeof( FPTBLOCK ) ) != sizeof( FPTBLOCK ) )
      {
         ulSize = 0;
      }
      else
      {
         ulSize = HB_GET_BE_ULONG( fptBlock.size );
      }

      ulType = HB_GET_BE_ULONG( fptBlock.type );
      pBuffer = ( BYTE * ) hb_xgrab( HB_MAX( ulSize + 1, 8 ) );
      memset( pBuffer, '\0', 8);
      if ( hb_fsReadLarge( pArea->hMemoFile, pBuffer, ulSize ) != ulSize )
      {
         errCode = EDBF_READ;
      }
      else
      {
         switch ( ulType )
         {
            case FPTIT_SIX_LNUM:
            case FPTIT_SIX_DNUM:
            case FPTIT_SIX_LDATE:
            case FPTIT_SIX_LOG:
            case FPTIT_SIX_CHAR:
            case FPTIT_SIX_ARRAY:
/*            case FPTIT_SIX_BLOCK: */
/*            case FPTIT_SIX_VREF:  */
/*            case FPTIT_SIX_MREF:  */
               bMemoBuf = pBuffer;
               errCode = hb_fptReadSixItem( pArea, &bMemoBuf, bMemoBuf + ulSize, pItem );
               break;
            case FPTIT_FLEX_ARRAY:
               bMemoBuf = pBuffer;
               errCode = hb_fptReadFlexItem( pArea, &bMemoBuf, bMemoBuf + ulSize, pItem, TRUE );
               break;
            case FPTIT_FLEX_NIL:
               hb_itemClear( pItem );
               break;
            case FPTIT_FLEX_TRUE:
               hb_itemPutL( pItem, TRUE );
               break;
            case FPTIT_FLEX_FALSE:
               hb_itemPutL( pItem, FALSE );
               break;
            case FPTIT_FLEX_LDATE:
               hb_itemPutDL( pItem, (LONG) HB_GET_LE_ULONG( pBuffer ) );
               break;
            case FPTIT_FLEX_BYTE:
               hb_itemPutNI( pItem, pBuffer[0] );
               break;
            case FPTIT_FLEX_SHORT:
               hb_itemPutNI( pItem, (SHORT) HB_GET_LE_USHORT( pBuffer ) );
               break;
            case FPTIT_FLEX_LONG:
               hb_itemPutNL( pItem, (LONG) HB_GET_LE_ULONG( pBuffer ) );
               break;
            case FPTIT_FLEX_DOUBLE:
               hb_itemPutND( pItem, HB_GET_LE_DOUBLE( pBuffer ) );
               break;
            case FPTIT_TEXT:
#ifndef HB_CDP_SUPPORT_OFF
               hb_cdpnTranslate( ( char *) pBuffer, pArea->cdPage, hb_cdp_page, ulSize );
#endif
            case FPTIT_PICT:
               pBuffer[ ulSize ] = '\0';
               hb_itemPutCPtr( pItem, ( char * ) pBuffer, ulSize );
               hb_itemSetCMemo( pItem );
               pBuffer = NULL;
               break;
            default:
               hb_itemClear( pItem );
               break;
         }
         if ( pBuffer )
            hb_xfree(pBuffer);
      }
   }
   else
   {
      hb_itemPutC( pItem, "" );
      hb_itemSetCMemo( pItem );
   }
   return errCode;
}

/*
 * Calculate the size of SIX memo item
 */
static ULONG hb_fptCountSixItemLength( FPTAREAP pArea, PHB_ITEM pItem )
{
   ULONG ulLen, i, ulSize;
   USHORT usType;

   usType = hb_itemType( pItem );
   switch ( usType )
   {
      case HB_IT_ARRAY: /* HB_IT_OBJECT = HB_IT_ARRAY */
         ulSize = SIX_ITEM_BUFSIZE;
         ulLen = hb_arrayLen( pItem );
         if ( pArea->bMemoType == MEMO_FPT_SIX )
         {
              ulLen &= 0xFFFF; /* only 2 bytes (SHORT) for SIX compatibility */
         }
         for ( i = 1 ; i <= ulLen ; i++ )
         {
            ulSize += hb_fptCountSixItemLength( pArea, hb_arrayGetItemPtr( pItem, i ) );
         }
         break;
      case HB_IT_MEMO:
      case HB_IT_STRING:
         ulSize = SIX_ITEM_BUFSIZE;
         ulLen = pItem->item.asString.length;
         if ( pArea->bMemoType == MEMO_FPT_SIX )
         {
              ulLen &= 0xFFFF; /* only 2 bytes (SHORT) for SIX compatibility */
         }
         ulSize += ulLen;
         break;
      case HB_IT_INTEGER:
      case HB_IT_LONG:
      case HB_IT_DOUBLE:
      case HB_IT_DATE:
      case HB_IT_LOGICAL:
      default:
         ulSize = SIX_ITEM_BUFSIZE;
   }
   return ulSize;
}

/*
 * Write fpt vartype as SIX memos.
 */
static ULONG hb_fptStoreSixItem( FPTAREAP pArea, PHB_ITEM pItem, BYTE ** bBufPtr )
{
   USHORT usType;
   ULONG ulLen, i, ulSize;
   PHB_ITEM pTmpItem;

   memset( *bBufPtr, '\0', SIX_ITEM_BUFSIZE );
   usType = hb_itemType( pItem );
   ulSize = SIX_ITEM_BUFSIZE;
   switch ( usType )
   {
      case HB_IT_ARRAY: /* HB_IT_OBJECT = HB_IT_ARRAY */
         HB_PUT_LE_USHORT( &(*bBufPtr)[0], FPTIT_SIX_ARRAY );
         ulLen = hb_arrayLen( pItem );
         if ( pArea->bMemoType == MEMO_FPT_SIX )
         {
              ulLen &= 0xFFFF; /* only 2 bytes (SHORT) for SIX compatibility */
         }
         HB_PUT_LE_ULONG( &(*bBufPtr)[2], ulLen );
         *bBufPtr += SIX_ITEM_BUFSIZE;
         for ( i = 1 ; i <= ulLen ; i++ )
         {
            pTmpItem = hb_arrayGetItemPtr( pItem, i );
            ulSize += hb_fptStoreSixItem( pArea, pTmpItem, bBufPtr );
         }
         break;

      case HB_IT_INTEGER:
         HB_PUT_LE_USHORT( &(*bBufPtr)[0], FPTIT_SIX_LNUM );
         HB_PUT_LE_USHORT( &(*bBufPtr)[2], pItem->item.asInteger.length );
         HB_PUT_LE_ULONG(  &(*bBufPtr)[6], pItem->item.asInteger.value );
         *bBufPtr += SIX_ITEM_BUFSIZE;
         break;

      case HB_IT_LONG:
         HB_PUT_LE_USHORT( &(*bBufPtr)[0], FPTIT_SIX_LNUM );
         HB_PUT_LE_USHORT( &(*bBufPtr)[2], pItem->item.asLong.length );
         HB_PUT_LE_ULONG(  &(*bBufPtr)[6], pItem->item.asLong.value );
         *bBufPtr += SIX_ITEM_BUFSIZE;
         break;

      case HB_IT_DOUBLE:
         HB_PUT_LE_USHORT( &(*bBufPtr)[0], FPTIT_SIX_DNUM );
         HB_PUT_LE_USHORT( &(*bBufPtr)[2], pItem->item.asDouble.length );
         HB_PUT_LE_USHORT( &(*bBufPtr)[4], pItem->item.asDouble.decimal );
         HB_PUT_LE_DOUBLE( &(*bBufPtr)[6], pItem->item.asDouble.value );
         *bBufPtr += SIX_ITEM_BUFSIZE;
         break;

      case HB_IT_DATE:
         HB_PUT_LE_USHORT( &(*bBufPtr)[0], FPTIT_SIX_LDATE );
         HB_PUT_LE_ULONG(  &(*bBufPtr)[6], pItem->item.asDate.value );
         *bBufPtr += SIX_ITEM_BUFSIZE;
         break;

      case HB_IT_LOGICAL:
         HB_PUT_LE_USHORT( &(*bBufPtr)[0], FPTIT_SIX_LOG );
         *(BOOL*) ( &(*bBufPtr)[6] ) = pItem->item.asLogical.value;
         *bBufPtr += SIX_ITEM_BUFSIZE;
         break;

      case HB_IT_STRING:
      case HB_IT_MEMO:
         HB_PUT_LE_USHORT( &(*bBufPtr)[0], FPTIT_SIX_CHAR );
         ulLen = pItem->item.asString.length;
         if ( pArea->bMemoType == MEMO_FPT_SIX )
         {
              ulLen &= 0xFFFF; /* only 2 bytes (SHORT) for SIX compatibility */
         }
         HB_PUT_LE_ULONG( &(*bBufPtr)[2], ulLen );
         *bBufPtr += SIX_ITEM_BUFSIZE;
         if ( ulLen > 0 )
         {
            memcpy( *bBufPtr, pItem->item.asString.value, ulLen );
#ifndef HB_CDP_SUPPORT_OFF
            hb_cdpnTranslate( ( char *) *bBufPtr, pArea->cdPage, hb_cdp_page, ulLen );
#endif
            *bBufPtr += ulLen;
         }
         break;
      default:
         *bBufPtr += SIX_ITEM_BUFSIZE;
         break;
   }
   return ulSize;
}

/*
 * Calculate the size of FLEX memo item
 */
static ULONG hb_fptCountFlexItemLength( FPTAREAP pArea, PHB_ITEM pItem )
{
   ULONG ulLen, i, ulSize = 1;
   USHORT usType;

   usType = hb_itemType( pItem );
   switch ( usType )
   {
      case HB_IT_ARRAY:
         ulSize += 2;
         ulLen = hb_arrayLen( pItem ) & 0xFFFF;
         for ( i = 1 ; i <= ulLen ; i++ )
         {
            ulSize += hb_fptCountFlexItemLength( pArea, hb_arrayGetItemPtr( pItem, i ) );
         }
         break;
      case HB_IT_MEMO:
      case HB_IT_STRING:
         ulLen = pItem->item.asString.length & 0xFFFF;
         ulSize += ulLen + 2;
         break;
      case HB_IT_DATE:
         ulSize += 4;
         break;
      case HB_IT_INTEGER:
         ulSize += 3;
         break;
      case HB_IT_LONG:
         ulSize += 6;
         break;
      case HB_IT_DOUBLE:
         ulSize += 10;
         break;
   }
   return ulSize;
}

/*
 * Store in buffer fpt vartype as FLEX memos.
 */
static void hb_fptStoreFlexItem( FPTAREAP pArea, PHB_ITEM pItem, BYTE ** bBufPtr )
{
   ULONG ulLen, i;
   USHORT usType;

   usType = hb_itemType( pItem );
   switch ( usType )
   {
      case HB_IT_ARRAY:
         ulLen = hb_arrayLen( pItem ) & 0xFFFF;
         *(*bBufPtr)++ = FPTIT_FLEXAR_ARAY;
         HB_PUT_LE_USHORT( *bBufPtr, ( USHORT ) ulLen );
         *bBufPtr += 2;
         for ( i = 1 ; i <= ulLen ; i++ )
         {
            hb_fptStoreFlexItem( pArea, hb_arrayGetItemPtr( pItem, i ), bBufPtr );
         }
         break;
      case HB_IT_MEMO:
      case HB_IT_STRING:
         *(*bBufPtr)++ = FPTIT_FLEXAR_STR;
         ulLen = pItem->item.asString.length & 0xFFFF;
         HB_PUT_LE_USHORT( *bBufPtr, ( USHORT ) ulLen );
         *bBufPtr += 2;
         memcpy( *bBufPtr, pItem->item.asString.value, ulLen );
#ifndef HB_CDP_SUPPORT_OFF
         hb_cdpnTranslate( ( char *) *bBufPtr, pArea->cdPage, hb_cdp_page, ulLen );
#endif
         *bBufPtr += ulLen;
         break;
      case HB_IT_DATE:
         *(*bBufPtr)++ = FPTIT_FLEXAR_DATE;
         HB_PUT_LE_ULONG( *bBufPtr, pItem->item.asDate.value );
         *bBufPtr += 4;
         break;
      case HB_IT_INTEGER:
         *(*bBufPtr)++ = FPTIT_FLEXAR_SHORT;
         HB_PUT_LE_USHORT( *bBufPtr, pItem->item.asInteger.value );
         *bBufPtr += 2;
         *(*bBufPtr)++ = '\0';
         break;
      case HB_IT_LONG:
         *(*bBufPtr)++ = FPTIT_FLEXAR_LONG;
         HB_PUT_LE_ULONG( *bBufPtr, pItem->item.asLong.value );
         *bBufPtr += 4;
         *(*bBufPtr)++ = '\0';
         *(*bBufPtr)++ = '\0';
         break;
      case HB_IT_DOUBLE:
         *(*bBufPtr)++ = FPTIT_FLEXAR_DBL;
         *(*bBufPtr)++ = (BYTE) pItem->item.asDouble.length;
         *(*bBufPtr)++ = (BYTE) pItem->item.asDouble.decimal;
         HB_PUT_LE_DOUBLE( *bBufPtr, pItem->item.asDouble.value );
         *bBufPtr += 8;
         break;
      case HB_IT_LOGICAL:
         *(*bBufPtr)++ = pItem->item.asLogical.value ?
                                   FPTIT_FLEXAR_TRUE : FPTIT_FLEXAR_FALSE;
         break;
      case HB_IT_NIL:
      default:
         *(*bBufPtr)++ = FPTIT_FLEXAR_NIL;
   }
}

/*
 * Write memo data.
 */
static ERRCODE hb_fptWriteMemo( FPTAREAP pArea, ULONG ulBlock, BYTE *bBufPtr,
                                ULONG ulType, ULONG ulLen, ULONG * ulStoredBlock )
{
   MEMOGCTABLE fptGCtable;
   ERRCODE errCode;
   BOOL bWrite;

   HB_TRACE(HB_TR_DEBUG, ("hb_fptWriteMemo(%p, %lu, %p, %hu, %lu, %p)",
               pArea, ulBlock, bBufPtr, ulType, ulLen, ulStoredBlock));

   bWrite = ( ulLen != 0 || ( ulType != FPTIT_TEXT && ulType != FPTIT_BINARY ) );

   if ( ulBlock == 0 && !bWrite )
   {
      * ulStoredBlock = 0;
      return SUCCESS;
   }

   hb_fptInitGCdata( &fptGCtable );
   errCode = hb_fptReadGCdata( pArea, &fptGCtable );
   if ( errCode != SUCCESS )
   {
      return errCode;
   }

   if ( ulBlock > 0 )
   {
      errCode = hb_fptGCfreeBlock( pArea, &fptGCtable, ulBlock, 0 );
      if ( errCode != SUCCESS )
      {
         hb_fptDestroyGCdata( &fptGCtable );
         return errCode;
      }
   }

   /* Write memo header and data */
   if( bWrite )
   {
      FPTBLOCK fptBlock;

      errCode = hb_fptGCgetFreeBlock( pArea, &fptGCtable, ulStoredBlock, ulLen );
      if ( errCode != SUCCESS )
      {
         hb_fptDestroyGCdata( &fptGCtable );
         return errCode;
      }

      HB_PUT_BE_ULONG( fptBlock.type, ulType );
      HB_PUT_BE_ULONG( fptBlock.size, ulLen );
      hb_fsSeek( pArea->hMemoFile, *ulStoredBlock * pArea->uiMemoBlockSize, FS_SET );
      hb_fsWrite( pArea->hMemoFile, ( BYTE * ) &fptBlock, sizeof( FPTBLOCK ) );

      if ( ulLen > 0 )
      {
         if ( hb_fsWriteLarge( pArea->hMemoFile, bBufPtr, ulLen ) != ulLen )
         {
            errCode = EDBF_WRITE;
         }
      }
      /* if written block is smaller then block size we should write at last
         block byte 0xAF to be FLEX compatible */
      if ( errCode == SUCCESS &&
           pArea->bMemoType != MEMO_FPT_SIX &&
           pArea->bMemoType != MEMO_FPT_SIXHB &&
           ( ulLen + sizeof( FPTBLOCK ) ) % pArea->uiMemoBlockSize != 0 )
      {
         ULONG ulBlocks = ( ulLen + sizeof( FPTBLOCK ) + pArea->uiMemoBlockSize - 1 ) /
                           pArea->uiMemoBlockSize;
         hb_fsSeek( pArea->hMemoFile, ( *ulStoredBlock + ulBlocks ) *
                                      pArea->uiMemoBlockSize - 1, FS_SET );
         hb_fsWrite( pArea->hMemoFile, ( BYTE * ) "\xAF", 1 );
      }
   }
   else
   {
      * ulStoredBlock = 0;
   }

   if ( errCode == SUCCESS )
   {
      errCode = hb_fptWriteGCdata( pArea, &fptGCtable );
   }
   hb_fptDestroyGCdata( &fptGCtable );

   return errCode;
}

/*
 * Assign a value to the specified memo field.
 */
static ERRCODE hb_fptPutMemo( FPTAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   ULONG ulLen, ulBlock, ulType;
   BYTE itmBuffer[FLEX_ITEM_BUFSIZE];
	 BYTE  *bBufPtr = NULL, *bBufAlloc = NULL;
   ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("hb_fptPutMemo(%p, %hu, %p)", pArea, uiIndex, pItem));

   if ( HB_IS_STRING( pItem ) )
   {
      ulType = FPTIT_TEXT;
      ulLen = pItem->item.asString.length;
      bBufPtr = ( BYTE *) pItem->item.asString.value;
#ifndef HB_CDP_SUPPORT_OFF
      if ( pArea->cdPage != hb_cdp_page )
      {
         bBufAlloc = ( BYTE * ) hb_xgrab( ulLen );
         memcpy( bBufAlloc, bBufPtr, ulLen );
         hb_cdpnTranslate( ( char *) bBufAlloc, hb_cdp_page, pArea->cdPage, ulLen );
         bBufPtr = bBufAlloc;
      }
#endif
   }
   else if ( pArea->bMemoType == MEMO_FPT_SIX ||
             pArea->bMemoType == MEMO_FPT_SIXHB )
   {
      ulType = hb_itemType( pItem );
      ulLen = hb_fptCountSixItemLength( pArea, pItem );
      if ( ulLen > 0 )
      {
         bBufPtr = bBufAlloc = ( BYTE * ) hb_xgrab( ulLen );
         hb_fptStoreSixItem( pArea, pItem, &bBufPtr );
         ulType = ( ULONG ) HB_GET_LE_USHORT( bBufAlloc );
         bBufPtr = bBufAlloc;
      }
   }
   else if ( pArea->bMemoType == MEMO_FPT_FLEX ||
             pArea->bMemoType == MEMO_FPT_HB )
   {
      ulType = hb_itemType( pItem );
      switch ( ulType )
      {
         case HB_IT_ARRAY:
            ulType = FPTIT_FLEX_ARRAY;
            ulLen = hb_fptCountFlexItemLength( pArea, pItem ) - 1;
            if ( ulLen > 0 )
            {
               bBufPtr = bBufAlloc = (BYTE *) hb_xgrab( ulLen + 1 );
               hb_fptStoreFlexItem( pArea, pItem, &bBufPtr );
               bBufPtr = bBufAlloc + 1; /* FLEX doesn't store the first byte of array ID */
            }
            break;
         case HB_IT_NIL:
            ulType = FPTIT_FLEX_NIL;
            ulLen = 0;
            break;
         case HB_IT_LOGICAL:
            ulType = pItem->item.asLogical.value ? FPTIT_FLEX_TRUE : FPTIT_FLEX_FALSE;
            ulLen = 0;
            break;
         case HB_IT_DATE:
            ulType = FPTIT_FLEX_LDATE;
            ulLen = 4;
            HB_PUT_LE_ULONG( itmBuffer, pItem->item.asDate.value );
            bBufPtr = itmBuffer;
            break;
         case HB_IT_INTEGER:
            ulType = FPTIT_FLEX_SHORT;
            ulLen = 2;
            HB_PUT_LE_USHORT( itmBuffer, pItem->item.asInteger.value );
            bBufPtr = itmBuffer;
            break;
         case HB_IT_LONG:
            ulType = FPTIT_FLEX_LONG;
            ulLen = 4;
            HB_PUT_LE_ULONG( itmBuffer, pItem->item.asLong.value );
            bBufPtr = itmBuffer;
            break;
         case HB_IT_DOUBLE:
            ulType = FPTIT_FLEX_DOUBLE;
            ulLen = 8;
            HB_PUT_LE_DOUBLE( itmBuffer, pItem->item.asDouble.value );
            bBufPtr = itmBuffer;
            break;
         default :
            ulType = FPTIT_BINARY;
            ulLen = 0;
            break;
      }
   }
   else
   {
      ulType = FPTIT_BINARY;
      ulLen = 0;
   }

   ulBlock = hb_dbfGetMemoBlock( (DBFAREAP) pArea, uiIndex );

   errCode = hb_fptWriteMemo( pArea, ulBlock, bBufPtr, ulType, ulLen, &ulBlock );

   if ( bBufAlloc != NULL )
   {
      hb_xfree( bBufAlloc );
   }

   hb_dbfPutMemoBlock( (DBFAREAP) pArea, uiIndex, ulBlock );

   return errCode;
}


/* FPT METHODS */

/*
 * Obtain the length of a field value.
 * ( DBENTRYP_SVL )   hb_fptGetVarLen
 */
static ERRCODE hb_fptGetVarLen( FPTAREAP pArea, USHORT uiIndex, ULONG * pLength )
{
   BOOL bDeleted;

   HB_TRACE(HB_TR_DEBUG, ("hb_fptGetVarLen(%p, %hu, %p)", pArea, uiIndex, pLength));

   /* Force read record */
   if( SELF_DELETED( ( AREAP ) pArea, &bDeleted ) == FAILURE )
      return FAILURE;

   if( pArea->fHasMemo && pArea->hMemoFile != FS_ERROR &&
       pArea->lpFields[ uiIndex - 1 ].uiType == HB_IT_MEMO )
   {
      if( hb_fptFileLockSh( pArea ) )
      {
         * pLength = hb_fptGetMemoLen( pArea, uiIndex - 1 );
         hb_fptFileUnLock( pArea );
      }
      else
      {
         * pLength = 0;
      }
      return SUCCESS;
   }

   return SUPER_GETVARLEN( ( AREAP ) pArea, uiIndex, pLength );
}

/*
 * Retrieve information about the current driver.
 * ( DBENTRYP_SI )    hb_fptInfo
 */
static ERRCODE hb_fptInfo( FPTAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fptInfo(%p, %hu, %p)", pArea, uiIndex, pItem));

   switch( uiIndex )
   {
      case DBI_MEMOEXT:
         if ( pArea->fHasMemo && pArea->hMemoFile != FS_ERROR )
         {
            PHB_FNAME pFileName;

            pFileName = hb_fsFNameSplit( ( char * ) pArea->szMemoFileName );
            hb_itemPutC( pItem, pFileName->szExtension );
            hb_xfree( pFileName );
         }
         else
         {
            hb_itemPutC( pItem, ( hb_set.HB_SET_MFILEEXT &&
                                  strlen( hb_set.HB_SET_MFILEEXT ) > 0 ) ?
                                 hb_set.HB_SET_MFILEEXT :
                                 FPT_MEMOEXT );
         }
         break;

      /* case DBI_RDD_VERSION */

      default:
         return SUPER_INFO( ( AREAP ) pArea, uiIndex, pItem );
   }

   return SUCCESS;
}

/*
 * Open a data store in the WorkArea.
 * ( DBENTRYP_VP )    hb_fptOpen            : NULL
 */

/*
 * Retrieve the size of the WorkArea structure.
 * ( DBENTRYP_SP )    hb_fptStructSize
 */
static ERRCODE hb_fptStructSize( FPTAREAP pArea, USHORT * uiSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fptStrucSize(%p, %p)", pArea, uiSize));
   HB_SYMBOL_UNUSED( pArea );

   * uiSize = sizeof( FPTAREA );
   return SUCCESS;
}

/*
 * Obtain the name of replaceable database driver (RDD) subsystem.
 * ( DBENTRYP_P )     hb_fptSysName
 */
static ERRCODE hb_fptSysName( FPTAREAP pArea, BYTE * pBuffer )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fptSysName(%p, %p)", pArea, pBuffer));
   HB_SYMBOL_UNUSED( pArea );

   strncpy( ( char * ) pBuffer, "DBFFPT", 7  /* HARBOUR_MAX_RDD_DRIVERNAME_LENGTH */ );
   return SUCCESS;
}

/*
 * Obtain the current value of a field.
 * ( DBENTRYP_SI )    hb_fptGetValue
 */
static ERRCODE hb_fptGetValue( FPTAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   BOOL bDeleted;
   ERRCODE uiError;

   HB_TRACE(HB_TR_DEBUG, ("hb_fptGetValue(%p, %hu, %p)", pArea, uiIndex, pItem));

   if( pArea->fHasMemo && pArea->hMemoFile != FS_ERROR &&
       pArea->lpFields[ uiIndex - 1 ].uiType == HB_IT_MEMO )
   {
      /* Force read record */
      if( SELF_DELETED( ( AREAP ) pArea, &bDeleted ) == FAILURE )
         return FAILURE;
      if( hb_fptFileLockSh( pArea ) )
      {
         uiError = hb_fptGetMemo( pArea, uiIndex - 1, pItem );
         hb_fptFileUnLock( pArea );
      }
      else
      {
         uiError = EDBF_LOCK;
      }
      if ( uiError != SUCCESS )
      {
         PHB_ITEM pError = hb_errNew();
         ERRCODE uiErrorG = hb_dbfGetEGcode( uiError );

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
   else
      return SUPER_GETVALUE( ( AREAP ) pArea, uiIndex, pItem );
}

/*
 * Assign a value to a field.
 * ( DBENTRYP_SI )    hb_fptPutValue
 */
static ERRCODE hb_fptPutValue( FPTAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   BOOL bDeleted;
   ERRCODE uiError;

   HB_TRACE(HB_TR_DEBUG, ("hb_fptPutValue(%p, %hu, %p)", pArea, uiIndex, pItem));

   if( pArea->fHasMemo && pArea->hMemoFile != FS_ERROR &&
       pArea->lpFields[ uiIndex - 1 ].uiType == HB_IT_MEMO )
   {
      /* Force read record */
      if( SELF_DELETED( ( AREAP ) pArea, &bDeleted ) == FAILURE )
         return FAILURE;

      if( !pArea->fPositioned )
         return SUCCESS;

      /* Buffer is hot? */
      if( !pArea->fRecordChanged && SELF_GOHOT( ( AREAP ) pArea ) == FAILURE )
         return FAILURE;

      if( hb_fptFileLockEx( pArea ) )
      {
         uiError = hb_fptPutMemo( pArea, uiIndex -1, pItem );
         hb_fptFileUnLock( pArea );
      }
      else
      {
         uiError = EDBF_LOCK;
      }
      /* Update deleted flag */
      pArea->pRecord[ 0 ] = (BYTE) (pArea->fDeleted ? '*' : ' ');

      if( uiError != SUCCESS )
      {
         PHB_ITEM pError;
         ERRCODE uiErrorG = hb_dbfGetEGcode( uiError );

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


/* ( DBENTRYP_V )     hb_fptCloseMemFile    : NULL */

/*
 * Create a memo file in the WorkArea.
 * ( DBENTRYP_VP )    hb_fptCreateMemFile
 */
static ERRCODE hb_fptCreateMemFile( FPTAREAP pArea, LPDBOPENINFO pCreateInfo )
{
   FPTHEADER fptHeader;
   ULONG ulNextBlock, ulSize, ulLen;
   BOOL bRetry;
   PHB_ITEM pError;

   HB_TRACE(HB_TR_DEBUG, ("hb_fptCreateMemFile(%p, %p)", pArea, pCreateInfo));

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

   memset( &fptHeader, 0, sizeof( FPTHEADER ) );
   pArea->uiMemoBlockSize = ( hb_set.HB_SET_MBLOCKSIZE > 0 &&
                              hb_set.HB_SET_MBLOCKSIZE < 0xFFFF ) ?
                            hb_set.HB_SET_MBLOCKSIZE : FPT_DEFBLOCKSIZE;
   pArea->bMemoType = MEMO_FPT_HB;
   /* pArea->bMemoType = MEMO_FPT_SIX; */

   ulNextBlock = ( sizeof( FPTHEADER ) + pArea->uiMemoBlockSize - 1 ) / pArea->uiMemoBlockSize;
   HB_PUT_BE_ULONG( fptHeader.nextBlock, ulNextBlock );
   HB_PUT_BE_USHORT( fptHeader.blockSize, pArea->uiMemoBlockSize );
   if ( pArea->bMemoType == MEMO_FPT_SIX ||
        pArea->bMemoType == MEMO_FPT_SIXHB )
   {
      strcpy( ( char *) fptHeader.signature1, "SIxMemo" );
      ulSize = 512;
   }
   else
   {
      strcpy( ( char *) fptHeader.signature1, "Harbour" );
      strcpy( ( char *) fptHeader.signature2, "FlexFile3\003" );
      ulSize = sizeof( FPTHEADER );
   }
   if( hb_fsWrite( pArea->hMemoFile, ( BYTE * ) &fptHeader, ( USHORT) ulSize ) != ( USHORT ) ulSize )
      return FAILURE;

   memset( &fptHeader, 0, sizeof( FPTHEADER ) );
   ulLen = ulNextBlock * pArea->uiMemoBlockSize - ulSize;
   while ( ulLen > 0 )
   {
      ulSize = HB_MIN( ulLen, sizeof( FPTHEADER ) );
      if( hb_fsWrite( pArea->hMemoFile, ( BYTE * ) &fptHeader, ( USHORT ) ulSize ) != ( USHORT ) ulSize )
         return FAILURE;
      ulLen -= ulSize;
   }
   /* trunc file */
   hb_fsWrite( pArea->hMemoFile, NULL, 0 );
   return SUCCESS;
}


/* ( DBENTRYP_SVPB )  hb_fptGetValueFile    : NULL */

/*
 * Open a memo file in the specified WorkArea.
 * ( DBENTRYP_VP )    hb_fptOpenMemFile
 */
static ERRCODE hb_fptOpenMemFile( FPTAREAP pArea, LPDBOPENINFO pOpenInfo )
{
   USHORT uiFlags;
   BOOL bRetry;
   FPTHEADER fptHeader;
   PHB_ITEM pError;

   HB_TRACE(HB_TR_DEBUG, ("hb_fptOpenMemFile(%p, %p)", pArea, pOpenInfo));

   uiFlags = (pOpenInfo->fReadonly ? FO_READ : FO_READWRITE) |
             (pOpenInfo->fShared ? FO_DENYNONE : FO_EXCLUSIVE);
   pError = NULL;

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
      hb_itemRelease( pError );

   if( pArea->hMemoFile == FS_ERROR )
      return FAILURE;

   pArea->uiMemoBlockSize = 0;
   memset( &fptHeader, 0, sizeof( FPTHEADER ) );
   hb_fsSeek( pArea->hMemoFile, 0, FS_SET );
   if ( hb_fsRead( pArea->hMemoFile, ( BYTE * ) &fptHeader, sizeof( FPTHEADER ) ) >= 512 )
   {
      pArea->uiMemoBlockSize = HB_GET_BE_USHORT( fptHeader.blockSize );
      pArea->bMemoType = 0;
      /* Check for compatibility with Harbour memo headers */
      if ( memcmp( fptHeader.signature1, "Harbour", 7 ) == 0 )
      {
         /* hack for detecting old harbour FPT files without FLEX support */
         if ( HB_GET_BE_ULONG( fptHeader.signature2 ) == FPTIT_TEXT )
            pArea->bMemoType = MEMO_FPT_SIXHB;
         else
            pArea->bMemoType = MEMO_FPT_HB;
      }
      /* Check for compatibility with SIX memo headers */
      else if ( memcmp( fptHeader.signature1, "SIxMemo", 7 ) == 0 )
      {
         pArea->bMemoType = MEMO_FPT_SIX;
      }
      /* Check for compatibility with CLIP (www.itk.ru) memo headers */
      else if( memcmp( fptHeader.signature1, "Made by CLIP", 12 ) == 0 )
      {
         pArea->bMemoType = MEMO_FPT_CLIP;
      }
      /* Check for compatibility with Clipper 5.3/FlexFile3 malformed memo headers */
      if ( pArea->bMemoType != MEMO_FPT_SIX &&
           memcmp( fptHeader.signature2, "FlexFile3\003", 10) == 0 )
      {
         pArea->bMemoType = MEMO_FPT_FLEX;
         if ( pArea->uiMemoBlockSize == 0 )
            pArea->uiMemoBlockSize = HB_GET_LE_USHORT( fptHeader.flexSize );
      }
   }

   if ( pArea->uiMemoBlockSize == 0 )
   {
      pError = hb_errNew();
      hb_errPutGenCode( pError, EG_CORRUPTION );
      hb_errPutSubCode( pError, EDBF_CORRUPT );
      hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_CORRUPTION ) );
      hb_errPutFileName( pError, (char *) pOpenInfo->abName );
      SELF_ERROR( ( AREAP ) pArea, pError );
      hb_itemRelease( pError );
      return FAILURE;
   }

   return SUCCESS;
}

/* ( DBENTRYP_SVP )   hb_fptPutValueFile    : NULL */

/*
 * Read the database file header record in the WorkArea.
 * ( DBENTRYP_V )     hb_fptReadDBHeader
 */
static ERRCODE hb_fptReadDBHeader( FPTAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fptReadDBHeader(%p)", pArea));

   if( SUPER_READDBHEADER( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   pArea->fHasMemo = ( pArea->bVersion == 0xF5 );

   return SUCCESS;
}

/*
 * Write the database file header record in the WorkArea.
 * ( DBENTRYP_V )     hb_fptWriteDBHeader
 */
static ERRCODE hb_fptWriteDBHeader( FPTAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fptWriteDBHeader(%p)", pArea));

   if ( pArea->fHasMemo )
      pArea->bVersion = 0xF5;

   return SUPER_WRITEDBHEADER( ( AREAP ) pArea );
}
