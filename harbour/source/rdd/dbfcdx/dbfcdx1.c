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

#define SUPERTABLE ( &cdxSuper )

#include <time.h>
#include <ctype.h>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapirdd.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbdate.h"
#include "hbvm.h"
#include "hbinit.h"
#include "rddsys.ch"

#define LOCK_START                                            0x40000000L
#define LOCK_APPEND                                           0x7FFFFFFEL
#define LOCK_FILE                                             0x3FFFFFFFL
#define MEMO_BLOCK                                                     64
#define CDX_MAX_KEY                                                   240
#define CDX_MAX_TAG_NAME_LEN                                           10
#define CDX_BLOCK_SIZE                                                512
#define CDX_INTERNAL_SPACE                                            500
#define CDX_EXTERNAL_SPACE                                            488
#define CDX_MAX_REC_NUM                                       0x7FFFFFFFL
#define CDX_IGNORE_REC_NUM                                             -1
#define PAGE_ROOT                                                       1
#define PAGE_NODE                                                       2
#define PAGE_LEAF                                                       3
#define TOP_RECORD                                                      1
#define BTTM_RECORD                                                     2
#define PREV_RECORD                                                     3
#define NEXT_RECORD                                                     4
#define SORT_CHUNK_LIMIT                                            16384
#define SORT_ACTIVE_LIST                                                0
#define SORT_END_OF_KEY                                                 1
#define SORT_END_OF_WORD                                                2
#define SORT_STACK_OF_CHAR                                              3

typedef struct _DBFHEADER
{
   BYTE   bVersion;
   BYTE   bYear;
   BYTE   bMonth;
   BYTE   bDay;
   ULONG  ulRecords;
   USHORT uiHeaderLen;
   USHORT uiRecordLen;
   BYTE   bReserved1[ 16 ];
   BYTE   bHasMDX;
   BYTE   bReserved2[ 3 ];
} DBFHEADER;

typedef DBFHEADER * LPDBFHEADER;


typedef struct _DBFFIELD
{
   BYTE bName[ 11 ];
   BYTE bType;
   BYTE bReserved1[ 4 ];
   BYTE bLen;
   BYTE bDec;
   BYTE bReserved2[ 13 ];
   BYTE bHasTag;
} DBFFIELD;

typedef DBFFIELD * LPDBFFIELD;


typedef struct
{
   ULONG  lNextBlock;
   ULONG  lBlockSize;
} MEMOHEADER;

typedef MEMOHEADER * LPMEMOHEADER;


typedef struct _DBFMEMO
{
   BOOL   fChanged;             /* Memo status */
   BYTE * pData;                /* Memo data */
   USHORT uiLen;                /* Len of data */
} DBFMEMO;

typedef DBFMEMO * LPDBFMEMO;


typedef struct _CDXHEADER
{
   LONG   Root;
   LONG   FreePtr;
   LONG   ChgFlag;
   USHORT Key_Lgth;
   BYTE   IndexOpts;
   BYTE   IndexSig;
   BYTE   Reserve3[ 486 ];
   USHORT AscDesc;
   USHORT Reserve4;
   USHORT ForExpLen;
   USHORT Reserve5;
   USHORT KeyExpLen;
   BYTE   KeyPool[ CDX_BLOCK_SIZE ];
} CDXHEADER;

typedef CDXHEADER * LPCDXHEADER;


typedef struct
{
   USHORT FreeSpace;
   LONG   RecNumMask;
   BYTE   DupCntMask;
   BYTE   TrlCntMask;
   BYTE   RecNumBits;
   BYTE   DupCntBits;
   BYTE   TrlCntBits;
   BYTE   ShortBytes;
   BYTE   ExtData[ CDX_EXTERNAL_SPACE ];
} CDXEXTERNAL;


typedef struct
{
   BYTE   IntData[ CDX_INTERNAL_SPACE ];
} CDXINTERNAL;


typedef struct _CDXDATA
{
   USHORT Node_Atr;
   USHORT Entry_Ct;
   LONG   Left_Ptr;
   LONG   Rght_Ptr;
   union
   {
      CDXEXTERNAL External;
      CDXINTERNAL Internal;
   } cdxu;
} CDXDATA;

typedef CDXDATA * LPCDXDATA;


typedef struct
{
   char   Character;
   BYTE   NUse;
   USHORT WordArray;
   USHORT Fill02;
   USHORT LevelLink;
} SORT_A;


typedef struct
{
   BYTE Fill03[ 4 ];
   char ChrStack[ 4 ];
} SORT_B;


typedef struct
{
   LONG Fill04;
   LONG ChrFill;
} SORT_C;


typedef struct
{
   union
   {
      SORT_A A;
      SORT_B B;
      SORT_C C;
   } sortu;
} SORTDATA;

typedef SORTDATA * LPSORTDATA;


typedef struct
{
   LONG       WordCount;
   LONG       RootLink;
   LONG       LevelPtr;
   LONG       PriorPtr;
   LONG       KeyTot;
   LONG       KeyCnt;
   LONG       LastTag;
   LONG *     ChunkList;
   BYTE *     SortBuffer;
   USHORT     SortChunk;
   USHORT     NodeLimit;
   USHORT     NodeMask;
   USHORT     NodeShift;
   USHORT     ChunkSize;
   USHORT     ChunkLimit;
   USHORT     ChunkCur;
   USHORT     NodeCur;
   USHORT     KeySize;
   USHORT     WCur;
   BOOL       Unique;
   BOOL       Ascend;
   BOOL       Closing;
   char       WPch[ 256 ];
   SORTDATA * WAdr;
   LPTAGINFO  CurTag;
   LPCDXDATA  NodeList[ 32 ];
   LPKEYINFO  KeyWork;
   LPKEYINFO  LastKey;
} SORTINFO;

typedef SORTINFO * LPSORTINFO;


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

static RDDFUNCS cdxSuper = { 0 };

static ERRCODE cdxOrderListClear( AREAP pArea );

static LPKEYINFO hb_cdxKeyNew( void );
static void hb_cdxKeyFree( LPKEYINFO pKey );
static int hb_cdxKeyCompare( LPKEYINFO pKey1, LPKEYINFO pKey2, USHORT * EndPos, BOOL Exact );

static LPTAGINFO hb_cdxTagNew( LPINDEXINFO PIF, char * ITN, LONG TagHdr );
static void hb_cdxTagFree( LPTAGINFO pTag );
static void hb_cdxTagIndexTagNew( LPTAGINFO pTag, char * KeyExp, PHB_ITEM pKeyItem,
                                  BYTE bType, USHORT uiLen, char * ForExp,
                                  PHB_ITEM pForItem, BOOL Ascnd, BOOL Uniq );
static void hb_cdxTagDoIndex( LPTAGINFO pTag );
static void hb_cdxTagEmptyIndex( LPTAGINFO pTag );
static void hb_cdxTagTagStore( LPTAGINFO pTag );
static void hb_cdxTagTagOpen( LPTAGINFO pTag, BYTE bCode );
static void hb_cdxTagTagClose( LPTAGINFO pTag );
static LONG hb_cdxTagNewRoot( LPTAGINFO pTag );
static void hb_cdxTagPageLoad( LPTAGINFO pTag, LPPAGEINFO pPage );
static void hb_cdxTagKeyRead( LPTAGINFO pTag, BYTE bTypRead );
static void hb_cdxTagKeyAdd( LPTAGINFO pTag, LPKEYINFO pKey );
static void hb_cdxTagPageStore( LPTAGINFO pTag, LPPAGEINFO PIK );
static void hb_cdxTagExtNodeWrite( LPTAGINFO pTag, LONG PN, LPCDXDATA pData, LPPAGEINFO PIK );
static USHORT hb_cdxTagFillExternalNode( LPTAGINFO pTag, LPCDXDATA pData, LPPAGEINFO PIK,
                                         USHORT kcnt, USHORT ck, LPKEYINFO * p );
static void hb_cdxTagExtNodeBuild( LPTAGINFO pTag, LPCDXDATA pData, LPPAGEINFO pPage );
static void hb_cdxTagTagLoad( LPTAGINFO pTag );
static void hb_cdxTagSetRoot( LPTAGINFO pTag, LPPAGEINFO PIK );
static void hb_cdxTagIntNodeWrite( LPTAGINFO pTag, LONG PN, LPCDXDATA pData, LPPAGEINFO PIK );
static USHORT hb_cdxTagFillInternalNode( LPTAGINFO pTag, LPCDXDATA pData, LPPAGEINFO PIK,
                                         USHORT kcnt, USHORT ck, LPKEYINFO * p );
static void hb_cdxTagIntNodeBuild( LPTAGINFO pTag, LPCDXDATA pData, LPPAGEINFO pPage );
static LONG hb_cdxTagKeyFind( LPTAGINFO pTag, LPKEYINFO pKey );

static LPPAGEINFO hb_cdxPageNew( LPTAGINFO PIT, LPPAGEINFO PIK, LONG FilePosn );
static void hb_cdxPageFree( LPPAGEINFO pPage );
static BOOL hb_cdxPageReadTopKey( LPPAGEINFO pPage );
static BOOL hb_cdxPageReadBottomKey( LPPAGEINFO pPage );
static int hb_cdxPageSeekKey( LPPAGEINFO pPage, LONG lBlock, LPKEYINFO pKey, BOOL bExact );
static void hb_cdxPageInsertKey( LPPAGEINFO pPage, LPKEYINFO pKey, BOOL bAddAfter );
static void hb_cdxPagePageStore( LPPAGEINFO pPage );
static BOOL hb_cdxPageReadNextKey( LPPAGEINFO pPage );
static LPKEYINFO hb_cdxPageGetKey( LPPAGEINFO pPage, USHORT uiKey );
static void hb_cdxPagePageLoad( LPPAGEINFO pPage );
static int hb_cdxPageRetrieveKey( LPPAGEINFO pPage, LPKEYINFO pKey );
static void hb_cdxPageAddNodeKey( LPPAGEINFO pPage, LPKEYINFO pKey );
static int hb_cdxPageSeekNodeTag( LPPAGEINFO pPage, LONG Tag );
static BOOL hb_cdxPageGetChild( LPPAGEINFO pPage, LONG Tag );
static void hb_cdxPageDeleteKey( LPPAGEINFO pPage );

static LPINDEXINFO hb_cdxIndexNew( AREAP pArea );
static void hb_cdxIndexFree( LPINDEXINFO pIndex );
static LONG hb_cdxIndexGetAvailPage( LPINDEXINFO pIndex );
static void hb_cdxIndexResetAvailPage( LPINDEXINFO pIndex );
static void hb_cdxIndexPageRead( LPINDEXINFO pIndex, LONG lPos, void * pBuffer, USHORT uiSize );
static void hb_cdxIndexPageWrite( LPINDEXINFO pIndex, LONG lPos, void * pBuffer, USHORT uiSize );
static void hb_cdxIndexAddTag( LPINDEXINFO pIndex, char * szTagName, char * szKeyExp,
                               PHB_ITEM pKeyItem, BYTE bType, USHORT uiLen, char * szForExp,
                               PHB_ITEM pForItem, BOOL bAscending, BOOL bUnique );

static LPSORTINFO hb_cdxSortNew( LPTAGINFO pTag, BOOL bUnique );
static void hb_cdxSortFree( LPSORTINFO pSort );
static void hb_cdxSortLinkNew( LPSORTINFO pSort, LONG * NewLink );
static void hb_cdxSortGetNewChunk( LPSORTINFO pSort );
static void hb_cdxSortInsertWord( LPSORTINFO pSort, LONG Tag, char * Value );
static void hb_cdxSortStuffKey( LPSORTINFO pSort, LPSORTDATA * wx );
static void hb_cdxSortGetNode( LPSORTINFO pSort, char Character, LONG * NewLink );
static LPSORTDATA hb_cdxSortLinkGet( LPSORTINFO pSort, LONG Value );
static void hb_cdxSortDisplayWord( LPSORTINFO pSort );
static void hb_cdxSortRecurseDict( LPSORTINFO pSort, LONG WPtr, LONG WBgn );
static void hb_cdxSortSendWord( LPSORTINFO pSort, char * Value );
static void hb_cdxSortOutputWord( LPSORTINFO pSort, LONG Tag, char * Value );
static void hb_cdxSortAddToNode( LPSORTINFO pSort, USHORT Lvl, LONG Tag, LONG Link,
                                 LPKEYINFO Value );
static void hb_cdxSortAddExternal( LPSORTINFO pSort, USHORT Lvl, LONG Tag, LONG Link,
                                   LPKEYINFO Value );
static void hb_cdxSortAddInternal( LPSORTINFO pSort, USHORT Lvl, LONG Tag, LONG Link,
                                   LPKEYINFO Value );

static BOOL hb_cdxltoa( LONG lValue, char * szBuffer, USHORT uiLen )
{
   LONG lAbsNumber;
   int iCount, iPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxltoa(%ld, %p, %hu)", lValue, szBuffer, uiLen));

   lAbsNumber = ( lValue > 0 ) ? lValue : - lValue;
   iCount = iPos = uiLen;
   while( iCount-- > 0 )
   {
      szBuffer[ iCount ] = ( '0' + lAbsNumber % 10 );
      lAbsNumber /= 10;
   }

   if( lAbsNumber > 0 )
   {
      memset( szBuffer, ' ', uiLen );
      return FALSE;
   }

   uiLen--;
   for( iCount = 0; iCount < uiLen; iCount++ )
      if( szBuffer[ iCount ] == '0' )
         szBuffer[ iCount ] = ' ';
      else
         break;

   if( lValue < 0 )
   {
      if( szBuffer[ 0 ] != ' ' )
      {
         memset( szBuffer, ' ', iPos );
         return FALSE;
      }
      for( iCount = uiLen; iCount >= 0; iCount-- )
      {
         if( szBuffer[ iCount ] == ' ' )
         {
            szBuffer[ iCount ] = '-';
            break;
         }
      }
   }
   return TRUE;
}

static ULONG hb_cdxSwapBytes( ULONG ulValue )
{
   BYTE * pValue, * pNewValue;
   ULONG ulNewValue;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxSwapBytes(%lu)", ulValue));

   pValue = ( BYTE * ) &ulValue;
   pNewValue = ( BYTE * ) &ulNewValue;
   pNewValue[ 0 ] = pValue[ 3 ];
   pNewValue[ 1 ] = pValue[ 2 ];
   pNewValue[ 2 ] = pValue[ 1 ];
   pNewValue[ 3 ] = pValue[ 0 ];
   return ulNewValue;
}

static LONG hb_cdxMakeMask( BYTE bByte )
{
   BYTE bCount;
   LONG lMask;

   for( lMask = 0, bCount = 1; bCount <= bByte; bCount++,
   lMask = ( lMask << 1 ) + 1 );
   return lMask;
}

static void hb_cdxReadMemo( AREAP pArea, LPDBFMEMO pMemo, ULONG lMemoBlock )
{
   ULONG ulSpaceUsed;
   MEMOHEADER pMemoHeader;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxReadMemo(%p, %p, %lu)", pArea, pMemo, lMemoBlock));

   hb_fsSeek( pArea->lpDataInfo->pNext->hFile, lMemoBlock * MEMO_BLOCK, FS_SET );
   hb_fsRead( pArea->lpDataInfo->pNext->hFile, ( BYTE * ) &pMemoHeader,
              sizeof( MEMOHEADER ) );
   ulSpaceUsed = hb_cdxSwapBytes( pMemoHeader.lBlockSize );
   if( pMemo->uiLen != ulSpaceUsed )
   {
      if( pMemo->uiLen > 0 )
         pMemo->pData = ( BYTE * ) hb_xrealloc( pMemo->pData, ulSpaceUsed + 1 );
      else
         pMemo->pData = ( BYTE * ) hb_xgrab( ulSpaceUsed + 1 );
      pMemo->uiLen = ulSpaceUsed;
   }
   hb_fsRead( pArea->lpDataInfo->pNext->hFile, pMemo->pData, pMemo->uiLen );
}

static BOOL hb_cdxWriteMemo( AREAP pArea, LPDBFMEMO pMemo, ULONG * lNewRecNo )
{
   USHORT uiNumBlocks;
   MEMOHEADER pMemoHeader;
   BYTE * pBuffer;

   HB_TRACE(HB_TR_DEBUG, ("hb_cdxWriteMemo(%p, %p, %p)", pArea, pMemo, lNewRecNo));

   if( !pArea->lpExtendInfo->fExclusive && !pArea->lpDataInfo->fFileLocked &&
       !hb_fsLock( pArea->lpDataInfo->pNext->hFile, LOCK_APPEND - 1, 1, FL_LOCK ) )
      return FALSE;

   uiNumBlocks = 1 + ( pMemo->uiLen + sizeof( MEMOHEADER ) ) / MEMO_BLOCK;
   if( * lNewRecNo > 0 )
   {
      hb_fsSeek( pArea->lpDataInfo->pNext->hFile, * lNewRecNo * MEMO_BLOCK, FS_SET );
      hb_fsRead( pArea->lpDataInfo->pNext->hFile, ( BYTE * ) &pMemoHeader,
                 sizeof( MEMOHEADER ) );
      if( pMemo->uiLen > hb_cdxSwapBytes( pMemoHeader.lBlockSize ) )
         * lNewRecNo = 0;                    /* Not room for data */
   }

   if( * lNewRecNo == 0 )                    /* Add an entry at eof */
   {
      hb_fsSeek( pArea->lpDataInfo->pNext->hFile, 0, FS_SET );
      hb_fsRead( pArea->lpDataInfo->pNext->hFile, ( BYTE * ) &pMemoHeader,
                 sizeof( MEMOHEADER ) );
      * lNewRecNo = hb_cdxSwapBytes( pMemoHeader.lNextBlock );
      pMemoHeader.lNextBlock = hb_cdxSwapBytes( * lNewRecNo + uiNumBlocks );
      hb_fsSeek( pArea->lpDataInfo->pNext->hFile, 0, FS_SET );
      hb_fsWrite( pArea->lpDataInfo->pNext->hFile, ( BYTE * ) &pMemoHeader,
                  sizeof( MEMOHEADER ) );
   }

   hb_fsSeek( pArea->lpDataInfo->pNext->hFile, * lNewRecNo * MEMO_BLOCK, FS_SET );
   pMemoHeader.lNextBlock = hb_cdxSwapBytes( 1 );
   pMemoHeader.lBlockSize = hb_cdxSwapBytes( pMemo->uiLen );
   hb_fsWrite( pArea->lpDataInfo->pNext->hFile, ( BYTE * ) &pMemoHeader,
               sizeof( MEMOHEADER ) );
   if( hb_fsWrite( pArea->lpDataInfo->pNext->hFile, pMemo->pData,
                  pMemo->uiLen ) != pMemo->uiLen )
   {
      if( !pArea->lpExtendInfo->fExclusive && !pArea->lpDataInfo->fFileLocked )
         hb_fsLock( pArea->lpDataInfo->pNext->hFile, LOCK_APPEND - 1, 1, FL_UNLOCK );
      return FALSE;
   }
   uiNumBlocks = ( pMemo->uiLen + sizeof( MEMOHEADER ) ) % MEMO_BLOCK;
   if( uiNumBlocks > 0 )
   {
      pBuffer = ( BYTE * ) hb_xgrab( MEMO_BLOCK );
      memset( pBuffer, 0, MEMO_BLOCK );
      hb_fsWrite( pArea->lpDataInfo->pNext->hFile, pBuffer, MEMO_BLOCK - uiNumBlocks );
      hb_xfree( pBuffer);
   }

   if( !pArea->lpExtendInfo->fExclusive && !pArea->lpDataInfo->fFileLocked )
      hb_fsLock( pArea->lpDataInfo->pNext->hFile, LOCK_APPEND - 1, 1, FL_UNLOCK );
   return TRUE;
}

static LPKEYINFO hb_cdxKeyNew()
{
   LPKEYINFO pKey;

   pKey = ( LPKEYINFO ) hb_xgrab( sizeof( KEYINFO ) );
   pKey->pItem = hb_itemNew( NULL );
   pKey->Tag = pKey->Xtra = 0;
   pKey->pNext = NULL;
   return pKey;
}

static void hb_cdxKeyFree( LPKEYINFO pKey )
{
   hb_itemRelease( pKey->pItem );
   hb_xfree( pKey );
}

static int hb_cdxKeyCompare( LPKEYINFO pKey1, LPKEYINFO pKey2, USHORT * EndPos, BOOL Exact )
{
   int iLimit, iResult;

   * EndPos = 0;
   if( pKey2 == NULL || pKey2->pItem->item.asString.length == 0 )
      return 1;
   if( pKey1 == NULL || pKey1->pItem->item.asString.length == 0 )
      return ( pKey2->pItem->item.asString.length == 0 ) ? 0: -1;
   switch( hb_itemType( pKey1->pItem ) )
   {
      case HB_IT_STRING:
         iLimit = ( pKey1->pItem->item.asString.length >
                    pKey2->pItem->item.asString.length ) ?
                    pKey2->pItem->item.asString.length :
                    pKey1->pItem->item.asString.length;
         do
         {
            iResult = pKey1->pItem->item.asString.value[ * EndPos ] -
                      pKey2->pItem->item.asString.value[ * EndPos ];
            * EndPos += 1;
         } while( iResult == 0 && * EndPos < iLimit );
         if( iResult == 0 )
         {
            * EndPos += 1;
            iResult = pKey1->pItem->item.asString.length -
                      pKey2->pItem->item.asString.length;
         }
         if( iResult < 0 && * EndPos > pKey1->pItem->item.asString.length && !Exact )
            iResult = 0;
         break;

      default:
         printf( "hb_cdxKeyCompare()" );
   }
   if( iResult < 0 )
      return -1;
   else if( iResult > 0 )
      return 1;
   else
      return 0;
}

static LPTAGINFO hb_cdxTagNew( LPINDEXINFO PIF, char * ITN, LONG TagHdr )
{
   LPTAGINFO pTag;

   pTag = ( LPTAGINFO ) hb_xgrab( sizeof( TAGINFO ) );
   memset( pTag, 0, sizeof( TAGINFO ) );
   pTag->TagName = ( char * ) hb_xgrab( CDX_MAX_TAG_NAME_LEN + 1 );
   hb_strncpyUpper( pTag->TagName, ITN, CDX_MAX_TAG_NAME_LEN );
   pTag->Owner = PIF;
   pTag->AscendKey = TRUE;
   pTag->KeyType = 'C';
   pTag->CurKeyInfo = hb_cdxKeyNew();
   if( TagHdr == -1 )
   {
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

static void hb_cdxTagFree( LPTAGINFO pTag )
{
   if( pTag->RootPage != NULL )
   {
      hb_cdxPageFree( pTag->RootPage );
      pTag->RootPage = NULL;
   }
   if( pTag->TagChanged )
      hb_cdxTagTagStore( pTag );
   if( pTag->TagName != NULL )
      hb_xfree( pTag->TagName );
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

static void hb_cdxTagIndexTagNew( LPTAGINFO pTag, char * KeyExp, PHB_ITEM pKeyItem,
                                  BYTE bType, USHORT uiLen, char * ForExp,
                                  PHB_ITEM pForItem, BOOL Ascnd, BOOL Uniq )
{
   CDXHEADER pHeader;

   if( KeyExp != NULL )
   {
      pTag->KeyExpr = ( char * ) hb_xgrab( CDX_MAX_KEY + 1 );
      hb_strncpyUpper( pTag->KeyExpr, KeyExp, CDX_MAX_KEY );
   }
   pTag->pKeyItem = pKeyItem;
   if( ForExp != NULL )
   {
      pTag->ForExpr = ( char * ) hb_xgrab( CDX_MAX_KEY + 1 );
      hb_strncpyUpper( pTag->ForExpr, ForExp, CDX_MAX_KEY );
   }
   pTag->pForItem = pForItem;
   pTag->AscendKey = Ascnd;
   pTag->UniqueKey = Uniq;
   pTag->KeyType = bType;
   pTag->KeyLength = uiLen;
   pTag->MaxKeys = ( CDX_BLOCK_SIZE - 12 ) / ( uiLen + 8 );
   memset( &pHeader, 0, sizeof( CDXHEADER ) );
   hb_cdxIndexPageWrite( pTag->Owner, pTag->TagBlock, &pHeader, sizeof( CDXHEADER ) );
   hb_cdxTagDoIndex( pTag );
}

static void hb_cdxTagDoIndex( LPTAGINFO pTag )
{
   ULONG ulRecNo;
   BOOL bWhileOk;
   LPSORTINFO pSort;
   LPKEYINFO pKey;
   HB_MACRO_PTR pMacro;

   if( pTag->OptFlags & 0x80 )
      hb_cdxTagEmptyIndex( pTag );
   else
   {
      pSort = hb_cdxSortNew( pTag, pTag->UniqueKey );
      pKey = hb_cdxKeyNew();
      for( ulRecNo = 1; ulRecNo <= pTag->Owner->Owner->lpExtendInfo->ulRecCount; ulRecNo++ )
      {
         hb_fsSeek( pTag->Owner->Owner->lpDataInfo->hFile,
                    pTag->Owner->Owner->lpExtendInfo->uiHeaderLen +
                    ( ulRecNo - 1 ) * pTag->Owner->Owner->lpExtendInfo->uiRecordLen,
                    FS_SET );
         hb_fsRead( pTag->Owner->Owner->lpDataInfo->hFile,
                    pTag->Owner->Owner->lpExtendInfo->bRecord,
                    pTag->Owner->Owner->lpExtendInfo->uiRecordLen );
         if( pTag->pForItem != NULL )
            /* TODO: test for expression */
            bWhileOk = TRUE;
         else
            bWhileOk = TRUE;
         if( bWhileOk )
         {
            if( hb_itemType( pTag->pKeyItem ) == HB_IT_BLOCK )
            {

               hb_vmPushSymbol( &hb_symEval );
               hb_vmPush( pTag->pKeyItem );
               hb_vmDo( 0 );
            }
            else
            {
               pMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pTag->pKeyItem );
               hb_macroRun( pMacro );
            }
            hb_itemCopy( pKey->pItem, &hb_stack.Return );
            switch( hb_itemType( pKey->pItem ) )
            {
               case HB_IT_STRING:
                  hb_cdxSortInsertWord( pSort, ulRecNo, pKey->pItem->item.asString.value );
                  break;

               default:
                  printf( "hb_cdxTagDoIndex( LPTAGINFO pTag )" );
            }
         }
      }
      if( pSort->WordCount > 0 )
         hb_cdxSortDisplayWord( pSort );
      else
         hb_cdxTagEmptyIndex( pTag );
      hb_cdxSortFree( pSort );
      hb_cdxKeyFree( pKey );
   }
   pTag->TagChanged = TRUE;
   hb_cdxTagTagStore( pTag );
}

static void hb_cdxTagEmptyIndex( LPTAGINFO pTag )
{
   USHORT uiKeyLength, uiBitCount;
   CDXDATA pData;

   pTag->RootBlock = hb_cdxIndexGetAvailPage( pTag->Owner );
   memset( &pData, 0, sizeof( CDXDATA ) );
   pData.Node_Atr = 3;
   pData.Left_Ptr = pData.Rght_Ptr = -1;
   uiKeyLength = pTag->KeyLength;
   for( uiBitCount = 0; uiKeyLength; uiBitCount++, uiKeyLength >>= 1 );
   pData.cdxu.External.ShortBytes = 3;
   pData.cdxu.External.RecNumBits = 24 - uiBitCount * 2;
   pData.cdxu.External.RecNumMask = hb_cdxMakeMask( pData.cdxu.External.RecNumBits );
   pData.cdxu.External.FreeSpace = CDX_EXTERNAL_SPACE;
   pData.cdxu.External.DupCntBits = pData.cdxu.External.TrlCntBits = uiBitCount;
   pData.cdxu.External.DupCntMask = hb_cdxMakeMask( pData.cdxu.External.DupCntBits );
   pData.cdxu.External.TrlCntMask = hb_cdxMakeMask( pData.cdxu.External.TrlCntBits );
   hb_cdxIndexPageWrite( pTag->Owner, pTag->RootBlock, &pData, sizeof( CDXDATA ) );
}

static void hb_cdxTagTagStore( LPTAGINFO pTag )
{
   USHORT uiForLen;
   CDXHEADER pHeader;

   if( !pTag->TagChanged )
      return;
   pTag->TagChanged = FALSE;
   if( pTag->UniqueKey )
      pTag->OptFlags |= 0x01;
   if( pTag->pForItem != NULL )
      pTag->OptFlags |= 0x08;
   memset( &pHeader, 0, sizeof( CDXHEADER ) );
   pHeader.Root = pTag->RootBlock;
   pHeader.Key_Lgth = pTag->KeyLength;
   pHeader.IndexOpts = pTag->OptFlags;
   pHeader.IndexSig = 1;
   if( !pTag->AscendKey )
      pHeader.AscDesc = 1;
   pHeader.Reserve4 = 1 + ( pTag->KeyExpr == NULL ? 0 : strlen( pTag->KeyExpr ) );
   pHeader.KeyExpLen = pHeader.Reserve4;
   if( pTag->KeyExpr != NULL )
      strcpy( ( char * ) pHeader.KeyPool, pTag->KeyExpr );
   uiForLen = pTag->ForExpr == NULL ? 0 : strlen( pTag->ForExpr );
   if( uiForLen > 0 )
   {
      pHeader.ForExpLen = uiForLen + 1;
      strcpy( ( char * ) pHeader.KeyPool + pHeader.KeyExpLen, pTag->ForExpr );
   }
   else
      pHeader.ForExpLen = 1;
   hb_cdxIndexPageWrite( pTag->Owner, pTag->TagBlock, &pHeader, sizeof( CDXHEADER ) );
}

static void hb_cdxTagTagOpen( LPTAGINFO pTag, BYTE bCode )
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

static void hb_cdxTagTagClose( LPTAGINFO pTag )
{
   if( pTag->RootPage != NULL )
   {
      hb_cdxPageFree( pTag->RootPage );
      pTag->RootPage = NULL;
   }
   if( pTag->TagChanged )
      hb_cdxTagTagStore( pTag );
}

static LONG hb_cdxTagNewRoot( LPTAGINFO pTag )
{
   CDXHEADER pHeader;

   hb_cdxIndexPageRead( pTag->Owner, pTag->TagBlock, &pHeader, sizeof( CDXHEADER ) );
   return pHeader.Root;
}

static void hb_cdxTagPageLoad( LPTAGINFO pTag, LPPAGEINFO pPage )
{
   CDXDATA pData;

   hb_cdxIndexPageRead( pTag->Owner, pPage->Page, &pData, sizeof( CDXDATA ) );
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

static void hb_cdxTagKeyRead( LPTAGINFO pTag, BYTE bTypRead )
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
         if( pTag->pForItem != NULL )
            printf( "hb_cdxTagKeyRead()" );
         else
            pTag->TagBOF = !hb_cdxPageReadTopKey( pTag->RootPage );
         if( pTag->pForItem != NULL )
            printf( "hb_cdxTagTestRange()" );
         if( pTag->TagEOF )
            pTag->TagBOF = TRUE;
         pTag->TagEOF = pTag->TagBOF;
         break;

      case BTTM_RECORD:
         hb_cdxTagTagOpen( pTag, 2 );
         if( pTag->pForItem != NULL )
            printf( "hb_cdxTagKeyRead()" );
         else
            pTag->TagEOF = !hb_cdxPageReadBottomKey( pTag->RootPage );
         if( pTag->pForItem != NULL )
         {
            printf( "hb_cdxTagTestRange()" );
         }
         if( pTag->TagBOF )
            pTag->TagEOF = TRUE;
         pTag->TagBOF = pTag->TagEOF;
         break;

      case NEXT_RECORD:
         while( TRUE )
         {
            pTag->TagEOF = !hb_cdxPageReadNextKey( pTag->RootPage );
            if( pTag->pForItem != NULL )
               printf( "hb_cdxTagKeyRead()" );
            else
               break;
         }
         break;
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

static void hb_cdxTagKeyAdd( LPTAGINFO pTag, LPKEYINFO pKey )
{
   int iSeek;
   LONG lOldXtra;
   LPKEYINFO pNewKey;

   lOldXtra = pKey->Xtra;
   pKey->Xtra = pKey->Tag;
   hb_cdxTagTagOpen( pTag, 0 );
   pNewKey = hb_cdxKeyNew();
   hb_itemCopy( pNewKey->pItem, pKey->pItem );
   pNewKey->Tag = pKey->Tag;
   pNewKey->Xtra = pKey->Xtra;
   if( pTag->KeyType == 'C' )
   {
      while( pNewKey->pItem->item.asString.length > 0 &&
             pNewKey->pItem->item.asString.value[ pNewKey->pItem->item.asString.length - 1 ] == ' ' )
         pNewKey->pItem->item.asString.length--;
         pNewKey->pItem->item.asString.value[ pNewKey->pItem->item.asString.length ] = 0;
   }
   pTag->TagBOF = pTag->TagEOF = FALSE;
   iSeek = hb_cdxPageSeekKey( pTag->RootPage, pNewKey->Tag, pNewKey, TRUE );
   if( iSeek < 1 )
   {
      hb_cdxPageInsertKey( pTag->RootPage, pNewKey, FALSE );
      hb_cdxTagTagOpen( pTag, 0 );
      hb_cdxPageSeekKey( pTag->RootPage, pNewKey->Tag, pNewKey, FALSE );
      iSeek = hb_cdxPageRetrieveKey( pTag->RootPage, pTag->CurKeyInfo );
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

static void hb_cdxTagPageStore( LPTAGINFO pTag, LPPAGEINFO PIK )
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

static void hb_cdxTagExtNodeWrite( LPTAGINFO pTag, LONG PN, LPCDXDATA pData,
                                   LPPAGEINFO PIK )
{
   USHORT uiKeyLength, uiBitCount, cd, kcnt, lm, uiCount, ck, na;
   LONG sr, rp, lp, NPN, TmpTag;
   LPKEYINFO p, q;

   if( pTag->OptFlags & 0x80 )
      sr = hb_fsSeek( pTag->Owner->DiskFile, 0, FS_END );
   else
      sr = pTag->Owner->Owner->lpExtendInfo->ulRecCount;
   uiKeyLength = pTag->KeyLength;
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
   PIK->DCBits = PIK->TCBits = uiBitCount;
   PIK->DCMask = hb_cdxMakeMask( PIK->DCBits );
   PIK->TCMask = hb_cdxMakeMask( PIK->TCBits );
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
      cd = p->pItem->item.asString.length - cd;
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
      NPN = hb_cdxIndexGetAvailPage( pTag->Owner );
      TmpTag = p->Tag;
      p->Tag = NPN;
      hb_cdxPageAddNodeKey( PIK, p );
      p->Tag = TmpTag;
      if( PIK->PageType == PAGE_ROOT )
         PIK->PageType = PAGE_NODE;
      hb_cdxIndexPageWrite( pTag->Owner, NPN, pData, CDX_BLOCK_SIZE );
      if( lp > 0 )
      {
         hb_cdxIndexPageRead( pTag->Owner, lp, pData, CDX_BLOCK_SIZE );
         pData->Rght_Ptr = NPN;
         hb_cdxIndexPageWrite( pTag->Owner, lp, pData, CDX_BLOCK_SIZE );
      }
      memset( pData, 0, CDX_BLOCK_SIZE );
      pData->Node_Atr = na;
      pData->Rght_Ptr = rp;
      pData->Left_Ptr = NPN;
      pData->Entry_Ct = 0;
      kcnt = PIK->uiKeys;
   }
   hb_cdxTagFillExternalNode( pTag, pData, PIK, kcnt, ck, &p );
   pData->cdxu.External.FreeSpace = PIK->Space;
   hb_cdxIndexPageWrite( pTag->Owner, PN, pData, sizeof( CDXDATA ) );
}

static USHORT hb_cdxTagFillExternalNode( LPTAGINFO pTag, LPCDXDATA pData,
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
      ct = pTag->KeyLength - ( * p )->pItem->item.asString.length;
      if( q != NULL )
      {
         hb_cdxKeyCompare( * p,  q, &cd, TRUE );
         if( cd > 0 )
            cd--;
      }
      else
         cd = 0;
      q = * p;
      PIK->Space -= pTag->KeyLength + PIK->ReqByte - cd - ct;
      v = i * PIK->ReqByte;
      c = ( ct << ( 16 - PIK->TCBits ) ) | ( cd << ( 16 - PIK->TCBits - PIK->DCBits ) );
      memcpy( &pData->cdxu.External.ExtData[ v + PIK->ReqByte - 2 ], &c, 2 );
      memcpy( &r, &pData->cdxu.External.ExtData[ v ], 4 );
      r &= m;
      r |= ( * p )->Tag;
      memcpy( &pData->cdxu.External.ExtData[ v ], &r, 4 );
      k -= pTag->KeyLength - cd - ct;
      if( pTag->KeyLength - cd - ct > 0 )
         memcpy( &pData->cdxu.External.ExtData[ k ], ( * p )->pItem->item.asString.value + cd,
                 pTag->KeyLength - cd - ct );
      i++;
      ck++;
      pData->Entry_Ct++;
   }
   return ck;
}

static void hb_cdxTagExtNodeBuild( LPTAGINFO pTag, LPCDXDATA pData, LPPAGEINFO PIK )
{
   USHORT k, i, v, c, t, d;
   LONG r;
   LPKEYINFO pKey, pLastKey;
   static char szBuffer[ CDX_MAX_KEY + 1 ];

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
      k -= pTag->KeyLength - d - t;
      if( pTag->KeyLength - d - t > 0 )
         memcpy( &szBuffer[ d ], &pData->cdxu.External.ExtData[ k ],
                 pTag->KeyLength - d - t );
      szBuffer[ pTag->KeyLength - t ] = 0;
      pKey = hb_cdxKeyNew();
      pKey->Tag = r;
      pKey->Xtra = r;
      hb_itemPutC( pKey->pItem, szBuffer );
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

static void hb_cdxTagTagLoad( LPTAGINFO pTag )
{
   CDXHEADER pHeader;
   HB_MACRO_PTR pMacro;

   hb_cdxIndexPageRead( pTag->Owner, pTag->TagBlock, &pHeader, sizeof( CDXHEADER ) );
   pTag->RootBlock = pHeader.Root;
   if( pTag->RootBlock == 0 || pTag->RootBlock % CDX_BLOCK_SIZE > 0 ||
       pTag->RootBlock > hb_fsSeek( pTag->Owner->DiskFile, 0, FS_END ) ||
       pHeader.Key_Lgth > CDX_MAX_KEY )
      return;
   pTag->KeyLength = pHeader.Key_Lgth;
   pTag->MaxKeys = ( CDX_BLOCK_SIZE - 12 ) / ( pTag->KeyLength + 8 );
   pTag->OptFlags = pHeader.IndexOpts;
   pTag->UniqueKey = ( pTag->OptFlags & 0x01 );
   pTag->AscendKey = ( pHeader.AscDesc == 0 );
   pTag->KeyExpr = ( char * ) hb_xgrab( CDX_MAX_KEY + 1 );
   hb_strncpyUpper( pTag->KeyExpr, ( char * ) pHeader.KeyPool, CDX_MAX_KEY );
   if( pTag->OptFlags < 0x80 && pTag->KeyExpr[ 0 ] == 0 )
      return;
   if( pTag->OptFlags & 0x80 )
      return;
   SELF_COMPILE( pTag->Owner->Owner, ( BYTE * ) pTag->KeyExpr );
   pTag->pKeyItem = pTag->Owner->Owner->valResult;
   pTag->Owner->Owner->valResult = NULL;
   pMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pTag->pKeyItem );
   hb_macroRun( pMacro );
   switch( hb_itemType( &hb_stack.Return ) )
   {
      case HB_IT_INTEGER:
      case HB_IT_LONG:
      case HB_IT_DOUBLE:
         pTag->KeyType = 'N';
         pTag->KeyLength = 10;
        break;

     case HB_IT_DATE:
         pTag->KeyType = 'D';
         pTag->KeyLength = 8;
         break;

      case HB_IT_LOGICAL:
         pTag->KeyType = 'C';
         pTag->KeyLength = 1;
         break;

      case HB_IT_STRING:
         pTag->KeyType = 'C';
         pTag->KeyLength = hb_stack.Return.item.asString.length > CDX_MAX_KEY ? CDX_MAX_KEY :
                           hb_stack.Return.item.asString.length;
         break;
   }

   if( pHeader.KeyPool[ strlen( pTag->KeyExpr ) + 1 ] == 0 )
      return;
   pTag->ForExpr = ( char * ) hb_xgrab( CDX_MAX_KEY + 1 );
   hb_strncpyUpper( pTag->ForExpr, ( const char * ) pHeader.KeyPool +
                    strlen( pTag->KeyExpr ) + 1, CDX_MAX_KEY );
   SELF_COMPILE( pTag->Owner->Owner, ( BYTE * ) pTag->ForExpr );
   pTag->pForItem = pTag->Owner->Owner->valResult;
   pTag->Owner->Owner->valResult = NULL;
   pMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pTag->pForItem );
   hb_macroRun( pMacro );
   if( hb_itemType( &hb_stack.Return ) != HB_IT_LOGICAL )
   {
      hb_macroDelete( pMacro );
      hb_itemRelease( pTag->pForItem );
      pTag->pForItem = NULL;
      hb_xfree( pTag->ForExpr );
      pTag->ForExpr = NULL;
   }
}

static void hb_cdxTagSetRoot( LPTAGINFO pTag, LPPAGEINFO PIK )
{
   LONG NRN, TmpTag;
   LPKEYINFO p, TmpStr;

   PIK->Owner = hb_cdxPageNew( pTag, NULL, 0 );
   NRN = hb_cdxIndexGetAvailPage( pTag->Owner );
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

static void hb_cdxTagIntNodeWrite( LPTAGINFO pTag, LONG PN, LPCDXDATA pData, LPPAGEINFO PIK )
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
         NPN = hb_cdxIndexGetAvailPage( pTag->Owner );
         TmpTag = p->Tag;
         p->Tag = NPN;
         hb_cdxPageAddNodeKey( PIK, p );
         p->Tag = TmpTag;
         if( PIK->PageType == PAGE_ROOT )
            PIK->PageType = PAGE_NODE;
         hb_cdxIndexPageWrite( pTag->Owner, NPN, pData, CDX_BLOCK_SIZE );
         if( lp > 0 )
         {
            hb_cdxIndexPageRead( pTag->Owner, lp, pData, CDX_BLOCK_SIZE );
            pData->Rght_Ptr = NPN;
            hb_cdxIndexPageWrite( pTag->Owner, lp, pData, CDX_BLOCK_SIZE );
         }
         memset( pData, 32, CDX_BLOCK_SIZE );
         pData->Node_Atr = na;
         pData->Rght_Ptr = rp;
         pData->Left_Ptr = NPN;
         pData->Entry_Ct = 0;
         kcnt = pTag->MaxKeys;
      }
      hb_cdxTagFillInternalNode( pTag, pData, PIK, kcnt, ck, &p );
   }
   hb_cdxIndexPageWrite( pTag->Owner, PN, pData, CDX_BLOCK_SIZE );
}

static USHORT hb_cdxTagFillInternalNode( LPTAGINFO pTag, LPCDXDATA pData,
                                         LPPAGEINFO PIK, USHORT kcnt,
                                         USHORT ck, LPKEYINFO * p )
{
   USHORT i, v;
   LONG r;

   i = 0;
   memset( pData->cdxu.Internal.IntData, ( pTag->KeyType == 'C' ) ? 32 : 0,
           sizeof( pData->cdxu.Internal.IntData ) );
   while( i < kcnt && ck < PIK->uiKeys )
   {
      * p = hb_cdxPageGetKey( PIK, ck );
      v = i * ( pTag->KeyLength + 8 );
      memcpy( &pData->cdxu.Internal.IntData[ v ],
              ( * p )->pItem->item.asString.value,
              ( * p )->pItem->item.asString.length );
      v += pTag->KeyLength;
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

static void hb_cdxTagIntNodeBuild( LPTAGINFO pTag, LPCDXDATA pData, LPPAGEINFO pPage )
{
   USHORT i, v;
   LONG r, n;
   LPKEYINFO pKey, pLastKey;
   static char szBuffer[ CDX_MAX_KEY + 1 ];

   i = 0;
   while( i < pData->Entry_Ct )
   {
      v = i * ( pTag->KeyLength + 8 );
      memmove( szBuffer, pData->cdxu.Internal.IntData + v, pTag->KeyLength );
      szBuffer[ pTag->KeyLength ] = 0;
      v += pTag->KeyLength;
      memcpy( &r, &pData->cdxu.Internal.IntData[ v ], 4 );
      r = hb_cdxSwapBytes( r );
      memcpy( &n, &pData->cdxu.Internal.IntData[ v + 4 ], 4 );
      n = hb_cdxSwapBytes( n );
      pKey = hb_cdxKeyNew();
      if( pTag->KeyType == 'C' )
      {
         v = strlen( szBuffer );
         while( v > 0 && szBuffer[ v - 1 ] == 32 )
            v--;
         szBuffer[ v ] = 0;
      }
      else
      {
         printf( "hb_cdxTagIntNodeBuild()" );
      }
      hb_itemPutC( pKey->pItem, szBuffer );
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

static LONG hb_cdxTagKeyFind( LPTAGINFO pTag, LPKEYINFO pKey )
{
   int K;
   LPKEYINFO stot;

   pTag->CurKeyInfo->Tag = 0;
   hb_cdxTagTagOpen( pTag, 0 );
   if( pTag->RootPage == NULL )
      return 0;
   stot = hb_cdxKeyNew();
   hb_itemCopy( stot->pItem, pKey->pItem );
   stot->Tag = pKey->Tag;
   stot->Xtra = pKey->Xtra;
   if( pTag->KeyType == 'C' )
   {
      while( stot->pItem->item.asString.length > 0 &&
             stot->pItem->item.asString.value[ stot->pItem->item.asString.length - 1 ] == ' ' )
         stot->pItem->item.asString.length--;
         stot->pItem->item.asString.value[ stot->pItem->item.asString.length ] = 0;
   }
   pTag->TagBOF = pTag->TagEOF = FALSE;
   K = hb_cdxPageSeekKey( pTag->RootPage, stot->Tag, stot, FALSE );
   hb_cdxKeyFree( stot );
   if( K == 0 )
   {
      hb_cdxPageRetrieveKey( pTag->RootPage, pTag->CurKeyInfo );
      if( pTag->pForItem == NULL )
         return pTag->CurKeyInfo->Tag;
      else
         /* TODO: test for expression */
         pTag->TagEOF = TRUE;
   }
   else if( K < 0 )
   {
      hb_cdxPageRetrieveKey( pTag->RootPage, pTag->CurKeyInfo );
      if( pTag->pForItem != NULL )
         /* TODO: test for expression */
         pTag->TagEOF = TRUE;
   }
   else
      pTag->TagEOF = TRUE;
   return 0;
}

static LPPAGEINFO hb_cdxPageNew( LPTAGINFO PIT, LPPAGEINFO PIK, LONG FilePosn )
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
      hb_itemCopy( pPage->TagParent->CurKeyInfo->pItem, pKey->pItem );
      pPage->TagParent->CurKeyInfo->Tag = pKey->Tag;
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
      printf( "hb_cdxPageReadBottomKey()" );
      return TRUE;
   }
   else
   {
      hb_itemCopy( pPage->TagParent->CurKeyInfo->pItem, pKey->pItem );
      pPage->TagParent->CurKeyInfo->Tag = pKey->Tag;
      return TRUE;
   }
}

static int hb_cdxPageSeekKey( LPPAGEINFO pPage, LONG lBlock, LPKEYINFO pKey, BOOL bExact )
{
   int k;
   USHORT dav;
   LPKEYINFO p;

   k = 1;
   bExact = ( bExact || pPage->TagParent->KeyType != 'C' );
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
      hb_itemCopy( pNewKey->pItem, pKey->pItem );
      pNewKey->Tag = pKey->Tag;
      pNewKey->Xtra = pKey->Xtra;
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
      pPage->Page = hb_cdxIndexGetAvailPage( pPage->TagParent->Owner );
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
         hb_itemCopy( pPage->TagParent->CurKeyInfo->pItem, p->pItem );
         pPage->TagParent->CurKeyInfo->Tag = p->Tag;
         return TRUE;
      }
      else
      {
         pPage->CurKey = pPage->uiKeys;
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

static int hb_cdxPageRetrieveKey( LPPAGEINFO pPage, LPKEYINFO pKey )
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
      hb_itemCopy( pKey->pItem, pNewKey->pItem );
      pKey->Tag = pNewKey->Tag;
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
   if( pPage->TagParent->Owner->NextAvail > 0 &&
       Tag > pPage->TagParent->Owner->NextAvail )
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
            printf( "ReplaceNodeKey();" );
            p->Tag = TempTag;
         }
         else
            printf( "DeleteNodeKey();" );
      }
   }
}

static LPINDEXINFO hb_cdxIndexNew( AREAP pArea )
{
   LPINDEXINFO pIndex;

   pIndex = ( LPINDEXINFO ) hb_xgrab( sizeof( INDEXINFO ) );
   memset( pIndex, 0, sizeof( INDEXINFO ) );
   pIndex->DiskFile = FS_ERROR;
   pIndex->Owner = pArea;
   pIndex->NextAvail = -1;
   return pIndex;
}

static void hb_cdxIndexFree( LPINDEXINFO pIndex )
{
   LPTAGINFO pTag;

   /* Free Compound tag */
   if( pIndex->CompoundTag != NULL )
   {
      hb_cdxTagTagClose( pIndex->CompoundTag );
      hb_cdxTagFree( pIndex->CompoundTag );
   }

   /* Free all tags */
   while( pIndex->TagList )
   {
      pTag = pIndex->TagList;
      pIndex->TagList = pTag->pNext;
      hb_cdxTagFree( pTag );
   }

   /* Close file */
   if( pIndex->DiskFile != FS_ERROR )
      hb_fsClose( pIndex->DiskFile );

   hb_xfree( pIndex );
}

static LONG hb_cdxIndexGetAvailPage( LPINDEXINFO pIndex )
{
   if( pIndex->NextAvail == -1 )
      hb_cdxIndexResetAvailPage( pIndex );
   pIndex->NextAvail += CDX_BLOCK_SIZE;
   return pIndex->NextAvail - CDX_BLOCK_SIZE;
}

static void hb_cdxIndexResetAvailPage( LPINDEXINFO pIndex )
{
   pIndex->NextAvail = hb_fsSeek( pIndex->DiskFile, 0, FS_END );
}

static void hb_cdxIndexPageRead( LPINDEXINFO pIndex, LONG lPos, void * pBuffer,
                                 USHORT uiSize )
{
   if( hb_fsSeek( pIndex->DiskFile, lPos, FS_SET ) == lPos )
      hb_fsRead( pIndex->DiskFile, ( BYTE * ) pBuffer, uiSize );
}

static void hb_cdxIndexPageWrite( LPINDEXINFO pIndex, LONG lPos, void * pBuffer,
                                  USHORT uiSize )
{
   if( hb_fsSeek( pIndex->DiskFile, lPos, FS_SET ) == lPos )
      hb_fsWrite( pIndex->DiskFile, ( BYTE * ) pBuffer, uiSize );
}

static void hb_cdxIndexAddTag( LPINDEXINFO pIndex, char * szTagName, char * szKeyExp,
                               PHB_ITEM pKeyItem, BYTE bType, USHORT uiLen, char * szForExp,
                               PHB_ITEM pForItem, BOOL bAscending, BOOL bUnique )
{
   LPTAGINFO pTag, pLastTag;
   LPKEYINFO pKey;

   hb_cdxTagTagOpen( pIndex->CompoundTag, 0 );
   pKey = hb_cdxKeyNew();
   hb_itemPutC( pKey->pItem, szTagName );
   pTag = pIndex->TagList;
   pLastTag = NULL;
   while( pTag != NULL )
   {
      if( hb_stricmp( pTag->TagName, szTagName ) == 0 )
      {
         pKey->Tag = pTag->TagBlock;
         if( hb_cdxTagKeyFind( pIndex->CompoundTag, pKey ) > 0 )
            hb_cdxPageDeleteKey( pIndex->CompoundTag->RootPage );
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
   hb_itemPutC( pKey->pItem, pTag->TagName );
   pKey->Tag = pTag->TagBlock;
   hb_cdxTagKeyAdd( pIndex->CompoundTag, pKey );
   hb_cdxKeyFree( pKey );
   pIndex->CompoundTag->RootPage->Changed = TRUE;
   hb_cdxTagTagClose( pIndex->CompoundTag );
}

static LPSORTINFO hb_cdxSortNew( LPTAGINFO pTag, BOOL bUnique )
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
   pSort->KeyTot = pTag->Owner->Owner->lpExtendInfo->ulRecCount;
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
            hb_cdxIndexPageWrite( pSort->CurTag->Owner, pa, pSort->NodeList[ usCount ],
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

static void hb_cdxSortInsertWord( LPSORTINFO pSort, LONG Tag, char * Value )
{
   char s[ 34 ];
   USHORT w, cc, nc, EOK;
   SHORT v;
   LPSORTDATA wx;

   hb_cdxltoa( Tag, s + 1, 10 );
   w = strlen( Value );
   if( pSort->NodeLimit - pSort->NodeCur < w + strlen( s + 1 ) + 1 )
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
   strcpy( pSort->WPch, Value );
   v = strlen( pSort->WPch );
   if( v > 0 )
   {
      if( v > pSort->KeySize )
         pSort->KeySize = v;
      v--;
   }
   while( v >= 0 && pSort->WPch[ v ] == ' ' )
   {
      pSort->WPch[ v ] = 0;
      v--;
   }
   v++;
   EOK = v;
   if( !pSort->Unique )
   {
      s[ 0 ] = ( char ) strlen( s + 1 );
      memcpy( &pSort->WPch[ v ], s, s[ 0 ] + 1 );
      v += ( SHORT ) ( s[ 0 ] + 1 );
      pSort->WPch[ v ] = 0;
   }
   pSort->LevelPtr = pSort->RootLink;
   pSort->PriorPtr = 0;
   pSort->WCur = 0;
   do
      hb_cdxSortStuffKey( pSort, &wx );
   while( pSort->WPch[ pSort->WCur ] != 0 );
   if( pSort->Unique )
   {
      if( wx->sortu.A.NUse == SORT_END_OF_KEY )
         return;
      wx->sortu.A.NUse = SORT_END_OF_KEY;
      if( EOK == 0 )
         EOK++;
      memcpy( &pSort->WPch[ EOK ], s, s[ 0 ] + 1 );
      v = EOK + s[ 0 ] + 1;
      pSort->WPch[ v ] = 0;
      do
         hb_cdxSortStuffKey( pSort, &wx );
      while( pSort->WPch[ pSort->WCur ] != 0 );
   }
   wx->sortu.A.NUse = SORT_END_OF_WORD;
}

static void hb_cdxSortStuffKey( LPSORTINFO pSort, LPSORTDATA * wx )
{
   USHORT w;
   SHORT v;
   LONG p1;
   LPSORTDATA x;

   hb_cdxSortGetNode( pSort, pSort->WPch[ pSort->WCur ], &p1 );
   * wx = hb_cdxSortLinkGet( pSort, p1 );
   pSort->WCur++;
   if( pSort->LevelPtr == 0 )
   {
      if( pSort->PriorPtr > 0 )
      {
         x = hb_cdxSortLinkGet( pSort, pSort->PriorPtr );
         x->sortu.A.WordArray = p1;
      }
      w = strlen( pSort->WPch );
      v = w - pSort->WCur - 1;
      if( v > 0 )
      {
         if( v > 4 )
            v = 4;
         memcpy( ( * wx )->sortu.B.ChrStack, &pSort->WPch[ pSort->WCur ], v );
         ( * wx )->sortu.A.NUse = SORT_STACK_OF_CHAR;
         pSort->WCur += v;
      }
   }
   pSort->PriorPtr = p1;
   pSort->LevelPtr = ( * wx )->sortu.A.WordArray;
}

static void hb_cdxSortGetNode( LPSORTINFO pSort, char Character, LONG * NewLink )
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
      return;
   }
   p = pSort->LevelPtr;
   px = hb_cdxSortLinkGet( pSort, pSort->LevelPtr );
   q = pSort->PriorPtr;
   if( px->sortu.A.NUse == SORT_STACK_OF_CHAR )
   {
      hb_cdxSortLinkNew( pSort, &r );
      c = px->sortu.A.Character;
      qx = hb_cdxSortLinkGet( pSort, q );
      qx->sortu.A.WordArray = r;
      rx = hb_cdxSortLinkGet( pSort, r );
      rx->sortu.A.Character = c;
      rx->sortu.A.WordArray = p;
      px = hb_cdxSortLinkGet( pSort, p );
      px->sortu.A.Character = px->sortu.B.ChrStack[ 0 ];
      memmove( &px->sortu.B.ChrStack[ 0 ], &px->sortu.B.ChrStack[ 1 ], 3 );
      px->sortu.B.ChrStack[ 3 ] = 0;
      if( px->sortu.C.ChrFill != 0 )
         px->sortu.A.NUse = SORT_STACK_OF_CHAR;
      else
         px->sortu.A.NUse = SORT_ACTIVE_LIST;
      p = r;
      px = hb_cdxSortLinkGet( pSort, p );
   }
   if( Character > px->sortu.A.Character )
      df = 1;
   else if( Character < px->sortu.A.Character )
      df = -1;
   else
      df = 0;
   if( !pSort->Ascend && Character > 0 && px->sortu.A.Character > 0 )
      df = -df;
   while( px->sortu.A.LevelLink != 0 && df > 0 )
   {
      q = p;
      p = px->sortu.A.LevelLink;
      px = hb_cdxSortLinkGet( pSort, p );
      if( px->sortu.A.NUse == SORT_STACK_OF_CHAR )
      {
         hb_cdxSortLinkNew( pSort, &r );
         c = px->sortu.A.Character;
         qx = hb_cdxSortLinkGet( pSort, q );
         qx->sortu.A.WordArray = r;
         rx = hb_cdxSortLinkGet( pSort, r );
         rx->sortu.A.Character = c;
         rx->sortu.A.WordArray = p;
         px = hb_cdxSortLinkGet( pSort, p );
         px->sortu.A.Character = px->sortu.B.ChrStack[ 0 ];
         memmove( &px->sortu.B.ChrStack[ 0 ], &px->sortu.B.ChrStack[ 1 ], 3 );
         px->sortu.B.ChrStack[ 3 ] = 0;
         if( px->sortu.C.ChrFill != 0 )
            px->sortu.A.NUse = SORT_STACK_OF_CHAR;
         else
            px->sortu.A.NUse = SORT_ACTIVE_LIST;
         p = r;
         px = hb_cdxSortLinkGet( pSort, p );
      }
      if( Character > px->sortu.A.Character )
         df = 1;
      else if( Character < px->sortu.A.Character )
         df = -1;
      else
         df = 0;
      if( !pSort->Ascend && Character > 0 && px->sortu.A.Character > 0 )
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
            qx->sortu.A.WordArray = r;
         else
            qx->sortu.A.LevelLink = r;
      }
      else
      {
         p = px->sortu.A.LevelLink;
         px->sortu.A.LevelLink = r;
      }
      rx = hb_cdxSortLinkGet( pSort, r );
      rx->sortu.A.LevelLink = p;
      rx->sortu.A.Character = Character;
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
   WCnt = strlen( pSort->WPch );
   pSort->WAdr = hb_cdxSortLinkGet( pSort, WPtr );
   if( pSort->WAdr->sortu.A.Character != 0 )
   {
      pSort->WPch[ WCnt ] = pSort->WAdr->sortu.A.Character;
      pSort->WPch[ WCnt + 1 ] = 0;
   }
   if( pSort->WAdr->sortu.A.NUse == SORT_STACK_OF_CHAR )
   {
      memcpy( &pSort->WPch[ strlen( pSort->WPch ) ], pSort->WAdr->sortu.B.ChrStack, 4 );
      pSort->WPch[ WCnt + 5 ] = 0;
   }
   if( pSort->WAdr->sortu.A.NUse == SORT_END_OF_WORD )
      hb_cdxSortSendWord( pSort, pSort->WPch );
   else
   {
      if( pSort->WAdr->sortu.A.WordArray != 0 )
         hb_cdxSortRecurseDict( pSort, pSort->WAdr->sortu.A.WordArray, WBgn );
      pSort->WAdr = hb_cdxSortLinkGet( pSort, WPtr );
   }
   pSort->WPch[ WCnt ] = 0;
   if( pSort->WAdr->sortu.A.LevelLink != 0 && pSort->WAdr->sortu.A.NUse != SORT_STACK_OF_CHAR )
      hb_cdxSortRecurseDict( pSort, pSort->WAdr->sortu.A.LevelLink, WCnt );
}

static void hb_cdxSortSendWord( LPSORTINFO pSort, char * Value )
{
   LONG Tag;
   BYTE OldByte;
   char * pce;

   pce = Value + strlen( Value ) - 1;
   while( pce[ 0 ] > 31 )
      pce--;
   Tag = atol( pce + 1 );
   OldByte = pce[ 0 ];
   pce[ 0 ] = 0;
   hb_cdxSortOutputWord( pSort, Tag, Value );
   pce[ 0 ] = OldByte;
}

static void hb_cdxSortOutputWord( LPSORTINFO pSort, LONG Tag, char * Value )
{
   pSort->KeyCnt++;
   hb_itemPutC( pSort->KeyWork->pItem, Value );
   if( pSort->CurTag->KeyType != 'C' )
      printf( "pSort->CurTag->AdjustValue( &KeyWork );" );
   hb_cdxSortAddToNode( pSort, 0, Tag, Tag, pSort->KeyWork );
   pSort->LastTag = Tag;
   hb_itemCopy( pSort->LastKey->pItem, pSort->KeyWork->pItem );
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
         i = pSort->CurTag->KeyLength;
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
            pSort->NodeList[ 0 ]->cdxu.External.TrlCntBits = bitcnt;
         pSort->NodeList[ 0 ]->cdxu.External.DupCntMask =
            hb_cdxMakeMask( pSort->NodeList[ 0 ]->cdxu.External.DupCntBits );
         pSort->NodeList[ 0 ]->cdxu.External.TrlCntMask =
            hb_cdxMakeMask( pSort->NodeList[ 0 ]->cdxu.External.TrlCntBits );
      }
      pSort->NodeList[ Lvl ]->Left_Ptr = -1;
      pSort->NodeList[ Lvl ]->Rght_Ptr = hb_cdxIndexGetAvailPage( pSort->CurTag->Owner );
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
      hb_itemPutC( pSort->LastKey->pItem, "" );
   }
   m = ~pSort->NodeList[ Lvl ]->cdxu.External.RecNumMask;
   ct = ( USHORT ) ( pSort->CurTag->KeyLength -
        Value->pItem->item.asString.length );
   hb_cdxKeyCompare( Value, pSort->LastKey, &cd, TRUE );
   if( cd > 0 )
      cd -= ( USHORT ) 1;
   v = ( USHORT ) ( pSort->NodeList[ Lvl ]->Entry_Ct *
       pSort->NodeList[ Lvl ]->cdxu.External.ShortBytes );
   k = ( USHORT ) ( pSort->NodeList[ Lvl ]->cdxu.External.FreeSpace + v );
   pSort->NodeList[ Lvl ]->cdxu.External.FreeSpace -=
      ( USHORT ) ( pSort->CurTag->KeyLength +
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
   k -= ( USHORT ) ( pSort->CurTag->KeyLength - cd - ct );
   if( pSort->CurTag->KeyLength - cd - ct > 0 )
      memcpy( &pSort->NodeList[ Lvl ]->cdxu.External.ExtData[ k ],
              Value->pItem->item.asString.value + cd,
              pSort->CurTag->KeyLength - cd - ct );
   pSort->NodeList[ Lvl ]->Entry_Ct++;
   if( pSort->NodeList[ Lvl ]->cdxu.External.FreeSpace <
       ( pSort->CurTag->KeyLength + 8 +
       pSort->NodeList[ Lvl ]->cdxu.External.ShortBytes ) * 2 )
   {
      pa = pSort->NodeList[ Lvl ]->Rght_Ptr;
      if( pSort->KeyCnt < pSort->KeyTot )
         pSort->NodeList[ Lvl ]->Rght_Ptr = hb_cdxIndexGetAvailPage( pSort->CurTag->Owner );
      else
         pSort->NodeList[ Lvl ]->Rght_Ptr = -1;
      pSort->NodeList[ Lvl ]->Node_Atr = 2;
      hb_cdxIndexPageWrite( pSort->CurTag->Owner, pa, pSort->NodeList[ Lvl ],
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
              pSort->CurTag->KeyType == 'C' ? 32 : 0,
              sizeof( pSort->NodeList[ Lvl ]->cdxu.Internal.IntData ) );
   v = ( USHORT ) ( pSort->NodeList[ Lvl ]->Entry_Ct *
       ( pSort->CurTag->KeyLength + 8 ) );
   memcpy( &pSort->NodeList[ Lvl ]->cdxu.Internal.IntData[ v ],
           Value->pItem->item.asString.value, Value->pItem->item.asString.length );
   v += pSort->CurTag->KeyLength;
   r = hb_cdxSwapBytes( Link );
   memcpy( &pSort->NodeList[ Lvl ]->cdxu.Internal.IntData[ v ], &r, 4 );
   r = hb_cdxSwapBytes( Tag );
   memcpy( &pSort->NodeList[ Lvl ]->cdxu.Internal.IntData[ v + 4 ], &r, 4 );
   pSort->NodeList[ Lvl ]->Entry_Ct++;
   if( pSort->NodeList[ Lvl ]->Entry_Ct >= pSort->CurTag->MaxKeys )
   {
      pa = pSort->NodeList[ Lvl ]->Rght_Ptr;
      if( !pSort->Closing )
         pSort->NodeList[ Lvl ]->Rght_Ptr = hb_cdxIndexGetAvailPage( pSort->CurTag->Owner );
      else
         pSort->NodeList[ Lvl ]->Rght_Ptr = -1;
      pSort->NodeList[ Lvl ]->Node_Atr = 0;
      hb_cdxIndexPageWrite( pSort->CurTag->Owner, pa, pSort->NodeList[ Lvl ],
                            sizeof( CDXDATA ) );
      pSort->NodeList[ Lvl ]->Left_Ptr = pa;
      hb_cdxSortAddToNode( pSort, ( USHORT ) ( Lvl + 1 ), pa, Link, Value );
      pSort->NodeList[ Lvl ]->Entry_Ct = 0;
   }
}

/*
 * -- CDX METHODS --
 */

#define cdxBof                                  NULL
#define cdxEof                                  NULL
#define cdxFound                                NULL
#define cdxGoBottom                             NULL
#define cdxGoTo                                 NULL
#define cdxGoToId                               NULL
#define cdxGoTop                                NULL
#define cdxSkip                                 NULL
#define cdxSkipFilter                           NULL
#define cdxSkipRaw                              NULL
#define cdxAddField                             NULL
#define cdxAppend                               NULL
#define cdxCreateFields                         NULL
#define cdxDeleteRec                            NULL
#define cdxDeleted                              NULL
#define cdxFieldCount                           NULL
#define cdxFieldDisplay                         NULL
#define cdxFieldInfo                            NULL
#define cdxFieldName                            NULL
#define cdxFlush                                NULL
#define cdxGetRec                               NULL
#define cdxGetValue                             NULL
#define cdxGetVarLen                            NULL
#define cdxGoCold                               NULL
#define cdxGoHot                                NULL
#define cdxPutRec                               NULL
#define cdxPutValue                             NULL
#define cdxRecAll                               NULL
#define cdxRecCount                             NULL
#define cdxRecInfo                              NULL
#define cdxRecNo                                NULL
#define cdxSetFieldsExtent                      NULL
#define cdxAlias                                NULL
#define cdxCreate                               NULL
#define cdxNewArea                              NULL
#define cdxOpen                                 NULL
#define cdxRelease                              NULL
#define cdxStructSize                           NULL
#define cdxSysName                              NULL
#define cdxEval                                 NULL
#define cdxPack                                 NULL
#define cdxZap                                  NULL
#define cdxOrderCondition                       NULL
#define cdxClearFilter                          NULL
#define cdxClearLocate                          NULL
#define cdxFilterText                           NULL
#define cdxSetFilter                            NULL
#define cdxSetLocate                            NULL
#define cdxCompile                              NULL
#define cdxError                                NULL
#define cdxEvalBlock                            NULL
#define cdxRawLock                              NULL
#define cdxLock                                 NULL
#define cdxUnLock                               NULL
#define cdxCloseMemFile                         NULL
#define cdxReadDBHeader                         NULL
#define cdxWhoCares                             NULL

static ERRCODE cdxClose( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("cdxClose(%p)", pArea));

   cdxOrderListClear( pArea );
   return SUPER_CLOSE( pArea );
}

static ERRCODE cdxCreateMemFile( AREAP pArea, LPDBOPENINFO pCreateInfo )
{
   LPFILEINFO lpMemInfo;
   LPMEMOHEADER pMemoHeader;
   BOOL bError;
   PHB_ITEM pError = NULL;

   HB_TRACE(HB_TR_DEBUG, ("cdxCreateMemFile(%p, %p)", pArea, pCreateInfo));

   lpMemInfo = pArea->lpDataInfo->pNext;
   do
   {
      lpMemInfo->hFile = hb_fsCreate( pCreateInfo->abName, FC_NORMAL );
      if( lpMemInfo->hFile == FS_ERROR )
      {
         if( !pError )
         {
            pError = hb_errNew();
            hb_errPutGenCode( pError, EG_CREATE );
            hb_errPutSubCode( pError, 1005 );
            hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_CREATE ) );
            hb_errPutFileName( pError, ( char * ) pCreateInfo->abName );
            hb_errPutFlags( pError, EF_CANRETRY );
         }
         bError = ( SELF_ERROR( pArea, pError ) == E_RETRY );
      }
      else
         bError = FALSE;
   } while( bError );
   if( pError )
      hb_errRelease( pError );

   if( lpMemInfo->hFile == FS_ERROR )
      return FAILURE;

   pMemoHeader = ( LPMEMOHEADER ) hb_xgrab( 512 );
   memset( pMemoHeader, 0, 512 );
   pMemoHeader->lNextBlock = hb_cdxSwapBytes( 512 / MEMO_BLOCK );
   pMemoHeader->lBlockSize = hb_cdxSwapBytes( MEMO_BLOCK );
   bError = ( hb_fsWrite( lpMemInfo->hFile, ( BYTE * ) pMemoHeader, 512 ) != 512 );
   hb_xfree( pMemoHeader );
   hb_fsClose( lpMemInfo->hFile );
   lpMemInfo->hFile = FS_ERROR;
   if( bError )
      return FAILURE;
   else
      return SUCCESS;
}

static ERRCODE cdxGetValueFile( AREAP pArea, USHORT uiIndex, void * pFile )
{
   ULONG lRecNo, lNewRecNo;
   BYTE * szText, szEndChar;
   LPFIELD pField;

   HB_TRACE(HB_TR_DEBUG, ("cdxGetValueFile(%p, %hu, %p)", pArea, uiIndex, pFile));
   HB_SYMBOL_UNUSED( pFile );

   if( uiIndex > pArea->uiFieldCount )
      return FAILURE;

   pField = pArea->lpFields + uiIndex - 1;
   szText = pArea->lpExtendInfo->bRecord + pField->uiOffset;
   if( !( ( LPDBFMEMO ) pField->memo )->pData )
      memset( szText, ' ', pField->uiLen );
   else
   {
      szEndChar = * ( szText + pField->uiLen );
      * ( szText + pField->uiLen ) = 0;
      lRecNo = atol( ( char * ) szText );
      lNewRecNo = lRecNo;
      if( !hb_cdxWriteMemo( pArea, ( LPDBFMEMO ) pField->memo, &lNewRecNo ) )
         return FAILURE;
      if( lNewRecNo != lRecNo )
         hb_cdxltoa( lNewRecNo, ( char * ) szText, pField->uiLen );
      * ( szText + pField->uiLen ) = szEndChar;
   }
   ( ( LPDBFMEMO ) pField->memo )->fChanged = FALSE;
   return SUCCESS;
}

static ERRCODE cdxInfo( AREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("cdxInfo(%p, %hu, %p)", pArea, uiIndex, pItem));

   if( uiIndex == DBI_MEMOEXT )
   {
      hb_itemPutC( pItem, ".fpt" );
      return SUCCESS;
   }

   return SUPER_INFO( pArea, uiIndex, pItem );
}

static ERRCODE cdxOpenMemFile( AREAP pArea, LPDBOPENINFO pOpenInfo )
{
   LPFILEINFO lpMemInfo;
   LPMEMOHEADER pMemoHeader;
   USHORT uiFlags;
   PHB_ITEM pError = NULL;
   BOOL bRetry;

   HB_TRACE(HB_TR_DEBUG, ("cdxOpenMemFile(%p, %p)", pArea, pOpenInfo));

   lpMemInfo = pArea->lpDataInfo->pNext;
   uiFlags = pOpenInfo->fReadonly ? FO_READ : FO_READWRITE;
   uiFlags |= pOpenInfo->fShared ? FO_DENYNONE : FO_EXCLUSIVE;
   do
   {
      lpMemInfo->hFile = hb_fsOpen( pOpenInfo->abName, uiFlags );
      if( lpMemInfo->hFile == FS_ERROR )
      {
         if( !pError )
         {
            pError = hb_errNew();
            hb_errPutGenCode( pError, EG_OPEN );
            hb_errPutSubCode( pError, 1002 );
            hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_OPEN ) );
            hb_errPutFileName( pError, ( char * ) pOpenInfo->abName );
            hb_errPutFlags( pError, EF_CANRETRY );
         }
         bRetry = ( SELF_ERROR( pArea, pError ) == E_RETRY );
      }
      else
         bRetry = FALSE;
   } while( bRetry );
   if( pError )
      hb_errRelease( pError );

   if( lpMemInfo->hFile == FS_ERROR )
      return FAILURE;

   pMemoHeader = ( LPMEMOHEADER ) hb_xgrab( 512 );
   if( hb_fsRead( lpMemInfo->hFile, ( BYTE * ) pMemoHeader, 512 ) != 512 )
   {
      hb_xfree( pMemoHeader );
      return FAILURE;
   }
   hb_xfree( pMemoHeader );
   return SUCCESS;
}

static ERRCODE cdxOrderCreate( AREAP pArea, LPDBORDERCREATEINFO pOrderInfo )
{
   PHB_ITEM pExpr, pKeyExp, pForExp, pResult, pError;
   HB_MACRO_PTR pExpMacro, pForMacro;
   USHORT uiType, uiLen, uiCount;
   char * szFileName, * szTagName;
   PHB_FNAME pFileName;
   DBORDERINFO pExtInfo;
   LPINDEXINFO pIndex;
   LPTAGINFO pTag, pLastTag;
   DBFHEADER pHeader;
   BYTE bType;
   BOOL bNewFile;

   HB_TRACE(HB_TR_DEBUG, ("cdxOrderCreate(%p, %p)", pArea, pOrderInfo));

   if( SELF_GOCOLD( pArea ) == FAILURE )
      return FAILURE;

   /* If we have a codeblock for the expression, use it */
   if( pOrderInfo->itmCobExpr )
      pExpr = pOrderInfo->itmCobExpr;
   else /* Otherwise, try compiling the key expression string */
   {
      if( SELF_COMPILE( pArea, ( BYTE * ) pOrderInfo->abExpr->item.asString.value ) == FAILURE )
         return FAILURE;
      pExpr = pArea->valResult;
      pArea->valResult = NULL;
   }

   /* Save for later use */
   pKeyExp = hb_itemNew( NULL );
   hb_itemCopy( pKeyExp, pExpr );

   /* Get a blank record before testing expression */
   SELF_GOBOTTOM( pArea );
   SELF_SKIP( pArea, 1 );
   pExpMacro = pForMacro = NULL;
   if( hb_itemType( pExpr ) == HB_IT_BLOCK )
   {
      if( SELF_EVALBLOCK( pArea, pExpr ) == FAILURE )
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
      hb_itemCopy( pResult, &hb_stack.Return );
   }

   uiType = hb_itemType( pResult );
   uiLen = 0;
   switch( uiType )
   {
      case HB_IT_INTEGER:
      case HB_IT_LONG:
      case HB_IT_DOUBLE:
         bType = 'N';
         uiLen = 10;
         break;

      case HB_IT_DATE:
         bType = 'D';
         uiLen = 8;
         break;

      case HB_IT_LOGICAL:
         bType = 'C';
         uiLen = 1;
         break;

      case HB_IT_STRING:
         bType = 'C';
         uiLen = pResult->item.asString.length > CDX_MAX_KEY ? CDX_MAX_KEY :
                 pResult->item.asString.length;
         break;
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
      SELF_ERROR( pArea, pError );
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
         if( SELF_COMPILE( pArea, pArea->lpdbOrdCondInfo->abFor ) == FAILURE )
         {
            hb_itemRelease( pKeyExp );
            if( pExpMacro != NULL )
               hb_macroDelete( pExpMacro );
            return FAILURE;
         }
         pExpr = pArea->valResult;
         pArea->valResult = NULL;
      }
      /* Save for later use */
      pForExp = hb_itemNew( NULL );
      hb_itemCopy( pForExp, pExpr );
   }

   /* Test conditional expression */
   if( pExpr )
   {
      if( hb_itemType( pExpr ) == HB_IT_BLOCK )
      {
         if( SELF_EVALBLOCK( pArea, pExpr ) == FAILURE )
         {
            hb_itemRelease( pKeyExp );
            hb_itemRelease( pForExp );
            if( pExpMacro != NULL )
               hb_macroDelete( pExpMacro );
            return FAILURE;
         }
         pResult = pArea->valResult;
      }
      else
      {
         pForMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pExpr );
         hb_macroRun( pForMacro );
         pResult = pExpr;
         hb_itemCopy( pResult, &hb_stack.Return );
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
      pFileName = hb_fsFNameSplit( pArea->lpDataInfo->szFileName );
      if( pFileName->szDrive )
         strcat( szFileName, pFileName->szDrive );
      if( pFileName->szPath )
         strcat( szFileName, pFileName->szPath );
      strcat( szFileName, pFileName->szName );
      pExtInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( pArea, DBOI_BAGEXT, &pExtInfo );
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
         SELF_ORDINFO( pArea, DBOI_BAGEXT, &pExtInfo );
         strcat( szFileName, pExtInfo.itmResult->item.asString.value );
         hb_itemRelease( pExtInfo.itmResult );
      }
   }
   szTagName = ( char * ) hb_xgrab( CDX_MAX_TAG_NAME_LEN + 1 );
   hb_strncpyUpper( szTagName, pFileName->szName, CDX_MAX_TAG_NAME_LEN );
   hb_xfree( pFileName );

   /* Close all index */
   cdxOrderListClear( pArea );

   pIndex = hb_cdxIndexNew( pArea );
   pArea->lpIndexInfo = pIndex;

   /* New file? */
   if( !hb_fsFile( ( BYTE * ) szFileName ) )
   {
      pIndex->DiskFile = hb_fsCreate( ( BYTE * ) szFileName, FC_NORMAL );
      bNewFile = TRUE;
   }
   else
   {
      pIndex->DiskFile = hb_fsOpen( ( BYTE * ) szFileName, FO_READWRITE |
                                    ( pArea->lpExtendInfo->fExclusive ?
                                      FO_EXCLUSIVE : FO_DENYNONE ) );
      bNewFile = FALSE;
   }

   if( pIndex->DiskFile == FS_ERROR )
   {
      cdxOrderListClear( pArea );
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
      bNewFile = ( hb_fsSeek( pIndex->DiskFile, 0, FS_END ) <= CDX_BLOCK_SIZE );
      hb_fsSeek( pIndex->DiskFile, 0, FS_SET );
   }

   if( bNewFile )
   {
      pIndex->NextAvail = 0;
      pIndex->CompoundTag = hb_cdxTagNew( pIndex, szTagName, -1 );
      pIndex->CompoundTag->OptFlags = 0xE0;
      hb_cdxTagIndexTagNew( pIndex->CompoundTag, NULL, NULL, 'C', 10, NULL, NULL,
                            TRUE, FALSE );
      hb_cdxTagTagOpen( pIndex->CompoundTag, 0 );
   }
   else
   {
      pIndex->CompoundTag = hb_cdxTagNew( pIndex, szTagName, 0 );
      pIndex->CompoundTag->OptFlags = 0xE0;
      hb_cdxIndexResetAvailPage( pIndex );
      hb_cdxTagTagOpen( pIndex->CompoundTag, 0 );
      while( !pIndex->CompoundTag->TagEOF )
      {
         pTag = hb_cdxTagNew( pIndex,
                              pIndex->CompoundTag->CurKeyInfo->pItem->item.asString.value,
                              pIndex->CompoundTag->CurKeyInfo->Tag );
         if( pIndex->TagList == NULL )
            pIndex->TagList = pTag;
         else
         {
            pLastTag = pIndex->TagList;
            while( pLastTag->pNext )
               pLastTag = pLastTag->pNext;
            pLastTag->pNext = pTag;
         }
         hb_cdxTagKeyRead( pIndex->CompoundTag, NEXT_RECORD );
      }
   }

   /* Update DBF header */
   if( !pArea->lpExtendInfo->fHasMDX )
   {
      pFileName = hb_fsFNameSplit( pArea->lpDataInfo->szFileName );
      hb_strncpyUpper( szFileName, pFileName->szName, CDX_MAX_TAG_NAME_LEN );
      hb_xfree( pFileName );
      if( strcmp( szFileName, szTagName ) == 0 && hb_fsRead( pArea->lpDataInfo->hFile,
          ( BYTE * ) &pHeader, sizeof( DBFHEADER ) ) == sizeof( DBFHEADER ) )
      {
         pHeader.bHasMDX = 1;
         hb_fsWrite( pArea->lpDataInfo->hFile, ( BYTE * ) &pHeader, sizeof( DBFHEADER ) );
      }
   }

   hb_xfree( szFileName );

   hb_strncpyUpper( szTagName, ( const char * ) pOrderInfo->atomBagName, CDX_MAX_TAG_NAME_LEN );
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
   SELF_ORDSETCOND( pArea, NULL );
   return SELF_GOTOP( pArea );
}

static ERRCODE cdxOrderDestroy( AREAP pArea, LPDBORDERINFO pOrderInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("cdxOrderDestroy(%p, %p)", pArea, pOrderInfo));

   HB_SYMBOL_UNUSED( pArea );
   HB_SYMBOL_UNUSED( pOrderInfo );

   return SUCCESS;
}

static ERRCODE cdxOrderInfo( AREAP pArea, USHORT uiIndex, LPDBORDERINFO pInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("cdxOrderInfo(%p, %hu, %p)", pArea, uiIndex, pInfo));

   HB_SYMBOL_UNUSED( pArea );

   switch( uiIndex )
   {
      case DBOI_BAGEXT:
         hb_itemPutC( pInfo->itmResult, ".cdx" );
         break;
   }
   return SUCCESS;
}

static ERRCODE cdxOrderListAdd( AREAP pArea, LPDBORDERINFO pOrderInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("cdxOrderListAdd(%p, %p)", pArea, pOrderInfo));

   HB_SYMBOL_UNUSED( pArea );
   HB_SYMBOL_UNUSED( pOrderInfo );

   return SUCCESS;
}

static ERRCODE cdxOrderListClear( AREAP pArea )
{
   LPINDEXINFO pIndex;

   HB_TRACE(HB_TR_DEBUG, ("cdxOrderListClear(%p)", pArea));

   while( pArea->lpIndexInfo )
   {
      pIndex = pArea->lpIndexInfo;
      pArea->lpIndexInfo = pArea->lpIndexInfo->pNext;
      hb_cdxIndexFree( pIndex );
   }
   pArea->lpIndexInfo = NULL;
   return SUCCESS;
}

static ERRCODE cdxOrderListFocus( AREAP pArea, LPDBORDERINFO pOrderInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("cdxOrderListFocus(%p, %p)", pArea, pOrderInfo));

   HB_SYMBOL_UNUSED( pArea );
   HB_SYMBOL_UNUSED( pOrderInfo );

   return SUCCESS;
}

static ERRCODE cdxOrderListRebuild( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("cdxOrderListRebuild(%p)", pArea));

   HB_SYMBOL_UNUSED( pArea );

   return SUCCESS;
}

static ERRCODE cdxPutValueFile( AREAP pArea, USHORT uiIndex, void * pFile )
{
   LPFIELD pField;
   BYTE * szText, szEndChar;
   ULONG lMemoBlock;

   HB_TRACE(HB_TR_DEBUG, ("cdxPutValueFile(%p, %hu, %p)", pArea, uiIndex, pFile));
   HB_SYMBOL_UNUSED( pFile );

   if( uiIndex > pArea->uiFieldCount )
      return FAILURE;

   pField = pArea->lpFields + uiIndex - 1;;
   szText = pArea->lpExtendInfo->bRecord + pField->uiOffset;
   szEndChar = * ( szText + pField->uiLen );
   * ( szText + pField->uiLen ) = 0;
   lMemoBlock = atol( ( char * ) szText ) * MEMO_BLOCK;
   * ( szText + pField->uiLen ) = szEndChar;
   if( lMemoBlock > 0 )
      hb_cdxReadMemo( pArea, ( LPDBFMEMO ) pField->memo, lMemoBlock );
   else if( ( ( LPDBFMEMO ) pField->memo )->pData )
   {
      hb_xfree( ( ( LPDBFMEMO ) pField->memo )->pData );
      memset( pField->memo, 0, sizeof( DBFMEMO ) );
   }
   return SUCCESS;
}

static ERRCODE cdxSeek( AREAP pArea, BOOL bSoftSeek, PHB_ITEM pKey, BOOL bFindLast )
{
   HB_TRACE(HB_TR_DEBUG, ("cdxSeek(%p, %d, %p, %d)", pArea, bSoftSeek, pKey, bFindLast));

   HB_SYMBOL_UNUSED( pArea );
   HB_SYMBOL_UNUSED( bSoftSeek );
   HB_SYMBOL_UNUSED( pKey );
   HB_SYMBOL_UNUSED( bFindLast );

   return SUCCESS;
}

static ERRCODE cdxWriteDBHeader( AREAP pArea )
{
   DBFHEADER pHeader;
   DBFFIELD pDBField;
   USHORT uiCount;
   LPFIELD pField;
   time_t t;
   struct tm * pTime;

   HB_TRACE(HB_TR_DEBUG, ("cdxWriteDBHeader(%p)", pArea));

   memset( &pHeader, 0, sizeof( DBFHEADER ) );
   pHeader.uiRecordLen = 1;
   pHeader.bVersion = 0x03;
   pField = pArea->lpFields;
   for( uiCount = 0; uiCount < pArea->uiFieldCount; uiCount++ )
   {
      switch( pField->uiType )
      {
         case 'C':
         case 'N':
            pHeader.uiRecordLen += pField->uiLen;
            break;

         case 'M':
            pHeader.uiRecordLen += 10;
            pHeader.bVersion = 0xF5;
            pArea->lpExtendInfo->fHasMemo = TRUE;
            break;

         case 'D':
            pHeader.uiRecordLen += 8;
            break;

         case 'L':
            pHeader.uiRecordLen += 1;
            break;
      }
      pField++;
   }

   time( &t );
   pTime =  localtime( &t );
   pHeader.bYear = ( BYTE ) pTime->tm_year;
   pHeader.bMonth = ( BYTE ) pTime->tm_mon + 1;
   pHeader.bDay = ( BYTE ) pTime->tm_mday;
   pHeader.uiHeaderLen = ( USHORT ) ( 32 * ( pArea->uiFieldCount + 1 ) + 1 );
   pHeader.ulRecords = 0;
   pHeader.bHasMDX = pArea->lpExtendInfo->fHasMDX;
   if( hb_fsWrite( pArea->lpDataInfo->hFile, ( BYTE * ) &pHeader,
                   sizeof( DBFHEADER ) ) != sizeof( DBFHEADER ) )
      return FAILURE;

   pField = pArea->lpFields;
   for( uiCount = 0; uiCount < pArea->uiFieldCount; uiCount++ )
   {
      memset( &pDBField, 0, sizeof( DBFFIELD ) );
      strncpy( ( char * ) pDBField.bName, ( ( PHB_DYNS ) pField->sym )->pSymbol->szName,
               sizeof( pDBField.bName ) );
      pDBField.bType = pField->uiType;
      switch( pDBField.bType )
      {
         case 'C':
            pDBField.bLen = pField->uiLen & 0xFF;
            pDBField.bDec = pField->uiLen >> 8;
            break;

         case 'M':
            pDBField.bLen = 10;
            pDBField.bDec = 0;
            break;

         case 'D':
            pDBField.bLen = 8;
            pDBField.bDec = 0;
            break;

         case 'L':
            pDBField.bLen = 1;
            pDBField.bDec = 0;
            break;

         case 'N':
            pDBField.bLen = pField->uiLen;
            pDBField.bDec = pField->uiDec;
            break;
      }
      if( hb_fsWrite( pArea->lpDataInfo->hFile, ( BYTE * ) &pDBField,
                      sizeof( DBFFIELD ) ) != sizeof( DBFFIELD ) )
         return FAILURE;
      pField++;
   }
   if( hb_fsWrite( pArea->lpDataInfo->hFile, ( BYTE * ) "\15\32", 2 ) != 2 )
      return FAILURE;
   return SUCCESS;
}

static RDDFUNCS cdxTable = { cdxBof,
                             cdxEof,
                             cdxFound,
                             cdxGoBottom,
                             cdxGoTo,
                             cdxGoToId,
                             cdxGoTop,
                             cdxSeek,
                             cdxSkip,
                             cdxSkipFilter,
                             cdxSkipRaw,
                             cdxAddField,
                             cdxAppend,
                             cdxCreateFields,
                             cdxDeleteRec,
                             cdxDeleted,
                             cdxFieldCount,
                             cdxFieldDisplay,
                             cdxFieldInfo,
                             cdxFieldName,
                             cdxFlush,
                             cdxGetRec,
                             cdxGetValue,
                             cdxGetVarLen,
                             cdxGoCold,
                             cdxGoHot,
                             cdxPutRec,
                             cdxPutValue,
                             cdxRecAll,
                             cdxRecCount,
                             cdxRecInfo,
                             cdxRecNo,
                             cdxSetFieldsExtent,
                             cdxAlias,
                             cdxClose,
                             cdxCreate,
                             cdxInfo,
                             cdxNewArea,
                             cdxOpen,
                             cdxRelease,
                             cdxStructSize,
                             cdxSysName,
                             cdxEval,
                             cdxPack,
                             cdxZap,
                             cdxOrderListAdd,
                             cdxOrderListClear,
                             cdxOrderListFocus,
                             cdxOrderListRebuild,
                             cdxOrderCondition,
                             cdxOrderCreate,
                             cdxOrderDestroy,
                             cdxOrderInfo,
                             cdxClearFilter,
                             cdxClearLocate,
                             NULL,
                             NULL,
                             cdxFilterText,
                             NULL,
                             cdxSetFilter,
                             cdxSetLocate,
                             NULL,
                             NULL,
                             cdxCompile,
                             cdxError,
                             cdxEvalBlock,
                             cdxRawLock,
                             cdxLock,
                             cdxUnLock,
                             cdxCloseMemFile,
                             cdxCreateMemFile,
                             cdxGetValueFile,
                             cdxOpenMemFile,
                             cdxPutValueFile,
                             cdxReadDBHeader,
                             cdxWriteDBHeader,
                             cdxWhoCares
                           };

HB_FUNC( _DBFCDX )
{
}

HB_FUNC( DBFCDX_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;

   uiCount = ( USHORT * ) hb_parnl( 1 );
   * uiCount = RDDFUNCSCOUNT;
   pTable = ( RDDFUNCS * ) hb_parnl( 2 );
   if( pTable )
      hb_retni( hb_rddInherit( pTable, &cdxTable, &cdxSuper, ( BYTE * ) "DBF" ) );
   else
      hb_retni( FAILURE );
}

