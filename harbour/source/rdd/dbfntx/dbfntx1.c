/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DBFNTX RDD
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
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"
#include "rddsys.ch"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbrddntx.h"

HB_FUNC( _DBFNTX );
HB_FUNC( DBFNTX_GETFUNCTABLE );

HB_INIT_SYMBOLS_BEGIN( dbfntx1__InitSymbols )
{ "_DBFNTX",             HB_FS_PUBLIC, HB_FUNCNAME( _DBFNTX ),             0 },
{ "DBFNTX_GETFUNCTABLE", HB_FS_PUBLIC, HB_FUNCNAME( DBFNTX_GETFUNCTABLE) , 0 }
HB_INIT_SYMBOLS_END( dbfntx1__InitSymbols )
#if defined(_MSC_VER)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto_dbfntx1__InitSymbols = dbfntx1__InitSymbols;
   #pragma data_seg()
#elif ! defined(__GNUC__)
   #pragma startup dbfntx1__InitSymbols
#endif

static RDDFUNCS ntxSuper = { 0 };
static USHORT maxPagesPerTag = 5;

/* Functions for debug this module */
#ifdef DEBUG
static void hb_ntxIndexDump( LPNTXINDEX pIndex );
      /* Dump index. Uses hb_ntxPageDump */
static void hb_ntxPageDump( LPPAGEINFO pPage );
      /* Dump one page of index with all subpages */
#endif

/* Internal functions */
static LPKEYINFO hb_ntxKeyNew( void );
static void hb_ntxKeyFree( LPKEYINFO pKey );
static int hb_ntxPageSeekKey( LPPAGEINFO pPage, LONG lBlock, LPKEYINFO pKey, BOOL bExact );
static LONG hb_ntxTagKeyFind( LPTAGINFO pTag, LPKEYINFO pKey );
static int hb_ntxTagFindCurrentKey( LPPAGEINFO pPage, LONG lBlock, LPKEYINFO pKey, BOOL bExact, LONG* blockPrev, LONG* blockNext, SHORT* keyPrev, SHORT* keyNext );
static USHORT hb_ntxPageFindCurrentKey( LPPAGEINFO pPage, ULONG ulRecno );
static void hb_ntxGetCurrentKey( LPTAGINFO pTag, LPKEYINFO pKey );
static BOOL hb_ntxFindNextKey( LPTAGINFO pTag );
static BOOL hb_ntxPageReadNextKey( LPTAGINFO pTag );
static BOOL hb_ntxFindPrevKey( LPTAGINFO pTag );
static BOOL hb_ntxPageReadPrevKey( LPTAGINFO pTag );
static BOOL hb_ntxPageReadTopKey( LPPAGEINFO pPage, ULONG ulOffset );
static BOOL hb_ntxPageReadBottomKey( LPPAGEINFO pPage, ULONG ulOffset );
static LPPAGEINFO hb_ntxPageFind( LPNTXINDEX pIndex ,LONG ulOffset );
static LPPAGEINFO hb_ntxPageLast( LPNTXINDEX pIndex );
static ERRCODE hb_ntxHeaderLoad( LPNTXINDEX pIndex , char * ITN );
         /* Load NTX header an fill structures pIndex */
static void hb_ntxHeaderSave( LPNTXINDEX pIndex );
         /* Save NTX header */

static LPNTXINDEX hb_ntxIndexNew( NTXAREAP pArea );
         /* Allocate space for information about Index and find free ID */
static void  hb_ntxIndexFree( LPNTXINDEX pIndex );
         /* Release all resources associated with index */
static ERRCODE hb_ntxIndexCreate( LPNTXINDEX pIndex );
         /* Create index from database */

static LPTAGINFO hb_ntxTagNew( LPNTXINDEX PIF, char * ITN, char *szKeyExpr,
         PHB_ITEM pKeyExpr, BYTE bKeyType, USHORT uiKeyLen, char *szForExp,
         PHB_ITEM pForExp, BOOL fAscendKey, BOOL fUnique );
         /* Create Compound Tag with information about index */

static LPPAGEINFO hb_ntxPageNew(LPTAGINFO pParentTag );
         /* Allocate space for new page */
static void hb_ntxPageRelease( LPPAGEINFO pPage );
static void hb_ntxPageFree( LPPAGEINFO pPage, BOOL lFreeChild );
         /* Release memory allocated for page. If page was modified save it */
static void hb_ntxPageSave( LPPAGEINFO pPage );
         /* Save page */
static LPPAGEINFO hb_ntxPageLoad( ULONG ulOffset );
         /* Load page from disk */
static ERRCODE hb_ntxPageKeyDel( LPPAGEINFO pPage, SHORT pos );
         /* Delete key from page */
static ERRCODE hb_ntxPageKeyAdd( LPPAGEINFO pPage, PHB_ITEM pKey, int level);
         /* Add key to page */
static ERRCODE hb_ntxPageKeyInsert( LPPAGEINFO pPage, PHB_ITEM pKey, int pos );
         /* Insert page in position pos */
static int hb_ntxItemCompare( PHB_ITEM pKey1, PHB_ITEM pKey2, BOOL Exact );
         /* Compare 2 Keys (They must be keys !!! */
static ERRCODE hb_ntxPageAddPageKeyAdd( LPPAGEINFO pPage, PHB_ITEM pKey, int level, int pos );

/* Implementation of functions for debug this module */
#ifdef DEBUG
static void hb_ntxPageDump( LPPAGEINFO pPage )
{
   ULONG i;
   LPPAGEINFO pLoadedPage;

   printf("Dump page : %lX\n",pPage->Page);
   for( i = 0 ; i < pPage->uiKeys; i++ )
   {
      if( pPage->pKeys[i].Tag )
      {
         pLoadedPage = hb_ntxPageLoad( pPage->pKeys[i].Tag );
         hb_ntxPageDump( pLoadedPage );
         hb_ntxPageFree( pLoadedPage );
      }
      printf("  Record : %8lX Key : %s\n",
            pPage->pKeys[i].Xtra,
            pPage->pKeys[i].pItem->item.asString.value);
   }
   if( pPage->pKeys[i].Tag )
   {
      pLoadedPage = hb_ntxPageLoad( pPage->pKeys[i].Tag );
      hb_ntxPageDump( pLoadedPage );
      hb_ntxPageFree( pLoadedPage );
   }
   printf("Page end\n");
}

static void hb_ntxIndexDump( LPNTXINDEX pIndex )
{
   LPPAGEINFO pPage;

   pPage = hb_ntxPageLoad( 0 );
   if( pPage )
   {
      hb_ntxPageDump( pPage );
      hb_ntxPageFree( pPage );
   }
}
#endif

static void backcpy( BYTE* dest, BYTE* src, long mlen )
{
   long i;
   for( dest = dest + mlen - 1,src = src + mlen - 1, i = 0; i < mlen; i++, dest--,src-- )
        *dest = *src;
}

/* Implementation of internal functions */

static LPKEYINFO hb_ntxKeyNew( void )
{
   LPKEYINFO pKey;

   pKey = ( LPKEYINFO ) hb_xgrab( sizeof( KEYINFO ) );
   pKey->pItem = hb_itemNew( NULL );
   pKey->Tag = pKey->Xtra = 0;
   // pKey->pNext = NULL;
   return pKey;
}

static void hb_ntxKeyFree( LPKEYINFO pKey )
{
   hb_itemRelease( pKey->pItem );
   hb_xfree( pKey );
}

static int hb_ntxPageSeekKey( LPPAGEINFO pPage, LONG lBlock, LPKEYINFO pKey, BOOL bExact )
{
   int k = 1, kChild;
   LPKEYINFO p;
   LPPAGEINFO pChildPage;

   bExact = ( bExact || pPage->TagParent->KeyType != 'C' );
   pPage->CurKey = 0;
   if( pPage->uiKeys > 0 )
   {
      while( k > 0 && pPage->CurKey < pPage->uiKeys )
      {
         p =  pPage->pKeys + pPage->CurKey;
         k = hb_ntxItemCompare( pKey->pItem, p->pItem, bExact );
         if( !pPage->TagParent->AscendKey )
            k = -k;
         if( k == 0 && lBlock == NTX_MAX_REC_NUM )
            k = 1;
         if( k == 0 && lBlock != NTX_IGNORE_REC_NUM )
         {
            if( lBlock > p->Xtra )
               k = 1;
            else if( lBlock < p->Xtra )
               k = -1;
         }
         if( k <= 0 || pPage->CurKey == pPage->uiKeys - 1 )  /* pKey <= p */
         {
            if( k == 0 )
            {
               pKey->Xtra = p->Xtra;
               pPage->TagParent->CurKeyInfo->Xtra = pKey->Xtra;
               pPage->TagParent->CurKeyInfo->Tag = pPage->Page;
            }
            if( p->Tag )
            {
               pChildPage = hb_ntxPageLoad( p->Tag );
               kChild = hb_ntxPageSeekKey( pChildPage, lBlock, pKey, bExact );
               if( k != 0 || kChild == 0 )
                  k = kChild;
            }
            else if( k == 0 && lBlock != NTX_IGNORE_REC_NUM )
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
   hb_ntxPageRelease( pPage );
   return k;
}

static LONG hb_ntxTagKeyFind( LPTAGINFO pTag, LPKEYINFO pKey )
{
   int K;

   pTag->CurKeyInfo->Tag = 0;
   pTag->TagBOF = pTag->TagEOF = FALSE;
   K = hb_ntxPageSeekKey( hb_ntxPageLoad( 0 ), pKey->Tag, pKey, FALSE );
   if( K == 0 )
   {
      if( pTag->pForItem == NULL )
      {
         K = pKey->Xtra;
         return K;
      }
      else
         /* TODO: test for expression */
         pTag->TagEOF = TRUE;
   }
   else if( K < 0 )
   {
      if( pTag->pForItem != NULL )
         /* TODO: test for expression */
         pTag->TagEOF = TRUE;
   }
   else
      pTag->TagEOF = TRUE;
   return 0;
}

static int hb_ntxTagFindCurrentKey( LPPAGEINFO pPage, LONG lBlock, LPKEYINFO pKey, BOOL bExact, LONG* blockPrev, LONG* blockNext, SHORT* keyPrev, SHORT* keyNext )
{
   int k = 1, kChild;
   LPKEYINFO p;
   LPPAGEINFO pChildPage;

   bExact = ( bExact || pPage->TagParent->KeyType != 'C' );
   pPage->CurKey = 0;
   if( pPage->uiKeys > 0 )
   {
      while( k > 0 && pPage->CurKey <= pPage->uiKeys )
      {
         p =  pPage->pKeys + pPage->CurKey;
         if( pPage->CurKey == pPage->uiKeys )
         {
            if( !p->Tag )
               break;
            k = -1;
         }
         else
         {
            k = hb_ntxItemCompare( pKey->pItem, p->pItem, bExact );
            /* k > 0 : pKey->pItem > p->pItem */
            if( !pPage->TagParent->AscendKey )
               k = -k;
            if( k == 0 && lBlock == NTX_MAX_REC_NUM )
               k = 1;
            if( k == 0 && lBlock != NTX_IGNORE_REC_NUM )
            {
               if( lBlock > p->Xtra )
                  k = 1;
               else if( lBlock < p->Xtra )
                  k = -1;
            }
         }
         if( k <= 0 ||
             ( k == 0 && (ULONG)p->Xtra != pPage->TagParent->Owner->Owner->ulRecNo ) )
         /* pKey <= p */
         {
            if( k == 0 && (ULONG)p->Xtra != pPage->TagParent->Owner->Owner->ulRecNo )
               k = 1;
            if( k == 0 )
            {
               pKey->Xtra = p->Xtra;
               pPage->TagParent->CurKeyInfo->Xtra = pKey->Xtra;
               pPage->TagParent->CurKeyInfo->Tag = pPage->Page;
            }
            if( p->Tag && (ULONG)p->Xtra != pPage->TagParent->Owner->Owner->ulRecNo )
            {
                  if( blockPrev && pPage->CurKey > 0 )
                  {
                     *blockPrev = pPage->Page;
                     *keyPrev = pPage->CurKey - 1;
                  }
                  if( blockNext && pPage->CurKey < pPage->uiKeys )
                  {
                     *blockNext = pPage->Page;
                     *keyNext = pPage->CurKey;
                  }
               pChildPage = hb_ntxPageLoad( p->Tag );
               kChild = hb_ntxTagFindCurrentKey( pChildPage, lBlock, pKey, bExact, blockPrev, blockNext, keyPrev, keyNext );
               if( k != 0 || kChild == 0 )
                  k = kChild;
               if( k > 0 )
               {
                  if( blockPrev && pPage->CurKey > 0 )
                     *blockPrev = 0;
                  if( blockNext && pPage->CurKey < pPage->uiKeys )
                     *blockNext = 0;
               }
            }
            else if( k == 0 && lBlock != NTX_IGNORE_REC_NUM )
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
   hb_ntxPageRelease( pPage );
   return k;
}

static USHORT hb_ntxPageFindCurrentKey( LPPAGEINFO pPage, ULONG ulRecno )
{
   int i;
   for( i=0; i < pPage->uiKeys; i++ )
      if( (ULONG)( pPage->pKeys+i )->Xtra == ulRecno )
          return ( i+1 );
   return 0;
}

static void hb_ntxGetCurrentKey( LPTAGINFO pTag, LPKEYINFO pKey )
{

   /* printf( "\n\rhb_ntxGetCurrentKey - 0:  |%s|",pTag->KeyExpr ); */
   if( hb_itemType( pTag->pKeyItem ) == HB_IT_BLOCK )
   {
      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( pTag->pKeyItem );
      hb_vmDo( 0 );
      hb_itemCopy( pKey->pItem , &hb_stack.Return );
   }
   else
   {
      HB_MACRO_PTR pMacro;
      pMacro = ( HB_MACRO_PTR ) hb_itemGetPtr( pTag->pKeyItem );
      hb_macroRun( pMacro );
      hb_itemCopy( pKey->pItem, hb_stackItemFromTop( - 1 ) );
      hb_stackPop();
   }

   /* printf( "\n\rhb_ntxGetCurrentKey - 3:  |%s|",(pKey->pItem)->item.asString.value ); */
   pKey->Tag = 0;
   pKey->Xtra = 0;

}

static BOOL hb_ntxFindNextKey( LPTAGINFO pTag )
{
   int seekRes;
   LPKEYINFO pKey;
   LONG blockNext = 0;
   SHORT keyNext = 0;
   LPPAGEINFO pPage;

   pKey = hb_ntxKeyNew();
   hb_ntxGetCurrentKey( pTag,pKey );
   pKey->Tag = NTX_IGNORE_REC_NUM;
   seekRes = hb_ntxTagFindCurrentKey( hb_ntxPageLoad( 0 ), pKey->Tag, pKey, FALSE, NULL, &blockNext, NULL, &keyNext );
   hb_ntxKeyFree( pKey );
   if( seekRes )
      printf( "\n\rhb_ntxFindNextKey: Cannot find current key:" );
   else
   {
      /* printf( "\n\rhb_ntxFindNextKey - 1" ); */
      pPage = hb_ntxPageLoad( pTag->CurKeyInfo->Tag );
      pPage->CurKey =  hb_ntxPageFindCurrentKey( pPage,pTag->Owner->Owner->ulRecNo );
      if( pPage->CurKey < pPage->uiKeys )
      {
         /* printf( "\n\rhb_ntxFindNextKey - 2" ); */
         pPage->TagParent->CurKeyInfo->Xtra = ( pPage->pKeys+pPage->CurKey )->Xtra;
         pPage->TagParent->CurKeyInfo->Tag = pPage->Page;
         hb_ntxPageRelease( pPage );
         return TRUE;
      }
      else
      {
         hb_ntxPageRelease( pPage );
         /* printf( "\n\rhb_ntxFindNextKey - 3 : %ld",blockNext ); */
         if( blockNext )
         {
            pPage = hb_ntxPageLoad( blockNext );
            pPage->CurKey =  keyNext;
            /* printf( "\n\rhb_ntxFindNextKey - 4 ( %d %d %ld)",pPage->CurKey, pPage->uiKeys,pTag->CurKeyInfo->Xtra ); */
            if( pPage->CurKey < pPage->uiKeys )
            {
            /*
               while( ( pPage->pKeys+pPage->CurKey )->Tag )
               {
                  pChildPage = hb_ntxPageLoad( ( pPage->pKeys+pPage->CurKey )->Tag );
                  hb_ntxPageRelease( pPage );
                  pPage = pChildPage;
                  pPage->CurKey = 0;
               }
            */
               pPage->TagParent->CurKeyInfo->Xtra = ( pPage->pKeys+pPage->CurKey )->Xtra;
               pPage->TagParent->CurKeyInfo->Tag = pPage->Page;
               hb_ntxPageRelease( pPage );
               return TRUE;
            }
            else
            {
               hb_ntxPageRelease( pPage );
               return FALSE;
            }
         }
         else
            return FALSE;
      }
   }
   return FALSE;
}

static BOOL hb_ntxPageReadNextKey( LPTAGINFO pTag )
{
   LPPAGEINFO pPage, pChildPage;

   if( pTag->CurKeyInfo->Tag )
   {
      pPage =  hb_ntxPageLoad( pTag->CurKeyInfo->Tag );
      pPage->CurKey =  hb_ntxPageFindCurrentKey( pPage,pTag->CurKeyInfo->Xtra );
      if( pPage->CurKey )
      {
         if( pPage->CurKey < pPage->uiKeys ||
                ( pPage->CurKey == pPage->uiKeys && ( pPage->pKeys+pPage->CurKey )->Tag ) )
         {
            while( ( pPage->pKeys+pPage->CurKey )->Tag )
            {
               pChildPage = hb_ntxPageLoad( ( pPage->pKeys+pPage->CurKey )->Tag );
               hb_ntxPageRelease( pPage );
               pPage = pChildPage;
               pPage->CurKey = 0;
            }
            hb_itemCopy( pPage->TagParent->CurKeyInfo->pItem, ( pPage->pKeys+pPage->CurKey )->pItem );
            pPage->TagParent->CurKeyInfo->Xtra = ( pPage->pKeys+pPage->CurKey )->Xtra;
            pPage->TagParent->CurKeyInfo->Tag = pPage->Page;
            hb_ntxPageRelease( pPage );
            return TRUE;
         }
         else
         {
            hb_ntxPageRelease( pPage );
            return hb_ntxFindNextKey( pTag );
         }
      }
      else
      {
         hb_ntxPageRelease( pPage );
         return hb_ntxFindNextKey( pTag );
      }
   }
   else
      return hb_ntxFindNextKey( pTag );
}

static BOOL hb_ntxFindPrevKey( LPTAGINFO pTag )
{
   int seekRes;
   LPKEYINFO pKey;
   LONG blockPrev = 0;
   SHORT keyPrev = 0;
   LPPAGEINFO pPage;

   pKey = hb_ntxKeyNew();
   hb_ntxGetCurrentKey( pTag, pKey );
   pKey->Tag = NTX_IGNORE_REC_NUM;
   seekRes = hb_ntxTagFindCurrentKey( hb_ntxPageLoad( 0 ), pKey->Tag, pKey, FALSE, &blockPrev, NULL, &keyPrev, NULL );
   hb_ntxKeyFree( pKey );
   if( seekRes )
      printf( "\n\rhb_ntxFindPrevKey: Cannot find current key: |%ld %s|",pTag->Owner->Owner->ulRecNo,pKey->pItem->item.asString.value );
   else
   {
      /* printf( "\n\rhb_ntxFindPrevKey - 1" ); */
      pPage = hb_ntxPageLoad( pTag->CurKeyInfo->Tag );
      pPage->CurKey =  hb_ntxPageFindCurrentKey( pPage,pTag->Owner->Owner->ulRecNo );
      pPage->CurKey--;
      pPage->CurKey--;
      if( pPage->CurKey >= 0 )
      {
         /* printf( "\n\rhb_ntxFindPrevKey - 2" ); */
         pPage->TagParent->CurKeyInfo->Xtra = ( pPage->pKeys+pPage->CurKey )->Xtra;
         pPage->TagParent->CurKeyInfo->Tag = pPage->Page;
         hb_ntxPageRelease( pPage );
         return TRUE;
      }
      else
      {
         hb_ntxPageRelease( pPage );
         /* printf( "\n\rhb_ntxFindPrevKey - 3 : %ld",blockPrev ); */
         if( blockPrev )
         {
            pPage = hb_ntxPageLoad( blockPrev );
            pPage->CurKey =  keyPrev;
            /* printf( "\n\rhb_ntxFindPrevKey - 4 ( %d %d %ld)",pPage->CurKey, pPage->uiKeys,pTag->CurKeyInfo->Xtra ); */
            if( pPage->CurKey < pPage->uiKeys )
            {
               pPage->TagParent->CurKeyInfo->Xtra = ( pPage->pKeys+pPage->CurKey )->Xtra;
               pPage->TagParent->CurKeyInfo->Tag = pPage->Page;
               hb_ntxPageRelease( pPage );
               return TRUE;
            }
            else
            {
               hb_ntxPageRelease( pPage );
               return FALSE;
            }
         }
         else
            return FALSE;
      }
   }
   return FALSE;
}

static BOOL hb_ntxPageReadPrevKey( LPTAGINFO pTag )
{
   LPPAGEINFO pPage, pChildPage;
   if( pTag->CurKeyInfo->Tag )
   {
      pPage =  hb_ntxPageLoad( pTag->CurKeyInfo->Tag );
      pPage->CurKey =  hb_ntxPageFindCurrentKey( pPage,pTag->CurKeyInfo->Xtra );
      if( pPage->CurKey )
      {
         pPage->CurKey--;
         if( ( pPage->pKeys+pPage->CurKey )->Tag )
         {
            if( ( pPage->pKeys+pPage->CurKey )->Tag )
            {
               pChildPage = hb_ntxPageLoad( ( pPage->pKeys+pPage->CurKey )->Tag );
               hb_ntxPageRelease( pPage );
               pPage = pChildPage;
               pPage->CurKey = pPage->uiKeys - 1;
            }
         }
         else
            pPage->CurKey--;
         if( pPage->CurKey >= 0 )
         {
            hb_itemCopy( pPage->TagParent->CurKeyInfo->pItem, ( pPage->pKeys+pPage->CurKey )->pItem );
            pPage->TagParent->CurKeyInfo->Xtra = ( pPage->pKeys+pPage->CurKey )->Xtra;
            pPage->TagParent->CurKeyInfo->Tag = pPage->Page;
            hb_ntxPageRelease( pPage );
            return TRUE;
         }
         else
         {
            hb_ntxPageRelease( pPage );
            return hb_ntxFindPrevKey( pTag );
         }
      }
      else
      {
         hb_ntxPageRelease( pPage );
         return hb_ntxFindPrevKey( pTag );
      }
   }
   else
      return hb_ntxFindPrevKey( pTag );
}

static BOOL hb_ntxPageReadTopKey( LPPAGEINFO pPage, ULONG ulOffset )
{
   LPPAGEINFO pChildPage;
   LPKEYINFO pKey;

   pChildPage = hb_ntxPageLoad( ulOffset );
   if( pPage )
   {
      hb_ntxPageRelease( pPage );
   }
   if( pChildPage != NULL )
   {
      pKey = pChildPage->pKeys;
      ulOffset = pKey->Tag;
      if( ulOffset )
      {
         return hb_ntxPageReadTopKey( pChildPage,ulOffset );
      }
      else
      {
         hb_itemCopy( pChildPage->TagParent->CurKeyInfo->pItem, pKey->pItem );
         pChildPage->TagParent->CurKeyInfo->Xtra = pKey->Xtra;
         pChildPage->TagParent->CurKeyInfo->Tag = pChildPage->Page;
         hb_ntxPageRelease( pChildPage );
         return TRUE;
      }
   }
   else
      return FALSE;
}

static BOOL hb_ntxPageReadBottomKey( LPPAGEINFO pPage, ULONG ulOffset )
{
   LPPAGEINFO pChildPage;
   LPKEYINFO pKey;

   pChildPage = hb_ntxPageLoad( ulOffset );
   if( pPage )
      hb_ntxPageRelease( pPage );
   if( pChildPage != NULL )
   {
      pKey = pChildPage->pKeys + pChildPage->uiKeys;
      ulOffset = pKey->Tag;
      if( ulOffset )
         return hb_ntxPageReadBottomKey( pChildPage,ulOffset );
      else
      {
         pKey -= 1;
         hb_itemCopy( pChildPage->TagParent->CurKeyInfo->pItem, pKey->pItem );
         pChildPage->TagParent->CurKeyInfo->Xtra = pKey->Xtra;
         pChildPage->TagParent->CurKeyInfo->Tag = pChildPage->Page;
         hb_ntxPageRelease( pChildPage );
         return TRUE;
      }
   }
   else
      return FALSE;
}

static void hb_ntxTagKeyRead( LPTAGINFO pTag, BYTE bTypRead )
{
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
   if( pTag->Owner->Owner->fShared )
      while( !hb_fsLock( pTag->Owner->DiskFile, 0, 512, FL_LOCK ) );
   switch( bTypRead )
   {
      case TOP_RECORD:
         if( pTag->pForItem != NULL )
            printf( "hb_ntxTagKeyRead()" );
         else
            pTag->TagBOF = !hb_ntxPageReadTopKey( NULL,0 );
         if( pTag->pForItem != NULL )
            printf( "hb_ntxTagTestRange()" );
         if( pTag->TagEOF )
            pTag->TagBOF = TRUE;
         pTag->TagEOF = pTag->TagBOF;
         break;

      case BTTM_RECORD:
         if( pTag->pForItem != NULL )
            printf( "hb_ntxTagKeyRead()" );
         else
            pTag->TagEOF = !hb_ntxPageReadBottomKey( NULL,0 );
         if( pTag->pForItem != NULL )
            printf( "hb_ntxTagTestRange()" );
         if( pTag->TagBOF )
            pTag->TagEOF = TRUE;
         pTag->TagBOF = pTag->TagEOF;
         break;

      case NEXT_RECORD:
         while( TRUE )
         {
            pTag->TagEOF = !hb_ntxPageReadNextKey( pTag );
            if( pTag->pForItem != NULL )
               printf( "hb_ntxTagKeyRead()" );
            else
               break;
         }
         break;

      case PREV_RECORD:
         while( TRUE )
         {
            pTag->TagBOF = !hb_ntxPageReadPrevKey( pTag );
            if( pTag->pForItem != NULL )
               printf( "hb_ntxTagKeyRead()" );
            else
               break;
         }
         break;

   }
   if( pTag->Owner->Owner->fShared )
   {
      hb_ntxPageFree( pTag->RootPage,TRUE );
      pTag->RootPage = NULL;
      hb_fsLock( pTag->Owner->DiskFile, 0, 512, FL_UNLOCK );
   }
   if( pTag->TagBOF || pTag->TagEOF )
   {
      if( !pTag->AscendKey )
      {
         pTag->CurKeyInfo->Xtra = pTag->TagEOF;
         pTag->TagEOF = pTag->TagBOF;
         pTag->TagBOF = ( BOOL ) pTag->CurKeyInfo->Xtra;
      }
      pTag->CurKeyInfo->Xtra = 0;
   }
}

static int hb_ntxItemCompare( PHB_ITEM pKey1, PHB_ITEM pKey2, BOOL Exact )
{
   int iLimit, iResult, i;
   USHORT EndPos = 0;

   if( pKey2 == NULL || pKey2->item.asString.length == 0 )
      return 1;
   if( pKey1 == NULL || pKey1->item.asString.length == 0 )
      return ( pKey2->item.asString.length == 0 ) ? 0 : -1;

   switch( hb_itemType( pKey1 ) )
   {
      case HB_IT_STRING:
         iLimit = ( pKey1->item.asString.length >
                    pKey2->item.asString.length ) ?
                    pKey2->item.asString.length : pKey1->item.asString.length;
         do
         {
            iResult = pKey1->item.asString.value[ EndPos ] -
                      pKey2->item.asString.value[ EndPos ];
            EndPos++;
         } while( iResult == 0 && EndPos < iLimit );

         if( iResult == 0 )
         {
            EndPos++;
            if( ( iResult = pKey1->item.asString.length -
                      pKey2->item.asString.length ) != 0 )
            {
               if( iResult > 0 )
               {
                  i = pKey1->item.asString.length;
                  while(  i > iLimit &&
                         pKey1->item.asString.value[ i - 1 ] == ' ' )
                     i--;
               }
               else
               {
                  i = pKey2->item.asString.length;
                  while(  i > iLimit &&
                         pKey2->item.asString.value[ i - 1 ] == ' ' )
                     i--;
               }
               if( i == iLimit )
                  iResult = 0;
            }
         }
         if( iResult < 0 && (ULONG)EndPos > pKey1->item.asString.length && !Exact )
            iResult = 0;
         break;
      default:
         iResult = 0;
         printf( "hb_ntxKeyCompare()" );
   }
   if( iResult < 0 )
      return -1;
   else if( iResult > 0 )
      return 1;
   else
      return 0;
}

static void hb_ntxPageSave( LPPAGEINFO pPage )
{
   char buffer[NTXBLOCKSIZE+1];
   int i;
   LPNTXBUFFER itemlist;
   LPNTXITEM item;
   LPKEYINFO pKey;

   memset( buffer, 0, NTXBLOCKSIZE+1 );
   pKey = pPage->pKeys;
   itemlist = ( LPNTXBUFFER ) buffer;
   itemlist->item_count = pPage->uiKeys;

   for( i = 0; i < pPage->uiKeys; i++ )
   {
      itemlist->item_offset[i] = 2 + 2 * ( pPage->TagParent->MaxKeys + 1 ) +
               i * ( pPage->TagParent->KeyLength + 8 );

      item=(NTXITEM *)(buffer+itemlist->item_offset[i]);
      item->page = pKey[i].Tag;
      item->rec_no = pKey[i].Xtra;
      strcpy(item->key, pKey[i].pItem->item.asString.value);
   }
   itemlist->item_offset[i] = 2 + 2 * ( pPage->TagParent->MaxKeys + 1 ) +
            i * ( pPage->TagParent->KeyLength + 8 );
   item=(NTXITEM *)(buffer+itemlist->item_offset[i]);
   item->page = pKey[i].Tag;
   hb_fsSeek( pPage->TagParent->Owner->DiskFile, pPage->Page, FS_SET );
   hb_fsWrite( pPage->TagParent->Owner->DiskFile, (BYTE *) buffer, NTXBLOCKSIZE );
   pPage->Changed = FALSE;
}

static LPPAGEINFO hb_ntxPageLoad( ULONG ulOffset )
{
   char buffer[NTXBLOCKSIZE];
   int i, uiCount;
   NTXAREAP pArea;
   LPNTXINDEX pIndex;
   LPNTXBUFFER itemlist;
   LPNTXITEM item;
   LPPAGEINFO pPage;
   BOOL bReplace = FALSE;
   USHORT uiKeysBefore;

   /* printf( "\nntxPageLoad - 0" ); */
   pArea = (NTXAREAP) hb_rddGetCurrentWorkAreaPointer();
   pIndex = pArea->lpCurIndex;
   pPage = hb_ntxPageFind( pIndex, (ulOffset)? ulOffset:pIndex->CompoundTag->RootBlock );
   if( pPage )
   {
      pPage->lBusy = TRUE;
      return pPage;
   }
   if( pIndex->CompoundTag->uiPages > maxPagesPerTag )
   {
      pPage = pIndex->CompoundTag->RootPage;
      while( pPage )
      {
         if( !pPage->lBusy )
           break;
         pPage = pPage->pNext;
      }
      if( pPage )
         bReplace = TRUE;
   }
   if( !ulOffset )
      hb_fsSeek( pIndex->DiskFile, pIndex->CompoundTag->RootBlock, FS_SET );
   else
      hb_fsSeek( pIndex->CompoundTag->Owner->DiskFile, ulOffset, FS_SET );
   uiCount = hb_fsRead( pIndex->DiskFile, ( BYTE * ) buffer, NTXBLOCKSIZE );
   if( uiCount != NTXBLOCKSIZE )
   {
      return NULL;
   }
   if( !bReplace )
   {
      pPage = ( LPPAGEINFO ) hb_xgrab( sizeof( HB_PAGEINFO ) );
      memset( pPage , 0 ,sizeof( HB_PAGEINFO ) );
      pPage->TagParent = pIndex->CompoundTag;
      if( !ulOffset )
      {
         /* printf( "\nntxPageLoad - 1 ( %5lx )",pPage ); */
         // pPage->pPrev = NULL;
         pPage->Page = pIndex->CompoundTag->RootBlock;
         // pIndex->CompoundTag->RootPage = pPage;
      }
      else
         pPage->Page = ulOffset;
      {
            LPPAGEINFO pLastPage;
            /* printf( "\nntxPageLoad - 2 ( %5lx %5lx )",pIndex->CompoundTag->RootPage,pPage ); */
            pLastPage = hb_ntxPageLast( pIndex );
            /* printf( "\nntxPageLoad - 3 ( %5lx )",pLastPage ); */
            pPage->pPrev = pLastPage;
            if( pLastPage )
               pLastPage->pNext = pPage;
            else
               pIndex->CompoundTag->RootPage = pPage;
      }
      pPage->pKeys = ( LPKEYINFO ) hb_xgrab( sizeof( KEYINFO ) * ( pPage->TagParent->MaxKeys + 1 ) );
      memset( pPage->pKeys, 0, sizeof( KEYINFO ) * ( pPage->TagParent->MaxKeys + 1 ) );
      pPage->pNext = NULL;
      pPage->TagParent->uiPages ++;
   }
   else
      pPage->Page = ( !ulOffset )? pIndex->CompoundTag->RootBlock:ulOffset;

   pPage->CurKey = -1;
   pPage->lBusy = TRUE;

   itemlist = ( LPNTXBUFFER ) buffer;
   if( bReplace )
   {
      uiKeysBefore = pPage->uiKeys;
      /* printf( "\nntxPageLoad - 4 ( %5lx %d %d)",pPage,pPage->uiKeys,itemlist->item_count ); */
      for( i = itemlist->item_count; i< pPage->uiKeys; i++)
         hb_itemRelease( pPage->pKeys[i].pItem );
   }

   pPage->uiKeys = itemlist->item_count ;
   for( i = 0; i < itemlist->item_count; i++ )
   {
      item=(NTXITEM *)(buffer+itemlist->item_offset[i]);
      pPage->pKeys[i].Xtra = item->rec_no;
      pPage->pKeys[i].Tag = item->page;
      if( !bReplace || i >= uiKeysBefore )
         pPage->pKeys[i].pItem = hb_itemNew( NULL );
      hb_itemPutCL( pPage->pKeys[i].pItem, item->key, pPage->TagParent->KeyLength );
   }
   item=(NTXITEM *)(buffer+itemlist->item_offset[i]);
   pPage->pKeys[i].Tag = item->page;

/*
   {
      LPPAGEINFO pP = pIndex->CompoundTag->RootPage;
      while( pP )
      {
         printf( "\nPageLoad  %d( %5lx %d )",bReplace,pP,pP->uiKeys );
         pP = pP->pNext;
      }
   }
*/

   return pPage;
}

static void hb_ntxPageRelease( LPPAGEINFO pPage )
{

   pPage->lBusy = FALSE;
   if( pPage->Changed )
      hb_ntxPageSave( pPage );
   if( pPage->NewRoot )
   {
      pPage->TagParent->RootBlock = pPage->Page;
      hb_ntxHeaderSave( pPage->TagParent->Owner );
   }
}

static void hb_ntxPageFree( LPPAGEINFO pPage, BOOL lFreeChild )
{
   int i;

   /* printf( "\n ntxPageFree - 0 ( %5lx %5lx )",pPage,pPage->pNext ); */
   if( lFreeChild && pPage->pNext )
   {
      hb_ntxPageFree( pPage->pNext,TRUE );
   }
   if( pPage->Changed )
      hb_ntxPageSave( pPage );
   if( pPage->NewRoot )
   {
      pPage->TagParent->RootBlock = pPage->Page;
      hb_ntxHeaderSave( pPage->TagParent->Owner );
   }
   if( lFreeChild )
   {
      if( pPage->pPrev )
         pPage->pPrev->pNext = NULL;
      pPage->pNext = NULL;
   }
   else
   {
      if( pPage->pPrev )
      {
         pPage->pPrev->pNext = pPage->pNext;
         if( pPage->pNext )
            pPage->pNext->pPrev = pPage->pPrev;
      }
      else
      {
         pPage->TagParent->RootPage = pPage->pNext;
         if( pPage->pNext )
            pPage->pNext->pPrev = NULL;
      }
      pPage->pNext = NULL;
   }
   if( pPage->pKeys )
   {
      for( i = 0; i< pPage->uiKeys; i++)
         hb_itemRelease( pPage->pKeys[i].pItem );
      pPage->TagParent->uiPages --;
      hb_xfree( pPage->pKeys );
      pPage->pKeys = NULL;
      hb_xfree( pPage );
   }
}


static LPPAGEINFO hb_ntxPageNew(LPTAGINFO pParentTag )
{
   LPPAGEINFO pPage, pLastPage;

   if( pParentTag->Owner->NextAvail > 0 )
   {
      /* Handling of a pool of empty pages.
         Some sources says that this address is in the first 4 bytes of
         a page ( http://www.e-bachmann.dk/docs/xbase.htm ).
         But as I understood, studying dumps of Clipper ntx'es, address of the
         next available page is in the address field of a first key item
         in the page - it is done here now in such a way.
         = Alexander Kresin =
      */
      pPage = hb_ntxPageLoad( pParentTag->Owner->NextAvail );
      pPage->Page = pParentTag->Owner->NextAvail;
      pParentTag->Owner->NextAvail = pPage->pKeys->Tag;
      hb_ntxHeaderSave( pParentTag->Owner );
   }
   else
   {
      pPage = ( LPPAGEINFO ) hb_xgrab( sizeof( HB_PAGEINFO ) );
      memset( pPage, 0, sizeof( HB_PAGEINFO ) );
      pPage->TagParent = pParentTag;
      pPage->CurKey = -1;
      pLastPage = hb_ntxPageLast( pParentTag->Owner );
      if( pLastPage )
      {
         pPage->pPrev = pLastPage;
         pLastPage->pNext = pPage;
      }
      else
      {
         pPage->pPrev = NULL;
         pParentTag->RootPage = pPage;
      }
      pPage->lBusy = TRUE;
      pParentTag->uiPages ++;
      pPage->pKeys = ( LPKEYINFO ) hb_xgrab( sizeof( KEYINFO ) * ( pParentTag->MaxKeys + 1 ) );
      memset( pPage->pKeys, 0, sizeof( KEYINFO ) * ( pParentTag->MaxKeys + 1 ) );
      pParentTag->TagBlock = pParentTag->TagBlock + 1024;
      pPage->Page = pParentTag->TagBlock;
   }

   return pPage;
}

static ERRCODE hb_ntxPageAddPageKeyAdd( LPPAGEINFO pPage, PHB_ITEM pKey, int level, int pos )
{
   int nBegin, nEnd;
   int nNewPos;
   int MaxKeys = pPage->TagParent->MaxKeys;
   int nCount, nMaxCount = MaxKeys / 5;
   LPPAGEINFO pNewPage;
/*
   int i;
   for( i = 0; i < pPage->uiKeys; i++ )
      printf( "\n --- ( %d %5lx %s )", i,pPage->pKeys[i].Tag,hb_itemGetCPtr( pPage->pKeys[i].pItem ) );
 */
   pNewPage = hb_ntxPageNew( pPage->TagParent );
   if( pNewPage == NULL )
      return FAILURE;
   if( pos > pPage->TagParent->MaxKeys )
   {
      memmove( pNewPage->pKeys , pPage->pKeys,
            ( pPage->uiKeys + 1 ) * sizeof( KEYINFO ) );
      pNewPage->uiKeys = pPage->uiKeys;
      pPage->uiKeys = 1;
      pPage->pKeys[0].Tag = pNewPage->Page;
      pPage->pKeys[0].Xtra = pPage->TagParent->Owner->Owner->ulRecNo;
      pPage->pKeys[0].pItem = hb_itemNew( pKey );
      /* printf( "\nntxPageAddPageKeyAdd - 2( %s %d %d )", hb_itemGetCPtr( pKey ),level,pos ); */
   }
   else

   {
      nBegin = pos;
      nCount = 0;
      while( ( nCount < nMaxCount ) && ( nBegin > 0 ) && ( pPage->pKeys[ nBegin - 1 ].Tag == 0 ) )
      {
         nBegin--;
         nCount++;
      }
      nEnd = pos;
      while( ( nCount < nMaxCount ) && ( nEnd < pPage->uiKeys  - 1 ) && ( pPage->pKeys[ nEnd + 1 ].Tag == 0 ) )
      {
         nEnd++;
         nCount++;
      }
      nNewPos = pos - nBegin ;
      if( nNewPos > 0 )
         memmove( pNewPage->pKeys, pPage->pKeys + nBegin,  nNewPos * sizeof( KEYINFO ));
      pNewPage->pKeys[nNewPos].Xtra = pPage->TagParent->Owner->Owner->ulRecNo;
      pNewPage->pKeys[nNewPos].pItem = hb_itemNew( pKey );
      /* printf( "\nntxPageAddPageKeyAdd - 1( %s %d %d ) %d %d %d %d", hb_itemGetCPtr( pKey ),level,pos,nBegin,nNewPos,nEnd,pPage->uiKeys ); */
      if( nEnd > pos )
         memmove( pNewPage->pKeys + nNewPos + 1, pPage->pKeys + pos,  ( nEnd - pos ) * sizeof( KEYINFO ));
      pPage->pKeys[nEnd].Tag = pNewPage->Page;
      memmove( pPage->pKeys + nBegin , pPage->pKeys + nEnd,
            ( pPage->uiKeys - nEnd + 1) * sizeof( KEYINFO ) );
      pPage->uiKeys -= nCount;
      pNewPage->uiKeys = nCount + 1;
   }
   pPage->Changed = TRUE;
   pNewPage->Changed = TRUE;
   hb_ntxPageRelease( pNewPage );

   HB_SYMBOL_UNUSED( level );
   return SUCCESS;
}

static ERRCODE hb_ntxPageKeyInsert( LPPAGEINFO pPage, PHB_ITEM pKey, int pos )
{
   /* printf( "\nntxPageKeyInsert ( %d %s )", pos,hb_itemGetCPtr( pKey ) ); */
   backcpy( (BYTE*)(pPage->pKeys + pos + 1) , (BYTE*)(pPage->pKeys + pos) ,
            ( pPage->uiKeys - pos + 1 ) * sizeof( KEYINFO ) );
   pPage->uiKeys++;
   pPage->Changed = TRUE;
   pPage->pKeys[pos].Xtra = pPage->TagParent->Owner->Owner->ulRecNo;
   pPage->pKeys[pos].pItem = hb_itemNew( pKey );
   pPage->pKeys[pos].Tag = 0;
   return SUCCESS;
}

static ERRCODE hb_ntxPageKeyDel( LPPAGEINFO pPage, SHORT pos )
{
   hb_itemRelease( pPage->pKeys[pos].pItem );
   memmove( pPage->pKeys + pos , pPage->pKeys + pos + 1,
         ( pPage->uiKeys - pos + 1) * sizeof( KEYINFO ) );
   pPage->uiKeys--;
   pPage->Changed = TRUE;
   if( !pPage->uiKeys )
   {
      pPage->pKeys->Tag = pPage->TagParent->Owner->NextAvail;
      pPage->TagParent->Owner->NextAvail = pPage->Page;
      hb_ntxHeaderSave( pPage->TagParent->Owner );
   }
   return SUCCESS;
}

static ERRCODE hb_ntxPageKeyAdd( LPPAGEINFO pPage, PHB_ITEM pKey, int level)
{
   int i,cmp;
   LPPAGEINFO pLoadedPage;

   /* printf( "\nntxPageKeyAdd - 0" ); */
   i=0;
   if( pPage->uiKeys == 0 )
   {
      pPage->uiKeys=1;
      pPage->Changed = TRUE;
      pPage->pKeys->Xtra = pPage->TagParent->Owner->Owner->ulRecNo;
      pPage->pKeys->pItem = hb_itemNew( pKey );
      return SUCCESS;
   }
   while( i < pPage->uiKeys )
   {
      cmp = hb_ntxItemCompare( pPage->pKeys[ i ].pItem , pKey, TRUE );
      if( cmp > 0 )
      {
         if( pPage->uiKeys == pPage->TagParent->MaxKeys && pPage->pKeys[i].Tag == 0 )
         {
            hb_ntxPageAddPageKeyAdd(pPage, pKey, level, i );
         }
         else if( pPage->pKeys[i].Tag != 0 )
         {
            pLoadedPage = hb_ntxPageLoad( pPage->pKeys[i].Tag );
            if( pLoadedPage == NULL )
            {
               /* TODO : Error recovery ??? */
               return FAILURE;
            }
            hb_ntxPageKeyAdd( pLoadedPage, pKey, level+1 );
            hb_ntxPageRelease( pLoadedPage );
         }
         else
         {
            hb_ntxPageKeyInsert( pPage, pKey, i );
         }
         return SUCCESS;
      }
      i++;
   }
   /* printf( "\nntxPageKeyAdd - 1" ); */
   if( pPage->uiKeys == pPage->TagParent->MaxKeys )
   {
      hb_ntxPageAddPageKeyAdd( pPage, pKey, level, pPage->uiKeys+1 );
   }
   else if( pPage->pKeys[i].Tag != 0 )
   {
      pLoadedPage = hb_ntxPageLoad( pPage->pKeys[i].Tag );
      if( pLoadedPage == NULL )
      {
         /* TODO : Error recovery ??? */

         return FAILURE;
      }
      hb_ntxPageKeyAdd( pLoadedPage, pKey, level+1 );
      hb_ntxPageRelease( pLoadedPage );
   }
   else
   {
      hb_ntxPageKeyInsert( pPage, pKey, i );
   }
   return SUCCESS;
}

static ERRCODE hb_ntxTagKeyAdd( LPTAGINFO pTag, PHB_ITEM pKey)
{
   LPPAGEINFO pPage;

   if( pTag->RootPage )
   {
      /* TODO :
         Add next keys */
      return hb_ntxPageKeyAdd( pTag->RootPage, pKey, 0);

   }
   else
   {
      pPage = hb_ntxPageNew( pTag );
      pPage->Changed = TRUE;
      pPage->NewRoot = TRUE;
      pPage->uiKeys = 1;
      pPage->pKeys[0].Xtra = pTag->Owner->Owner->ulRecNo;
      pPage->pKeys[0].pItem = hb_itemNew( pKey );
      pTag->RootPage = pPage;

   }
   return SUCCESS;
}


static ERRCODE hb_ntxIndexCreate( LPNTXINDEX pIndex )
{

   ULONG ulRecNo, ulRecCount;
   USHORT uiCurLen;
   NTXAREAP pArea;
   LPTAGINFO pTag;
   HB_MACRO_PTR pMacro;
   PHB_ITEM pItem;
   BOOL bWhileOk;
#ifdef DEBUG
   LPPAGEINFO pPage;

   ulRecCount = ( pIndex->Owner->ulRecCount > 200 ) ? 200 :

      pIndex->Owner->ulRecCount;
#else
   ulRecCount = pIndex->Owner->ulRecCount;
#endif
   /* printf( "\nntxIndexCreate - 0" ); */
   pArea = pIndex->Owner;
   pTag = pIndex->CompoundTag;
   pItem = hb_itemNew( NULL );
   for( ulRecNo = 1; ulRecNo <= ulRecCount; ulRecNo++)
   {
      /* printf( "\nntxIndexCreate - 1" ); */
      hb_fsSeek( pArea->hDataFile,
                 pArea->uiHeaderLen + ( ulRecNo - 1 ) * pArea->uiRecordLen,
                 FS_SET );
      hb_fsRead( pArea->hDataFile,
                 pArea->pRecord,
                 pArea->uiRecordLen );
      pArea->ulRecNo = ulRecNo;
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
               uiCurLen = hb_itemGetCLen( pItem );
               if(uiCurLen > NTX_MAX_KEY )
                  uiCurLen = NTX_MAX_KEY ;
               if( pTag->KeyLength != uiCurLen )
               {
                  hb_itemRelease( pItem );
                  return FAILURE;
               }
               hb_ntxTagKeyAdd( pTag, pItem );
#ifdef DEBUG
               pPage = pTag->RootPage;
               while( pPage )
               {
                  if( pPage->Changed )
                  {
                     if( pPage->NewRoot )
                     {
                        pPage->TagParent->RootBlock = pPage->Page;
                        hb_ntxHeaderSave( pIndex );
                     }
                     hb_ntxPageSave( pPage );
                  }
                  pPage = pPage->pNext;
               }
               hb_ntxIndexDump( pIndex );
#endif
               break;
            default:
               printf( "ntxCreateOrder" );
         }
      }
   }
   hb_itemRelease( pItem );
   hb_ntxPageFree( pTag->RootPage,TRUE );
   pTag->RootPage = NULL;
   /* printf( "\nntxIndexCreate - 10" ); */
   return SUCCESS;
}

static void hb_ntxHeaderSave( LPNTXINDEX pIndex )
{
   NTXHEADER Header;

   memset( (void*) &Header, 0, sizeof( NTXHEADER ) );
   Header.type = 15;
   Header.version = 1;
   Header.root = pIndex->CompoundTag->RootBlock;
   Header.next_page = pIndex->NextAvail;
   Header.item_size = pIndex->CompoundTag->KeyLength+8;


   Header.key_size = pIndex->CompoundTag->KeyLength;
   Header.key_dec = 0;
   Header.max_item = pIndex->CompoundTag->MaxKeys;
   Header.half_page = Header.max_item/2;
   strcpy( Header.key_expr , pIndex->CompoundTag->KeyExpr );
   Header.unique = pIndex->CompoundTag->UniqueKey;
   hb_fsSeek( pIndex->DiskFile , 0 , 0 );
   hb_fsWrite( pIndex->DiskFile,(BYTE*)&Header,sizeof(NTXHEADER));
   if( pIndex->CompoundTag->RootPage )
      pIndex->CompoundTag->RootPage->NewRoot = FALSE;
}

static LPTAGINFO hb_ntxTagNew( LPNTXINDEX PIF, char * ITN, char *szKeyExpr, PHB_ITEM pKeyExpr, BYTE bKeyType, USHORT uiKeyLen, char *szForExp, PHB_ITEM pForExp, BOOL fAscendKey, BOOL fUnique )
{
   LPTAGINFO pTag;

   pTag = ( LPTAGINFO ) hb_xgrab( sizeof( TAGINFO ) );
   memset( pTag, 0, sizeof( TAGINFO ) );
   /*
   pTag->TagName = (char *) hb_xgrab( strlen( ITN ) + 1 );
   hb_strncpyUpper( pTag->TagName, ITN, strlen( ITN ) + 1 );
   */
   pTag->TagName = ITN;
   if( szKeyExpr )
   {
      pTag->KeyExpr = (char *) hb_xgrab( NTX_MAX_KEY );
      strcpy( pTag->KeyExpr, szKeyExpr );
   }
   if( szForExp )
   {
      pTag->ForExpr = (char *) hb_xgrab( NTX_MAX_KEY );
      strcpy( pTag->ForExpr, szForExp );
   }
   pTag->pKeyItem = pKeyExpr;
   if( pTag->pKeyItem )
      hb_gcLockItem( pTag->pKeyItem );
   pTag->pForItem = pForExp;
   if( pTag->pForItem )
      hb_gcLockItem( pTag->pForItem );
   pTag->AscendKey = fAscendKey;
   pTag->UniqueKey = fUnique;
   pTag->KeyType = bKeyType;
   pTag->KeyLength = uiKeyLen;
   pTag->Owner = PIF;
   pTag->MaxKeys = (NTXBLOCKSIZE-6)/(uiKeyLen+10) - 1;
   pTag->CurKeyInfo = hb_ntxKeyNew();
   pTag->uiPages = 0;
   return pTag;
}

static LPNTXINDEX hb_ntxIndexNew( NTXAREAP pArea )
{
   LPNTXINDEX pIndex;

   pIndex = ( LPNTXINDEX ) hb_xgrab( sizeof( NTXINDEX ) );
   memset( pIndex, 0, sizeof( NTXINDEX ) );
   pIndex->DiskFile = FS_ERROR;

   pIndex->Owner = pArea;
   pIndex->NextAvail = 0;
   pIndex->TagRoot = 1;
   return pIndex;
}

static void hb_ntxIndexFree( LPNTXINDEX pIndex )
{
   LPTAGINFO pTag;

   pTag = pIndex->CompoundTag;
   if( pTag->RootPage > 0 )
      hb_ntxPageFree( pTag->RootPage,TRUE );
   pTag->RootPage = NULL;
   hb_fsClose( pIndex->DiskFile );
   hb_xfree( pTag->TagName );
   if( pTag->KeyExpr != NULL )
      hb_xfree( pTag->KeyExpr );
   if( pTag->ForExpr != NULL )
      hb_xfree( pTag->ForExpr );
   if( pTag->pKeyItem != NULL )
   {
      hb_gcUnlockItem( pTag->pKeyItem );
      if( hb_itemType( pTag->pKeyItem ) != HB_IT_BLOCK )
         hb_macroDelete( ( HB_MACRO_PTR ) hb_itemGetPtr( pTag->pKeyItem ) );
      hb_itemRelease( pTag->pKeyItem );
   }
   if( pTag->pForItem != NULL )
   {
      hb_gcUnlockItem( pTag->pForItem );
      hb_itemRelease( pTag->pForItem );
   }
   hb_ntxKeyFree( pTag->CurKeyInfo );
   hb_xfree( pTag );
   hb_xfree( pIndex->IndexName );
   hb_xfree( pIndex );
}

static ERRCODE hb_ntxHeaderLoad( LPNTXINDEX pIndex , char *ITN)
{
   NTXHEADER Header;
   LPTAGINFO pTag;
   PHB_ITEM pExpr, pKeyExp;
   ULONG ulPos;

   ulPos = hb_fsSeek( pIndex->DiskFile, 0, SEEK_END );

   hb_fsSeek( pIndex->DiskFile , 0 , 0 );
   if( hb_fsRead( pIndex->DiskFile,(BYTE*)&Header,sizeof(NTXHEADER)) != sizeof(NTXHEADER) )
      return FAILURE;
   if( SELF_COMPILE( ( AREAP ) pIndex->Owner, (BYTE*)Header.key_expr ) == FAILURE )
      return FAILURE;
   pExpr = pIndex->Owner->valResult;
   pKeyExp = hb_itemNew( NULL );
   hb_itemCopy( pKeyExp, pExpr );
   pTag = ( LPTAGINFO ) hb_xgrab( sizeof( TAGINFO ) );
   memset( pTag, 0, sizeof( TAGINFO ) );
   pIndex->CompoundTag = pTag;
   pIndex->NextAvail = Header.next_page;
   pTag->TagBlock = ulPos - 1024;
   pTag->RootBlock = Header.root;
   pTag->TagName = (char *) hb_xgrab( strlen( ITN ) + 1 );
   hb_strncpyUpper( pTag->TagName, ITN, strlen( ITN ) );
   pTag->KeyExpr = (char *) hb_xgrab( NTX_MAX_KEY );
   strcpy( pTag->KeyExpr, Header.key_expr );
   pTag->pKeyItem = pKeyExp;
   hb_gcLockItem( pTag->pKeyItem );
   pTag->AscendKey = 1; /* fAscendKey; */
   pTag->UniqueKey = Header.unique;
   pTag->KeyType = 'C'; /* bKeyType; */
   pTag->KeyLength = Header.key_size;
   pTag->Owner = pIndex;
   pTag->MaxKeys = Header.max_item;
   pTag->CurKeyInfo = hb_ntxKeyNew();
   return SUCCESS;
}

static LPPAGEINFO hb_ntxPageFind( LPNTXINDEX pIndex, LONG ulOffset )
{
   LPPAGEINFO pPage = pIndex->CompoundTag->RootPage;
      /* printf( "\nntxPageFind - 0 ( %5lx %5lx %d )",pIndex,pPage,ulOffset ); */
   while( pPage )
   {
      if( pPage->Page == ulOffset )
         break;
      pPage = pPage->pNext;
   }
   return pPage;
}

static LPPAGEINFO hb_ntxPageLast( LPNTXINDEX pIndex )
{
   LPPAGEINFO pPage = pIndex->CompoundTag->RootPage;
   if( pPage )
   {
      while( pPage->pNext )
      {
         pPage = pPage->pNext;
      }
   }
   return pPage;
}

static LPNTXINDEX ntxFindIndex( NTXAREAP pArea , PHB_ITEM lpOrder )
{
   LPNTXINDEX start, current;

   start = pArea->lpNtxIndex;
   current = start;

   if( !lpOrder )
      return pArea->lpCurIndex;
   else if( hb_itemType( lpOrder ) == HB_IT_STRING )
   {
      do
      {
         if( !strcmp( current->CompoundTag->TagName , hb_itemGetCPtr( lpOrder ) ) )
            return current;
         current = current->pNext;
      } while( current );
   }
   else
   {
      do
      {
         if( current->TagRoot == hb_itemGetNI( lpOrder ) )
            return current;
         current = current->pNext;
      } while( current );
   }
   return NULL;
}

/* Implementation of exported functions */

static ERRCODE ntxGoBottom( NTXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("ntxGoBottom(%p)", pArea));

   if ( !pArea->lpCurIndex || !pArea->lpNtxIndex )
     SUPER_GOBOTTOM( ( AREAP ) pArea );
   else
   {
     LPTAGINFO pTag;

     pTag = pArea->lpCurIndex->CompoundTag;
     hb_ntxTagKeyRead( pTag, BTTM_RECORD );
     SELF_GOTO( ( AREAP ) pArea, pTag->CurKeyInfo->Xtra );
   }
   return SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
}

static ERRCODE ntxGoTo( NTXAREAP pArea, ULONG ulRecNo )
{
   HB_TRACE(HB_TR_DEBUG, ("ntxGoTo(%p, %lu)", pArea, ulRecNo));
   if( pArea->lpCurIndex &&
         (ULONG)pArea->lpCurIndex->CompoundTag->CurKeyInfo->Xtra != ulRecNo )
      pArea->lpCurIndex->CompoundTag->CurKeyInfo->Tag = 0;
   return SUPER_GOTO( ( AREAP ) pArea,ulRecNo );
}

static ERRCODE ntxGoTop( NTXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("ntxGoTop(%p)", pArea));

   if ( !pArea->lpCurIndex || !pArea->lpNtxIndex )
     SUPER_GOTOP( ( AREAP ) pArea );
   else
   {
     LPTAGINFO pTag;

     pTag = pArea->lpCurIndex->CompoundTag;
     hb_ntxTagKeyRead( pTag, TOP_RECORD );
     SELF_GOTO( ( AREAP ) pArea, pTag->CurKeyInfo->Xtra );
   }
   return SELF_SKIPFILTER( ( AREAP ) pArea, 1 );
}

static ERRCODE ntxSeek( NTXAREAP pArea, BOOL bSoftSeek, PHB_ITEM pKey, BOOL bFindLast )
{
   PHB_ITEM pError;
   ERRCODE retvalue;
   HB_TRACE(HB_TR_DEBUG, ("ntxSeek(%p, %d, %p, %d)", pArea, bSoftSeek, pKey, bFindLast));

   if ( ! pArea->lpNtxIndex )
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
     LPTAGINFO pTag;
     LPNTXINDEX lpCurIndex;

     pTag = pArea->lpCurIndex->CompoundTag;
     pKey2 = hb_ntxKeyNew();
     hb_itemCopy( pKey2->pItem, pKey );
     if ( bFindLast )
       pKey2->Tag = NTX_MAX_REC_NUM;
     else
       pKey2->Tag = NTX_IGNORE_REC_NUM;
     pKey2->Xtra = 0;

     if( pArea->fShared )
        while( !hb_fsLock( pArea->lpCurIndex->DiskFile, 0, 512, FL_LOCK ) );
     lRecno = hb_ntxTagKeyFind( pTag, pKey2 );
     if( pArea->fShared )
     {
        hb_ntxPageFree( pTag->RootPage,TRUE );
        pTag->RootPage = NULL;
        hb_fsLock( pArea->lpCurIndex->DiskFile, 0, 512, FL_UNLOCK );
     }
     pArea->fEof = pTag->TagEOF;
     pArea->fBof = pTag->TagBOF;
     hb_ntxKeyFree( pKey2 );
     if ( lRecno > 0 )
     {
       retvalue = SELF_GOTO( ( AREAP ) pArea, lRecno );
       pArea->fFound = TRUE;
       return retvalue;
     }
     else
     {
       pArea->fFound = FALSE;
       if ( bSoftSeek && !pTag->TagEOF )
       {
         return SELF_GOTO( ( AREAP ) pArea, lRecno );
       }
       else
       {
         lpCurIndex = pArea->lpCurIndex;
         pArea->lpCurIndex = NULL;
         SELF_GOBOTTOM( ( AREAP ) pArea );
         retvalue = SELF_SKIPRAW( ( AREAP ) pArea, 1 );
         pArea->lpCurIndex = lpCurIndex;
         return retvalue;
       }
     }
   }
}

static ERRCODE ntxSkipRaw( NTXAREAP pArea, LONG lToSkip )
{
   HB_TRACE(HB_TR_DEBUG, ("ntxSkipRaw(%p, %ld)", pArea, lToSkip));

   if ( ! pArea->lpCurIndex )
     SUPER_SKIPRAW( ( AREAP ) pArea, lToSkip );
   else
   {
     LPTAGINFO pTag = pArea->lpCurIndex->CompoundTag;

     if ( pArea->fBof )
        SELF_GOTOP( ( AREAP ) pArea );

     if ( lToSkip == 0 )
       SUPER_SKIPRAW( ( AREAP ) pArea, 0 );
     else if ( lToSkip > 0 )
     {
       if ( !pArea->fEof )
       {
          while ( !pTag->TagEOF && lToSkip-- > 0 )
            hb_ntxTagKeyRead( pTag, NEXT_RECORD );
          if ( !pTag->TagEOF )
            SELF_GOTO( ( AREAP ) pArea, pTag->CurKeyInfo->Xtra );
          else
          {
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
         hb_ntxTagKeyRead( pTag, PREV_RECORD );
       if ( !pTag->TagBOF )
         SELF_GOTO( ( AREAP ) pArea, pTag->CurKeyInfo->Xtra );
       else
       {
         pTag->TagBOF = FALSE;
         SELF_GOTOP( ( AREAP ) pArea );
         pArea->fBof = pTag->TagBOF = TRUE;
       }
     }
   }
   return SUCCESS;
}

static ERRCODE ntxAppend( NTXAREAP pArea, BOOL bUnLockAll )
{
   HB_TRACE(HB_TR_DEBUG, ("ntxAppend(%p, %d)", pArea, bUnLockAll ));

   if( SUPER_APPEND( ( AREAP ) pArea, bUnLockAll ) == SUCCESS )
   {
      LPNTXINDEX lpIndex, lpIndexTmp;
      LPTAGINFO pTag;

      lpIndex = pArea->lpNtxIndex;
      lpIndexTmp = pArea->lpCurIndex;
      while( lpIndex )
      {
         pArea->lpCurIndex = lpIndex;
         pTag = lpIndex->CompoundTag;
         hb_ntxGetCurrentKey( pTag, pTag->CurKeyInfo );
         if( pArea->fShared )
            while( !hb_fsLock( lpIndex->DiskFile, 0, 512, FL_LOCK ) );
         hb_ntxPageKeyAdd( hb_ntxPageLoad( 0 ), pTag->CurKeyInfo->pItem, 0);
         if( pArea->fShared )
         {
            hb_ntxPageFree( pTag->RootPage,TRUE );
            pTag->RootPage = NULL;
            hb_fsLock( lpIndex->DiskFile, 0, 512, FL_UNLOCK );
         }
         lpIndex = lpIndex->pNext;
      }
      pArea->lpCurIndex = lpIndexTmp;
      return SUCCESS;
   }
   else
      return FAILURE;
}

static ERRCODE ntxPutValue( NTXAREAP pArea, USHORT uiIndex, PHB_ITEM pItem )
{
   LPNTXINDEX lpIndex, lpIndexTmp;
   LPKEYINFO pKey, pKeyOld;
   LPTAGINFO pTag;
   LPPAGEINFO pPage;
   PHB_ITEM pItemNew;

   HB_TRACE(HB_TR_DEBUG, ("ntxPutValue(%p, %hu, %p)", pArea, uiIndex, pItem));

   pItemNew = hb_itemNew( pItem );
   hb_itemClear( pItem );

   lpIndex = pArea->lpNtxIndex;
   lpIndexTmp = pArea->lpCurIndex;
   while( lpIndex )
   {
      pTag = lpIndex->CompoundTag;
      hb_ntxGetCurrentKey( pTag, pTag->CurKeyInfo );
      lpIndex = lpIndex->pNext;
   }

   /* printf( "\n\rntxPutValue - 1:  |%s|",pItemNew->item.asString.value ); */

   SUPER_PUTVALUE( ( AREAP ) pArea, uiIndex, pItemNew);
   hb_itemRelease( pItemNew );

   pKey = hb_ntxKeyNew();
   pKeyOld = hb_ntxKeyNew();
   lpIndex = pArea->lpNtxIndex;
   while( lpIndex )
   {
      pTag = lpIndex->CompoundTag;
      hb_ntxGetCurrentKey( pTag, pKey );
      if( hb_ntxItemCompare( pKey->pItem, pTag->CurKeyInfo->pItem, TRUE ) )
      {
         pArea->lpCurIndex = lpIndex;
         hb_itemCopy( pKeyOld->pItem, pTag->CurKeyInfo->pItem );
         pKeyOld->Xtra = pTag->CurKeyInfo->Xtra;
         pKeyOld->Tag = NTX_IGNORE_REC_NUM;
         if( pArea->fShared )
            while( !hb_fsLock( lpIndex->DiskFile, 0, 512, FL_LOCK ) );
         if( hb_ntxTagFindCurrentKey( hb_ntxPageLoad( 0 ), pKeyOld->Tag, pKeyOld, FALSE, NULL, NULL, NULL, NULL ) )
         {
             printf( "\n\rntxPutValue: Cannot find current key:" );
             lpIndex = lpIndex->pNext;
             continue;
         }
         pPage = hb_ntxPageLoad( pTag->CurKeyInfo->Tag );
         pPage->CurKey =  hb_ntxPageFindCurrentKey( pPage,pTag->CurKeyInfo->Xtra ) - 1;
         hb_ntxPageKeyDel( pPage, pPage->CurKey );
         hb_ntxPageKeyAdd( hb_ntxPageLoad( 0 ), pKey->pItem, 0);
         if( pArea->fShared )
         {
            hb_ntxPageFree( pTag->RootPage,TRUE );
            pTag->RootPage = NULL;
            hb_fsLock( lpIndex->DiskFile, 0, 512, FL_UNLOCK );
         }
      }
      lpIndex = lpIndex->pNext;
   }
   hb_ntxKeyFree( pKeyOld );
   hb_ntxKeyFree( pKey );
   pArea->lpCurIndex = lpIndexTmp;

   return SUCCESS;
}

/*
 * Retrieve the size of the WorkArea structure.
 */
static ERRCODE ntxStructSize( NTXAREAP pArea, USHORT * uiSize )

{
   HB_TRACE(HB_TR_DEBUG, ("ntxStrucSize(%p, %p)", pArea, uiSize));
   HB_SYMBOL_UNUSED( pArea );

   * uiSize = sizeof( NTXAREA );
   return SUCCESS;
}

static ERRCODE ntxOrderCreate( NTXAREAP pArea, LPDBORDERCREATEINFO pOrderInfo )
{
   PHB_ITEM pExpr, pResult, pError;
   PHB_ITEM pKeyExp, pForExp;
   HB_MACRO_PTR pExpMacro, pForMacro;
   USHORT uiType, uiLen;
   char * szFileName, * szTagName;
   LPNTXINDEX pIndex;
   LPTAGINFO pTag;
   PHB_FNAME pFileName;
   DBORDERINFO pExtInfo;
   BYTE bType;

   HB_TRACE(HB_TR_DEBUG, ("ntxOrderCreate(%p, %p)", pArea, pOrderInfo));

   SELF_ORDLSTCLEAR( ( AREAP ) pArea );
   if( SELF_GOCOLD( ( AREAP ) pArea ) == FAILURE )
      return FAILURE;

   /* If we have a codeblock for the expression, use it */
   if( pOrderInfo->itmCobExpr )
      pExpr = pOrderInfo->itmCobExpr;
   else /* Otherwise, try compiling the key expression string */
   {
      if( SELF_COMPILE( ( AREAP ) pArea, ( BYTE * ) pOrderInfo->abExpr->item.asString.value ) == FAILURE )
         return FAILURE;
      pExpr = pArea->valResult;
      // pArea->valResult = NULL;
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
      // pArea->valResult = NULL;
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
   bType = '\0';
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
         uiLen = pResult->item.asString.length > NTX_MAX_KEY ? NTX_MAX_KEY :
                 pResult->item.asString.length ;
         break;
   }

   /* hb_itemRelease( pResult ); */

   /* Make sure uiLen is not 0 */
   if( !uiLen )
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
         if( SELF_COMPILE( ( AREAP ) pArea, pArea->lpdbOrdCondInfo->abFor ) == FAILURE )
            return FAILURE;
         pExpr = pArea->valResult;
         // pArea->valResult = NULL;
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
         if( SELF_EVALBLOCK( ( AREAP ) pArea, pExpr ) == FAILURE )
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
      pFileName = hb_fsFNameSplit( pArea->szDataFileName );
      if( pFileName->szDrive )
         strcat( szFileName, pFileName->szDrive );
      if( pFileName->szPath )
         strcat( szFileName, pFileName->szPath );
      strcat( szFileName, pFileName->szName );
      pExtInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) pArea, DBOI_BAGEXT, &pExtInfo );
      strcat( szFileName, hb_itemGetCPtr( pExtInfo.itmResult ) );
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
         strcat( szFileName, hb_itemGetCPtr( pExtInfo.itmResult ) );
         hb_itemRelease( pExtInfo.itmResult );
      }
   }
   szTagName = ( char * ) hb_xgrab( strlen( pFileName->szName ) + 1 );
   hb_strncpyUpper( szTagName, pFileName->szName, strlen( pFileName->szName ) );
   hb_xfree( pFileName );

   pIndex = hb_ntxIndexNew( pArea );
   pIndex->IndexName = szFileName;
   pArea->lpNtxIndex = pIndex;
   pArea->lpCurIndex = pIndex;
   pTag = hb_ntxTagNew( pIndex, szTagName, pOrderInfo->abExpr->item.asString.value,
                        pKeyExp, bType, uiLen, (char *) ( pArea->lpdbOrdCondInfo ? pArea->lpdbOrdCondInfo->abFor : NULL ),
                        pForExp, pArea->lpdbOrdCondInfo ? !pArea->lpdbOrdCondInfo->fDescending : TRUE,
                        pOrderInfo->fUnique );
   pIndex->CompoundTag = pTag;

   pIndex->DiskFile = hb_fsCreate( ( BYTE * ) szFileName , FC_NORMAL );
   if(pIndex->DiskFile == FS_ERROR) {
      hb_xfree( szFileName );
      hb_itemRelease( pKeyExp );
      if( pForExp != NULL )
         hb_itemRelease( pForExp );
      if( pExpMacro != NULL )
         hb_macroDelete( pExpMacro );
      if( pForMacro != NULL )
         hb_macroDelete( pForMacro );
      return FAILURE;
   }
   hb_ntxHeaderSave( pIndex );
   if( hb_ntxIndexCreate( pIndex ) == FAILURE )
   {
      return FAILURE;
   }

   SELF_ORDSETCOND( ( AREAP ) pArea, NULL );
   return SELF_GOTOP( ( AREAP ) pArea );
}

static ERRCODE ntxOrderInfo( NTXAREAP pArea, USHORT uiIndex, LPDBORDERINFO pInfo )
{
   LPNTXINDEX pIndex;
   HB_TRACE(HB_TR_DEBUG, ("ntxOrderInfo(%p, %hu, %p)", pArea, uiIndex, pInfo));

   switch( uiIndex )
   {
      case DBOI_CONDITION:
         if( pArea->lpNtxIndex )
         {
            pIndex = ntxFindIndex( pArea , pInfo->itmOrder );
            if( pIndex && ( pIndex->CompoundTag->ForExpr != NULL ) )
            {
               hb_itemPutC( pInfo->itmResult , pIndex->CompoundTag->ForExpr );
               return SUCCESS;
            }
         }
         hb_itemPutC( pInfo->itmResult, "" );
         break;
      case DBOI_EXPRESSION:
         if( pArea->lpNtxIndex )
         {
            pIndex = ntxFindIndex( pArea , pInfo->itmOrder );
            if( pIndex )
            {
               hb_itemPutC( pInfo->itmResult , pIndex->CompoundTag->KeyExpr );
               return SUCCESS;
            }
         }
         hb_itemPutC( pInfo->itmResult, "" );
         break;
      case DBOI_NUMBER:
         if( pArea->lpNtxIndex )
         {
            pIndex = ntxFindIndex( pArea , pInfo->itmOrder );
            if( pIndex )
            {
               hb_itemPutNI( pInfo->itmResult, pIndex->TagRoot );
               return SUCCESS;
            }
         }
         /* TODO: Raise recoverable error */
         break;
      case DBOI_BAGNAME:
         if( pArea->lpNtxIndex )
         {
            pIndex = ntxFindIndex( pArea , pInfo->itmOrder );
            if( pIndex )
            {
               hb_itemPutC( pInfo->itmResult, pIndex->IndexName );
               return SUCCESS;
            }
         }
         hb_itemPutC( pInfo->itmResult, "" );
         break;
      case DBOI_BAGEXT:
         hb_itemPutC( pInfo->itmResult, ".ntx" );
         break;
   }
   return SUCCESS;
}

static ERRCODE ntxOrderListAdd( NTXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   USHORT uiFlags;
   char * szFileName;
   PHB_FNAME pFileName;
   DBORDERINFO pExtInfo;
   LPNTXINDEX pIndex, pIndexNext;
   PHB_ITEM pError = NULL;
   BOOL bRetry;

   HB_TRACE(HB_TR_DEBUG, ("ntxOrderListAdd(%p, %p)", pArea, pOrderInfo));

   szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 3 );
   szFileName[ 0 ] = '\0';
   strcpy( szFileName, hb_itemGetCPtr( pOrderInfo->atomBagName ) );
   pFileName = hb_fsFNameSplit( szFileName );
   if( !pFileName->szExtension )
   {
      pExtInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) pArea, DBOI_BAGEXT, &pExtInfo );
      strcat( szFileName, hb_itemGetCPtr( pExtInfo.itmResult ) );
      hb_itemRelease( pExtInfo.itmResult );
   }
   pIndex = hb_ntxIndexNew( pArea );

   uiFlags =  pArea->fReadonly  ? FO_READ : FO_READWRITE;
   uiFlags |= pArea->fShared ? FO_DENYNONE : FO_EXCLUSIVE;

   do
   {
     pIndex->DiskFile = hb_fsOpen( ( BYTE * ) szFileName, uiFlags );
     if( pIndex->DiskFile == FS_ERROR )
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

   if( pIndex->DiskFile == FS_ERROR )
   {
      ntxOrderListClear( pArea );
      hb_xfree( szFileName );
      return FAILURE;
   }

   if( hb_ntxHeaderLoad( pIndex, pFileName->szName ) == FAILURE )
   {
      hb_xfree( pIndex );
      hb_xfree( szFileName );
      hb_xfree( pFileName );
      hb_fsClose( pIndex->DiskFile );
      return FAILURE;
   }
   pIndex->IndexName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 1 );
   strncpy( pIndex->IndexName, szFileName, _POSIX_PATH_MAX );

   if( pArea->lpNtxIndex )
   {
      pIndex->TagRoot++;
      pIndexNext = pArea->lpNtxIndex;
      while( pIndexNext->pNext )
      {
         pIndex->TagRoot++;
         pIndexNext = pIndexNext->pNext;
      }
      pIndexNext->pNext = pIndex;
   }
   else
   {
      pArea->lpNtxIndex = pIndex;
   }
   if( !pArea->lpCurIndex )
      pArea->lpCurIndex = pIndex;
   hb_xfree( szFileName );
   hb_xfree( pFileName );
   return SUCCESS;
}

static ERRCODE ntxOrderListClear( NTXAREAP pArea )
{
   LPNTXINDEX pIndex, pIndexNext;

   HB_TRACE(HB_TR_DEBUG, ("ntxOrderListClear(%p)", pArea));

   pIndex = pArea->lpNtxIndex;
   while( pIndex )
   {
      pIndexNext = pIndex->pNext;
      hb_ntxIndexFree( pIndex );
      pIndex = pIndexNext;
   }
   pArea->lpNtxIndex = NULL;
   pArea->lpCurIndex = NULL;
   return SUCCESS;
}

static ERRCODE ntxOrderListFocus( NTXAREAP pArea, LPDBORDERINFO pOrderInfo )
{
   LPNTXINDEX pIndex;

   HB_TRACE(HB_TR_DEBUG, ("ntxOrderListFocus(%p, %p)", pArea, pOrderInfo));

   hb_itemPutNI( pOrderInfo->itmResult, pArea->lpCurIndex->TagRoot );

   if( hb_itemType( pOrderInfo->itmOrder ) != HB_IT_STRING &&
               hb_itemGetNI( pOrderInfo->itmOrder ) == 0 )
      pArea->lpCurIndex = NULL;
   else
   {
      pIndex = ntxFindIndex( pArea, pOrderInfo->itmOrder );
      if( pIndex )
         pArea->lpCurIndex = pIndex;
   }

   return SUCCESS;
}

static ERRCODE ntxClose( NTXAREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("ntxClose(%p)", pArea));

   ntxOrderListClear( pArea );
   return SUPER_CLOSE( ( AREAP ) pArea );
}

static ERRCODE ntxLock( NTXAREAP pArea, LPDBLOCKINFO pLockInfo )
{
   HB_TRACE(HB_TR_DEBUG, ("ntxLock(%p, %p)", pArea, pLockInfo));

   if( SUPER_LOCK( ( AREAP ) pArea, pLockInfo ) )
   {
      LPNTXINDEX lpIndex;

      if( pLockInfo->uiMethod == DBLM_FILE )
      {
         lpIndex = pArea->lpNtxIndex;
         while( lpIndex )
         {
            pArea->lpCurIndex = lpIndex;
            /* First 512 bytes of index file are blocked.
               I don't sure, is it right - should be checked.
               = Alexander Kresin =
            */
            if( !hb_fsLock( lpIndex->DiskFile, 0, 512, FL_LOCK ) )
            {
               ntxUnLock( pArea,0 );
               return FAILURE;
            }
            lpIndex = lpIndex->pNext;
         }
      }
      return SUCCESS;
   }
   else
      return FAILURE;
}

static ERRCODE ntxUnLock( NTXAREAP pArea, ULONG ulRecNo )
{
   BOOL fFLocked;

   HB_TRACE(HB_TR_DEBUG, ("ntxUnLock(%p, %lu)", pArea, ulRecNo));

   fFLocked = pArea->fFLocked;
   if( SUPER_UNLOCK( ( AREAP ) pArea, ulRecNo ) )
   {
      LPNTXINDEX lpIndex;

      if( fFLocked )
      {
         lpIndex = pArea->lpNtxIndex;
         while( lpIndex )
         {
            pArea->lpCurIndex = lpIndex;
            hb_fsLock( lpIndex->DiskFile, 0, 512, FL_UNLOCK );
            lpIndex = lpIndex->pNext;
         }
      }
      return SUCCESS;
   }
   else
      return FAILURE;
}

static RDDFUNCS ntxTable = { ntxBof,
                             ntxEof,
                             ntxFound,
                             ( DBENTRYP_V ) ntxGoBottom,
                             ( DBENTRYP_UL ) ntxGoTo,
                             ntxGoToId,
                             ( DBENTRYP_V ) ntxGoTop,
                             ( DBENTRYP_BIB ) ntxSeek,
                             ntxSkip,
                             ntxSkipFilter,
                             ( DBENTRYP_L ) ntxSkipRaw,
                             ntxAddField,
                             ( DBENTRYP_B ) ntxAppend,
                             ntxCreateFields,
                             ntxDeleteRec,
                             ntxDeleted,
                             ntxFieldCount,
                             ntxFieldDisplay,
                             ntxFieldInfo,
                             ntxFieldName,
                             ntxFlush,
                             ntxGetRec,
                             ntxGetValue,
                             ntxGetVarLen,
                             ntxGoCold,
                             ntxGoHot,
                             ntxPutRec,
                             ( DBENTRYP_SI ) ntxPutValue,
                             ntxRecall,
                             ntxRecCount,
                             ntxRecInfo,
                             ntxRecNo,
                             ntxSetFieldsExtent,
                             ntxAlias,
                             ( DBENTRYP_V ) ntxClose,
                             ntxCreate,
                             ntxInfo,
                             ntxNewArea,
                             ntxOpen,
                             ntxRelease,
                             ( DBENTRYP_SP ) ntxStructSize,
                             ntxSysName,
                             ntxEval,
                             ntxPack,
                             ntPackRec,
                             ntxSort,
                             ntxTrans,
                             ntxTransRec,
                             ntxZap,
                             ntxchildEnd,
                             ntxchildStart,
                             ntxchildSync,
                             ntxsyncChildren,
                             ntxclearRel,
                             ntxforceRel,
                             ntxrelArea,
                             ntxrelEval,
                             ntxrelText,
                             ntxsetRel,
                             ( DBENTRYP_OI ) ntxOrderListAdd,
                             ( DBENTRYP_V ) ntxOrderListClear,
                             ntxOrderListDelete,
                             ( DBENTRYP_OI ) ntxOrderListFocus,
                             ntxOrderListRebuild,
                             ntxOrderCondition,
                             ( DBENTRYP_VOC ) ntxOrderCreate,
                             ntxOrderDestroy,
                             ( DBENTRYP_OII ) ntxOrderInfo,
                             ntxClearFilter,
                             ntxClearLocate,
                             ntxClearScope,
                             ntxCountScope,
                             ntxFilterText,
                             ntxScopeInfo,
                             ntxSetFilter,
                             ntxSetLocate,
                             ntxSetScope,
                             ntxSkipScope,
                             ntxCompile,
                             ntxError,
                             ntxEvalBlock,
                             ntxRawLock,
                             ( DBENTRYP_VL ) ntxLock,
                             ( DBENTRYP_UL ) ntxUnLock,
                             ntxCloseMemFile,
                             ntxCreateMemFile,
                             ntxGetValueFile,
                             ntxOpenMemFile,
                             ntxPutValueFile,
                             ntxReadDBHeader,
                             ntxWriteDBHeader,
                             ntxWhoCares
                           };

HB_FUNC(_DBFNTX )
{
}

HB_FUNC( DBFNTX_GETFUNCTABLE )
{
   RDDFUNCS * pTable;
   USHORT * uiCount;

   uiCount = ( USHORT * ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   * uiCount = RDDFUNCSCOUNT;
   pTable = ( RDDFUNCS * ) hb_itemGetPtr( hb_param( 2, HB_IT_POINTER ) );
   if( pTable )
      hb_retni( hb_rddInherit( pTable, &ntxTable, &ntxSuper, ( BYTE * ) "DBF" ) );
   else
      hb_retni( FAILURE );
}
