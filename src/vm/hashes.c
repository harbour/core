/*
 * Harbour Project source code:
 *    The Hash tables API (C level)
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef _HB_HASH_INTERNAL_
#  define _HB_HASH_INTERNAL_
#endif

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbvm.h"
#include "hbxvm.h"
#include "hbstack.h"

#define HB_HASH_ITEM_ALLOC    16

/* internal structures for hashes */
typedef struct _HB_HASHPAIR
{
   HB_ITEM key;
   HB_ITEM value;
} HB_HASHPAIR, * PHB_HASHPAIR;

typedef struct _HB_BASEHASH
{
   PHB_HASHPAIR pPairs;       /* pointer to the array of key/value pairs */
   PHB_ITEM     pDefault;     /* default autoadd value */
   HB_SIZE *    pnPos;        /* the sort order for HB_HASH_KEEPORDER */
   HB_SIZE      nSize;        /* size of allocated pair array */
   HB_SIZE      nLen;         /* number of used items in pair array */
   int          iFlags;       /* hash item flags */
} HB_BASEHASH, * PHB_BASEHASH;


/* This releases hash when called from the garbage collector */
static HB_GARBAGE_FUNC( hb_hashGarbageRelease )
{
   PHB_BASEHASH pBaseHash = ( PHB_BASEHASH ) Cargo;

   HB_TRACE( HB_TR_INFO, ( "hb_hashGarbageRelease(%p)", pBaseHash ) );

   if( pBaseHash->nSize > 0 )
   {
      PHB_HASHPAIR pPairs = pBaseHash->pPairs;
      HB_SIZE nLen = pBaseHash->nLen;

      /*
       * clear the pBaseHash->pPairs to avoid infinite loop in cross
       * referenced items when pBaseArray is not freed due to buggy
       * object destructor [druzus]
       */
      pBaseHash->pPairs = NULL;
      pBaseHash->nLen  = 0;

      if( pBaseHash->pnPos )
      {
         hb_xfree( pBaseHash->pnPos );
         pBaseHash->pnPos = NULL;
      }

      while( nLen-- )
      {
         if( HB_IS_COMPLEX( &pPairs[ nLen ].key ) )
            hb_itemClear( &pPairs[ nLen ].key );
         if( HB_IS_COMPLEX( &pPairs[ nLen ].value ) )
            hb_itemClear( &pPairs[ nLen ].value );
      }
      hb_xfree( pPairs );
   }
   if( pBaseHash->pDefault )
   {
      PHB_ITEM pDefault = pBaseHash->pDefault;
      pBaseHash->pDefault = NULL;
      hb_itemRelease( pDefault );
   }
}

static HB_GARBAGE_FUNC( hb_hashGarbageMark )
{
   PHB_BASEHASH pBaseHash = ( PHB_BASEHASH ) Cargo;

   HB_TRACE( HB_TR_INFO, ( "hb_hashMarkGarbage(%p)", pBaseHash ) );

   if( pBaseHash->nLen > 0 )
   {
      PHB_HASHPAIR pPairs = pBaseHash->pPairs;
      HB_SIZE nLen = pBaseHash->nLen;

      while( nLen-- )
      {
         if( HB_IS_GCITEM( &pPairs[ nLen ].key ) )
            hb_gcItemRef( &pPairs[ nLen ].key );
         if( HB_IS_GCITEM( &pPairs[ nLen ].value ) )
            hb_gcItemRef( &pPairs[ nLen ].value );
      }
   }
   if( pBaseHash->pDefault )
      hb_gcMark( pBaseHash->pDefault );
}

static const HB_GC_FUNCS s_gcHashFuncs =
{
   hb_hashGarbageRelease,
   hb_hashGarbageMark
};

static int hb_hashItemCmp( PHB_ITEM pKey1, PHB_ITEM pKey2, int iFlags )
{
   if( HB_IS_STRING( pKey1 ) )
   {
      if( HB_IS_STRING( pKey2 ) )
      {
         if( iFlags & HB_HASH_BINARY )
            return pKey1->item.asString.length < pKey2->item.asString.length ? -1 :
                 ( pKey1->item.asString.length > pKey2->item.asString.length ? 1 :
                   memcmp( pKey1->item.asString.value,
                           pKey2->item.asString.value,
                           pKey1->item.asString.length ) );
         else if( iFlags & HB_HASH_IGNORECASE )
            return hb_itemStrICmp( pKey1, pKey2, HB_TRUE );
         else
            return hb_itemStrCmp( pKey1, pKey2, HB_TRUE );
      }
      else
         return 1;
   }
   else if( HB_IS_DATETIME( pKey1 ) )
   {
      if( HB_IS_DATETIME( pKey2 ) )
         return pKey1->item.asDateTime.julian < pKey2->item.asDateTime.julian ? -1 :
              ( pKey1->item.asDateTime.julian > pKey2->item.asDateTime.julian ? 1 :
              ( pKey1->item.asDateTime.time < pKey2->item.asDateTime.time ? -1 :
              ( pKey1->item.asDateTime.time < pKey2->item.asDateTime.time ? 1 : 0 ) ) );
      else if( HB_IS_STRING( pKey2 ) )
         return -1;
      else
         return 1;
   }
   else if( HB_IS_POINTER( pKey1 ) )
   {
      if( HB_IS_POINTER( pKey2 ) )
         return pKey1->item.asPointer.value < pKey2->item.asPointer.value ? -1 :
              ( pKey1->item.asPointer.value > pKey2->item.asPointer.value ? 1 : 0 );
      else if( HB_IS_STRING( pKey2 ) || HB_IS_DATETIME( pKey2 ) )
         return -1;
      else
         return 1;
   }
   else if( HB_IS_NUMINT( pKey1 ) && HB_IS_NUMINT( pKey2 ) )
   {
      HB_MAXINT n1 = HB_ITEM_GET_NUMINTRAW( pKey1 ),
                n2 = HB_ITEM_GET_NUMINTRAW( pKey2 );
      return n1 < n2 ? -1 : ( n1 > n2 ? 1 : 0 );
   }
   else if( HB_IS_NUMERIC( pKey2 ) )
   {
      double d1 = hb_itemGetND( pKey1 ), d2 = hb_itemGetND( pKey2 );
      return d1 < d2 ? -1 : ( d1 > d2 ? 1 : 0 );
   }
   return -1;
}

static void hb_hashResort( PHB_BASEHASH pBaseHash )
{
   HB_SIZE nPos;

   for( nPos = 0; nPos < pBaseHash->nLen; ++nPos )
   {
      HB_SIZE nFrom = pBaseHash->pnPos[ nPos ];

      if( nFrom != nPos )
      {
         HB_HASHPAIR pair;
         memcpy( &pair, pBaseHash->pPairs + nPos, sizeof( HB_HASHPAIR ) );
         memcpy( pBaseHash->pPairs + nPos, pBaseHash->pPairs + nFrom, sizeof( HB_HASHPAIR ) );
         memcpy( pBaseHash->pPairs + nFrom, &pair, sizeof( HB_HASHPAIR ) );
      }
   }
}

static void hb_hashSortDo( PHB_BASEHASH pBaseHash )
{
   HB_SIZE nPos, nFrom;
   int iFlags = pBaseHash->iFlags;

   if( pBaseHash->pnPos )
   {
      for( nFrom = 1; nFrom < pBaseHash->nLen; ++nFrom )
      {
         PHB_ITEM pKey = &pBaseHash->pPairs[ pBaseHash->pnPos[ nFrom ] ].key;
         HB_SIZE nLeft = 0, nRight = nFrom;

         while( nLeft < nRight )
         {
            HB_SIZE nMiddle = ( nLeft + nRight ) >> 1;
            int i = hb_hashItemCmp( &pBaseHash->pPairs[ pBaseHash->pnPos[ nMiddle ] ].key,
                                    pKey, iFlags );
            if( i > 0 )
               nRight = nMiddle;
            else
               nLeft = nMiddle + 1;
         }
         if( nLeft < nFrom )
         {
            nRight = pBaseHash->pnPos[ nLeft ];
            memmove( pBaseHash->pnPos + nLeft, pBaseHash->pnPos + nLeft + 1,
                     ( nFrom - nLeft ) * sizeof( HB_SIZE ) );
            pBaseHash->pnPos[ nFrom ] = nRight;
         }
      }
   }
   else
   {
      /* The hash array is probably quite well sorted so this trivial
       * algorithm is the most efficient one [druzus]
       */

      for( nFrom = 1; nFrom < pBaseHash->nLen; ++nFrom )
      {
         nPos = nFrom;
         while( nPos > 0 && hb_hashItemCmp( &pBaseHash->pPairs[ nPos - 1 ].key,
                                            &pBaseHash->pPairs[ nPos ].key,
                                            iFlags ) > 0 )
         {
            HB_HASHPAIR pair;
            memcpy( &pair, pBaseHash->pPairs + nPos - 1, sizeof( HB_HASHPAIR ) );
            memcpy( pBaseHash->pPairs + nPos - 1, pBaseHash->pPairs + nPos, sizeof( HB_HASHPAIR ) );
            memcpy( pBaseHash->pPairs + nPos, &pair, sizeof( HB_HASHPAIR ) );
            --nPos;
         }
      }
   }

   pBaseHash->iFlags &= ~HB_HASH_RESORT;
}

static HB_BOOL hb_hashFind( PHB_BASEHASH pBaseHash, PHB_ITEM pKey, HB_SIZE * pnPos )
{
   HB_SIZE nLeft, nRight, nMiddle;
   int iFlags = pBaseHash->iFlags;
   int i;

   if( iFlags & HB_HASH_RESORT )
      hb_hashSortDo( pBaseHash );

   nLeft = 0;
   nRight = pBaseHash->nLen;

   while( nLeft < nRight )
   {
      nMiddle = ( nLeft + nRight ) >> 1;
      i = hb_hashItemCmp( &pBaseHash->pPairs[ pBaseHash->pnPos ?
                              pBaseHash->pnPos[ nMiddle ] : nMiddle ].key,
                          pKey, iFlags );
      if( i == 0 )
      {
         *pnPos = pBaseHash->pnPos ? pBaseHash->pnPos[ nMiddle ] : nMiddle;
         return HB_TRUE;
      }
      else if( i < 0 )
         nLeft = nMiddle + 1;
      else
         nRight = nMiddle;
   }

   *pnPos = nLeft;
   return HB_FALSE;
}

static void hb_hashResize( PHB_BASEHASH pBaseHash, HB_SIZE nNewSize )
{
   if( pBaseHash->nSize < nNewSize )
   {
      if( pBaseHash->nSize )
      {
         pBaseHash->pPairs = ( PHB_HASHPAIR ) hb_xrealloc( pBaseHash->pPairs,
                                          nNewSize * sizeof( HB_HASHPAIR ) );
         if( pBaseHash->pnPos )
            pBaseHash->pnPos = ( HB_SIZE * ) hb_xrealloc( pBaseHash->pnPos,
                                             nNewSize * sizeof( HB_SIZE ) );
      }
      else
      {
         pBaseHash->pPairs = ( PHB_HASHPAIR ) hb_xgrab( nNewSize * sizeof( HB_HASHPAIR ) );
         if( pBaseHash->iFlags & HB_HASH_KEEPORDER )
            pBaseHash->pnPos = ( HB_SIZE * ) hb_xgrab( nNewSize * sizeof( HB_SIZE ) );
      }

      do
      {
         pBaseHash->pPairs[ pBaseHash->nSize ].key.type = HB_IT_NIL;
         pBaseHash->pPairs[ pBaseHash->nSize ].value.type = HB_IT_NIL;
      }
      while( ++pBaseHash->nSize < nNewSize );
   }
   else if( pBaseHash->nSize > nNewSize && pBaseHash->nLen <= nNewSize )
   {
      pBaseHash->nSize = nNewSize;
      if( nNewSize )
      {
         pBaseHash->pPairs = ( PHB_HASHPAIR ) hb_xrealloc( pBaseHash->pPairs,
                                          nNewSize * sizeof( HB_HASHPAIR ) );
         if( pBaseHash->pnPos )
            pBaseHash->pnPos = ( HB_SIZE * ) hb_xrealloc( pBaseHash->pnPos,
                                             nNewSize * sizeof( HB_SIZE ) );
      }
      else
      {
         hb_xfree( pBaseHash->pPairs );
         pBaseHash->pPairs = NULL;
         if( pBaseHash->pnPos )
         {
            hb_xfree( pBaseHash->pnPos );
            pBaseHash->pnPos = NULL;
         }
      }
   }
}

static PHB_ITEM hb_hashValuePtr( PHB_BASEHASH pBaseHash, PHB_ITEM pKey, HB_BOOL fAdd )
{
   HB_SIZE nPos;

   if( ! hb_hashFind( pBaseHash, pKey, &nPos ) )
   {
      if( ! fAdd )
         return NULL;

      if( pBaseHash->nSize == pBaseHash->nLen )
         hb_hashResize( pBaseHash, pBaseHash->nSize + HB_HASH_ITEM_ALLOC );

      if( pBaseHash->pnPos )
      {
         memmove( pBaseHash->pnPos + nPos + 1, pBaseHash->pnPos + nPos,
                  ( pBaseHash->nLen - nPos ) * sizeof( HB_SIZE ) );
         nPos = ( pBaseHash->pnPos[ nPos ] = pBaseHash->nLen );
      }
      else if( nPos < pBaseHash->nLen )
      {
         memmove( pBaseHash->pPairs + nPos + 1, pBaseHash->pPairs + nPos,
                  ( pBaseHash->nLen - nPos ) * sizeof( HB_HASHPAIR ) );
         pBaseHash->pPairs[ nPos ].key.type = HB_IT_NIL;
         pBaseHash->pPairs[ nPos ].value.type = HB_IT_NIL;
      }
      hb_itemCopy( &pBaseHash->pPairs[ nPos ].key, pKey );
      pBaseHash->nLen++;
      if( pBaseHash->pDefault )
         hb_itemCloneTo( &pBaseHash->pPairs[ nPos ].value, pBaseHash->pDefault );
   }

   return &pBaseHash->pPairs[ nPos ].value;
}

static HB_BOOL hb_hashNewValue( PHB_BASEHASH pBaseHash, PHB_ITEM pKey, PHB_ITEM pValue )
{
   HB_SIZE nPos;

   if( ! hb_hashFind( pBaseHash, pKey, &nPos ) )
   {
      if( pBaseHash->nSize == pBaseHash->nLen )
         hb_hashResize( pBaseHash, pBaseHash->nSize + HB_HASH_ITEM_ALLOC );

      if( pBaseHash->pnPos )
      {
         memmove( pBaseHash->pnPos + nPos + 1, pBaseHash->pnPos + nPos,
                  ( pBaseHash->nLen - nPos ) * sizeof( HB_SIZE ) );
         nPos = ( pBaseHash->pnPos[ nPos ] = pBaseHash->nLen );
      }
      else if( nPos < pBaseHash->nLen )
      {
         memmove( pBaseHash->pPairs + nPos + 1, pBaseHash->pPairs + nPos,
                  ( pBaseHash->nLen - nPos ) * sizeof( HB_HASHPAIR ) );
         pBaseHash->pPairs[ nPos ].key.type = HB_IT_NIL;
         pBaseHash->pPairs[ nPos ].value.type = HB_IT_NIL;
      }

      hb_itemCopy( &pBaseHash->pPairs[ nPos ].key, pKey );
      hb_itemCopyFromRef( &pBaseHash->pPairs[ nPos ].value, pValue );
      pBaseHash->nLen++;

      return HB_TRUE;
   }

   return HB_FALSE;
}

static void hb_hashNewPair( PHB_BASEHASH pBaseHash, PHB_ITEM * pKeyPtr, PHB_ITEM * pValPtr )
{
   if( pBaseHash->nSize == pBaseHash->nLen )
      hb_hashResize( pBaseHash, pBaseHash->nSize + HB_HASH_ITEM_ALLOC );

   if( pBaseHash->pnPos )
      pBaseHash->pnPos[ pBaseHash->nLen ] = pBaseHash->nLen;

   *pKeyPtr = &pBaseHash->pPairs[ pBaseHash->nLen ].key;
   *pValPtr = &pBaseHash->pPairs[ pBaseHash->nLen ].value;

   pBaseHash->nLen++;
}

static void hb_hashDelPair( PHB_BASEHASH pBaseHash, HB_SIZE nPos )
{
   if( --pBaseHash->nLen == 0 )
   {
      PHB_HASHPAIR pPairs = pBaseHash->pPairs;
      pBaseHash->pPairs = NULL;
      pBaseHash->nSize = 0;
      if( pBaseHash->pnPos )
      {
         hb_xfree( pBaseHash->pnPos );
         pBaseHash->pnPos = NULL;
      }
      if( HB_IS_COMPLEX( &pPairs->key ) )
         hb_itemClear( &pPairs->key );
      if( HB_IS_COMPLEX( &pPairs->value ) )
         hb_itemClear( &pPairs->value );
      hb_xfree( pPairs );
   }
   else
   {
      if( pBaseHash->pnPos )
      {
#ifdef HB_FAST_HASH_DEL
         HB_SIZE * pnPos, * pnDel, * pnLast;

         pnPos = pBaseHash->pnPos + pBaseHash->nLen;
         pnDel = pnLast = NULL;
         for( ;; )
         {
            if( *pnPos == nPos )
            {
               pnDel = pnPos;
               if( pnLast != NULL )
                  break;
            }
            if( *pnPos == pBaseHash->nLen )
            {
               pnLast = pnPos;
               if( pnDel != NULL )
                  break;
            }
            if( pnPos-- == pBaseHash->pnPos )
               hb_errInternal( HB_EI_ERRUNRECOV, "HB_HDEL(): corrupted hash index", NULL, NULL );
         }
         *pnLast = *pnDel;
         if( pnDel < pBaseHash->pnPos + pBaseHash->nLen )
            memmove( pnDel, pnDel + 1,
                     ( pBaseHash->pnPos + pBaseHash->nLen - pnDel ) * sizeof( HB_SIZE ) );
         if( nPos != pBaseHash->nLen )
         {
            HB_HASHPAIR pair;
            memcpy( &pair, pBaseHash->pPairs + nPos, sizeof( HB_HASHPAIR ) );
            memcpy( pBaseHash->pPairs + nPos, pBaseHash->pPairs + pBaseHash->nLen,
                    sizeof( HB_HASHPAIR ) );
            nPos = pBaseHash->nLen;
            memcpy( pBaseHash->pPairs + nPos, &pair, sizeof( HB_HASHPAIR ) );
         }
#else
         HB_SIZE n = 0;
         while( n < pBaseHash->nLen )
         {
            if( pBaseHash->pnPos[ n ] > nPos )
               pBaseHash->pnPos[ n++ ]--;
            else if( pBaseHash->pnPos[ n ] == nPos )
               memmove( &pBaseHash->pnPos[ n ], &pBaseHash->pnPos[ n + 1 ],
                        ( pBaseHash->nLen - n ) * sizeof( HB_SIZE ) );
            else
               ++n;
         }
#endif
      }

      if( nPos != pBaseHash->nLen )
      {
         HB_HASHPAIR pair;
         memcpy( &pair, pBaseHash->pPairs + nPos, sizeof( HB_HASHPAIR ) );
         memmove( pBaseHash->pPairs + nPos, pBaseHash->pPairs + nPos + 1,
                  ( pBaseHash->nLen - nPos ) * sizeof( HB_HASHPAIR ) );
         nPos = pBaseHash->nLen;
         memcpy( pBaseHash->pPairs + nPos, &pair, sizeof( HB_HASHPAIR ) );
      }

      hb_itemSetNil( &pBaseHash->pPairs[ nPos ].key );
      hb_itemSetNil( &pBaseHash->pPairs[ nPos ].value );
      if( pBaseHash->nSize - pBaseHash->nLen > ( HB_HASH_ITEM_ALLOC << 1 ) )
      {
         pBaseHash->nSize -= HB_HASH_ITEM_ALLOC;
         pBaseHash->pPairs = ( PHB_HASHPAIR ) hb_xrealloc( pBaseHash->pPairs,
                              pBaseHash->nSize * sizeof( HB_HASHPAIR ) );
         if( pBaseHash->pnPos )
            pBaseHash->pnPos = ( HB_SIZE * ) hb_xrealloc( pBaseHash->pnPos,
                                 pBaseHash->nSize * sizeof( HB_SIZE ) );
      }
   }
}

PHB_ITEM hb_hashNew( PHB_ITEM pItem )
{
   PHB_BASEHASH pBaseHash;

   HB_TRACE( HB_TR_DEBUG, ( "hb_hashNew(%p)", pItem ) );

   if( pItem == NULL )
      pItem = hb_itemNew( NULL );
   else if( HB_IS_COMPLEX( pItem ) )
      hb_itemClear( pItem );

   pBaseHash = ( PHB_BASEHASH ) hb_gcAllocRaw( sizeof( HB_BASEHASH ), &s_gcHashFuncs );
   pBaseHash->pPairs   = NULL;
   pBaseHash->pnPos    = NULL;
   pBaseHash->nSize    = 0;
   pBaseHash->nLen     = 0;
   pBaseHash->iFlags   = HB_HASH_FLAG_DEFAULT;
   pBaseHash->pDefault = NULL;

   pItem->type = HB_IT_HASH;
   pItem->item.asHash.value = pBaseHash;

   return pItem;
}

HB_SIZE hb_hashLen( PHB_ITEM pHash )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashLen(%p)", pHash ) );

   if( HB_IS_HASH( pHash ) )
      return pHash->item.asHash.value->nLen;
   else
      return 0;
}

void hb_hashPreallocate( PHB_ITEM pHash, HB_SIZE nNewSize )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashPreallocate(%p,%" HB_PFS "u)", pHash, nNewSize ) );

   if( HB_IS_HASH( pHash ) )
      hb_hashResize( pHash->item.asHash.value, nNewSize );
}

HB_BOOL hb_hashAllocNewPair( PHB_ITEM pHash, PHB_ITEM * pKeyPtr, PHB_ITEM * pValPtr )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashAllocNewPair(%p,%p,%p)", pHash, pKeyPtr, pValPtr ) );

   if( HB_IS_HASH( pHash ) )
   {
      hb_hashNewPair( pHash->item.asHash.value, pKeyPtr, pValPtr );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

void hb_hashSort( PHB_ITEM pHash )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashSort(%p)", pHash ) );

   if( HB_IS_HASH( pHash ) )
   {
      PHB_BASEHASH pBaseHash = pHash->item.asHash.value;

      if( pBaseHash->iFlags & HB_HASH_RESORT )
         hb_hashSortDo( pBaseHash );

      if( pBaseHash->pnPos )
         hb_hashResort( pBaseHash );
   }
}

PHB_ITEM hb_hashGetItemPtr( PHB_ITEM pHash, PHB_ITEM pKey, int iFlags )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashGetItemPtr(%p,%p,%d)", pHash, pKey, iFlags ) );

   if( HB_IS_HASH( pHash ) && HB_IS_HASHKEY( pKey ) )
   {
      PHB_ITEM pDest = hb_hashValuePtr( pHash->item.asHash.value, pKey,
         iFlags && ( pHash->item.asHash.value->iFlags & iFlags ) == iFlags );
      if( pDest )
         return HB_IS_BYREF( pDest ) ? hb_itemUnRef( pDest ) : pDest;
   }

   return NULL;
}

PHB_ITEM hb_hashGetCItemPtr( PHB_ITEM pHash, const char * pszKey )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashGetCItemPtr(%p,%s)", pHash, pszKey ) );

   if( HB_IS_HASH( pHash ) )
   {
      HB_STACK_TLS_PRELOAD
      /* we will not make any copy of pKey (autoadd is disabled) so it's
       * safe to use hb_itemPutCConst()
       */
      PHB_ITEM pKey = hb_itemPutCConst( hb_stackAllocItem(), pszKey );
      PHB_ITEM pDest = hb_hashValuePtr( pHash->item.asHash.value, pKey, HB_FALSE );
      hb_stackPop();
      if( pDest )
         return HB_IS_BYREF( pDest ) ? hb_itemUnRef( pDest ) : pDest;
   }

   return NULL;
}

HB_SIZE hb_hashGetCItemPos( PHB_ITEM pHash, const char * pszKey )
{
   HB_SIZE nPos = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_hashGetCItemPos(%p,%s)", pHash, pszKey ) );

   if( HB_IS_HASH( pHash ) )
   {
      HB_STACK_TLS_PRELOAD
      /* we will not make any copy of pKey (autoadd is disabled) so it's
       * safe to use hb_itemPutCConst()
       */
      PHB_ITEM pKey = hb_itemPutCConst( hb_stackAllocItem(), pszKey );

      if( hb_hashFind( pHash->item.asHash.value, pKey, &nPos ) )
         nPos++;
      else
         nPos = 0;
      hb_stackPop();
   }

   return nPos;
}

PHB_ITEM hb_hashGetItemRefPtr( PHB_ITEM pHash, PHB_ITEM pKey )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashGetItemRefPtr(%p,%p)", pHash, pKey ) );

   if( HB_IS_HASH( pHash ) && HB_IS_HASHKEY( pKey ) )
   {
      PHB_ITEM pDest = hb_hashValuePtr( pHash->item.asHash.value, pKey,
            ( pHash->item.asHash.value->iFlags & HB_HASH_AUTOADD_REFERENCE ) ==
            HB_HASH_AUTOADD_REFERENCE );
      if( pDest )
      {
         if( ! HB_IS_BYREF( pDest ) )
            pDest = hb_memvarDetachLocal( pDest );
         return pDest;
      }
   }

   return NULL;
}

HB_BOOL hb_hashScan( PHB_ITEM pHash, PHB_ITEM pKey, HB_SIZE * pnPos )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashScan(%p,%p,%p)", pHash, pKey, pnPos ) );

   if( HB_IS_HASH( pHash ) )
   {
      HB_SIZE nPos;
      if( HB_IS_HASHKEY( pKey ) )
      {
         if( hb_hashFind( pHash->item.asHash.value, pKey, &nPos ) )
         {
            if( pnPos )
               *pnPos = nPos + 1;
            return HB_TRUE;
         }
      }
      else if( HB_IS_HASH( pKey ) && pKey->item.asHash.value->nLen == 1 )
      {
         if( hb_hashFind( pHash->item.asHash.value, &pKey->item.asHash.value->pPairs[ 0 ].key, &nPos ) )
         {
            PHB_ITEM pVal1 = &pHash->item.asHash.value->pPairs[ nPos ].value;
            PHB_ITEM pVal2 = &pKey->item.asHash.value->pPairs[ 0 ].value;

            if( hb_itemEqual( pVal1, pVal2 ) )
            {
               if( pnPos )
                  *pnPos = nPos + 1;
               return HB_TRUE;
            }
         }
      }
   }
   if( pnPos )
      *pnPos = 0;
   return HB_FALSE;
}

HB_BOOL hb_hashScanSoft( PHB_ITEM pHash, PHB_ITEM pKey, HB_SIZE * pnPos )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashScanSoft(%p,%p,%p)", pHash, pKey, pnPos ) );

   if( HB_IS_HASH( pHash ) && HB_IS_HASHKEY( pKey ) )
   {
      HB_SIZE nPos;
      if( hb_hashFind( pHash->item.asHash.value, pKey, &nPos ) )
      {
         if( pnPos )
            *pnPos = nPos + 1;
         return HB_TRUE;
      }
      else
      {
         if( pnPos )
         {
            if( nPos != 0 && pHash->item.asHash.value->pnPos )
               nPos = pHash->item.asHash.value->pnPos[ nPos - 1 ] + 1;
            *pnPos = nPos;
         }
         return HB_FALSE;
      }
   }
   if( pnPos )
      *pnPos = 0;
   return HB_FALSE;
}

HB_BOOL hb_hashClear( PHB_ITEM pHash )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashClear(%p)", pHash ) );

   if( HB_IS_HASH( pHash ) )
   {
      if( pHash->item.asHash.value->nSize )
      {
         while( pHash->item.asHash.value->nLen )
         {
            pHash->item.asHash.value->nLen--;
            if( HB_IS_COMPLEX( &pHash->item.asHash.value->pPairs[ pHash->item.asHash.value->nLen ].key ) )
               hb_itemClear( &pHash->item.asHash.value->pPairs[ pHash->item.asHash.value->nLen ].key );
            if( HB_IS_COMPLEX( &pHash->item.asHash.value->pPairs[ pHash->item.asHash.value->nLen ].value ) )
               hb_itemClear( &pHash->item.asHash.value->pPairs[ pHash->item.asHash.value->nLen ].value );
         }
         /*
          * This condition is a protection against recursive call
          * from .prg object destructor [druzus]
          */
         if( pHash->item.asHash.value->nSize )
         {
            hb_xfree( pHash->item.asHash.value->pPairs );
            pHash->item.asHash.value->pPairs = NULL;
            pHash->item.asHash.value->nSize = 0;
            if( pHash->item.asHash.value->pnPos )
            {
               hb_xfree( pHash->item.asHash.value->pnPos );
               pHash->item.asHash.value->pnPos = NULL;
            }
         }
      }
      return HB_TRUE;
   }

   return HB_FALSE;
}

HB_BOOL hb_hashDel( PHB_ITEM pHash, PHB_ITEM pKey )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashDel(%p,%p)", pHash, pKey ) );

   if( HB_IS_HASH( pHash ) && HB_IS_HASHKEY( pKey ) )
   {
      PHB_BASEHASH pBaseHash = pHash->item.asHash.value;
      HB_SIZE nPos;

      if( hb_hashFind( pBaseHash, pKey, &nPos ) )
      {
         hb_hashDelPair( pBaseHash, nPos );
         return HB_TRUE;
      }
   }

   return HB_FALSE;
}

HB_BOOL hb_hashRemove( PHB_ITEM pHash, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashRemove(%p,%p)", pHash, pItem ) );

   if( HB_IS_HASH( pHash ) )
   {
      if( HB_IS_HASHKEY( pItem ) )
      {
         hb_hashDel( pHash, pItem );
         return HB_TRUE;
      }
      else if( HB_IS_ARRAY( pItem ) )
      {
         HB_SIZE n = 0;
         PHB_ITEM pKey;
         while( ( pKey = hb_arrayGetItemPtr( pItem, ++n ) ) != NULL )
            hb_hashDel( pHash, pKey );
         return HB_TRUE;
      }
      else if( HB_IS_HASH( pItem ) )
      {
         if( pHash->item.asHash.value == pItem->item.asHash.value )
            hb_hashClear( pHash );
         else
         {
            HB_SIZE nLen = 0;
            while( nLen < pItem->item.asHash.value->nLen )
               hb_hashDel( pHash, &pItem->item.asHash.value->pPairs[ nLen++ ].key );
         }
         return HB_TRUE;
      }
   }
   return HB_FALSE;
}

HB_BOOL hb_hashAdd( PHB_ITEM pHash, PHB_ITEM pKey, PHB_ITEM pValue )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashAdd(%p,%p,%p)", pHash, pKey, pValue ) );

   if( HB_IS_HASH( pHash ) && HB_IS_HASHKEY( pKey ) )
   {
      PHB_ITEM pDest = hb_hashValuePtr( pHash->item.asHash.value, pKey, HB_TRUE );
      if( pDest )
      {
         if( HB_IS_BYREF( pDest ) )
            pDest = hb_itemUnRef( pDest );
         if( pValue )
            hb_itemCopyFromRef( pDest, pValue );
         else
            hb_itemSetNil( pDest );
         return HB_TRUE;
      }
   }

   return HB_FALSE;
}

HB_BOOL hb_hashAddNew( PHB_ITEM pHash, PHB_ITEM pKey, PHB_ITEM pValue )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashAddNew(%p,%p,%p)", pHash, pKey, pValue ) );

   if( HB_IS_HASH( pHash ) && HB_IS_HASHKEY( pKey ) )
      return hb_hashNewValue( pHash->item.asHash.value, pKey, pValue );
   else
      return HB_FALSE;
}

PHB_ITEM hb_hashGetKeyAt( PHB_ITEM pHash, HB_SIZE nPos )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashGetKeyAt(%p,%" HB_PFS "u)", pHash, nPos ) );

   if( HB_IS_HASH( pHash ) && nPos > 0 && nPos <= pHash->item.asHash.value->nLen )
      return &pHash->item.asHash.value->pPairs[ nPos - 1 ].key;
   else
      return NULL;
}

PHB_ITEM hb_hashGetValueAt( PHB_ITEM pHash, HB_SIZE nPos )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashGetValueAt(%p,%" HB_PFS "u)", pHash, nPos ) );

   if( HB_IS_HASH( pHash ) && nPos > 0 && nPos <= pHash->item.asHash.value->nLen )
   {
      PHB_ITEM pValue = &pHash->item.asHash.value->pPairs[ nPos - 1 ].value;
      return HB_IS_BYREF( pValue ) ? hb_itemUnRef( pValue ) : pValue;
   }
   else
      return NULL;
}

HB_BOOL hb_hashDelAt( PHB_ITEM pHash, HB_SIZE nPos )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashDelAt(%p,%" HB_PFS "u)", pHash, nPos ) );

   if( HB_IS_HASH( pHash ) && nPos > 0 && nPos <= pHash->item.asHash.value->nLen )
   {
      hb_hashDelPair( pHash->item.asHash.value, nPos - 1 );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

/* retrives the hash unique ID */
void * hb_hashId( PHB_ITEM pHash )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashId(%p)", pHash ) );

   if( HB_IS_HASH( pHash ) )
      return ( void * ) pHash->item.asHash.value;
   else
      return NULL;
}

void hb_hashCloneBody( PHB_ITEM pHash, PHB_ITEM pDest, PHB_NESTED_CLONED pClonedList )
{
   HB_SIZE nPos;

   HB_TRACE( HB_TR_DEBUG, ( "hb_hashCloneBody(%p,%p,%p)", pHash, pDest, pClonedList ) );

   hb_hashNew( pDest );
   pDest->item.asHash.value->iFlags = pHash->item.asHash.value->iFlags;
   hb_hashResize( pDest->item.asHash.value, pHash->item.asHash.value->nLen );
   if( pHash->item.asHash.value->pDefault )
   {
      pDest->item.asHash.value->pDefault =
                              hb_itemNew( pHash->item.asHash.value->pDefault );
      hb_gcUnlock( pDest->item.asHash.value->pDefault );
   }
   if( pHash->item.asHash.value->pnPos )
      memcpy( pDest->item.asHash.value->pnPos,
              pHash->item.asHash.value->pnPos,
              pHash->item.asHash.value->nLen * sizeof( HB_SIZE ) );
   for( nPos = 0; nPos < pHash->item.asHash.value->nLen; ++nPos )
   {
      PHB_ITEM pValue = &pHash->item.asHash.value->pPairs[ nPos ].value;
      if( HB_IS_BYREF( pValue ) )
         pValue = hb_itemUnRef( pValue );
      hb_itemCopy( &pDest->item.asHash.value->pPairs[ nPos ].key,
                   &pHash->item.asHash.value->pPairs[ nPos ].key );
      pDest->item.asHash.value->nLen++;
      hb_cloneNested( &pDest->item.asHash.value->pPairs[ nPos ].value, pValue, pClonedList );
   }
}

PHB_ITEM hb_hashCloneTo( PHB_ITEM pDest, PHB_ITEM pHash )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashCloneTo(%p,%p)", pDest, pHash ) );

   if( HB_IS_HASH( pHash ) )
   {
      PHB_NESTED_CLONED pClonedList, pCloned;

      pClonedList = ( PHB_NESTED_CLONED ) hb_xgrab( sizeof( HB_NESTED_CLONED ) );
      pClonedList->value = ( void * ) pHash->item.asHash.value;
      pClonedList->pDest = pDest;
      pClonedList->pNext = NULL;

      hb_hashCloneBody( pHash, pDest, pClonedList );

      do
      {
         pCloned = pClonedList;
         pClonedList = pClonedList->pNext;
         hb_xfree( pCloned );
      }
      while( pClonedList );
   }

   return pDest;
}

PHB_ITEM hb_hashClone( PHB_ITEM pHash )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashClone(%p)", pHash ) );

   return hb_hashCloneTo( hb_itemNew( NULL ), pHash );
}

void hb_hashJoin( PHB_ITEM pDest, PHB_ITEM pSource, int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashJoin(%p,%p,%d)", pDest, pSource, iType ) );

   if( HB_IS_HASH( pDest ) && HB_IS_HASH( pSource ) )
   {
      PHB_BASEHASH pBaseHash;
      HB_SIZE nPos;

      switch( iType )
      {
         case HB_HASH_UNION:        /* OR */
            pBaseHash = pSource->item.asHash.value;
            if( pBaseHash != pDest->item.asHash.value )
            {
               for( nPos = 0; nPos < pBaseHash->nLen; ++nPos )
               {
                  PHB_ITEM pVal = &pBaseHash->pPairs[ nPos ].value;
                  if( HB_IS_BYREF( pVal ) )
                     pVal = hb_itemUnRef( pVal );
                  hb_hashAdd( pDest, &pBaseHash->pPairs[ nPos ].key, pVal );
               }
            }
            break;

         case HB_HASH_INTERSECT:    /* AND */
            pBaseHash = pDest->item.asHash.value;
            if( pBaseHash != pSource->item.asHash.value )
            {
               for( nPos = 0; nPos < pBaseHash->nLen; )
               {
                  HB_SIZE nSrcPos;
                  if( hb_hashFind( pSource->item.asHash.value,
                                   &pBaseHash->pPairs[ nPos ].key, &nSrcPos ) )
                  {
                     PHB_ITEM pDestVal = &pBaseHash->pPairs[ nPos ].value;
                     if( HB_IS_BYREF( pDestVal ) )
                        pDestVal = hb_itemUnRef( pDestVal );
                     hb_itemCopyFromRef( pDestVal,
                                         &pSource->item.asHash.value->pPairs[ nSrcPos ].value );
                     ++nPos;
                  }
                  else
                     hb_hashDelPair( pBaseHash, nPos );
               }
            }
            break;

         case HB_HASH_DIFFERENCE:   /* XOR */
            pBaseHash = pSource->item.asHash.value;
            if( pBaseHash == pDest->item.asHash.value )
               hb_hashClear( pDest );
            else
            {
               for( nPos = 0; nPos < pBaseHash->nLen; ++nPos )
               {
                  if( ! hb_hashDel( pDest, &pBaseHash->pPairs[ nPos ].key ) )
                  {
                     PHB_ITEM pVal = &pBaseHash->pPairs[ nPos ].value;
                     if( HB_IS_BYREF( pVal ) )
                        pVal = hb_itemUnRef( pVal );
                     hb_hashAdd( pDest, &pBaseHash->pPairs[ nPos ].key, pVal );
                  }
               }
            }
            break;

         case HB_HASH_REMOVE:       /* NOT -> h1 AND ( h1 XOR h2 ) */
            pBaseHash = pSource->item.asHash.value;
            if( pBaseHash == pDest->item.asHash.value )
               hb_hashClear( pDest );
            else
            {
               for( nPos = 0; nPos < pBaseHash->nLen; ++nPos )
                  hb_hashDel( pDest, &pBaseHash->pPairs[ nPos ].key );
            }
            break;
      }
   }
}

PHB_ITEM hb_hashGetKeys( PHB_ITEM pHash )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashGetKeys(%p)", pHash ) );

   if( HB_IS_HASH( pHash ) )
   {
      PHB_ITEM pKeys = hb_itemArrayNew( hb_hashLen( pHash ) ), pKey;
      HB_SIZE nPos = 0;

      while( ( pKey = hb_hashGetKeyAt( pHash, ++nPos ) ) != NULL )
      {
         PHB_ITEM pDest = hb_arrayGetItemPtr( pKeys, nPos );
         if( ! pDest )
            break;
         hb_itemCopy( pDest, pKey );
      }
      return pKeys;
   }

   return NULL;
}

PHB_ITEM hb_hashGetValues( PHB_ITEM pHash )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashGetValues(%p)", pHash ) );

   if( HB_IS_HASH( pHash ) )
   {
      PHB_ITEM pValues = hb_itemArrayNew( hb_hashLen( pHash ) ), pVal;
      HB_SIZE nPos = 0;

      while( ( pVal = hb_hashGetValueAt( pHash, ++nPos ) ) != NULL )
      {
         PHB_ITEM pDest = hb_arrayGetItemPtr( pValues, nPos );
         if( ! pDest )
            break;
         hb_itemCopy( pDest, pVal );
      }
      return pValues;
   }

   return NULL;
}

void hb_hashSetDefault( PHB_ITEM pHash, PHB_ITEM pValue )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashSetDefault(%p,%p)", pHash, pValue ) );

   if( HB_IS_HASH( pHash ) )
   {
      if( pHash->item.asHash.value->pDefault )
      {
         hb_itemRelease( pHash->item.asHash.value->pDefault );
         pHash->item.asHash.value->pDefault = NULL;
      }
      if( pValue && ! HB_IS_NIL( pValue ) &&
          ( ! HB_IS_HASH( pValue ) || pHash->item.asHash.value !=
                                     pValue->item.asHash.value ) )
      {
         pHash->item.asHash.value->pDefault = hb_itemClone( pValue );
         hb_gcUnlock( pHash->item.asHash.value->pDefault );
      }
   }
}

PHB_ITEM hb_hashGetDefault( PHB_ITEM pHash )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashGetDefault(%p)", pHash ) );

   if( HB_IS_HASH( pHash ) )
      return pHash->item.asHash.value->pDefault;
   else
      return NULL;
}

void hb_hashSetFlags( PHB_ITEM pHash, int iFlags )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashSetFlags(%p,%d)", pHash, iFlags ) );

   if( HB_IS_HASH( pHash ) )
   {
      pHash->item.asHash.value->iFlags |= iFlags;
      if( pHash->item.asHash.value->pnPos == NULL &&
          pHash->item.asHash.value->nSize &&
          ( pHash->item.asHash.value->iFlags & HB_HASH_KEEPORDER ) != 0 )
      {
         HB_SIZE n = pHash->item.asHash.value->nSize;

         pHash->item.asHash.value->pnPos = ( HB_SIZE * )
            hb_xgrab( pHash->item.asHash.value->nSize * sizeof( HB_SIZE ) );
         do
         {
            --n;
            pHash->item.asHash.value->pnPos[ n ] = n;
         }
         while( n );
      }
   }
}

void hb_hashClearFlags( PHB_ITEM pHash, int iFlags )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashClearFlags(%p,%d)", pHash, iFlags ) );

   if( HB_IS_HASH( pHash ) )
   {
      pHash->item.asHash.value->iFlags &= ~iFlags;
      if( pHash->item.asHash.value->pnPos != NULL &&
          ( pHash->item.asHash.value->iFlags & HB_HASH_KEEPORDER ) == 0 )
      {
         hb_hashResort( pHash->item.asHash.value );
      }
   }
}

int hb_hashGetFlags( PHB_ITEM pHash )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_hashGetFlags(%p)", pHash ) );

   if( HB_IS_HASH( pHash ) )
      return pHash->item.asHash.value->iFlags;
   else
      return 0;
}
