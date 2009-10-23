/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    The Hash tables API (C level)
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
   HB_ITEM  key;
   HB_ITEM  value;
} HB_HASHPAIR, * PHB_HASHPAIR;

typedef struct _HB_BASEHASH
{
   PHB_HASHPAIR   pPairs;     /* pointer to the array of key/value pairs */
   PHB_ITEM       pDefault;   /* default autoadd value */
   ULONG          ulSize;     /* size of allocated pair array */
   ULONG          ulLen;      /* number of used items in pair array */
   int            iFlags;     /* hash item flags */
} HB_BASEHASH, * PHB_BASEHASH, * HB_BASEHASH_PTR;


/* This releases hash when called from the garbage collector */
static HB_GARBAGE_FUNC( hb_hashGarbageRelease )
{
   PHB_BASEHASH pBaseHash = ( PHB_BASEHASH ) Cargo;

   HB_TRACE(HB_TR_INFO, ("hb_hashGarbageRelease(%p)", pBaseHash ));

   if( pBaseHash->ulSize > 0 )
   {
      PHB_HASHPAIR pPairs = pBaseHash->pPairs;
      ULONG ulLen = pBaseHash->ulLen;

      /*
       * clear the pBaseHash->pPairs to avoid infinite loop in cross
       * referenced items when pBaseArray is not freed due to buggy
       * object destructor [druzus]
       */
      pBaseHash->pPairs = NULL;
      pBaseHash->ulLen  = 0;

      while( ulLen-- )
      {
         if( HB_IS_COMPLEX( &pPairs[ ulLen ].key ) )
            hb_itemClear( &pPairs[ ulLen ].key );
         if( HB_IS_COMPLEX( &pPairs[ ulLen ].value ) )
            hb_itemClear( &pPairs[ ulLen ].value );
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

   HB_TRACE(HB_TR_INFO, ("hb_hashMarkGarbage(%p)", pBaseHash ));

   if( pBaseHash->ulLen > 0 )
   {
      PHB_HASHPAIR pPairs = pBaseHash->pPairs;
      ULONG ulLen = pBaseHash->ulLen;

      while( ulLen-- )
      {
         if( HB_IS_GCITEM( &pPairs[ ulLen ].key ) )
            hb_gcItemRef( &pPairs[ ulLen ].key );
         if( HB_IS_GCITEM( &pPairs[ ulLen ].value ) )
            hb_gcItemRef( &pPairs[ ulLen ].value );
      }
   }
   if( pBaseHash->pDefault && HB_IS_GCITEM( pBaseHash->pDefault ) )
      hb_gcItemRef( pBaseHash->pDefault );
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
            return hb_itemStrICmp( pKey1, pKey2, TRUE );
         else
            return hb_itemStrCmp( pKey1, pKey2, TRUE );
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
      HB_LONG l1 = HB_ITEM_GET_NUMINTRAW( pKey1 ),
              l2 = HB_ITEM_GET_NUMINTRAW( pKey2 );
      return l1 < l2 ? -1 : ( l1 > l2 ? 1 : 0 );
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
   ULONG ulPos, ulFrom;
   int iFlags = pBaseHash->iFlags;

   /* The hash array is probably quite well sorted so this trivial
    * algorithm is the most efficient one [druzus]
    */
   for( ulFrom = 1; ulFrom < pBaseHash->ulLen; ++ulFrom )
   {
      ulPos = ulFrom;
      while( ulPos > 0 && hb_hashItemCmp( &pBaseHash->pPairs[ ulPos - 1 ].key,
                                          &pBaseHash->pPairs[ ulPos ].key,
                                          iFlags ) > 0 )
      {
         HB_HASHPAIR pair;
         memcpy( &pair, pBaseHash->pPairs + ulPos - 1, sizeof( HB_HASHPAIR ) );
         memcpy( pBaseHash->pPairs + ulPos - 1, pBaseHash->pPairs + ulPos, sizeof( HB_HASHPAIR ) );
         memcpy( pBaseHash->pPairs + ulPos, &pair, sizeof( HB_HASHPAIR ) );
         --ulPos;
      }
   }

   pBaseHash->iFlags &= ~HB_HASH_RESORT;
}

static BOOL hb_hashFind( PHB_BASEHASH pBaseHash, PHB_ITEM pKey, ULONG * pulPos )
{
   ULONG ulLeft, ulRight, ulMiddle;
   int iFlags = pBaseHash->iFlags;
   int i;

   if( iFlags & HB_HASH_RESORT )
      hb_hashResort( pBaseHash );

   ulLeft = 0;
   ulRight = pBaseHash->ulLen;

   while( ulLeft < ulRight )
   {
      ulMiddle = ( ulLeft + ulRight ) >> 1;
      i = hb_hashItemCmp( &pBaseHash->pPairs[ ulMiddle ].key, pKey, iFlags );
      if( i == 0 )
      {
         if( pulPos )
            *pulPos = ulMiddle;
         return TRUE;
      }
      else if( i < 0 )
         ulLeft = ulMiddle + 1;
      else
         ulRight = ulMiddle;
   }

   if( pulPos )
      *pulPos = ulLeft;
   return FALSE;
}

static void hb_hashResize( PHB_BASEHASH pBaseHash, ULONG ulNewSize )
{
   if( pBaseHash->ulSize < ulNewSize )
   {
      if( pBaseHash->ulSize )
         pBaseHash->pPairs = ( PHB_HASHPAIR ) hb_xrealloc( pBaseHash->pPairs,
                                          ulNewSize * sizeof( HB_HASHPAIR ) );
      else
         pBaseHash->pPairs = ( PHB_HASHPAIR ) hb_xgrab( ulNewSize * sizeof( HB_HASHPAIR ) );

      do
      {
         pBaseHash->pPairs[ pBaseHash->ulSize ].key.type = HB_IT_NIL;
         pBaseHash->pPairs[ pBaseHash->ulSize ].value.type = HB_IT_NIL;
      }
      while( ++pBaseHash->ulSize < ulNewSize );
   }
   else if( pBaseHash->ulSize > ulNewSize && pBaseHash->ulLen <= ulNewSize )
   {
      pBaseHash->ulSize = ulNewSize;
      if( ulNewSize )
         pBaseHash->pPairs = ( PHB_HASHPAIR ) hb_xrealloc( pBaseHash->pPairs,
                                          ulNewSize * sizeof( HB_HASHPAIR ) );
      else
      {
         hb_xfree( pBaseHash->pPairs );
         pBaseHash->pPairs = NULL;
      }
   }
}

static PHB_ITEM hb_hashValuePtr( PHB_BASEHASH pBaseHash, PHB_ITEM pKey, BOOL fAdd )
{
   ULONG ulPos;

   if( !hb_hashFind( pBaseHash, pKey, &ulPos ) )
   {
      if( !fAdd )
         return NULL;

      if( pBaseHash->ulSize == pBaseHash->ulLen )
         hb_hashResize( pBaseHash, pBaseHash->ulSize + HB_HASH_ITEM_ALLOC );

      if( ulPos < pBaseHash->ulLen )
      {
         memmove( pBaseHash->pPairs + ulPos + 1, pBaseHash->pPairs + ulPos,
                  ( pBaseHash->ulLen - ulPos ) * sizeof( HB_HASHPAIR ) );
         pBaseHash->pPairs[ ulPos ].key.type = HB_IT_NIL;
         pBaseHash->pPairs[ ulPos ].value.type = HB_IT_NIL;
      }
      hb_itemCopy( &pBaseHash->pPairs[ ulPos ].key, pKey );
      pBaseHash->ulLen++;
      if( pBaseHash->pDefault )
         hb_itemCloneTo( &pBaseHash->pPairs[ ulPos ].value, pBaseHash->pDefault );
   }

   return &pBaseHash->pPairs[ ulPos ].value;
}

static BOOL hb_hashNewValue( PHB_BASEHASH pBaseHash, PHB_ITEM pKey, PHB_ITEM pValue )
{
   ULONG ulPos;

   if( !hb_hashFind( pBaseHash, pKey, &ulPos ) )
   {
      if( pBaseHash->ulSize == pBaseHash->ulLen )
         hb_hashResize( pBaseHash, pBaseHash->ulSize + HB_HASH_ITEM_ALLOC );

      if( ulPos < pBaseHash->ulLen )
      {
         memmove( pBaseHash->pPairs + ulPos + 1, pBaseHash->pPairs + ulPos,
                  ( pBaseHash->ulLen - ulPos ) * sizeof( HB_HASHPAIR ) );
         pBaseHash->pPairs[ ulPos ].key.type = HB_IT_NIL;
         pBaseHash->pPairs[ ulPos ].value.type = HB_IT_NIL;
      }

      hb_itemCopy( &pBaseHash->pPairs[ ulPos ].key, pKey );
      hb_itemCopyFromRef( &pBaseHash->pPairs[ ulPos ].value, pValue );
      pBaseHash->ulLen++;

      return TRUE;
   }

   return FALSE;
}

static void hb_hashNewPair( PHB_BASEHASH pBaseHash, PHB_ITEM * pKeyPtr, PHB_ITEM * pValPtr )
{
   if( pBaseHash->ulSize == pBaseHash->ulLen )
      hb_hashResize( pBaseHash, pBaseHash->ulSize + HB_HASH_ITEM_ALLOC );

   * pKeyPtr = &pBaseHash->pPairs[ pBaseHash->ulLen ].key;
   * pValPtr = &pBaseHash->pPairs[ pBaseHash->ulLen ].value;
   pBaseHash->ulLen++;
}

static void hb_hashDelPair( PHB_BASEHASH pBaseHash, ULONG ulPos )
{
   if( --pBaseHash->ulLen == 0 )
   {
      PHB_HASHPAIR pPairs = pBaseHash->pPairs;
      pBaseHash->pPairs = NULL;
      pBaseHash->ulSize = 0;
      if( HB_IS_COMPLEX( &pPairs->key ) )
         hb_itemClear( &pPairs->key );
      if( HB_IS_COMPLEX( &pPairs->value ) )
         hb_itemClear( &pPairs->value );
      hb_xfree( pPairs );
   }
   else
   {
      if( ulPos != pBaseHash->ulLen )
      {
         HB_HASHPAIR pair;
         memcpy( &pair, pBaseHash->pPairs + ulPos, sizeof( HB_HASHPAIR ) );
         memmove( pBaseHash->pPairs + ulPos, pBaseHash->pPairs + ulPos + 1,
                  ( pBaseHash->ulLen - ulPos ) * sizeof( HB_HASHPAIR ) );
         ulPos = pBaseHash->ulLen;
         memcpy( pBaseHash->pPairs + ulPos, &pair, sizeof( HB_HASHPAIR ) );
      }
      hb_itemSetNil( &pBaseHash->pPairs[ ulPos ].key );
      hb_itemSetNil( &pBaseHash->pPairs[ ulPos ].value );
      if( pBaseHash->ulSize - pBaseHash->ulLen > ( HB_HASH_ITEM_ALLOC << 1 ) )
      {
         pBaseHash->ulSize -= HB_HASH_ITEM_ALLOC;
         pBaseHash->pPairs = ( PHB_HASHPAIR ) hb_xrealloc( pBaseHash->pPairs,
                              pBaseHash->ulSize * sizeof( HB_HASHPAIR ) );
      }
   }
}

PHB_ITEM hb_hashNew( PHB_ITEM pItem )
{
   PHB_BASEHASH pBaseHash;

   HB_TRACE(HB_TR_DEBUG, ("hb_hashNew(%p)", pItem));

   if( pItem == NULL )
      pItem = hb_itemNew( NULL );
   else if( HB_IS_COMPLEX( pItem ) )
      hb_itemClear( pItem );

   pBaseHash = ( PHB_BASEHASH ) hb_gcAllocRaw( sizeof( HB_BASEHASH ), &s_gcHashFuncs );
   pBaseHash->pPairs   = NULL;
   pBaseHash->ulSize   = 0;
   pBaseHash->ulLen    = 0;
   pBaseHash->iFlags   = HB_HASH_FLAG_DEFAULT;
   pBaseHash->pDefault = NULL;

   pItem->type = HB_IT_HASH;
   pItem->item.asHash.value = pBaseHash;

   return pItem;
}

ULONG hb_hashLen( PHB_ITEM pHash )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashLen(%p)", pHash));

   if( HB_IS_HASH( pHash ) )
      return pHash->item.asHash.value->ulLen;
   else
      return 0;
}

void hb_hashPreallocate( PHB_ITEM pHash, ULONG ulNewSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashPreallocate(%p,%lu)", pHash, ulNewSize));

   if( HB_IS_HASH( pHash ) )
      hb_hashResize( pHash->item.asHash.value, ulNewSize );
}

BOOL hb_hashAllocNewPair( PHB_ITEM pHash, PHB_ITEM * pKeyPtr, PHB_ITEM * pValPtr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashAllocNewPair(%p,%p,%p)", pHash, pKeyPtr, pValPtr));

   if( HB_IS_HASH( pHash ) )
   {
      hb_hashNewPair( pHash->item.asHash.value, pKeyPtr, pValPtr );
      return TRUE;
   }
   else
      return FALSE;
}

void hb_hashSort( PHB_ITEM pHash )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashSort(%p)", pHash));

   if( HB_IS_HASH( pHash ) )
   {
      if( pHash->item.asHash.value->iFlags & HB_HASH_RESORT )
         hb_hashResort( pHash->item.asHash.value );
   }
}

PHB_ITEM hb_hashGetItemPtr( PHB_ITEM pHash, PHB_ITEM pKey, int iFlags )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashGetItemPtr(%p,%p,%d)", pHash, pKey, iFlags));

   if( HB_IS_HASH( pHash ) && HB_IS_HASHKEY( pKey ) )
   {
      PHB_ITEM pDest = hb_hashValuePtr( pHash->item.asHash.value, pKey,
         iFlags && ( pHash->item.asHash.value->iFlags & iFlags ) == iFlags );
      if( pDest )
         return HB_IS_BYREF( pDest ) ? hb_itemUnRef( pDest ) : pDest;
   }

   return NULL;
}

PHB_ITEM hb_hashGetItemRefPtr( PHB_ITEM pHash, PHB_ITEM pKey )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashGetItemRefPtr(%p,%p)", pHash, pKey));

   if( HB_IS_HASH( pHash ) && HB_IS_HASHKEY( pKey ) )
   {
      PHB_ITEM pDest = hb_hashValuePtr( pHash->item.asHash.value, pKey,
            ( pHash->item.asHash.value->iFlags & HB_HASH_AUTOADD_REFERENCE ) ==
            HB_HASH_AUTOADD_REFERENCE );
      if( pDest )
      {
         if( !HB_IS_BYREF( pDest ) )
            pDest = hb_memvarDetachLocal( pDest );
         return pDest;
      }
   }

   return NULL;
}

BOOL hb_hashScan( PHB_ITEM pHash, PHB_ITEM pKey, ULONG * pulPos )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashScan(%p,%p,%p)", pHash, pKey, pulPos));

   if( HB_IS_HASH( pHash ) )
   {
      ULONG ulPos;
      if( HB_IS_HASHKEY( pKey ) )
      {
         if( hb_hashFind( pHash->item.asHash.value, pKey, &ulPos ) )
         {
            if( pulPos )
               *pulPos = ulPos + 1;
            return TRUE;
         }
      }
      else if( HB_IS_HASH( pKey ) && pKey->item.asHash.value->ulLen == 1 )
      {
         if( hb_hashFind( pHash->item.asHash.value, &pKey->item.asHash.value->pPairs[ 0 ].key, &ulPos ) )
         {
            PHB_ITEM pVal1 = &pHash->item.asHash.value->pPairs[ ulPos ].value;
            PHB_ITEM pVal2 = &pKey->item.asHash.value->pPairs[ 0 ].value;
            BOOL fResult = FALSE;
            if( HB_IS_STRING( pVal1 ) && HB_IS_STRING( pVal2 ) )
               fResult = hb_itemStrCmp( pVal1, pVal2, TRUE ) == 0;
            else if( HB_IS_NUMINT( pVal1 ) && HB_IS_NUMINT( pVal2 ) )
               fResult = HB_ITEM_GET_NUMINTRAW( pVal1 ) == HB_ITEM_GET_NUMINTRAW( pVal2 );
            else if( HB_IS_NUMERIC( pVal1 ) && HB_IS_NUMERIC( pVal2 ) )
               fResult = hb_itemGetND( pVal1 ) == hb_itemGetND( pVal2 );
            else if( HB_IS_NIL( pVal1 ) && HB_IS_NIL( pVal2 ) )
               fResult = TRUE;
            else if( hb_itemType( pVal1 ) & hb_itemType( pVal2 ) )
            {
               hb_vmPush( pVal1 );
               hb_vmPush( pVal2 );
               if( !hb_xvmExactlyEqual() )
               {
                  HB_STACK_TLS_PRELOAD
                  fResult = hb_itemGetL( hb_stackItemFromTop( -1 ) );
                  hb_stackPop();
               }
            }
            if( fResult )
            {
               if( pulPos )
                  *pulPos = ulPos + 1;
               return TRUE;
            }
         }
      }
   }
   if( pulPos )
      *pulPos = 0;
   return FALSE;
}

static BOOL hb_hashClear( PHB_ITEM pHash )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashClear(%p)", pHash));

   if( HB_IS_HASH( pHash ) )
   {
      if( pHash->item.asHash.value->ulSize )
      {
         while( pHash->item.asHash.value->ulLen )
         {
            pHash->item.asHash.value->ulLen--;
            if( HB_IS_COMPLEX( &pHash->item.asHash.value->pPairs[ pHash->item.asHash.value->ulLen ].key ) )
               hb_itemClear( &pHash->item.asHash.value->pPairs[ pHash->item.asHash.value->ulLen ].key );
            if( HB_IS_COMPLEX( &pHash->item.asHash.value->pPairs[ pHash->item.asHash.value->ulLen ].value ) )
               hb_itemClear( &pHash->item.asHash.value->pPairs[ pHash->item.asHash.value->ulLen ].value );
         }
         /*
          * This condition is a protection against recursive call
          * from .prg object destructor [druzus]
          */
         if( pHash->item.asHash.value->ulSize )
         {
            hb_xfree( pHash->item.asHash.value->pPairs );
            pHash->item.asHash.value->pPairs = NULL;
            pHash->item.asHash.value->ulSize = 0;
         }
      }
      return TRUE;
   }

   return FALSE;
}

BOOL hb_hashDel( PHB_ITEM pHash, PHB_ITEM pKey )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashDel(%p,%p)", pHash, pKey));

   if( HB_IS_HASH( pHash ) && HB_IS_HASHKEY( pKey ) )
   {
      PHB_BASEHASH pBaseHash = pHash->item.asHash.value;
      ULONG ulPos;

      if( hb_hashFind( pBaseHash, pKey, &ulPos ) )
      {
         hb_hashDelPair( pBaseHash, ulPos );
         return TRUE;
      }
   }

   return FALSE;
}

BOOL hb_hashRemove( PHB_ITEM pHash, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashRemove(%p,%p)", pHash, pItem));

   if( HB_IS_HASH( pHash ) )
   {
      if( HB_IS_HASHKEY( pItem ) )
      {
         hb_hashDel( pHash, pItem );
         return TRUE;
      }
      else if( HB_IS_ARRAY( pItem ) )
      {
         ULONG ul = 0;
         PHB_ITEM pKey;
         while( ( pKey = hb_arrayGetItemPtr( pItem, ++ul ) ) != NULL )
            hb_hashDel( pHash, pKey );
         return TRUE;
      }
      else if( HB_IS_HASH( pItem ) )
      {
         if( pHash->item.asHash.value == pItem->item.asHash.value )
            hb_hashClear( pHash );
         else
         {
            ULONG ulLen = 0;
            while( ulLen < pItem->item.asHash.value->ulLen )
               hb_hashDel( pHash, &pItem->item.asHash.value->pPairs[ ulLen++ ].key );
         }
         return TRUE;
      }
   }
   return FALSE;
}

BOOL hb_hashAdd( PHB_ITEM pHash, PHB_ITEM pKey, PHB_ITEM pValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashAdd(%p,%p,%p)", pHash, pKey, pValue));

   if( HB_IS_HASH( pHash ) && HB_IS_HASHKEY( pKey ) )
   {
      PHB_ITEM pDest = hb_hashValuePtr( pHash->item.asHash.value, pKey, TRUE );
      if( pDest )
      {
         if( HB_IS_BYREF( pDest ) )
            pDest = hb_itemUnRef( pDest );
         if( pValue )
            hb_itemCopyFromRef( pDest, pValue );
         else
            hb_itemSetNil( pDest );
         return TRUE;
      }
   }

   return FALSE;
}

BOOL hb_hashAddNew( PHB_ITEM pHash, PHB_ITEM pKey, PHB_ITEM pValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashAddNew(%p,%p,%p)", pHash, pKey, pValue));

   if( HB_IS_HASH( pHash ) && HB_IS_HASHKEY( pKey ) )
      return hb_hashNewValue( pHash->item.asHash.value, pKey, pValue );
   else
      return FALSE;
}

PHB_ITEM hb_hashGetKeyAt( PHB_ITEM pHash, ULONG ulPos )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashGetKeyAt(%p,%lu)", pHash, ulPos));

   if( HB_IS_HASH( pHash ) && ulPos > 0 && ulPos <= pHash->item.asHash.value->ulLen )
      return &pHash->item.asHash.value->pPairs[ ulPos - 1 ].key;
   else
      return NULL;
}

PHB_ITEM hb_hashGetValueAt( PHB_ITEM pHash, ULONG ulPos )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashGetValueAt(%p,%lu)", pHash, ulPos));

   if( HB_IS_HASH( pHash ) && ulPos > 0 && ulPos <= pHash->item.asHash.value->ulLen )
      return HB_IS_BYREF( &pHash->item.asHash.value->pPairs[ ulPos - 1 ].value ) ?
             hb_itemUnRef( &pHash->item.asHash.value->pPairs[ ulPos - 1 ].value ) :
             &pHash->item.asHash.value->pPairs[ ulPos - 1 ].value;
   else
      return NULL;
}

BOOL hb_hashDelAt( PHB_ITEM pHash, ULONG ulPos )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashDelAt(%p,%lu)", pHash, ulPos));

   if( HB_IS_HASH( pHash ) && ulPos > 0 && ulPos <= pHash->item.asHash.value->ulLen )
   {
      hb_hashDelPair( pHash->item.asHash.value, ulPos - 1 );
      return TRUE;
   }
   else
      return FALSE;
}

/* retrives the hash unique ID */
void * hb_hashId( PHB_ITEM pHash )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashId(%p)", pHash));

   if( HB_IS_HASH( pHash ) )
      return ( void * ) pHash->item.asHash.value;
   else
      return NULL;
}

void hb_hashCloneBody( PHB_ITEM pHash, PHB_ITEM pDest, PHB_NESTED_CLONED pClonedList )
{
   ULONG ulPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_hashCloneBody(%p,%p,%p)", pHash, pDest, pClonedList));

   hb_hashNew( pDest );
   hb_hashResize( pDest->item.asHash.value, pHash->item.asHash.value->ulSize );
   pDest->item.asHash.value->iFlags = pHash->item.asHash.value->iFlags;
   if( pHash->item.asHash.value->pDefault )
   {
      pDest->item.asHash.value->pDefault =
                              hb_itemNew( pHash->item.asHash.value->pDefault );
      hb_gcUnlock( pDest->item.asHash.value->pDefault );
   }
   for( ulPos = 0; ulPos < pHash->item.asHash.value->ulLen; ++ulPos )
   {
      PHB_ITEM pValue = &pHash->item.asHash.value->pPairs[ ulPos ].value;
      if( HB_IS_BYREF( pValue ) )
         pValue = hb_itemUnRef( pValue );
      hb_itemCopy( &pDest->item.asHash.value->pPairs[ ulPos ].key,
                   &pHash->item.asHash.value->pPairs[ ulPos ].key );
      pDest->item.asHash.value->ulLen++;
      hb_cloneNested( &pDest->item.asHash.value->pPairs[ ulPos ].value, pValue, pClonedList );
   }
}

PHB_ITEM hb_hashCloneTo( PHB_ITEM pDest, PHB_ITEM pHash )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashCloneTo(%p,%p)", pDest, pHash));

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
   HB_TRACE(HB_TR_DEBUG, ("hb_hashClone(%p)", pHash));

   return hb_hashCloneTo( hb_itemNew( NULL ), pHash );
}

void hb_hashJoin( PHB_ITEM pDest, PHB_ITEM pSource, int iType )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashJoin(%p,%p,%d)", pDest, pSource, iType));

   if( HB_IS_HASH( pDest ) && HB_IS_HASH( pSource ) )
   {
      PHB_BASEHASH pBaseHash;
      ULONG ulPos;

      switch( iType )
      {
         case HB_HASH_UNION:        /* OR */
            pBaseHash = pSource->item.asHash.value;
            for( ulPos = 0; ulPos < pBaseHash->ulLen; ++ulPos )
            {
               PHB_ITEM pVal = &pBaseHash->pPairs[ ulPos ].value;
               if( HB_IS_BYREF( pVal ) )
                  pVal = hb_itemUnRef( pVal );
               hb_hashAdd( pDest, &pBaseHash->pPairs[ ulPos ].key, pVal );
            }
            break;

         case HB_HASH_INTERSECT:    /* AND */
            pBaseHash = pDest->item.asHash.value;
            for( ulPos = 0; ulPos < pBaseHash->ulLen; ++ulPos )
            {
               if( !hb_hashFind( pSource->item.asHash.value,
                                 &pBaseHash->pPairs[ ulPos ].key, NULL ) )
                  hb_hashDel( pDest, &pBaseHash->pPairs[ ulPos ].key );
            }
            break;

         case HB_HASH_DIFFERENCE:   /* XOR */
            pBaseHash = pSource->item.asHash.value;
            for( ulPos = 0; ulPos < pBaseHash->ulLen; ++ulPos )
            {
               if( !hb_hashDel( pDest, &pBaseHash->pPairs[ ulPos ].key ) )
               {
                  PHB_ITEM pVal = &pBaseHash->pPairs[ ulPos ].value;
                  if( HB_IS_BYREF( pVal ) )
                     pVal = hb_itemUnRef( pVal );
                  hb_hashAdd( pDest, &pBaseHash->pPairs[ ulPos ].key, pVal );
               }
            }
            break;

         case HB_HASH_REMOVE:       /* NOT -> h1 & ( h1 ^ h2 ) */
            pBaseHash = pSource->item.asHash.value;
            if( pDest->item.asHash.value == pBaseHash )
               hb_hashClear( pDest );
            else
            {
               for( ulPos = 0; ulPos < pBaseHash->ulLen; ++ulPos )
                  hb_hashDel( pDest, &pBaseHash->pPairs[ ulPos ].key );
            }
            break;
      }
   }
}

PHB_ITEM hb_hashGetKeys( PHB_ITEM pHash )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashGetKeys(%p)", pHash));

   if( HB_IS_HASH( pHash ) )
   {
      PHB_ITEM pKeys = hb_itemArrayNew( hb_hashLen( pHash ) ), pKey;
      ULONG ulPos = 0;

      while( ( pKey = hb_hashGetKeyAt( pHash, ++ulPos ) ) != NULL )
      {
         PHB_ITEM pDest = hb_arrayGetItemPtr( pKeys, ulPos );
         if( !pDest )
            break;
         hb_itemCopy( pDest, pKey );
      }
      return pKeys;
   }

   return NULL;
}

PHB_ITEM hb_hashGetValues( PHB_ITEM pHash )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashGetValues(%p)", pHash));

   if( HB_IS_HASH( pHash ) )
   {
      PHB_ITEM pValues = hb_itemArrayNew( hb_hashLen( pHash ) ), pVal;
      ULONG ulPos = 0;

      while( ( pVal = hb_hashGetValueAt( pHash, ++ulPos ) ) != NULL )
      {
         PHB_ITEM pDest = hb_arrayGetItemPtr( pValues, ulPos );
         if( !pDest )
            break;
         hb_itemCopy( pDest, pVal );
      }
      return pValues;
   }

   return NULL;
}

void hb_hashSetDefault( PHB_ITEM pHash, PHB_ITEM pValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashSetDefault(%p,%p)", pHash, pValue));

   if( HB_IS_HASH( pHash ) )
   {
      if( pHash->item.asHash.value->pDefault )
      {
         hb_itemRelease( pHash->item.asHash.value->pDefault );
         pHash->item.asHash.value->pDefault = NULL;
      }
      if( pValue && !HB_IS_NIL( pValue ) &&
          ( !HB_IS_HASH( pValue ) || pHash->item.asHash.value !=
                                     pValue->item.asHash.value ) )
      {
         pHash->item.asHash.value->pDefault = hb_itemClone( pValue );
         hb_gcUnlock( pHash->item.asHash.value->pDefault );
      }
   }
}

PHB_ITEM hb_hashGetDefault( PHB_ITEM pHash )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashGetDefault(%p)", pHash));

   if( HB_IS_HASH( pHash ) )
      return pHash->item.asHash.value->pDefault;
   else
      return NULL;
}

void hb_hashSetFlags( PHB_ITEM pHash, int iFlags )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashSetFlags(%p,%d)", pHash, iFlags));

   if( HB_IS_HASH( pHash ) )
      pHash->item.asHash.value->iFlags |= iFlags;
}

void hb_hashClearFlags( PHB_ITEM pHash, int iFlags )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashClearFlags(%p,%d)", pHash, iFlags));

   if( HB_IS_HASH( pHash ) )
      pHash->item.asHash.value->iFlags &= ~iFlags;
}

int hb_hashGetFlags( PHB_ITEM pHash )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hashGetFlags(%p)", pHash));

   if( HB_IS_HASH( pHash ) )
      return pHash->item.asHash.value->iFlags;
   else
      return 0;
}
