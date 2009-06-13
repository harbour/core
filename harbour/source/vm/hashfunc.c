/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    The Hash tables API (PRG level)
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

#include "hbvmint.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbvm.h"
#include "hbstack.h"

HB_FUNC( HB_HASH )
{
   int iPCount = hb_pcount(), iParam;

   if( iPCount & 1 )
      hb_errRT_BASE( EG_BOUND, 1131, NULL, hb_langDGetErrorDesc( EG_ARRDIMENSION ), HB_ERR_ARGS_BASEPARAMS );
   else
   {
      PHB_ITEM pHash = hb_hashNew( NULL );
      for( iParam = 1; iParam <= iPCount; iParam += 2 )
      {
         PHB_ITEM pKey = hb_param( iParam, HB_IT_HASHKEY );
         PHB_ITEM pValue = hb_param( iParam + 1, HB_IT_ANY );
         if( pKey )
            hb_hashAdd( pHash, pKey, pValue );
         else
         {
            hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 3, pHash, hb_param( iParam, HB_IT_ANY ), pValue );
            break;
         }
      }
      hb_itemReturnRelease( pHash );
   }
}

HB_FUNC( HB_HHASKEY )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pKey = hb_param( 2, HB_IT_HASHKEY );

   if( pHash && pKey )
      hb_retl( hb_hashScan( pHash, pKey, NULL ) );
   else
      hb_errRT_BASE( EG_ARG, 1123, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HPOS )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pKey = hb_param( 2, HB_IT_HASHKEY );

   if( pHash && pKey )
   {
      ULONG ulPos;
      hb_hashScan( pHash, pKey, &ulPos );
      hb_retnint( ulPos );
   }
   else
      hb_errRT_BASE( EG_ARG, 1123, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HGET )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pKey = hb_param( 2, HB_IT_HASHKEY );

   if( pHash && pKey )
   {
      PHB_ITEM pDest = hb_hashGetItemPtr( pHash, pKey, HB_HASH_AUTOADD_ACCESS );
      if( pDest )
         hb_itemReturn( pDest );
      else
         hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pHash, pKey );
   }
   else
      hb_errRT_BASE( EG_ARG, 1123, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HSET )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pKey = hb_param( 2, HB_IT_HASHKEY );
   PHB_ITEM pValue = hb_param( 3, HB_IT_ANY );

   if( pHash && pKey && pValue )
   {
      hb_hashAdd( pHash, pKey, pValue );
      hb_itemReturn( pHash );
   }
   else
      hb_errRT_BASE( EG_ARG, 1123, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HDEL )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pKey = hb_param( 2, HB_IT_HASHKEY );

   if( pHash && pKey )
   {
      if( hb_hashDel( pHash, pKey ) )
         hb_itemReturn( pHash );
      else
         hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 2, pHash, pKey );
   }
   else
      hb_errRT_BASE( EG_ARG, 1123, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HKEYAT )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pPos = hb_param( 2, HB_IT_NUMERIC );

   if( pHash && pPos )
   {
      PHB_ITEM pKey = hb_hashGetKeyAt( pHash, hb_itemGetNL( pPos ) );
      if( pKey )
         hb_itemReturn( pKey );
      else
         hb_errRT_BASE( EG_BOUND, 1187, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
   else
      hb_errRT_BASE( EG_ARG, 1123, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HVALUEAT )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pPos = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pValue = hb_param( 3, HB_IT_ANY );

   if( pHash && pPos )
   {
      PHB_ITEM pItem = hb_hashGetValueAt( pHash, hb_itemGetNL( pPos ) );
      if( pItem )
      {
         if( pValue )
            hb_itemCopy( pItem, pValue );
         else
            pValue = pItem;
         hb_itemReturn( pValue );
      }
      else
         hb_errRT_BASE( EG_BOUND, 1187, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
   else
      hb_errRT_BASE( EG_ARG, 1123, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HPAIRAT )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pPos = hb_param( 2, HB_IT_NUMERIC );

   if( pHash && pPos )
   {
      PHB_ITEM pKey = hb_hashGetKeyAt( pHash, hb_itemGetNL( pPos ) );
      PHB_ITEM pValue = hb_hashGetValueAt( pHash, hb_itemGetNL( pPos ) );
      if( pKey && pValue )
      {
         PHB_ITEM pDstKey = hb_param( 3, HB_IT_BYREF );
         PHB_ITEM pDstVal = hb_param( 4, HB_IT_BYREF );
         if( pDstKey && pDstVal )
         {
            hb_itemCopy( pDstKey, pKey );
            hb_itemCopy( pDstVal, pValue );
         }
         else
         {
            PHB_ITEM pResult = hb_itemArrayNew( 2 );
            hb_arraySet( pResult, 1, pKey );
            hb_arraySet( pResult, 2, pValue );
            hb_itemReturnRelease( pResult );
         }
      }
      else
         hb_errRT_BASE( EG_BOUND, 1187, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
   else
      hb_errRT_BASE( EG_ARG, 1123, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HDELAT )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pPos = hb_param( 2, HB_IT_NUMERIC );

   if( pHash && pPos )
   {
      if( hb_hashDelAt( pHash, hb_itemGetNL( pPos ) ) )
         hb_itemReturn( pHash );
      else
         hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 2, pHash, pPos );
   }
   else
      hb_errRT_BASE( EG_ARG, 1123, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


HB_FUNC( HB_HKEYS )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );

   if( pHash )
      hb_itemReturnRelease( hb_hashGetKeys( pHash ) );
   else
      hb_errRT_BASE( EG_ARG, 1123, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HVALUES )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );

   if( pHash )
      hb_itemReturnRelease( hb_hashGetValues( pHash ) );
   else
      hb_errRT_BASE( EG_ARG, 1123, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HFILL )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pValue = hb_param( 2, HB_IT_ANY );

   if( pHash && pValue )
   {
      PHB_ITEM pDest;
      ULONG ulPos = 0;

      while( ( pDest = hb_hashGetValueAt( pHash, ++ulPos ) ) != NULL )
         hb_itemCopy( pDest, pValue );

      hb_itemReturn( pHash );
   }
   else
      hb_errRT_BASE( EG_ARG, 1123, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HCLONE )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );

   if( pHash )
      hb_itemReturnRelease( hb_hashClone( pHash ) );
   else
      hb_errRT_BASE( EG_ARG, 1123, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HCOPY )
{
   PHB_ITEM pSource = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pDest = hb_param( 2, HB_IT_HASH );

   if( pSource && pDest )
   {
      ULONG ulLen = hb_hashLen( pSource ), ulStart, ulCount;

      ulStart = hb_parnl( 3 );
      if( !ulStart )
         ++ulStart;
      ulCount = HB_ISNUM( 4 ) ? ( ULONG ) hb_parnl( 4 ) : ulLen - ulStart + 1;

      while( ulCount-- )
      {
         PHB_ITEM pKey = hb_hashGetKeyAt( pSource, ulStart );
         PHB_ITEM pValue = hb_hashGetValueAt( pSource, ulStart );
         if( pKey && pValue )
            hb_hashAdd( pDest, pKey, pValue );
         else
            break;
         ++ulStart;
      }

      hb_itemReturn( pDest );
   }
   else
      hb_errRT_BASE( EG_ARG, 1123, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HMERGE )
{
   PHB_ITEM pDest = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pSource = hb_param( 2, HB_IT_HASH );
   PHB_ITEM pAction = hb_param( 3, HB_IT_BLOCK | HB_IT_NUMERIC );

   if( pDest && pSource )
   {
      if( pAction && HB_IS_BLOCK( pAction ) )
      {
         ULONG ulLen = hb_hashLen( pSource ), ulPos = 0;
         while( ++ulPos <= ulLen )
         {
            PHB_ITEM pKey = hb_hashGetKeyAt( pSource, ulPos );
            PHB_ITEM pValue = hb_hashGetValueAt( pSource, ulPos );
            if( pKey && pValue )
            {
               hb_vmPushEvalSym();
               hb_vmPush( pAction );
               hb_vmPush( pKey );
               hb_vmPush( pValue );
               hb_vmPushLong( ulPos );
               hb_vmSend( 3 );
               {
                  PHB_ITEM pReturn = hb_stackReturnItem();
                  if( HB_IS_LOGICAL( pReturn ) && hb_itemGetL( pReturn ) )
                     hb_hashAdd( pDest, pKey, pValue );
               }
            }
            else
               break;
         }
      }
      else
         hb_hashJoin( pDest, pSource, pAction ? hb_itemGetNI( pAction ) : HB_HASH_UNION );

      hb_itemReturn( pDest );
   }
   else
      hb_errRT_BASE( EG_ARG, 1123, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HEVAL )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pBlock = hb_param( 2, HB_IT_BLOCK );

   if( pHash && pBlock )
   {
      ULONG ulLen = hb_hashLen( pHash ), ulStart, ulCount;

      ulStart = hb_parnl( 3 );
      if( !ulStart )
         ++ulStart;
      ulCount = HB_ISNUM( 4 ) ? ( ULONG ) hb_parnl( 4 ) : ulLen - ulStart + 1;

      while( ulCount-- )
      {
         PHB_ITEM pKey = hb_hashGetKeyAt( pHash, ulStart );
         PHB_ITEM pValue = hb_hashGetValueAt( pHash, ulStart );
         if( pKey && pValue )
         {
            hb_vmPushEvalSym();
            hb_vmPush( pBlock );
            hb_vmPush( pKey );
            hb_vmPush( pValue );
            hb_vmPushLong( ulStart );
            hb_vmSend( 3 );
         }
         else
            break;
         ++ulStart;
      }

      hb_itemReturn( pHash );
   }
   else
      hb_errRT_BASE( EG_ARG, 1123, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HSCAN )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pValue = hb_param( 2, HB_IT_ANY );

   if( pHash && pValue )
   {
      BOOL fExact = hb_parl( 5 ), fFound = FALSE;
      ULONG ulLen = hb_hashLen( pHash ), ulStart, ulCount;

      ulStart = hb_parnl( 3 );
      if( !ulStart )
         ++ulStart;
      ulCount = HB_ISNUM( 4 ) ? ( ULONG ) hb_parnl( 4 ) : ulLen - ulStart + 1;

      if( HB_IS_BLOCK( pValue ) )
      {
         while( ulCount-- )
         {
            PHB_ITEM pKey = hb_hashGetKeyAt( pHash, ulStart );
            PHB_ITEM pVal = hb_hashGetValueAt( pHash, ulStart );
            if( pKey && pValue )
            {
               hb_vmPushEvalSym();
               hb_vmPush( pValue );
               hb_vmPush( pKey );
               hb_vmPush( pVal );
               hb_vmPushLong( ulStart );
               hb_vmSend( 3 );
               {
                  PHB_ITEM pReturn = hb_stackReturnItem();
                  if( HB_IS_LOGICAL( pReturn ) && hb_itemGetL( pReturn ) )
                  {
                     fFound = TRUE;
                     break;
                  }
               }
            }
            else
               break;
            ++ulStart;
         }
      }
      else if( HB_IS_STRING( pValue ) )
      {
         while( ulCount-- )
         {
            PHB_ITEM pItem = hb_hashGetValueAt( pHash, ulStart );
            if( pItem )
            {
               if( HB_IS_STRING( pItem ) && hb_itemStrCmp( pItem, pValue, fExact ) == 0 )
               {
                  fFound = TRUE;
                  break;
               }
            }
            else
               break;
            ++ulStart;
         }
      }
      else if( HB_IS_NUMERIC( pValue ) )
      {
         double dValue = hb_itemGetND( pValue );
         while( ulCount-- )
         {
            PHB_ITEM pItem = hb_hashGetValueAt( pHash, ulStart );
            if( pItem )
            {
               if( HB_IS_NUMERIC( pItem ) && hb_itemGetND( pItem ) == dValue )
               {
                  fFound = TRUE;
                  break;
               }
            }
            else
               break;
            ++ulStart;
         }
      }
      else if( HB_IS_DATETIME( pValue ) )
      {
         while( ulCount-- )
         {
            PHB_ITEM pItem = hb_hashGetValueAt( pHash, ulStart );
            if( pItem )
            {
               if( HB_IS_DATETIME( pItem ) &&
                   pItem->item.asDateTime.julian == pValue->item.asDateTime.julian &&
                   ( !fExact || pItem->item.asDateTime.time == pValue->item.asDateTime.time ) )
               {
                  fFound = TRUE;
                  break;
               }
            }
            else
               break;
            ++ulStart;
         }
      }
      else if( HB_IS_LOGICAL( pValue ) )
      {
         BOOL fValue = hb_itemGetDL( pValue );
         while( ulCount-- )
         {
            PHB_ITEM pItem = hb_hashGetValueAt( pHash, ulStart );
            if( pItem )
            {
               if( HB_IS_LOGICAL( pItem ) && hb_itemGetL( pItem ) == fValue )
               {
                  fFound = TRUE;
                  break;
               }
            }
            else
               break;
            ++ulStart;
         }
      }
      else if( HB_IS_NIL( pValue ) )
      {
         while( ulCount-- )
         {
            PHB_ITEM pItem = hb_hashGetValueAt( pHash, ulStart );
            if( pItem )
            {
               if( HB_IS_NIL( pItem ) )
               {
                  fFound = TRUE;
                  break;
               }
            }
            else
               break;
            ++ulStart;
         }
      }
      else if( HB_IS_POINTER( pValue ) )
      {
         while( ulCount-- )
         {
            PHB_ITEM pItem = hb_hashGetValueAt( pHash, ulStart );
            if( pItem )
            {
               if( HB_IS_POINTER( pItem ) &&
                   pItem->item.asPointer.value == pValue->item.asPointer.value )
               {
                  fFound = TRUE;
                  break;
               }
            }
            else
               break;
            ++ulStart;
         }
      }
      else if( fExact && HB_IS_ARRAY( pValue ) )
      {
         while( ulCount-- )
         {
            PHB_ITEM pItem = hb_hashGetValueAt( pHash, ulStart );
            if( pItem )
            {
               if( HB_IS_ARRAY( pItem ) &&
                   pItem->item.asArray.value == pValue->item.asArray.value )
               {
                  fFound = TRUE;
                  break;
               }
            }
            else
               break;
            ++ulStart;
         }
      }
      else if( fExact && HB_IS_HASH( pValue ) )
      {
         while( ulCount-- )
         {
            PHB_ITEM pItem = hb_hashGetValueAt( pHash, ulStart );
            if( pItem )
            {
               if( HB_IS_HASH( pItem ) &&
                   pItem->item.asHash.value == pValue->item.asHash.value )
               {
                  fFound = TRUE;
                  break;
               }
            }
            else
               break;
            ++ulStart;
         }
      }

      hb_retnint( fFound ? ulStart : 0 );
   }
   else
      hb_errRT_BASE( EG_ARG, 1123, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HSORT )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );

   if( pHash )
   {
      hb_hashSort( pHash );
      hb_itemReturn( pHash );
   }
   else
      hb_errRT_BASE( EG_ARG, 2017, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HCASEMATCH )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pValue = hb_param( 2, HB_IT_LOGICAL );

   if( pHash )
   {
      int iFlags = hb_hashGetFlags( pHash );
      hb_retl( ( iFlags & HB_HASH_IGNORECASE ) == 0 );
      if( pValue )
      {
         if( hb_itemGetL( pValue ) )
         {
            if( ( iFlags & HB_HASH_IGNORECASE ) != 0 )
            {
               hb_hashClearFlags( pHash, HB_HASH_IGNORECASE );
               hb_hashSetFlags( pHash, HB_HASH_RESORT );
            }
         }
         else if( ( iFlags & HB_HASH_IGNORECASE ) == 0 )
         {
            hb_hashClearFlags( pHash, HB_HASH_BINARY );
            hb_hashSetFlags( pHash, HB_HASH_IGNORECASE | HB_HASH_RESORT );
         }
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2017, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HBINARY )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pValue = hb_param( 2, HB_IT_LOGICAL );

   if( pHash )
   {
      int iFlags = hb_hashGetFlags( pHash );
      hb_retl( ( iFlags & HB_HASH_BINARY ) != 0 );
      if( pValue )
      {
         if( hb_itemGetL( pValue ) )
         {
            if( ( iFlags & HB_HASH_BINARY ) == 0 )
            {
               hb_hashClearFlags( pHash, HB_HASH_IGNORECASE );
               hb_hashSetFlags( pHash, HB_HASH_BINARY | HB_HASH_RESORT );
            }
         }
         else if( ( iFlags & HB_HASH_BINARY ) != 0 )
         {
            hb_hashClearFlags( pHash, HB_HASH_BINARY );
            hb_hashSetFlags( pHash, HB_HASH_RESORT );
         }
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2017, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HAUTOADD )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pValue = hb_param( 2, HB_IT_LOGICAL | HB_IT_NUMERIC );

   if( pHash )
   {
      int iOldFlags = hb_hashGetFlags( pHash ) & HB_HASH_AUTOADD_MASK;
      hb_retni( iOldFlags );
      if( pValue )
      {
         if( HB_IS_LOGICAL( pValue ) )
         {
            if( hb_itemGetL( pValue ) )
               hb_hashSetFlags( pHash, hb_hashGetDefault( pHash ) ?
                           HB_HASH_AUTOADD_ALWAYS : HB_HASH_AUTOADD_ASSIGN );
            else if( iOldFlags )
               hb_hashClearFlags( pHash, iOldFlags );
         }
         else
         {
            int iNewFlags = hb_itemGetNI( pValue );
            if( ( iNewFlags | iOldFlags ) != iNewFlags )
               hb_hashClearFlags( pHash, iOldFlags );
            if( iNewFlags )
               hb_hashSetFlags( pHash, iNewFlags );
         }
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2017, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HALLOCATE )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pValue = hb_param( 2, HB_IT_NUMERIC );

   if( pHash && pValue )
   {
      LONG lMem = hb_itemGetNL( pValue );
      if( lMem >= 0 )
         hb_hashPreallocate( pHash, lMem );
   }
   else
      hb_errRT_BASE( EG_ARG, 2017, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HDEFAULT )
{
   PHB_ITEM pHash = hb_param( 1, HB_IT_HASH );

   if( pHash )
   {
      hb_itemReturn( hb_hashGetDefault( pHash ) );
      if( hb_pcount() > 1 )
         hb_hashSetDefault( pHash, hb_param( 2, HB_IT_ANY ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2017, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_HSETAUTOADD )     { HB_FUNC_EXEC( HB_HAUTOADD ); hb_itemReturn( hb_param( 1, HB_IT_HASH ) ); }
HB_FUNC( HB_HSETCASEMATCH )   { HB_FUNC_EXEC( HB_HCASEMATCH ); hb_itemReturn( hb_param( 1, HB_IT_HASH ) ); }
HB_FUNC( HB_HSETBINARY )      { HB_FUNC_EXEC( HB_HBINARY ); hb_itemReturn( hb_param( 1, HB_IT_HASH ) ); }
