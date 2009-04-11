/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Extend API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_retnlen()
 *    hb_retnilen()
 *    hb_retnllen()
 *    hb_retndlen()
 *    hb_retdl()
 *
 * Copyright 2000 Jose Lalin <dezac@corevia.com>
 *    hb_retd()
 *
 * See COPYING for licensing terms.
 *
 */

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbset.h"
#include "hbdate.h"
#include "hbstack.h"

/* NOTE: iParam = -1 can be used to access the return value. */
/* NOTE: iParam = 0 can be used to access the SELF object. */

PHB_ITEM hb_param( int iParam, long lMask )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_param(%d, %ld)", iParam, lMask));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam  );

      if( pItem->type & HB_IT_BYREF )
      {
         pItem = hb_itemUnRef( pItem );
         if( ( HB_TYPE ) lMask == HB_IT_BYREF )
            return pItem;
      }

      if( ( pItem->type & ( HB_TYPE ) lMask ) || ( HB_TYPE ) lMask == HB_IT_ANY )
         return pItem;
   }

   return NULL;
}

PHB_ITEM  hb_paramError( int iParam )
{
   static HB_ITEM s_NIL;

   PHB_ITEM pParam = hb_param( iParam, HB_IT_ANY );

   if( pParam == NULL )
   {
      hb_itemClear( &s_NIL );
      pParam = &s_NIL;
   }

   return pParam;
}

/* function to be called from pcode DLLs to detect if the extend system
 * is going to use an array item */

BOOL hb_extIsArray( int iParam )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   if( iParam == -1 )
      pItem = hb_stackReturnItem();
   else if( iParam >= 0 && iParam <= hb_pcount() )
      pItem = hb_stackItemFromBase( iParam );
   else
      return FALSE;

   if( HB_IS_BYREF( pItem ) )
      pItem = hb_itemUnRef( pItem );

   return HB_IS_ARRAY( pItem ) && !HB_ARRAY_OBJ( pItem );
}

/* function to be called from pcode DLLs to detect if the extend system
 * is going to use an object item */

BOOL hb_extIsObject( int iParam )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   if( iParam == -1 )
      pItem = hb_stackReturnItem();
   else if( iParam >= 0 && iParam <= hb_pcount() )
      pItem = hb_stackItemFromBase( iParam );
   else
      return FALSE;

   if( HB_IS_BYREF( pItem ) )
      pItem = hb_itemUnRef( pItem );

   return HB_IS_OBJECT( pItem );
}

/* NOTE: Caller should not modify the buffer returned by this function.
         [vszakats] */

char * hb_parc( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parc(%d, ...)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_STRING( pItem ) )
         return pItem->item.asString.value;
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetCPtr( pItem, ulArrayIndex );
      }
   }

   return ( char * ) NULL;
}

char * hb_parcx( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parcx(%d, ...)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_STRING( pItem ) )
         return pItem->item.asString.value;
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetCPtr( pItem, ulArrayIndex );
      }
   }

   return ( char * ) "";
}

ULONG  hb_parclen( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parclen(%d, ...)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_STRING( pItem ) )
         return pItem->item.asString.length;
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetCLen( pItem, ulArrayIndex );
      }
   }

   return 0;
}

/* NOTE: Similar to _parclen() but returns the length including the
         terminating zero byte, and it only works for parameters passed by
         reference. [vszakats] */

ULONG  hb_parcsiz( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parcsiz(%d, ...)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      /* NOTE: hb_parcsiz() will only work for strings passed by reference.
               CA-Cl*pper works like this. [vszakats] */

      if( HB_IS_BYREF( pItem ) )
      {
         pItem = hb_itemUnRef( pItem );

         if( HB_IS_STRING( pItem ) )
            return pItem->item.asString.length + 1;
         else if( HB_IS_ARRAY( pItem ) )
         {
            va_list va;
            ULONG ulArrayIndex;

            va_start( va, iParam );
            ulArrayIndex = va_arg( va, ULONG );
            va_end( va );

            return hb_arrayGetCLen( pItem, ulArrayIndex ) + 1;
         }
      }
   }

   return 0;
}

/* NOTE: Using hb_stackDateBuffer() a temporary date buffer guaranties
         good behavior when multithreading. */

char  * hb_pards( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_pards(%d, ...)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DATETIME( pItem ) )
         return hb_dateDecStr( hb_stackDateBuffer(), pItem->item.asDateTime.julian );
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetDS( pItem, ulArrayIndex, hb_stackDateBuffer() );
      }
   }

   return hb_dateDecStr( hb_stackDateBuffer(), 0 );
}

/* NOTE: szDate must be a 9 chars wide buffer. [vszakats] */

char  * hb_pardsbuff( char * szDate, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_pardsbuff(%p, %d, ...)", szDate, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DATETIME( pItem ) )
         return hb_dateDecStr( szDate, pItem->item.asDateTime.julian );
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetDS( pItem, ulArrayIndex, szDate );
      }
   }

   return hb_dateDecStr( szDate, 0 );
}

/* retrieve a date as long integer - number of days from Julian's day */

LONG  hb_pardl( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_pardl(%d, ...)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         pItem = hb_itemUnRef( pItem );
      }

      if( HB_IS_DATETIME( pItem ) )
      {
         return pItem->item.asDateTime.julian;
      }
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetDL( pItem, ulArrayIndex );
      }
   }

   return hb_itemGetDL( NULL );
}

double hb_partd( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_partd(%d, ...)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         pItem = hb_itemUnRef( pItem );
      }

      if( HB_IS_DATETIME( pItem ) )
      {
         return hb_timeStampPackDT( pItem->item.asDateTime.julian,
                                    pItem->item.asDateTime.time );
      }
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetTD( pItem, ulArrayIndex );
      }
   }

   return 0;
}

BOOL hb_partdt( LONG * plJulian, LONG * plMilliSec , int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_partdt(%p,%p,%d, ...)", plJulian, plMilliSec, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         pItem = hb_itemUnRef( pItem );
      }

      if( HB_IS_DATETIME( pItem ) )
      {
         *plJulian = pItem->item.asDateTime.julian;
         *plMilliSec = pItem->item.asDateTime.time;
         return TRUE;
      }
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetTDT( pItem, ulArrayIndex, plJulian, plMilliSec );
      }
   }

   return FALSE;
}


int  hb_parl( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parl(%d, ...)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_LOGICAL( pItem ) )
         return pItem->item.asLogical.value ? 1 : 0;
      else if( HB_IS_INTEGER( pItem ) )
         return pItem->item.asInteger.value != 0 ? 1 : 0;
      else if( HB_IS_LONG( pItem ) )
         return pItem->item.asLong.value != 0 ? 1 : 0;
      else if( HB_IS_DOUBLE( pItem ) )
         return pItem->item.asDouble.value != 0.0 ? 1 : 0;
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetL( pItem, ulArrayIndex ) ? 1 : 0;
      }
   }

   return 0;
}

double  hb_parnd( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parnd(%d, ...)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DOUBLE( pItem ) )
         return pItem->item.asDouble.value;
      else if( HB_IS_INTEGER( pItem ) )
         return ( double ) pItem->item.asInteger.value;
      else if( HB_IS_LONG( pItem ) )
         return ( double ) pItem->item.asLong.value;
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetND( pItem, ulArrayIndex );
      }
   }

   return 0;
}

int  hb_parni( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parni(%d, ...)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_INTEGER( pItem ) )
         return pItem->item.asInteger.value;
      else if( HB_IS_LONG( pItem ) )
         return ( int ) pItem->item.asLong.value;
      else if( HB_IS_DOUBLE( pItem ) )
         return ( int ) pItem->item.asDouble.value;
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetNI( pItem, ulArrayIndex );
      }
   }

   return 0;
}

long  hb_parnl( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parnl(%d, ...)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_LONG( pItem ) )
         return ( long ) pItem->item.asLong.value;
      else if( HB_IS_INTEGER( pItem ) )
         return ( long ) pItem->item.asInteger.value;
      else if( HB_IS_DOUBLE( pItem ) )
#ifdef __GNUC__
         return ( long ) ( unsigned long ) pItem->item.asDouble.value;
#else
         return ( long ) pItem->item.asDouble.value;
#endif
      /* DATETIME TODO: remove it */
      else if( HB_IS_DATETIME( pItem ) )
         return ( long ) pItem->item.asDateTime.julian;
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetNL( pItem, ulArrayIndex );
      }
   }

   return 0;
}

#ifndef HB_LONG_LONG_OFF
LONGLONG  hb_parnll( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parnll(%d, ...)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_LONG( pItem ) )
         return ( LONGLONG ) pItem->item.asLong.value;
      else if( HB_IS_INTEGER( pItem ) )
         return ( LONGLONG ) pItem->item.asInteger.value;
      else if( HB_IS_DOUBLE( pItem ) )
#ifdef __GNUC__
         return ( LONGLONG ) ( ULONGLONG ) pItem->item.asDouble.value;
#else
         return ( LONGLONG ) pItem->item.asDouble.value;
#endif
      /* DATETIME TODO: remove it */
      else if( HB_IS_DATETIME( pItem ) )
         return ( LONGLONG ) pItem->item.asDateTime.julian;
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetNLL( pItem, ulArrayIndex );
      }
   }

   return 0;
}
#endif

HB_LONG  hb_parnint( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parnint(%d, ...)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_LONG( pItem ) )
         return ( HB_LONG ) pItem->item.asLong.value;
      else if( HB_IS_INTEGER( pItem ) )
         return ( HB_LONG ) pItem->item.asInteger.value;
      else if( HB_IS_DOUBLE( pItem ) )
#ifdef __GNUC__
         return ( HB_LONG ) ( HB_ULONG ) pItem->item.asDouble.value;
#else
         return ( HB_LONG ) pItem->item.asDouble.value;
#endif
      /* DATETIME TODO: remove it */
      else if( HB_IS_DATETIME( pItem ) )
         return ( HB_LONG ) pItem->item.asDateTime.julian;
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetNInt( pItem, ulArrayIndex );
      }
   }

   return 0;
}

void * hb_parptr( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parptr(%d, ...)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_POINTER( pItem ) )
         return pItem->item.asPointer.value;
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return hb_arrayGetPtr( pItem, ulArrayIndex );
      }
   }

   return NULL;
}

void * hb_parptrGC( HB_GARBAGE_FUNC_PTR pFunc, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parptrGC(%p,%d, ...)", pFunc, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_POINTER( pItem ) )
      {
         if( pItem->item.asPointer.collect &&
             hb_gcFunc( pItem->item.asPointer.value ) == pFunc )
            return pItem->item.asPointer.value;
      }
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         pItem = hb_arrayGetItemPtr( pItem, ulArrayIndex );
         if( pItem && HB_IS_POINTER( pItem ) &&
             pItem->item.asPointer.collect &&
             hb_gcFunc( pItem->item.asPointer.value ) == pFunc )
            return pItem->item.asPointer.value;
      }
   }

   return NULL;
}

ULONG  hb_parinfa( int iParamNum, ULONG uiArrayIndex )
{
   PHB_ITEM pArray;

   HB_TRACE(HB_TR_DEBUG, ("hb_parinfa(%d, %lu)", iParamNum, uiArrayIndex));

   pArray = hb_param( iParamNum, HB_IT_ARRAY );

   if( pArray )
   {
      if( uiArrayIndex == 0 )
         return hb_arrayLen( pArray );
      else
         return ( long ) hb_arrayGetType( pArray, uiArrayIndex );
   }
   else
      return 0;
}

ULONG  hb_parinfo( int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parinfo(%d)", iParam));

   if( iParam == 0 )
      return ( ULONG ) hb_pcount();
   else
   {
      if( iParam >= -1 && iParam <= hb_pcount() )
      {
         PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
         HB_TYPE uiType = HB_ITEM_TYPE( pItem );

         if( uiType & HB_IT_BYREF )
            uiType |= HB_ITEM_TYPE( hb_itemUnRef( pItem ) );

         return ( ULONG ) uiType;
      }
      else
         return 0;
   }
}

#undef hb_ret
void  hb_ret( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_ret()"));

   hb_itemClear( hb_stackReturnItem() );
}

#undef hb_reta
void  hb_reta( ULONG ulLen )  /* undocumented hb_reta() */
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_reta(%lu)", ulLen));

   hb_arrayNew( hb_stackReturnItem(), ulLen );
}

#undef hb_retc
void hb_retc( const char * szText )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retc(%s)", szText));

   hb_itemPutC( hb_stackReturnItem(), szText );
}

#undef hb_retc_null
void hb_retc_null( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retc_null()"));

   hb_itemPutC( hb_stackReturnItem(), NULL );
}

#undef hb_retc_buffer
void hb_retc_buffer( char * szText )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retc_buffer(%s)", szText));

   hb_itemPutCPtr2( hb_stackReturnItem(), szText );
}

#undef hb_retc_const
void hb_retc_const( const char * szText )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retc_const(%s)", szText));

   hb_itemPutCConst( hb_stackReturnItem(), szText );
}

#undef hb_retclen
void  hb_retclen( const char * szText, ULONG ulLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retclen(%s, %lu)", szText, ulLen));

   hb_itemPutCL( hb_stackReturnItem(), szText, ulLen );
}

#undef hb_retclen_buffer
void  hb_retclen_buffer( char * szText, ULONG ulLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retclen_buffer(%s, %lu)", szText, ulLen));

   hb_itemPutCLPtr( hb_stackReturnItem(), szText, ulLen );
}

/* szDate must have YYYYMMDD format */

#undef hb_retds
void hb_retds( const char * szDate )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retds(%s)", szDate));

   hb_itemPutDS( hb_stackReturnItem(), szDate );
}

#undef hb_retd
void hb_retd( int iYear, int iMonth, int iDay )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retd(%04i, %02i, %02i)", iYear, iMonth, iDay));

   hb_itemPutD( hb_stackReturnItem(), iYear, iMonth, iDay );
}

#undef hb_retdl
void hb_retdl( long lJulian )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retdl(%ld)", lJulian));

   hb_itemPutDL( hb_stackReturnItem(), lJulian );
}

#undef hb_rettd
void hb_rettd( double dTimeStamp )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_rettd(%lf)", dTimeStamp));

   hb_itemPutTD( hb_stackReturnItem(), dTimeStamp );
}

#undef hb_rettdt
void hb_rettdt( LONG lJulian, LONG lMilliSec )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_rettdt(%ld, %ld)", lJulian, lMilliSec));

   hb_itemPutTDT( hb_stackReturnItem(), lJulian, lMilliSec );
}

#undef hb_retl
void hb_retl( int iLogical )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retl(%d)", iLogical));

   hb_itemPutL( hb_stackReturnItem(), iLogical ? TRUE : FALSE );
}

#undef hb_retnd
void hb_retnd( double dNumber )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retnd(%lf)", dNumber));

   hb_itemPutND( hb_stackReturnItem(), dNumber );
}

#undef hb_retni
void hb_retni( int iNumber )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retni(%d)", iNumber));

   hb_itemPutNI( hb_stackReturnItem(), iNumber );
}

#undef hb_retnl
void hb_retnl( long lNumber )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retnl(%ld)", lNumber));

   hb_itemPutNL( hb_stackReturnItem(), lNumber );
}

#ifndef HB_LONG_LONG_OFF
#undef hb_retnll
void hb_retnll( LONGLONG llNumber )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retnll(%" PFLL "d)", llNumber));

   hb_itemPutNLL( hb_stackReturnItem(), llNumber );
}
#endif

#undef hb_retnint
void hb_retnint( HB_LONG lNumber )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retnl(%" PFHL "d )", lNumber));

   hb_itemPutNInt( hb_stackReturnItem(), lNumber );
}

#undef hb_retnlen
void hb_retnlen( double dNumber, int iWidth, int iDec )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retnlen(%lf, %d, %d)", dNumber, iWidth, iDec));

   hb_itemPutNLen( hb_stackReturnItem(), dNumber, iWidth, iDec );
}

#undef hb_retndlen
void hb_retndlen( double dNumber, int iWidth, int iDec )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retndlen(%lf, %d, %d)", dNumber, iWidth, iDec));

   hb_itemPutNDLen( hb_stackReturnItem(), dNumber, iWidth, iDec );
}

#undef hb_retnilen
void hb_retnilen( int iNumber, int iWidth )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retnilen(%d, %d)", iNumber, iWidth));

   hb_itemPutNILen( hb_stackReturnItem(), iNumber, iWidth );
}

#undef hb_retnllen
void hb_retnllen( long lNumber, int iWidth )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retnllen(%ld, %d)", lNumber, iWidth));

   hb_itemPutNLLen( hb_stackReturnItem(), lNumber, iWidth );
}

#ifndef HB_LONG_LONG_OFF
#undef hb_retnlllen
void hb_retnlllen( LONGLONG llNumber, int iWidth )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retnlllen(%" PFLL "d, %d)", llNumber, iWidth));

   hb_itemPutNLLLen( hb_stackReturnItem(), llNumber, iWidth );
}
#endif

#undef hb_retnintlen
void hb_retnintlen( HB_LONG lNumber, int iWidth )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retnintlen(%" PFHL "d, %d)", lNumber, iWidth));

   hb_itemPutNIntLen( hb_stackReturnItem(), lNumber, iWidth );
}

#undef hb_retptr
void hb_retptr( void * pointer )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retptr(%p)", pointer));

   hb_itemPutPtr( hb_stackReturnItem(), pointer );
}

#undef hb_retptrGC
void hb_retptrGC( void * pointer )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retptrGC(%p)", pointer));

   hb_itemPutPtrGC( hb_stackReturnItem(), pointer );
}


int hb_storc( const char * szText, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storc(%s, %d, ...)", szText, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetC( pItem, va_arg( va, ULONG ), szText ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutC( pItem, szText );
         return 1;
      }
   }

   return 0;
}

int hb_storclen( const char * szText, ULONG ulLen, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storclen(%s, %lu, %d, ...)", szText, ulLen, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetCL( pItem, va_arg( va, ULONG ), szText, ulLen ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutCL( pItem, szText, ulLen );
         return 1;
      }
   }

   return 0;
}

int hb_storclen_buffer( char * szText, ULONG ulLen, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storclen_buffer(%s, %lu, %d, ...)", szText, ulLen, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetCPtr( pItem, va_arg( va, ULONG ), szText, ulLen ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutCLPtr( pItem, szText, ulLen );
         return 1;
      }
   }

   return 0;
}

/* szDate must have YYYYMMDD format */

int hb_stords( const char * szDate, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_stords(%s, %d, ...)", szDate, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetDS( pItem, va_arg( va, ULONG ), szDate ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutDS( pItem, szDate );
         return 1;
      }
   }

   return 0;
}

int hb_stordl( long lJulian, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_stordl(%ld, %d, ...)", lJulian, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetDL( pItem, va_arg( va, ULONG ), lJulian ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutDL( pItem, lJulian );
         return 1;
      }
   }

   return 0;
}

int hb_stortd( double dTimeStamp, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_stortd(%lf, %d, ...)", dTimeStamp, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetTD( pItem, va_arg( va, ULONG ), dTimeStamp ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutTD( pItem, dTimeStamp );
         return 1;
      }
   }

   return 0;
}

int hb_stortdt( LONG lJulian, LONG lMilliSec, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_stortd(%ld, %ld, %d, ...)", lJulian, lMilliSec, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetTDT( pItem, va_arg( va, ULONG ), lJulian, lMilliSec ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutTDT( pItem, lJulian, lMilliSec );
         return 1;
      }
   }

   return 0;
}

int hb_storl( int iLogical, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storl(%d, %d, ...)", iLogical, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetL( pItem, va_arg( va, ULONG ), iLogical ? TRUE : FALSE ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutL( pItem, iLogical ? TRUE : FALSE );
         return 1;
      }
   }

   return 0;
}

int hb_storni( int iValue, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storni(%d, %d, ...)", iValue, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetNI( pItem, va_arg( va, ULONG ), iValue ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutNI( pItem, iValue );
         return 1;
      }
   }

   return 0;
}

int hb_stornl( long lValue, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_stornl(%ld, %d, ...)", lValue, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetNL( pItem, va_arg( va, ULONG ), lValue ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutNL( pItem, lValue );
         return 1;
      }
   }

   return 0;
}

#ifndef HB_LONG_LONG_OFF
int hb_stornll( LONGLONG llValue, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_stornll(%" PFLL "d, %d, ...)", llValue, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetNLL( pItem, va_arg( va, ULONG ), llValue ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutNLL( pItem, llValue );
         return 1;
      }
   }

   return 0;
}
#endif

int hb_stornint( HB_LONG lValue, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_stornint(%" PFHL "d, %d, ...)", lValue, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetNInt( pItem, va_arg( va, ULONG ), lValue ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutNInt( pItem, lValue );
         return 1;
      }
   }

   return 0;
}

int hb_stornd( double dNumber, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_stornd(%lf, %d, ...)", dNumber, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetND( pItem, va_arg( va, ULONG ), dNumber ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutND( pItem, dNumber );
         return 1;
      }
   }

   return 0;
}

int hb_storptr( void * pointer, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storptr(%p, %d, ...)", pointer, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetPtr( pItem, va_arg( va, ULONG ), pointer ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutPtr( pItem, pointer );
         return 1;
      }
   }

   return 0;
}

int hb_storptrGC( void * pointer, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storptrGC(%p, %d, ...)", pointer, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetPtrGC( pItem, va_arg( va, ULONG ), pointer ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutPtrGC( pItem, pointer );
         return 1;
      }
   }

   return 0;
}

#undef hb_pcount
int  hb_pcount( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_pcount()"));

   return ( int ) ( hb_stackBaseItem() )->item.asSymbol.paramcnt;
}
