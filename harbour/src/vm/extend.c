/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Extend API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
 * www - http://harbour-project.org
 *
 * Copyright 1999-2009 Viktor Szakats (harbour syenar.net)
 *    hb_stor()
 *    hb_retnlen()
 *    hb_retnilen()
 *    hb_retnllen()
 *    hb_retndlen()
 *    hb_retdl()
 *    hb_parnidef() (based on idea by Mindaugas Kavaliauskas)
 *    hb_parnldef()
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

PHB_ITEM hb_paramError( int iParam )
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

HB_ULONG hb_parinfo( int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parinfo(%d)", iParam));

   if( iParam == 0 )
      return ( HB_ULONG ) hb_pcount();
   else
   {
      if( iParam >= -1 && iParam <= hb_pcount() )
      {
         PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
         HB_TYPE uiType = HB_ITEM_TYPE( pItem );

         if( uiType & HB_IT_BYREF )
            uiType |= HB_ITEM_TYPE( hb_itemUnRef( pItem ) );

         return ( HB_ULONG ) uiType;
      }
      else
         return 0;
   }
}

HB_SIZE hb_parinfa( int iParamNum, HB_SIZE nArrayIndex )
{
   PHB_ITEM pArray;

   HB_TRACE(HB_TR_DEBUG, ("hb_parinfa(%d, %" HB_PFS "u)", iParamNum, nArrayIndex));

   pArray = hb_param( iParamNum, HB_IT_ARRAY );

   if( pArray )
   {
      if( nArrayIndex == 0 )
         return hb_arrayLen( pArray );
      else
         return ( HB_ISIZ ) hb_arrayGetType( pArray, nArrayIndex );
   }
   else
      return 0;
}

/* function to be called from pcode DLLs to detect if the extend system
 * is going to use an array item */

HB_BOOL hb_extIsArray( int iParam )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   if( iParam == -1 )
      pItem = hb_stackReturnItem();
   else if( iParam >= 0 && iParam <= hb_pcount() )
      pItem = hb_stackItemFromBase( iParam );
   else
      return HB_FALSE;

   if( HB_IS_BYREF( pItem ) )
      pItem = hb_itemUnRef( pItem );

   return HB_IS_ARRAY( pItem ) && !HB_ARRAY_OBJ( pItem );
}

/* function to be called from pcode DLLs to detect if the extend system
 * is going to use an object item */

HB_BOOL hb_extIsObject( int iParam )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   if( iParam == -1 )
      pItem = hb_stackReturnItem();
   else if( iParam >= 0 && iParam <= hb_pcount() )
      pItem = hb_stackItemFromBase( iParam );
   else
      return HB_FALSE;

   if( HB_IS_BYREF( pItem ) )
      pItem = hb_itemUnRef( pItem );

   return HB_IS_OBJECT( pItem );
}

/* NOTE: Caller should not modify the buffer returned by this function.
         [vszakats] */

const char * hb_parc( int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parc(%d)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_STRING( pItem ) )
         return pItem->item.asString.value;
   }

   return NULL;
}

const char * hb_parcx( int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parcx(%d)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_STRING( pItem ) )
         return pItem->item.asString.value;
   }

   return "";
}

HB_SIZE hb_parclen( int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parclen(%d)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_STRING( pItem ) )
         return pItem->item.asString.length;
   }

   return 0;
}

/* NOTE: Similar to _parclen() but returns the length including the
         terminating zero byte, and it only works for parameters passed by
         reference. [vszakats] */

HB_SIZE hb_parcsiz( int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parcsiz(%d)", iParam));

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
      }
   }

   return 0;
}

/* NOTE: Using hb_stackDateBuffer() a temporary date buffer guaranties
         good behavior when multithreading. */

const char * hb_pards( int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_pards(%d)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DATETIME( pItem ) )
         return hb_dateDecStr( hb_stackDateBuffer(), pItem->item.asDateTime.julian );
   }

   return hb_dateDecStr( hb_stackDateBuffer(), 0 );
}

/* NOTE: szDate must be a 9 chars wide buffer. [vszakats] */

char * hb_pardsbuff( char * szDate, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_pardsbuff(%p, %d)", szDate, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DATETIME( pItem ) )
         return hb_dateDecStr( szDate, pItem->item.asDateTime.julian );
   }

   return hb_dateDecStr( szDate, 0 );
}

/* retrieve a date as long integer - number of days from Julian's day */

long hb_pardl( int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_pardl(%d)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DATETIME( pItem ) )
         return pItem->item.asDateTime.julian;
   }

   return hb_itemGetDL( NULL );
}

double hb_partd( int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_partd(%d)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DATETIME( pItem ) )
         return hb_timeStampPackDT( pItem->item.asDateTime.julian,
                                    pItem->item.asDateTime.time );
   }

   return 0;
}

HB_BOOL hb_partdt( long * plJulian, long * plMilliSec, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_partdt(%p,%p,%d)", plJulian, plMilliSec, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DATETIME( pItem ) )
      {
         *plJulian = pItem->item.asDateTime.julian;
         *plMilliSec = pItem->item.asDateTime.time;
         return HB_TRUE;
      }
   }

   return HB_FALSE;
}


int  hb_parl( int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parl(%d)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_LOGICAL( pItem ) )
         return pItem->item.asLogical.value ? 1 : 0;
   }

   return 0;
}

int  hb_parldef( int iParam, int iDefValue )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parldef(%d,%d)", iParam, iDefValue));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_LOGICAL( pItem ) )
         return pItem->item.asLogical.value ? 1 : 0;
   }

   return iDefValue;
}

double  hb_parnd( int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parnd(%d)", iParam));

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
   }

   return 0;
}

int  hb_parni( int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parni(%d)", iParam));

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
#if defined( __GNUC__ )
         return ( int ) ( unsigned int ) pItem->item.asDouble.value;
#else
         return ( int ) pItem->item.asDouble.value;
#endif
   }

   return 0;
}

int  hb_parnidef( int iParam, int iDefValue )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parni(%d, %d)", iParam, iDefValue));

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
#if defined( __GNUC__ )
         return ( int ) ( unsigned int ) pItem->item.asDouble.value;
#else
         return ( int ) pItem->item.asDouble.value;
#endif
   }

   return iDefValue;
}

long  hb_parnl( int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parnl(%d)", iParam));

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
#if defined( __GNUC__ )
         return ( long ) ( unsigned long ) pItem->item.asDouble.value;
#else
         return ( long ) pItem->item.asDouble.value;
#endif
   }

   return 0;
}

long  hb_parnldef( int iParam, long lDefValue )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parnldef(%d, %ld)", iParam, lDefValue));

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
#if defined( __GNUC__ )
         return ( long ) ( unsigned long ) pItem->item.asDouble.value;
#else
         return ( long ) pItem->item.asDouble.value;
#endif
   }

   return lDefValue;
}

HB_ISIZ hb_parns( int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parns(%d)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_LONG( pItem ) )
         return ( HB_ISIZ ) pItem->item.asLong.value;
      else if( HB_IS_INTEGER( pItem ) )
         return ( HB_ISIZ ) pItem->item.asInteger.value;
      else if( HB_IS_DOUBLE( pItem ) )
         return ( HB_ISIZ ) pItem->item.asDouble.value;
   }

   return 0;
}

HB_ISIZ hb_parnsdef( int iParam, HB_ISIZ nDefValue )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parnsdef(%d, %" HB_PFS "d)", iParam, nDefValue));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_LONG( pItem ) )
         return ( HB_ISIZ ) pItem->item.asLong.value;
      else if( HB_IS_INTEGER( pItem ) )
         return ( HB_ISIZ ) pItem->item.asInteger.value;
      else if( HB_IS_DOUBLE( pItem ) )
         return ( HB_ISIZ ) pItem->item.asDouble.value;
   }

   return nDefValue;
}

#ifndef HB_LONG_LONG_OFF
HB_LONGLONG  hb_parnll( int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parnll(%d)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_LONG( pItem ) )
         return ( HB_LONGLONG ) pItem->item.asLong.value;
      else if( HB_IS_INTEGER( pItem ) )
         return ( HB_LONGLONG ) pItem->item.asInteger.value;
      else if( HB_IS_DOUBLE( pItem ) )
#if defined( __GNUC__ )
         return ( HB_LONGLONG ) ( HB_ULONGLONG ) pItem->item.asDouble.value;
#else
         return ( HB_LONGLONG ) pItem->item.asDouble.value;
#endif
   }

   return 0;
}
#endif

HB_MAXINT hb_parnint( int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parnint(%d)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_LONG( pItem ) )
         return ( HB_MAXINT ) pItem->item.asLong.value;
      else if( HB_IS_INTEGER( pItem ) )
         return ( HB_MAXINT ) pItem->item.asInteger.value;
      else if( HB_IS_DOUBLE( pItem ) )
#if defined( __GNUC__ )
         return ( HB_MAXINT ) ( HB_MAXUINT ) pItem->item.asDouble.value;
#else
         return ( HB_MAXINT ) pItem->item.asDouble.value;
#endif
   }

   return 0;
}

HB_MAXINT hb_parnintdef( int iParam, HB_MAXINT nDefValue )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parnintdef(%d, %" PFHL "d)", iParam, nDefValue));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_LONG( pItem ) )
         return ( HB_MAXINT ) pItem->item.asLong.value;
      else if( HB_IS_INTEGER( pItem ) )
         return ( HB_MAXINT ) pItem->item.asInteger.value;
      else if( HB_IS_DOUBLE( pItem ) )
#if defined( __GNUC__ )
         return ( HB_MAXINT ) ( HB_MAXUINT ) pItem->item.asDouble.value;
#else
         return ( HB_MAXINT ) pItem->item.asDouble.value;
#endif
   }

   return nDefValue;
}

void * hb_parptr( int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parptr(%d)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_POINTER( pItem ) )
         return pItem->item.asPointer.value;
   }

   return NULL;
}

void * hb_parptrGC( const HB_GC_FUNCS * pFuncs, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parptrGC(%p,%d)", pFuncs, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_POINTER( pItem ) && pItem->item.asPointer.collect &&
          hb_gcFuncs( pItem->item.asPointer.value ) == pFuncs )
         return pItem->item.asPointer.value;
   }

   return NULL;
}

/* NOTE: Caller should not modify the buffer returned by this function.
         [vszakats] */

const char * hb_parvc( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parvc(%d, ...)", iParam));

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
         HB_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, HB_SIZE );
         va_end( va );

         pItem = hb_arrayGetItemPtr( pItem, nArrayIndex );
         return pItem && HB_IS_STRING( pItem ) ? hb_itemGetCPtr( pItem ) : NULL;
      }
   }

   return NULL;
}

const char * hb_parvcx( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parvcx(%d, ...)", iParam));

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
         HB_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, HB_SIZE );
         va_end( va );

         return hb_arrayGetCPtr( pItem, nArrayIndex );
      }
   }

   return "";
}

HB_SIZE hb_parvclen( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parvclen(%d, ...)", iParam));

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
         HB_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, HB_SIZE );
         va_end( va );

         return hb_arrayGetCLen( pItem, nArrayIndex );
      }
   }

   return 0;
}

/* NOTE: Similar to _parclen() but returns the length including the
         terminating zero byte, and it only works for parameters passed by
         reference. [vszakats] */

HB_SIZE hb_parvcsiz( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parvcsiz(%d, ...)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      /* NOTE: hb_parvcsiz() will only work for strings passed by reference.
               CA-Cl*pper works like this. [vszakats] */

      if( HB_IS_BYREF( pItem ) )
      {
         pItem = hb_itemUnRef( pItem );

         if( HB_IS_STRING( pItem ) )
            return pItem->item.asString.length + 1;
         else if( HB_IS_ARRAY( pItem ) )
         {
            va_list va;
            HB_SIZE nArrayIndex;

            va_start( va, iParam );
            nArrayIndex = va_arg( va, HB_SIZE );
            va_end( va );

            return hb_arrayGetCLen( pItem, nArrayIndex ) + 1;
         }
      }
   }

   return 0;
}

/* NOTE: Using hb_stackDateBuffer() a temporary date buffer guaranties
         good behavior when multithreading. */

const char * hb_parvds( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parvds(%d, ...)", iParam));

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
         HB_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, HB_SIZE );
         va_end( va );

         return hb_arrayGetDS( pItem, nArrayIndex, hb_stackDateBuffer() );
      }
   }

   return hb_dateDecStr( hb_stackDateBuffer(), 0 );
}

/* NOTE: szDate must be a 9 chars wide buffer. [vszakats] */

char  * hb_parvdsbuff( char * szDate, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parvdsbuff(%p, %d, ...)", szDate, iParam));

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
         HB_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, HB_SIZE );
         va_end( va );

         return hb_arrayGetDS( pItem, nArrayIndex, szDate );
      }
   }

   return hb_dateDecStr( szDate, 0 );
}

/* retrieve a date as long integer - number of days from Julian's day */

long hb_parvdl( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parvdl(%d, ...)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DATETIME( pItem ) )
         return pItem->item.asDateTime.julian;
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, HB_SIZE );
         va_end( va );

         return hb_arrayGetDL( pItem, nArrayIndex );
      }
   }

   return hb_itemGetDL( NULL );
}

double hb_parvtd( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parvtd(%d, ...)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DATETIME( pItem ) )
         return hb_timeStampPackDT( pItem->item.asDateTime.julian,
                                    pItem->item.asDateTime.time );
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, HB_SIZE );
         va_end( va );

         return hb_arrayGetTD( pItem, nArrayIndex );
      }
   }

   return 0;
}

HB_BOOL hb_parvtdt( long * plJulian, long * plMilliSec, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parvtdt(%p,%p,%d, ...)", plJulian, plMilliSec, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_DATETIME( pItem ) )
      {
         *plJulian = pItem->item.asDateTime.julian;
         *plMilliSec = pItem->item.asDateTime.time;
         return HB_TRUE;
      }
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, HB_SIZE );
         va_end( va );

         return hb_arrayGetTDT( pItem, nArrayIndex, plJulian, plMilliSec );
      }
   }

   return HB_FALSE;
}


int  hb_parvl( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parvl(%d, ...)", iParam));

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
         HB_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, HB_SIZE );
         va_end( va );

         return hb_arrayGetL( pItem, nArrayIndex ) ? 1 : 0;
      }
   }

   return 0;
}

double  hb_parvnd( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parvnd(%d, ...)", iParam));

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
         HB_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, HB_SIZE );
         va_end( va );

         return hb_arrayGetND( pItem, nArrayIndex );
      }
   }

   return 0;
}

int  hb_parvni( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parvni(%d, ...)", iParam));

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
#if defined( __GNUC__ )
         return ( int ) ( unsigned int ) pItem->item.asDouble.value;
#else
         return ( int ) pItem->item.asDouble.value;
#endif
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, HB_SIZE );
         va_end( va );

         return hb_arrayGetNI( pItem, nArrayIndex );
      }
   }

   return 0;
}

long  hb_parvnl( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parvnl(%d, ...)", iParam));

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
#if defined( __GNUC__ )
         return ( long ) ( unsigned long ) pItem->item.asDouble.value;
#else
         return ( long ) pItem->item.asDouble.value;
#endif
      /* CA-Cl*pper does it */
      else if( HB_IS_DATETIME( pItem ) )
         return ( long ) pItem->item.asDateTime.julian;
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, HB_SIZE );
         va_end( va );

         return hb_arrayGetNL( pItem, nArrayIndex );
      }
   }

   return 0;
}

HB_ISIZ hb_parvns( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parvns(%d, ...)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_LONG( pItem ) )
         return ( HB_ISIZ ) pItem->item.asLong.value;
      else if( HB_IS_INTEGER( pItem ) )
         return ( HB_ISIZ ) pItem->item.asInteger.value;
      else if( HB_IS_DOUBLE( pItem ) )
         return ( HB_ISIZ ) pItem->item.asDouble.value;
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, HB_SIZE );
         va_end( va );

         return hb_arrayGetNS( pItem, nArrayIndex );
      }
   }

   return 0;
}

#ifndef HB_LONG_LONG_OFF
HB_LONGLONG hb_parvnll( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parvnll(%d, ...)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_LONG( pItem ) )
         return ( HB_LONGLONG ) pItem->item.asLong.value;
      else if( HB_IS_INTEGER( pItem ) )
         return ( HB_LONGLONG ) pItem->item.asInteger.value;
      else if( HB_IS_DOUBLE( pItem ) )
#if defined( __GNUC__ )
         return ( HB_LONGLONG ) ( HB_ULONGLONG ) pItem->item.asDouble.value;
#else
         return ( HB_LONGLONG ) pItem->item.asDouble.value;
#endif
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, HB_SIZE );
         va_end( va );

         return hb_arrayGetNLL( pItem, nArrayIndex );
      }
   }

   return 0;
}
#endif

HB_MAXINT hb_parvnint( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parvnint(%d, ...)", iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_LONG( pItem ) )
         return ( HB_MAXINT ) pItem->item.asLong.value;
      else if( HB_IS_INTEGER( pItem ) )
         return ( HB_MAXINT ) pItem->item.asInteger.value;
      else if( HB_IS_DOUBLE( pItem ) )
#if defined( __GNUC__ )
         return ( HB_MAXINT ) ( HB_MAXUINT ) pItem->item.asDouble.value;
#else
         return ( HB_MAXINT ) pItem->item.asDouble.value;
#endif
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, HB_SIZE );
         va_end( va );

         return hb_arrayGetNInt( pItem, nArrayIndex );
      }
   }

   return 0;
}

void * hb_parvptr( int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parvptr(%d, ...)", iParam));

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
         HB_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, HB_SIZE );
         va_end( va );

         return hb_arrayGetPtr( pItem, nArrayIndex );
      }
   }

   return NULL;
}

void * hb_parvptrGC( const HB_GC_FUNCS * pFuncs, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_parvptrGC(%p,%d, ...)", pFuncs, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_POINTER( pItem ) )
      {
         if( pItem->item.asPointer.collect &&
             hb_gcFuncs( pItem->item.asPointer.value ) == pFuncs )
            return pItem->item.asPointer.value;
      }
      else if( HB_IS_ARRAY( pItem ) )
      {
         va_list va;
         HB_SIZE nArrayIndex;

         va_start( va, iParam );
         nArrayIndex = va_arg( va, HB_SIZE );
         va_end( va );

         pItem = hb_arrayGetItemPtr( pItem, nArrayIndex );
         if( pItem && HB_IS_POINTER( pItem ) &&
             pItem->item.asPointer.collect &&
             hb_gcFuncs( pItem->item.asPointer.value ) == pFuncs )
            return pItem->item.asPointer.value;
      }
   }

   return NULL;
}

#undef hb_ret
void hb_ret( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_ret()"));

   hb_itemClear( hb_stackReturnItem() );
}

#undef hb_reta
void hb_reta( HB_SIZE nLen )  /* undocumented hb_reta() */
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_reta(%" HB_PFS "u)", nLen));

   hb_arrayNew( hb_stackReturnItem(), nLen );
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

   hb_itemPutCPtr( hb_stackReturnItem(), szText );
}

#undef hb_retc_const
void hb_retc_const( const char * szText )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retc_const(%s)", szText));

   hb_itemPutCConst( hb_stackReturnItem(), szText );
}

#undef hb_retclen
void hb_retclen( const char * szText, HB_SIZE nLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retclen(%.*s, %" HB_PFS "u)", ( int ) nLen, szText, nLen));

   hb_itemPutCL( hb_stackReturnItem(), szText, nLen );
}

#undef hb_retclen_buffer
void hb_retclen_buffer( char * szText, HB_SIZE nLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retclen_buffer(%.*s, %" HB_PFS "u)", ( int ) nLen, szText, nLen));

   hb_itemPutCLPtr( hb_stackReturnItem(), szText, nLen );
}

#undef hb_retclen_const
void hb_retclen_const( const char * szText, HB_SIZE nLen )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retclen_const(%.*s, %" HB_PFS "u)", ( int ) nLen, szText, nLen));

   hb_itemPutCLConst( hb_stackReturnItem(), szText, nLen );
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
void hb_rettdt( long lJulian, long lMilliSec )
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

   hb_itemPutL( hb_stackReturnItem(), iLogical ? HB_TRUE : HB_FALSE );
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

#undef hb_retns
void hb_retns( HB_ISIZ nNumber )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retns(%" HB_PFS "d )", nNumber));

   hb_itemPutNS( hb_stackReturnItem(), nNumber );
}

#ifndef HB_LONG_LONG_OFF
#undef hb_retnll
void hb_retnll( HB_LONGLONG llNumber )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retnll(%" PFLL "d)", llNumber));

   hb_itemPutNLL( hb_stackReturnItem(), llNumber );
}
#endif

#undef hb_retnint
void hb_retnint( HB_MAXINT nNumber )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retnl(%" PFHL "d )", nNumber));

   hb_itemPutNInt( hb_stackReturnItem(), nNumber );
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
void hb_retnlllen( HB_LONGLONG llNumber, int iWidth )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retnlllen(%" PFLL "d, %d)", llNumber, iWidth));

   hb_itemPutNLLLen( hb_stackReturnItem(), llNumber, iWidth );
}
#endif

#undef hb_retnintlen
void hb_retnintlen( HB_MAXINT nNumber, int iWidth )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_retnintlen(%" PFHL "d, %d)", nNumber, iWidth));

   hb_itemPutNIntLen( hb_stackReturnItem(), nNumber, iWidth );
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

int hb_stor( int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_stor(%d)", iParam));

   if( iParam == -1 )
   {
      hb_itemClear( hb_stackReturnItem() );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemClear( hb_itemUnRef( pItem ) );
         return 1;
      }
   }

   return 0;
}

int hb_storc( const char * szText, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storc(%s, %d)", szText, iParam));

   if( iParam == -1 )
   {
      hb_itemPutC( hb_stackReturnItem(), szText );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutC( hb_itemUnRef( pItem ), szText );
         return 1;
      }
   }

   return 0;
}

int hb_storclen( const char * szText, HB_SIZE nLen, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storclen(%.*s, %" HB_PFS "u, %d)", ( int ) nLen, szText, nLen, iParam));

   if( iParam == -1 )
   {
      hb_itemPutCL( hb_stackReturnItem(), szText, nLen );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutCL( hb_itemUnRef( pItem ), szText, nLen );
         return 1;
      }
   }

   return 0;
}

int hb_storclen_buffer( char * szText, HB_SIZE nLen, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storclen_buffer(%.*s, %" HB_PFS "u, %d)", ( int ) nLen, szText, nLen, iParam));

   if( iParam == -1 )
   {
      hb_itemPutCLPtr( hb_stackReturnItem(), szText, nLen );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutCLPtr( hb_itemUnRef( pItem ), szText, nLen );
         return 1;
      }
   }

   return 0;
}

/* szDate must have YYYYMMDD format */

int hb_stords( const char * szDate, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_stords(%s, %d)", szDate, iParam));

   if( iParam == -1 )
   {
      hb_itemPutDS( hb_stackReturnItem(), szDate );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutDS( hb_itemUnRef( pItem ), szDate );
         return 1;
      }
   }

   return 0;
}

int hb_stordl( long lJulian, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_stordl(%ld, %d)", lJulian, iParam));

   if( iParam == -1 )
   {
      hb_itemPutDL( hb_stackReturnItem(), lJulian );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutDL( hb_itemUnRef( pItem ), lJulian );
         return 1;
      }
   }

   return 0;
}

int hb_stortd( double dTimeStamp, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_stortd(%lf, %d)", dTimeStamp, iParam));

   if( iParam == -1 )
   {
      hb_itemPutTD( hb_stackReturnItem(), dTimeStamp );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutTD( hb_itemUnRef( pItem ), dTimeStamp );
         return 1;
      }
   }

   return 0;
}

int hb_stortdt( long lJulian, long lMilliSec, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_stortd(%ld, %ld, %d)", lJulian, lMilliSec, iParam));

   if( iParam == -1 )
   {
      hb_itemPutTDT( hb_stackReturnItem(), lJulian, lMilliSec );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutTDT( hb_itemUnRef( pItem ), lJulian, lMilliSec );
         return 1;
      }
   }

   return 0;
}

int hb_storl( int iLogical, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storl(%d, %d)", iLogical, iParam));

   if( iParam == -1 )
   {
      hb_itemPutL( hb_stackReturnItem(), iLogical ? HB_TRUE : HB_FALSE );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutL( hb_itemUnRef( pItem ), iLogical ? HB_TRUE : HB_FALSE );
         return 1;
      }
   }

   return 0;
}

int hb_storni( int iValue, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storni(%d, %d)", iValue, iParam));

   if( iParam == -1 )
   {
      hb_itemPutNI( hb_stackReturnItem(), iValue );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutNI( hb_itemUnRef( pItem ), iValue );
         return 1;
      }
   }

   return 0;
}

int hb_stornl( long lValue, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_stornl(%ld, %d)", lValue, iParam));

   if( iParam == -1 )
   {
      hb_itemPutNL( hb_stackReturnItem(), lValue );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutNL( hb_itemUnRef( pItem ), lValue );
         return 1;
      }
   }

   return 0;
}

int hb_storns( HB_ISIZ nValue, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storns(%" HB_PFS "d, %d)", nValue, iParam));

   if( iParam == -1 )
   {
      hb_itemPutNS( hb_stackReturnItem(), nValue );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutNS( hb_itemUnRef( pItem ), nValue );
         return 1;
      }
   }

   return 0;
}

#ifndef HB_LONG_LONG_OFF
int hb_stornll( HB_LONGLONG llValue, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_stornll(%" PFLL "d, %d)", llValue, iParam));

   if( iParam == -1 )
   {
      hb_itemPutNLL( hb_stackReturnItem(), llValue );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutNLL( hb_itemUnRef( pItem ), llValue );
         return 1;
      }
   }

   return 0;
}
#endif

int hb_stornint( HB_MAXINT nValue, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_stornint(%" PFHL "d, %d)", nValue, iParam));

   if( iParam == -1 )
   {
      hb_itemPutNInt( hb_stackReturnItem(), nValue );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutNInt( hb_itemUnRef( pItem ), nValue );
         return 1;
      }
   }

   return 0;
}

int hb_stornd( double dNumber, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_stornd(%lf, %d)", dNumber, iParam));

   if( iParam == -1 )
   {
      hb_itemPutND( hb_stackReturnItem(), dNumber );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutND( hb_itemUnRef( pItem ), dNumber );
         return 1;
      }
   }

   return 0;
}

int hb_storptr( void * pointer, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storptr(%p, %d)", pointer, iParam));

   if( iParam == -1 )
   {
      hb_itemPutPtr( hb_stackReturnItem(), pointer );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutPtr( hb_itemUnRef( pItem ), pointer );
         return 1;
      }
   }

   return 0;
}

int hb_storptrGC( void * pointer, int iParam )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storptrGC(%p, %d)", pointer, iParam));

   if( iParam == -1 )
   {
      hb_itemPutPtrGC( hb_stackReturnItem(), pointer );
      return 1;
   }
   else if( iParam >= 0 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam );

      if( HB_IS_BYREF( pItem ) )
      {
         hb_itemPutPtrGC( hb_itemUnRef( pItem ), pointer );
         return 1;
      }
   }

   return 0;
}

/* hb_storv*() similar to hb_stor*() but they accepts optional array index
 * just like Cl*pper's _stor*() functions
 */

int hb_storvc( const char * szText, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storvc(%s, %d, ...)", szText, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      HB_BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetC( pItem, va_arg( va, HB_SIZE ), szText ) ? 1 : 0;
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

int hb_storvclen( const char * szText, HB_SIZE nLen, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storvclen(%.*s, %" HB_PFS "u, %d, ...)", ( int ) nLen, szText, nLen, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      HB_BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetCL( pItem, va_arg( va, HB_SIZE ), szText, nLen ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutCL( pItem, szText, nLen );
         return 1;
      }
   }

   return 0;
}

int hb_storvclen_buffer( char * szText, HB_SIZE nLen, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storvclen_buffer(%.*s, %" HB_PFS "u, %d, ...)", ( int ) nLen, szText, nLen, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      HB_BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetCLPtr( pItem, va_arg( va, HB_SIZE ), szText, nLen ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutCLPtr( pItem, szText, nLen );
         return 1;
      }
   }

   return 0;
}

/* szDate must have YYYYMMDD format */

int hb_storvds( const char * szDate, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storvds(%s, %d, ...)", szDate, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      HB_BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetDS( pItem, va_arg( va, HB_SIZE ), szDate ) ? 1 : 0;
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

int hb_storvdl( long lJulian, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storvdl(%ld, %d, ...)", lJulian, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      HB_BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetDL( pItem, va_arg( va, HB_SIZE ), lJulian ) ? 1 : 0;
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

int hb_storvtd( double dTimeStamp, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storvtd(%lf, %d, ...)", dTimeStamp, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      HB_BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetTD( pItem, va_arg( va, HB_SIZE ), dTimeStamp ) ? 1 : 0;
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

int hb_storvtdt( long lJulian, long lMilliSec, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storvtd(%ld, %ld, %d, ...)", lJulian, lMilliSec, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      HB_BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetTDT( pItem, va_arg( va, HB_SIZE ), lJulian, lMilliSec ) ? 1 : 0;
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

int hb_storvl( int iLogical, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storvl(%d, %d, ...)", iLogical, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      HB_BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetL( pItem, va_arg( va, HB_SIZE ), iLogical ? HB_TRUE : HB_FALSE ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutL( pItem, iLogical ? HB_TRUE : HB_FALSE );
         return 1;
      }
   }

   return 0;
}

int hb_storvni( int iValue, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storvni(%d, %d, ...)", iValue, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      HB_BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetNI( pItem, va_arg( va, HB_SIZE ), iValue ) ? 1 : 0;
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

int hb_storvnl( long lValue, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storvnl(%ld, %d, ...)", lValue, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      HB_BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetNL( pItem, va_arg( va, HB_SIZE ), lValue ) ? 1 : 0;
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

int hb_storvns( HB_ISIZ nValue, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storvns(%" HB_PFS "d, %d, ...)", nValue, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      HB_BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetNS( pItem, va_arg( va, HB_SIZE ), nValue ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutNS( pItem, nValue );
         return 1;
      }
   }

   return 0;
}

#ifndef HB_LONG_LONG_OFF
int hb_storvnll( HB_LONGLONG llValue, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storvnll(%" PFLL "d, %d, ...)", llValue, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      HB_BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetNLL( pItem, va_arg( va, HB_SIZE ), llValue ) ? 1 : 0;
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

int hb_storvnint( HB_MAXINT nValue, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storvnint(%" PFHL "d, %d, ...)", nValue, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      HB_BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetNInt( pItem, va_arg( va, HB_SIZE ), nValue ) ? 1 : 0;
         va_end( va );
         return iRetVal;
      }
      else if( bByRef || iParam == -1 )
      {
         hb_itemPutNInt( pItem, nValue );
         return 1;
      }
   }

   return 0;
}

int hb_storvnd( double dNumber, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storvnd(%lf, %d, ...)", dNumber, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      HB_BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetND( pItem, va_arg( va, HB_SIZE ), dNumber ) ? 1 : 0;
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

int hb_storvptr( void * pointer, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storvptr(%p, %d, ...)", pointer, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      HB_BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetPtr( pItem, va_arg( va, HB_SIZE ), pointer ) ? 1 : 0;
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

int hb_storvptrGC( void * pointer, int iParam, ... )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE(HB_TR_DEBUG, ("hb_storvptrGC(%p, %d, ...)", pointer, iParam));

   if( iParam >= -1 && iParam <= hb_pcount() )
   {
      PHB_ITEM pItem = ( iParam == -1 ) ? hb_stackReturnItem() : hb_stackItemFromBase( iParam );
      HB_BOOL bByRef = HB_IS_BYREF( pItem );

      if( bByRef  )
         pItem = hb_itemUnRef( pItem );

      if( HB_IS_ARRAY( pItem ) )
      {
         int iRetVal;
         va_list va;
         va_start( va, iParam );
         iRetVal = hb_arraySetPtrGC( pItem, va_arg( va, HB_SIZE ), pointer ) ? 1 : 0;
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
