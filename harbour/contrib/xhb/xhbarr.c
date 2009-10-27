/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * The FastItem Optimization API
 *
 * Copyright 2008 Ron Pinkas <ron@@ronpinkas.com>
 * www - http://www.xharbour.org
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
 * As a special exception, xHarbour license gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released with this xHarbour
 * explicit exception.  If you add/copy code from other sources,
 * as the General Public License permits, the above exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

//#include "hbvmint.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapierr.h"

/* aSplice( <aArray> [, <nPos>] [, <nCount>] [,<xVal1>] [, ...] [, <xValN>]  ) => <aDeleted>
 * Removes elements and return them as array, optionally add items
 */
HB_FUNC( ASPLICE )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      ULONG ulStart, ulRemove, ulIndex, ulAdd;
      ULONG ulLen = hb_arrayLen( pArray );
      PHB_ITEM pReturn = hb_stackReturnItem();

      if( ulLen == 0 )
      {
         hb_arrayNew( pReturn, 0 );
         return;
      }

      if( HB_ISNUM( 2 ) )
      {
         ulStart = ( ULONG ) hb_parnl( 2 );
      }
      else
      {
         ulStart = ulLen + ( hb_pcount() > 3 && !HB_ISNUM( 3 ) ? 1 : 0 );
      }

      if( HB_ISNUM( 3 ) )
      {
         ulRemove = ( ULONG ) hb_parnl( 3 );
      }
      else
      {
         ulRemove = ( hb_pcount() > 3 && ulStart == ulLen + 1 ) ? 0 : 1;
      }

      if( ulStart == 0 || ulStart > ulLen )
      {
         if( ! ( ulStart == ulLen + 1 && hb_pcount() > 3 && ulRemove == 0 ) )
         {
            hb_errRT_BASE( EG_ARG, 1003, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            return;
         }
      }

      if( ulStart + ulRemove - 1 > ulLen )
      {
         ulRemove = ulLen - ulStart + 1;
      }

      hb_arrayNew( pReturn, ulRemove );

      /* 0 Based */
      ulStart--;

      for( ulIndex = ulStart + 1; ( ulIndex - ulStart ) <= ulRemove; ulIndex++ )
      {
         hb_itemForwardValue( hb_arrayGetItemPtr( pReturn, ulIndex - ulStart ),
                              hb_arrayGetItemPtr( pArray, ulIndex ) );
      }

      if( hb_pcount() > 3 )
      {
         ULONG ulNew = 0;
         ulAdd = hb_pcount() - 3;

         if( ulAdd > ulRemove )
         {
            ULONG ulMore = ulAdd - ulRemove;
            ULONG ulShift = ulLen - (ulStart + ulRemove);

            hb_arraySize( pArray, ulLen + ulMore );

            /* Shift right BEFORE adding, so that new items will not override existing values. */
            for( ulIndex = ulLen; ulIndex && --ulShift; --ulIndex )
            {
               hb_itemForwardValue( hb_arrayGetItemPtr( pArray, ulIndex + ulMore ),
                                    hb_arrayGetItemPtr( pArray, ulIndex ) );
            }

            /* Now insert new values into emptied space. */
            for( ulIndex = ulStart; ++ulNew <= ulAdd; ulIndex++ )
            {
               hb_itemForwardValue( hb_arrayGetItemPtr( pArray, ulIndex + 1 ),
                                    hb_param( 3 + ulNew, HB_IT_ANY ) );
            }
         }
         else
         {
            /* Insert over the space emptied by removed items */
            for( ulIndex = ulStart; ++ulNew <= ulAdd; ulIndex++ )
            {
               hb_itemForwardValue( hb_arrayGetItemPtr( pArray, ulIndex + 1 ), hb_param( 3 + ulNew, HB_IT_ANY ) );
            }

            if( ulRemove > ulAdd )
            {
               ulRemove -= ulAdd;

               /* Shift left to compact the emptied hole. */
               for( ulIndex = ulStart + ulAdd + 1; ulIndex + ulRemove <= ulLen; ulIndex++ )
               {
                  hb_itemForwardValue( hb_arrayGetItemPtr( pArray, ulIndex ),
                                       hb_arrayGetItemPtr( pArray, ulIndex + ulRemove ) );
               }
            }
         }
      }
      else
      {
         for( ulIndex = ulStart + 1; ulIndex + ulRemove <= ulLen; ulIndex++ )
         {
            hb_itemForwardValue( hb_arrayGetItemPtr( pArray, ulIndex ),
                                 hb_arrayGetItemPtr( pArray, ulIndex + ulRemove ) );
         }

         hb_arraySize( pArray, ulLen - ulRemove );
      }
   }
}

/* Synonym of aSplice() Xbase++ compatability (extended with optional replacemenet values) */
HB_FUNC( AREMOVE )
{
   HB_FUNC_EXEC( ASPLICE )
}

/* aMerge( <aTarget>, <aSource> [, <nPos>] ) => aTarget */
HB_FUNC( AMERGE )
{
   PHB_ITEM pArray1 = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pArray2 = hb_param( 2, HB_IT_ARRAY );

   if( pArray1 && pArray2 )
   {
      ULONG ulLen = hb_arrayLen( pArray1 );
      ULONG ulAdd = hb_arrayLen( pArray2 );
      ULONG ulIndex, ulStart;

      hb_arraySize( pArray1, ulLen + ulAdd );

      if( HB_ISNUM( 3 ) )
      {
         ulStart = hb_parnl( 3 ) - 1;
         if( ulStart > ulLen )
         {
            hb_errRT_BASE( EG_ARG, 1003, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            return;
         }

         /* Shift right BEFORE merging, so that merged items will not override existing values. */
         for( ulIndex = ulLen; ulIndex > ulStart; --ulIndex )
         {
            hb_itemForwardValue( hb_arrayGetItemPtr( pArray1, ulIndex + ulAdd ),
                                 hb_arrayGetItemPtr( pArray1, ulIndex ) );
         }
      }
      else
      {
         ulStart = ulLen;
      }

      for( ulIndex = 1; ulIndex <= ulAdd; ulIndex++ )
      {
         hb_itemCopy( hb_arrayGetItemPtr( pArray1, ulStart + ulIndex ),
                      hb_arrayGetItemPtr( pArray2, ulIndex ) );
      }

      hb_itemCopy( hb_stackReturnItem(), pArray1 );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 1003, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( XHB_ADEL )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      long lPos = hb_parnl( 2 );

      if( lPos == 0 )
         lPos = 1;
      else if( lPos < 0 )
         lPos += hb_arrayLen( pArray ) + 1;

      if( hb_arrayDel( pArray, lPos ) )
      {
         if( hb_parl( 3 ) )
            hb_arraySize( pArray, hb_arrayLen( pArray ) - 1 );
      }

      hb_itemReturn( pArray ); /* ADel() returns the array itself */
   }
}

HB_FUNC( XHB_AINS )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      long lPos = hb_parnl( 2 );


      if( hb_parl( 4 ) )
      {
         ULONG ulLen = hb_arrayLen( pArray ) + 1;
         if( lPos == 0 )
            lPos = 1;
         else if( lPos < 0 )
            lPos += ulLen + 1;
         if( lPos >= 1 && ( ULONG ) lPos <= ulLen )
            hb_arraySize( pArray, ulLen );
      }
      else if( lPos == 0 )
         lPos = 1;
      else if( lPos < 0 )
         lPos += hb_arrayLen( pArray ) + 1;

      if( hb_arrayIns( pArray, lPos ) )
      {
         if( ! HB_ISNIL( 3 ) )
            hb_arraySet( pArray, lPos, hb_param( 3, HB_IT_ANY ) );
      }

      hb_itemReturn( pArray ); /* AIns() returns the array itself */
   }
}
