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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapierr.h"

/* ASplice( <aArray> [, <nPos>] [, <nCount>] [, <xVal1>] [, ...] [, <xValN>] ) --> <aDeleted>
 * Removes elements and return them as array, optionally add items
 */
HB_FUNC( ASPLICE )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      HB_SIZE  nStart, nRemove, nIndex;
      HB_SIZE  nLen    = hb_arrayLen( pArray );
      PHB_ITEM pReturn = hb_stackReturnItem();

      if( nLen == 0 )
      {
         hb_arrayNew( pReturn, 0 );
         return;
      }

      if( HB_ISNUM( 2 ) )
         nStart = hb_parns( 2 );
      else
         nStart = nLen + ( hb_pcount() > 3 && ! HB_ISNUM( 3 ) ? 1 : 0 );

      if( HB_ISNUM( 3 ) )
         nRemove = hb_parns( 3 );
      else
         nRemove = ( hb_pcount() > 3 && nStart == nLen + 1 ) ? 0 : 1;

      if( nStart == 0 || nStart > nLen )
      {
         if( ! ( nStart == nLen + 1 && hb_pcount() > 3 && nRemove == 0 ) )
         {
            hb_errRT_BASE( EG_ARG, 1003, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            return;
         }
      }

      if( nStart + nRemove - 1 > nLen )
         nRemove = nLen - nStart + 1;

      hb_arrayNew( pReturn, nRemove );

      /* 0 Based */
      nStart--;

      for( nIndex = nStart + 1; ( nIndex - nStart ) <= nRemove; nIndex++ )
         hb_itemMove( hb_arrayGetItemPtr( pReturn, nIndex - nStart ),
                      hb_arrayGetItemPtr( pArray, nIndex ) );

      if( hb_pcount() > 3 )
      {
         int nNew = 0;
         int nAdd = hb_pcount() - 3;

         if( ( HB_SIZE ) nAdd > nRemove )
         {
            HB_SIZE nMore  = nAdd - nRemove;
            HB_SIZE nShift = nLen - ( nStart + nRemove );

            hb_arraySize( pArray, nLen + nMore );

            /* Shift right BEFORE adding, so that new items will not override existing values. */
            for( nIndex = nLen; nIndex && --nShift; --nIndex )
               hb_itemMove( hb_arrayGetItemPtr( pArray, nIndex + nMore ),
                            hb_arrayGetItemPtr( pArray, nIndex ) );

            /* Now insert new values into emptied space. */
            for( nIndex = nStart; ++nNew <= nAdd; nIndex++ )
               hb_itemMove( hb_arrayGetItemPtr( pArray, nIndex + 1 ),
                            hb_param( 3 + nNew, HB_IT_ANY ) );
         }
         else
         {
            /* Insert over the space emptied by removed items */
            for( nIndex = nStart; ++nNew <= nAdd; nIndex++ )
               hb_itemMove( hb_arrayGetItemPtr( pArray, nIndex + 1 ), hb_param( 3 + nNew, HB_IT_ANY ) );

            if( nRemove > ( HB_SIZE ) nAdd )
            {
               nRemove -= nAdd;

               /* Shift left to compact the emptied hole. */
               for( nIndex = nStart + nAdd + 1; nIndex + nRemove <= nLen; nIndex++ )
                  hb_itemMove( hb_arrayGetItemPtr( pArray, nIndex ),
                               hb_arrayGetItemPtr( pArray, nIndex + nRemove ) );
            }
         }
      }
      else
      {
         for( nIndex = nStart + 1; nIndex + nRemove <= nLen; nIndex++ )
            hb_itemMove( hb_arrayGetItemPtr( pArray, nIndex ),
                         hb_arrayGetItemPtr( pArray, nIndex + nRemove ) );

         hb_arraySize( pArray, nLen - nRemove );
      }
   }
}

/* AMerge( <aTarget>, <aSource> [, <nPos>] ) => aTarget */
HB_FUNC( AMERGE )
{
   PHB_ITEM pArray1 = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pArray2 = hb_param( 2, HB_IT_ARRAY );

   if( pArray1 && pArray2 )
   {
      HB_SIZE nLen = hb_arrayLen( pArray1 );
      HB_SIZE nAdd = hb_arrayLen( pArray2 );
      HB_SIZE nIndex, nStart;

      hb_arraySize( pArray1, nLen + nAdd );

      if( HB_ISNUM( 3 ) )
      {
         nStart = hb_parns( 3 ) - 1;
         if( nStart > nLen )
         {
            hb_errRT_BASE( EG_ARG, 1003, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            return;
         }

         /* Shift right BEFORE merging, so that merged items will not override existing values. */
         for( nIndex = nLen; nIndex > nStart; --nIndex )
            hb_itemMove( hb_arrayGetItemPtr( pArray1, nIndex + nAdd ),
                         hb_arrayGetItemPtr( pArray1, nIndex ) );
      }
      else
         nStart = nLen;

      for( nIndex = 1; nIndex <= nAdd; nIndex++ )
         hb_itemCopy( hb_arrayGetItemPtr( pArray1, nStart + nIndex ),
                      hb_arrayGetItemPtr( pArray2, nIndex ) );

      hb_itemCopy( hb_stackReturnItem(), pArray1 );
   }
   else
      hb_errRT_BASE( EG_ARG, 1003, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( XHB_ADEL )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      HB_ISIZ nPos = hb_parns( 2 );

      if( nPos == 0 )
         nPos = 1;
      else if( nPos < 0 )
         nPos += hb_arrayLen( pArray ) + 1;

      if( hb_arrayDel( pArray, nPos ) )
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
      HB_ISIZ nPos = hb_parns( 2 );

      if( hb_parl( 4 ) )
      {
         HB_SIZE nLen = hb_arrayLen( pArray ) + 1;
         if( nPos == 0 )
            nPos = 1;
         else if( nPos < 0 )
            nPos += nLen + 1;
         if( nPos >= 1 && ( HB_SIZE ) nPos <= nLen )
            hb_arraySize( pArray, nLen );
      }
      else if( nPos == 0 )
         nPos = 1;
      else if( nPos < 0 )
         nPos += hb_arrayLen( pArray ) + 1;

      if( hb_arrayIns( pArray, nPos ) )
      {
         if( ! HB_ISNIL( 3 ) )
            hb_arraySet( pArray, nPos, hb_param( 3, HB_IT_ANY ) );
      }

      hb_itemReturn( pArray ); /* AIns() returns the array itself */
   }
}

HB_FUNC_TRANSLATE( RASCAN, HB_RASCAN )
