/*
 * Harbour Project source code:
 *    Xbase++ compatible messages used in overloaded scalar classes
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbstack.h"

/*
 * check if array/string index is in valid range, update it if necessary
 * in Xbase++ compatibility mode where negative indexes are used to access
 * data from tail
 */
#define XHB_IS_VALID_INDEX( idx, max )  ( ( ( HB_ISIZ ) ( idx ) < 0 ? ( idx ) += ( max ) + 1 : ( idx ) ) > 0 && ( HB_SIZE ) ( idx ) <= ( max ) )

HB_FUNC( XPP_INDEX )
{
   PHB_ITEM pSelf  = hb_stackSelfItem();
   PHB_ITEM pIndex = hb_param( 1, HB_IT_ANY );

   if( hb_pcount() == 2 ) /* ASSIGN */
   {
      PHB_ITEM pValue = hb_param( 2, HB_IT_ANY );
      if( HB_IS_NUMERIC( pIndex ) )
      {
         HB_SIZE nIndex = hb_itemGetNS( pIndex );
         if( HB_IS_ARRAY( pSelf ) )
         {
            HB_SIZE nLen = hb_arrayLen( pSelf );
            if( XHB_IS_VALID_INDEX( nIndex, nLen ) )
               hb_itemMoveRef( hb_arrayGetItemPtr( pSelf, nIndex ), pValue );
            else
               hb_errRT_BASE( EG_BOUND, 1012, "Error in array index", hb_langDGetErrorDesc( EG_ARRASSIGN ), 1, pIndex );
         }
         else if( HB_IS_STRING( pSelf ) )
         {
            HB_SIZE nLen = hb_itemGetCLen( pSelf );
            if( XHB_IS_VALID_INDEX( nIndex, nLen ) )
            {
               char cValue = HB_IS_STRING( pValue ) ? hb_itemGetCPtr( pValue )[ 0 ] :
                             ( char ) hb_itemGetNI( pValue );
               if( nLen == 1 )
                  hb_itemPutCL( pSelf, &cValue, 1 );
               else
               {
                  char * pszText;
                  if( hb_itemGetWriteCL( pSelf, &pszText, &nLen ) &&
                      nIndex < nLen )
                     pszText[ nIndex - 1 ] = cValue;
               }
            }
            else
               hb_errRT_BASE( EG_BOUND, 1012, "Error in array index", hb_langDGetErrorDesc( EG_ARRASSIGN ), 1, pIndex );
         }
         else
            hb_errRT_BASE( EG_ARG, 1069, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 1, pIndex );  /* TODO: Emulate exact XPP error msg */
      }
      else
         hb_errRT_BASE( EG_ARG, 1069, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 1, pIndex );  /* TODO: Emulate exact XPP error msg */

      hb_itemReturn( pSelf );
   }
   else /* ACCESS */
   {
      if( HB_IS_NUMERIC( pIndex ) )
      {
         HB_SIZE nIndex = hb_itemGetNS( pIndex );
         if( HB_IS_ARRAY( pSelf ) )
         {
            HB_SIZE nLen = hb_arrayLen( pSelf );
            if( XHB_IS_VALID_INDEX( nIndex, nLen ) )
               hb_itemReturn( hb_arrayGetItemPtr( pSelf, nIndex ) );
            else
               hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pSelf, pIndex );  /* TODO: Emulate exact XPP error msg */
         }
         else if( HB_IS_STRING( pSelf ) )
         {
            HB_SIZE nLen = hb_itemGetCLen( pSelf );
            if( XHB_IS_VALID_INDEX( nIndex, nLen ) )
               hb_retclen( hb_itemGetCPtr( pSelf ) + nIndex - 1, 1 );
            else
               hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pSelf, pIndex );  /* TODO: Emulate exact XPP error msg */
         }
         else
            hb_errRT_BASE( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pSelf, pIndex );  /* TODO: Emulate exact XPP error msg */
      }
      else
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pSelf, pIndex ); /* TODO: Emulate exact XPP error msg */
         if( pResult )
            hb_itemReturnRelease( pResult );
      }
   }
}

HB_FUNC( XPP_INCLUDE )
{
   PHB_ITEM pSelf = hb_stackSelfItem();
   PHB_ITEM pKey  = hb_param( 1, HB_IT_ANY );

   if( HB_IS_ARRAY( pSelf ) )
   {
      hb_retl( hb_arrayScan( pSelf, pKey, NULL, NULL, HB_FALSE ) != 0 );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1109, NULL, "$", 2, pKey, pSelf );
      if( pResult )
         hb_itemReturnRelease( pResult );
   }
}
