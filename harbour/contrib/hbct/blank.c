/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 BLANK function
 *
 * Copyright 2009 Pavel Tsarenko <tpe2@mail.ru>
 * http://www.xharbour.org
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

#include "hbapi.h"
#include "hbapiitm.h"

#include "ct.h"

HB_FUNC( BLANK )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );
   HB_BOOL bRef = HB_ISBYREF( 1 );
   HB_BOOL bRet = ! ct_getref();

   if( ! pItem )
   {
      if( bRet )
         hb_retl( HB_FALSE );
   }
   else if( HB_IS_TIMESTAMP( pItem ) )
   {
      if( bRef )
         hb_stortdt( 0, 0, 1 );
      if( bRet )
         hb_rettdt( 0, 0 );
   }
   else if( HB_IS_DATE( pItem ) )
   {
      if( bRef )
         hb_stordl( 0, 1 );
      if( bRet )
         hb_retdl( 0 );
   }
   else if( HB_IS_NUMBER( pItem ) )
   {
      if( bRef )
         hb_stornl( 0, 1 );
      if( bRet )
         hb_retnl( 0 );
   }
   else if( HB_IS_STRING( pItem ) )
   {
      PHB_ITEM pMode = hb_param( 2, HB_IT_LOGICAL );

      if( pMode && hb_itemGetL( pMode ) )
      {
         HB_SIZE nLen = hb_itemGetCLen( pItem );
         char * szResult = ( char * ) hb_xgrab( nLen + 1 );

         hb_xmemset( szResult, ' ', nLen );
         if( bRef )
            hb_storclen( szResult, nLen, 1 );
         if( bRet )
            hb_retclen_buffer( szResult, nLen );
         else
            hb_xfree( szResult );
      }
      else
      {
         if( bRef )
            hb_storc( NULL, 1 );
         if( bRet )
            hb_retc_null();
      }
   }
   else if( HB_IS_ARRAY( pItem ) )
   {
      if( bRef )
         hb_arraySize( pItem, 0 );
      if( bRet )
         hb_reta( 0 );
   }
   else if( HB_IS_LOGICAL( pItem ) )
   {
      if( bRef )
         hb_storl( HB_FALSE, 1 );
      if( bRet )
         hb_retl( HB_FALSE );
   }
   else
   {
      if( bRet )
         hb_retl( HB_FALSE );
   }
   if( ! bRet )
      hb_ret();
}
