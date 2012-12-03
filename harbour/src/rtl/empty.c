/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * EMPTY() function
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

HB_FUNC( EMPTY )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );
   long lDate, lTime;
   PHB_SYMB pSym;

   switch( hb_itemType( pItem ) )
   {
      case HB_IT_ARRAY:
         hb_retl( hb_arrayLen( pItem ) == 0 );
         break;

      case HB_IT_HASH:
         hb_retl( hb_hashLen( pItem ) == 0 );
         break;

      case HB_IT_STRING:
      case HB_IT_MEMO:
         hb_retl( hb_strEmpty( hb_itemGetCPtr( pItem ), hb_itemGetCLen( pItem ) ) );
         break;

      case HB_IT_INTEGER:
         hb_retl( hb_itemGetNI( pItem ) == 0 );
         break;

      case HB_IT_LONG:
         hb_retl( hb_itemGetNInt( pItem ) == 0 );
         break;

      case HB_IT_DOUBLE:
         hb_retl( hb_itemGetND( pItem ) == 0.0 );
         break;

      case HB_IT_DATE:
         hb_retl( hb_itemGetDL( pItem ) == 0 );
         break;

      case HB_IT_TIMESTAMP:
         hb_itemGetTDT( pItem, &lDate, &lTime );
         hb_retl( lDate == 0 && lTime == 0 );
         break;

      case HB_IT_LOGICAL:
         hb_retl( ! hb_itemGetL( pItem ) );
         break;

      case HB_IT_BLOCK:
         hb_retl( HB_FALSE );
         break;

      case HB_IT_POINTER:
         hb_retl( hb_itemGetPtr( pItem ) == NULL );
         break;

      case HB_IT_SYMBOL:
         pSym = hb_itemGetSymbol( pItem );
         if( pSym && ( pSym->scope.value & HB_FS_DEFERRED ) && \
             pSym->pDynSym )
            pSym = hb_dynsymSymbol( pSym->pDynSym );
         hb_retl( pSym == NULL || pSym->value.pFunPtr == NULL );
         break;

      default:
         hb_retl( HB_TRUE );
         break;
   }
}
