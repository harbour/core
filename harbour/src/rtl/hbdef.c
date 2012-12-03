/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_DEFAULT() and __DEFAULTNIL() functions
 *
 * Copyright 2012 Viktor Szakats (harbour syenar.net)
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

typedef enum
{
   HB_IT_U,
   HB_IT_N,
   HB_IT_C,
   HB_IT_L,
   HB_IT_T,
   HB_IT_B,
   HB_IT_H,
   HB_IT_A,
   HB_IT_O,
   HB_IT_P,
   HB_IT_S
} HB_IT_BASIC;

static HB_IT_BASIC s_hb_itemTypeBasic( PHB_ITEM pItem )
{
   switch( HB_ITEM_TYPE( pItem ) )
   {
      case HB_IT_ARRAY:
         return hb_arrayIsObject( pItem ) ? HB_IT_O : HB_IT_A;

      case HB_IT_BLOCK:
         return HB_IT_B;

      case HB_IT_DATE:
      case HB_IT_TIMESTAMP:
         return HB_IT_T;

      case HB_IT_LOGICAL:
         return HB_IT_L;

      case HB_IT_INTEGER:
      case HB_IT_LONG:
      case HB_IT_DOUBLE:
         return HB_IT_N;

      case HB_IT_STRING:
      case HB_IT_MEMO:
         return HB_IT_C;

      case HB_IT_HASH:
         return HB_IT_H;

      case HB_IT_POINTER:
         return HB_IT_P;

      case HB_IT_SYMBOL:
         return HB_IT_S;
   }

   return HB_IT_U;
}

HB_FUNC( HB_DEFAULT )
{
   PHB_ITEM pDefault = hb_param( 2, HB_IT_ANY );

   if( pDefault &&
       s_hb_itemTypeBasic( hb_param( 1, HB_IT_ANY ) ) !=
       s_hb_itemTypeBasic( pDefault ) )
      hb_itemParamStore( 1, pDefault );
}

/* For compatibility with legacy DEFAULT ... TO ... command.
   Not recommended for new code. */
HB_FUNC( __DEFAULTNIL )
{
   if( hb_pcount() >= 2 && HB_IS_NIL( hb_param( 1, HB_IT_ANY ) ) )
      hb_itemParamStore( 1, hb_param( 2, HB_IT_ANY ) );
}
