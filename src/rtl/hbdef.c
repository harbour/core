/*
 * hb_default() and __defaultNIL() functions
 *
 * Copyright 2012 Viktor Szakats (vszakats.net/harbour)
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

HB_FUNC( HB_DEFAULT )
{
   PHB_ITEM pDefault = hb_param( 2, HB_IT_ANY );

   if( pDefault && ! hb_itemTypeCmp( hb_param( 1, HB_IT_ANY ), pDefault ) )
      hb_itemParamStore( 1, pDefault );
}

HB_FUNC( HB_DEFAULTVALUE )
{
   PHB_ITEM pParam = hb_param( 1, HB_IT_ANY );
   PHB_ITEM pDefault = hb_param( 2, HB_IT_ANY );

   if( pDefault && ! hb_itemTypeCmp( pParam, pDefault ) )
      pParam = pDefault;

   hb_itemReturn( pParam );
}

/* For compatibility with legacy DEFAULT ... TO ... command.
   Not recommended for new code. */
HB_FUNC( __DEFAULTNIL )
{
   if( hb_pcount() >= 2 && HB_IS_NIL( hb_param( 1, HB_IT_ANY ) ) )
      hb_itemParamStore( 1, hb_param( 2, HB_IT_ANY ) );
}
