/*
 * Harbour Project source code:
 * Bin2W(), Bin2I(), Bin2L(), I2Bin(), L2Bin() functions
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * Copyright 1999 Manuel Ruiz <mrt@joca.es>
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

HB_FUNC( BIN2W )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );
   HB_U16 uiResult = 0;

   if( pItem )
   {
      HB_SIZE nLen = hb_itemGetCLen( pItem );
      if( nLen )
      {
         const char * pszString = hb_itemGetCPtr( pItem );
         uiResult = HB_GET_LE_INT16( pszString );
      }
   }
   hb_retnint( uiResult );
}

HB_FUNC( BIN2I )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );
   HB_I16 iResult = 0;

   if( pItem )
   {
      HB_SIZE nLen = hb_itemGetCLen( pItem );
      if( nLen )
      {
         const char * pszString = hb_itemGetCPtr( pItem );
         iResult = HB_GET_LE_UINT16( pszString );
      }
   }
   hb_retnint( iResult );
}

HB_FUNC( BIN2L )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );
   HB_I32 iResult = 0;

   if( pItem )
   {
      HB_SIZE nLen = hb_itemGetCLen( pItem );
      if( nLen )
      {
         const char * pszString = hb_itemGetCPtr( pItem );
         if( nLen >= 3 )
            iResult = HB_GET_LE_INT32( pszString );
         else
            iResult = HB_GET_LE_UINT16( pszString );
      }
   }
   hb_retnint( iResult );
}

HB_FUNC( I2BIN )
{
   char szResult[ 2 ];
   HB_I16 iValue = ( HB_I16 ) hb_parni( 1 );

   HB_PUT_LE_UINT16( szResult, iValue );
   hb_retclen( szResult, 2 );
}

HB_FUNC( L2BIN )
{
   char szResult[ 4 ];
   HB_I32 iValue = ( HB_I32 ) hb_parnl( 1 );

   HB_PUT_LE_UINT32( szResult, iValue );
   hb_retclen( szResult, 4 );
}
