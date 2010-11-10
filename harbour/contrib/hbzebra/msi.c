/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Zebra barcode library
 *
 * Copyright 2010 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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

#include "hbzebra.h"
#include "hbapiitm.h"
#include "hbapierr.h"


static char _msi_checksum( const char * szCode )
{
   int   i, j = 1, k, sum = 0;

   /* Luhn algorithm */
   for ( i = ( int ) strlen( szCode ) - 1; i >= 0; i-- )
   {
      k = ( szCode[ i ] - '0' ) * ( j ? 2 : 1 );
      if( k > 9 )
         k -= 9;
      sum += k;
      j = 1 - j;
   }
   sum %= 10;
   return ( char ) ( '0' + ( sum ? 10 - sum : 0 ) );
}

PHB_ZEBRA hb_zebra_create_msi( const char * szCode, HB_SIZE nLen, int iFlags )
{
   PHB_ZEBRA  pZebra;
   int        i, j, iN, iW, iLen = ( int ) nLen;

   pZebra = hb_zebra_create();
   pZebra->iType = HB_ZEBRA_TYPE_MSI;

   for( i = 0; i < iLen; i++ )
   {
      if( szCode[ i ] < '0' || szCode[ i ] > '9' )
      {
         pZebra->iError = HB_ZEBRA_ERROR_INVALIDCODE;
         return pZebra;
      }
   }

   pZebra->szCode = ( char * ) hb_xgrab( iLen + 1 );
   hb_xmemcpy( pZebra->szCode, szCode, iLen );
   pZebra->szCode[ iLen ] = '\0';
   szCode = pZebra->szCode;

   pZebra->pBits = hb_bitbuffer_create();

   if( iFlags & HB_ZEBRA_FLAG_WIDE2_5 )
   {
      iN = 2;
      iW = 5;
   }
   else if( iFlags & HB_ZEBRA_FLAG_WIDE3 )
   {
      iN = 1;
      iW = 3;
   }
   else
   {
      iN = 1;
      iW = 2;
   }

   /* start */
   hb_bitbuffer_cat_int( pZebra->pBits, 31, iW );
   hb_bitbuffer_cat_int( pZebra->pBits,  0, iN );
   for( i = 0; i < iLen; i++ )
   {
      char code = szCode[ i ] - '0';
      for( j = 0; j < 4; j++ )
      {
         if( code & 8 )
         {
            hb_bitbuffer_cat_int( pZebra->pBits, 31, iW );
            hb_bitbuffer_cat_int( pZebra->pBits,  0, iN );
         }
         else
         {
            hb_bitbuffer_cat_int( pZebra->pBits, 3, iN );
            hb_bitbuffer_cat_int( pZebra->pBits, 0, iW );
         }
         code <<= 1;
      }
   }
   if( iFlags & HB_ZEBRA_FLAG_CHECKSUM )
   {
      char code = _msi_checksum( szCode );
      code -= '0';
      for( j = 0; j < 4; j++ )
      {
         if( code & 8 )
         {
            hb_bitbuffer_cat_int( pZebra->pBits, 31, iW );
            hb_bitbuffer_cat_int( pZebra->pBits,  0, iN );
         }
         else
         {
            hb_bitbuffer_cat_int( pZebra->pBits, 3, iN );
            hb_bitbuffer_cat_int( pZebra->pBits, 0, iW );
         }
         code <<= 1;
      }
   }
   /* stop */
   hb_bitbuffer_cat_int( pZebra->pBits, 3, iN );
   hb_bitbuffer_cat_int( pZebra->pBits, 0, iW );
   hb_bitbuffer_cat_int( pZebra->pBits, 3, iN );
   return pZebra;
}


HB_FUNC( HB_ZEBRA_CREATE_MSI )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );
   if( pItem )
   {
      hb_zebra_ret( hb_zebra_create_msi( hb_itemGetCPtr( pItem ), hb_itemGetCLen( pItem ), hb_parni( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
