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

static const char s_code[] = {
   0x10,  /* 0 */
   0x11,  /* 1 */
   0x12,  /* 2 */
   0x03,  /* 3 */
   0x14,  /* 4 */
   0x05,  /* 5 */
   0x06,  /* 6 */
   0x18,  /* 7 */
   0x09,  /* 8 */
   0x01,  /* 9 */
   0x04,  /* - 10 */
   0x0C};  /* Start/Stop 11 */

static int _code11_charno( char ch )
{
   if( '0' <= ch && ch <= '9' )
      return ch - '0';
   else if( ch == '-' )
      return 10;
   return -1;
}

static void _code11_add( PHB_BITBUFFER pBits, char code, int iFlags, HB_BOOL fLast )
{
   int i;

   if( iFlags & HB_ZEBRA_FLAG_WIDE2_5 )
   {
      for( i = 0; i < 5; i++ )
      {
         hb_bitbuffer_cat_int( pBits, i & 1 ? 0 : 31, code & 1 ? 5 : 2 );
         code >>= 1;
      }
      if( ! fLast )
         hb_bitbuffer_cat_int( pBits, 0, 2 );
   }
   else if( iFlags & HB_ZEBRA_FLAG_WIDE3 )
   {
      for( i = 0; i < 5; i++ )
      {
         hb_bitbuffer_cat_int( pBits, i & 1 ? 0 : 31, code & 1 ? 3 : 1 );
         code >>= 1;
      }
      if( ! fLast )
         hb_bitbuffer_cat_int( pBits, 0, 1 );
   }
   else
   {
      for( i = 0; i < 5; i++ )
      {
         hb_bitbuffer_cat_int( pBits, i & 1 ? 0 : 31, code & 1 ? 2 : 1 );
         code >>= 1;
      }
      if( ! fLast )
         hb_bitbuffer_cat_int( pBits, 0, 1 );
   }
}

PHB_ZEBRA hb_zebra_create_code11( const char * szCode, HB_SIZE nLen, int iFlags )
{
   PHB_ZEBRA  pZebra;
   int        csum, ksum, i, iLen = ( int ) nLen;

   pZebra = hb_zebra_create();
   pZebra->iType = HB_ZEBRA_TYPE_CODE11;

   for( i = 0; i < iLen; i++ )
   {
      if( _code11_charno( szCode[ i ] ) < 0 )
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

   _code11_add( pZebra->pBits, s_code[ 11 ], iFlags, HB_FALSE );  /* start */

   csum = ksum = 0;
   for( i = 0; i < iLen; i++ )
   {
      int no = _code11_charno( szCode[ i ] );
      _code11_add( pZebra->pBits, s_code[ no ], iFlags, HB_FALSE );
      ksum += ( ( iLen + 1 - i ) % 9 ? ( iLen + 1 - i ) % 9 : 9 ) * no;
      csum += ( ( iLen - i ) % 10 ? ( iLen - i ) % 10 : 10 ) * no;
   }

   /* checksum */
   _code11_add( pZebra->pBits, s_code[ csum % 11 ], iFlags, HB_FALSE );
   if( iFlags & HB_ZEBRA_FLAG_CHECKSUM )
   {
      ksum += csum % 11;
      _code11_add( pZebra->pBits, s_code[ ksum % 11 ], iFlags, HB_FALSE );
   }

   _code11_add( pZebra->pBits, s_code[ 11 ], iFlags, HB_TRUE );  /* stop */

   return pZebra;
}


HB_FUNC( HB_ZEBRA_CREATE_CODE11 )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );
   if( pItem )
   {
      hb_zebra_ret( hb_zebra_create_code11( hb_itemGetCPtr( pItem ), hb_itemGetCLen( pItem ), hb_parni( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

