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


/* we do not store L-code, but just to bit inversion R-code to obtain it */
static const char s_first[] = { 0x00, 0x34, 0x2C, 0x1C, 0x32, 0x26, 0x0E, 0x2A, 0x1A, 0x16 };
static const char s_rcode[] = { 0x27, 0x33, 0x1B, 0x21, 0x1D, 0x39, 0x05, 0x11, 0x09, 0x17 };
static const char s_gcode[] = { 0x72, 0x66, 0x6C, 0x42, 0x5C, 0x4E, 0x50, 0x44, 0x48, 0x74 };  /* reversion of R-code */


static char _ean13_checksum( const char * szCode )
{
   int   i, sum = 0;

   for ( i = 0; i < 12; i++ )
      sum += ( szCode[ i ] - '0' ) * ( i & 1 ? 3 : 1 );
   return '0' + ( 10000 - sum ) % 10;
}

static char _ean8_checksum( const char * szCode )
{
   int   i, sum = 0;

   for ( i = 0; i < 7; i++ )
      sum += ( szCode[ i ] - '0' ) * ( i & 1 ? 1 : 3 );
   return '0' + ( 10000 - sum ) % 10;
}

static char _upca_checksum( const char * szCode )
{
   int   i, sum = 0;

   for ( i = 0; i < 11; i++ )
      sum += ( szCode[ i ] - '0' ) * ( i & 1 ? 1 : 3 );
   return '0' + ( 10000 - sum ) % 10;
}

static char _upce_checksum( const char * szCode )
{
   char szExp[ 11 ];

   /* Expanding to UPC-A */
   szExp[ 0 ] = '0';
   szExp[ 1 ] = szCode[ 0 ];
   szExp[ 2 ] = szCode[ 1 ];
   if( szCode[ 5 ] == '0' || szCode[ 5 ] == '1' || szCode[ 5 ] == '2' )
   {
      szExp[ 3 ] = szCode[ 5 ];
      szExp[ 4 ] = szExp[ 5 ] = szExp[ 6 ] = szExp[ 7 ] = '0';
      szExp[ 8 ] = szCode[ 2 ];
      szExp[ 9 ] = szCode[ 3 ];
      szExp[ 10 ] = szCode[ 4 ];
   }
   else if( szCode[ 5 ] == '3' )
   {
      szExp[ 3 ] = szCode[ 2 ];
      szExp[ 4 ] = szExp[ 5 ] = szExp[ 6 ] = szExp[ 7 ] = szExp[ 8 ] = '0';
      szExp[ 9 ] = szCode[ 3 ];
      szExp[ 10 ] = szCode[ 4 ];
   }
   else if( szCode[ 5 ] == '4' )
   {
      szExp[ 3 ] = szCode[ 2 ];
      szExp[ 4 ] = szCode[ 3 ];
      szExp[ 5 ] = szExp[ 6 ] = szExp[ 7 ] = szExp[ 8 ] = szExp[ 9 ] = '0';
      szExp[ 10 ] = szCode[ 4 ];
   }
   else
   {
      szExp[ 3 ] = szCode[ 2 ];
      szExp[ 4 ] = szCode[ 3 ];
      szExp[ 5 ] = szCode[ 4 ];
      szExp[ 6 ] = szExp[ 7 ] = szExp[ 8 ] = szExp[ 9 ] = '0';
      szExp[ 10 ] = szCode[ 5 ];
   }
   return _upca_checksum( szExp );
}

PHB_ZEBRA hb_zebra_create_ean13( const char * szCode, HB_SIZE nLen, int iFlags )
{
   PHB_ZEBRA  pZebra;
   int        i, iLen = ( int ) nLen;

   HB_SYMBOL_UNUSED( iFlags );

   pZebra = hb_zebra_create();
   pZebra->iType = HB_ZEBRA_TYPE_EAN13;

   if( iLen != 12 && iLen != 13 )
   {
      pZebra->iError = HB_ZEBRA_ERROR_INVALIDCODE;
      return pZebra;
   }

   for( i = 0; i < iLen; i++ )
   {
      if( szCode[ i ] < '0' || szCode[ i ] > '9' )
      {
         pZebra->iError = HB_ZEBRA_ERROR_INVALIDCODE;
         return pZebra;
      }
   }

   if( iLen == 12 )
   {
      pZebra->szCode = ( char * ) hb_xgrab( 14 );
      hb_xmemcpy( pZebra->szCode, szCode, 12 );
      pZebra->szCode[ 12 ] = _ean13_checksum( szCode );
      pZebra->szCode[ 13 ] = '\0';
   }
   else
   {
      if( szCode[ 12 ] != _ean13_checksum( szCode ) )
      {
         pZebra->iError = HB_ZEBRA_ERROR_BADCHECKSUM;
         return pZebra;
      }
      pZebra->szCode = ( char * ) hb_xgrab( 14 );
      hb_xmemcpy( pZebra->szCode, szCode, 13 );
      pZebra->szCode[ 13 ] = '\0';
   }
   szCode = pZebra->szCode;

   pZebra->pBits = hb_bitbuffer_create();

   hb_bitbuffer_cat_int( pZebra->pBits, 5, 3 );   /* start */

   for( i = 1; i <= 6; i++ )
   {
      if( s_first[ szCode[ 0 ] - '0' ] & ( 1 << ( i - 1 ) ) )
         hb_bitbuffer_cat_int( pZebra->pBits, s_gcode[ szCode[ i ] - '0' ], 7 );
      else
         hb_bitbuffer_cat_int( pZebra->pBits, s_rcode[ szCode[ i ] - '0' ] ^ 0x7F, 7 );
   }

   hb_bitbuffer_cat_int( pZebra->pBits, 10, 5 );  /* middle */

   for( i = 7; i <= 12; i++ )
      hb_bitbuffer_cat_int( pZebra->pBits, s_rcode[ szCode[ i ] - '0' ], 7 );

   hb_bitbuffer_cat_int( pZebra->pBits, 5, 3 );   /* stop */

   return pZebra;
}


PHB_ZEBRA hb_zebra_create_ean8( const char * szCode, HB_SIZE nLen, int iFlags )
{
   PHB_ZEBRA  pZebra;
   int        i, iLen = ( int ) nLen;

   HB_SYMBOL_UNUSED( iFlags );

   pZebra = hb_zebra_create();
   pZebra->iType = HB_ZEBRA_TYPE_EAN8;

   if( iLen != 7 && iLen != 8 )
   {
      pZebra->iError = HB_ZEBRA_ERROR_INVALIDCODE;
      return pZebra;
   }

   for( i = 0; i < iLen; i++ )
   {
      if( szCode[ i ] < '0' || szCode[ i ] > '9' )
      {
         pZebra->iError = HB_ZEBRA_ERROR_INVALIDCODE;
         return pZebra;
      }
   }

   if( iLen == 7 )
   {
      pZebra->szCode = ( char * ) hb_xgrab( 9 );
      hb_xmemcpy( pZebra->szCode, szCode, 7 );
      pZebra->szCode[ 7 ] = _ean8_checksum( szCode );
      pZebra->szCode[ 8 ] = '\0';
   }
   else
   {
      if( szCode[ 7 ] != _ean8_checksum( szCode ) )
      {
         pZebra->iError = HB_ZEBRA_ERROR_BADCHECKSUM;
         return pZebra;
      }
      pZebra->szCode = ( char * ) hb_xgrab( 9 );
      hb_xmemcpy( pZebra->szCode, szCode, 8 );
      pZebra->szCode[ 8 ] = '\0';
   }
   szCode = pZebra->szCode;

   pZebra->pBits = hb_bitbuffer_create();

   hb_bitbuffer_cat_int( pZebra->pBits, 5, 3 );   /* start */

   for( i = 0; i < 4; i++ )
      hb_bitbuffer_cat_int( pZebra->pBits, s_rcode[ szCode[ i ] - '0' ] ^ 0x7F, 7 );

   hb_bitbuffer_cat_int( pZebra->pBits, 10, 5 );  /* middle */

   for( i = 4; i < 8; i++ )
      hb_bitbuffer_cat_int( pZebra->pBits, s_rcode[ szCode[ i ] - '0' ], 7 );

   hb_bitbuffer_cat_int( pZebra->pBits, 5, 3 );   /* stop */
   return pZebra;
}


PHB_ZEBRA hb_zebra_create_upca( const char * szCode, HB_SIZE nLen, int iFlags )
{
   PHB_ZEBRA  pZebra;
   int        i, iLen = ( int ) nLen;

   HB_SYMBOL_UNUSED( iFlags );

   pZebra = hb_zebra_create();
   pZebra->iType = HB_ZEBRA_TYPE_UPCA;

   if( iLen != 11 && iLen != 12 )
   {
      pZebra->iError = HB_ZEBRA_ERROR_INVALIDCODE;
      return pZebra;
   }

   for( i = 0; i < iLen; i++ )
   {
      if( szCode[ i ] < '0' || szCode[ i ] > '9' )
      {
         pZebra->iError = HB_ZEBRA_ERROR_INVALIDCODE;
         return pZebra;
      }
   }

   if( iLen == 11 )
   {
      pZebra->szCode = ( char * ) hb_xgrab( 13 );
      hb_xmemcpy( pZebra->szCode, szCode, 11 );
      pZebra->szCode[ 11 ] = _upca_checksum( szCode );
      pZebra->szCode[ 12 ] = '\0';
   }
   else
   {
      if( szCode[ 11 ] != _upca_checksum( szCode ) )
      {
         pZebra->iError = HB_ZEBRA_ERROR_BADCHECKSUM;
         return pZebra;
      }
      pZebra->szCode = ( char * ) hb_xgrab( 13 );
      hb_xmemcpy( pZebra->szCode, szCode, 12 );
      pZebra->szCode[ 12 ] = '\0';
   }
   szCode = pZebra->szCode;

   pZebra->pBits = hb_bitbuffer_create();

   hb_bitbuffer_cat_int( pZebra->pBits, 5, 3 );   /* start */

   for( i = 0; i < 6; i++ )
      hb_bitbuffer_cat_int( pZebra->pBits, s_rcode[ szCode[ i ] - '0' ] ^ 0x7F, 7 );

   hb_bitbuffer_cat_int( pZebra->pBits, 10, 5 );  /* middle */

   for( i = 6; i < 12; i++ )
      hb_bitbuffer_cat_int( pZebra->pBits, s_rcode[ szCode[ i ] - '0' ], 7 );

   hb_bitbuffer_cat_int( pZebra->pBits, 5, 3 );   /* stop */
   return pZebra;
}

PHB_ZEBRA hb_zebra_create_upce( const char * szCode, HB_SIZE nLen, int iFlags )
{
   PHB_ZEBRA  pZebra;
   int        i, iLen = ( int ) nLen;
   char       sumcode;

   HB_SYMBOL_UNUSED( iFlags );

   pZebra = hb_zebra_create();
   pZebra->iType = HB_ZEBRA_TYPE_UPCE;

   if( iLen != 6 && iLen != 7 )
   {
      pZebra->iError = HB_ZEBRA_ERROR_INVALIDCODE;
      return pZebra;
   }

   for( i = 0; i < iLen; i++ )
   {
      if( szCode[ i ] < '0' || szCode[ i ] > '9' )
      {
         pZebra->iError = HB_ZEBRA_ERROR_INVALIDCODE;
         return pZebra;
      }
   }

   if( iLen == 6 )
   {
      pZebra->szCode = ( char * ) hb_xgrab( 8 );
      hb_xmemcpy( pZebra->szCode, szCode, 6 );
      pZebra->szCode[ 6 ] = _upce_checksum( szCode );
      pZebra->szCode[ 7 ] = '\0';
   }
   else
   {
      if( szCode[ 6 ] != _upce_checksum( szCode ) )
      {
         pZebra->iError = HB_ZEBRA_ERROR_BADCHECKSUM;
         return pZebra;
      }
      pZebra->szCode = ( char * ) hb_xgrab( 8 );
      hb_xmemcpy( pZebra->szCode, szCode, 7 );
      pZebra->szCode[ 7 ] = '\0';
   }
   szCode = pZebra->szCode;

   sumcode = szCode[ 6 ] == '0' ? 0x38 : s_first[ szCode[ 6 ] - '0' ];
   pZebra->pBits = hb_bitbuffer_create();

   hb_bitbuffer_cat_int( pZebra->pBits, 5, 3 );   /* start */

   for( i = 0; i < 6; i++ )
   {
      if( sumcode & ( 1 << i ) )
         hb_bitbuffer_cat_int( pZebra->pBits, s_rcode[ szCode[ i ] - '0' ] ^ 0x7F, 7 );
      else
         hb_bitbuffer_cat_int( pZebra->pBits, s_gcode[ szCode[ i ] - '0' ], 7 );
   }
   hb_bitbuffer_cat_int( pZebra->pBits, 42, 6 );   /* stop */
   return pZebra;
}


HB_FUNC( HB_ZEBRA_CREATE_EAN13 )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );
   if( pItem )
   {
      hb_zebra_ret( hb_zebra_create_ean13( hb_itemGetCPtr( pItem ), hb_itemGetCLen( pItem ), hb_parni( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_ZEBRA_CREATE_EAN8 )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );
   if( pItem )
   {
      hb_zebra_ret( hb_zebra_create_ean8( hb_itemGetCPtr( pItem ), hb_itemGetCLen( pItem ), hb_parni( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_ZEBRA_CREATE_UPCA )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );
   if( pItem )
   {
      hb_zebra_ret( hb_zebra_create_upca( hb_itemGetCPtr( pItem ), hb_itemGetCLen( pItem ), hb_parni( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_ZEBRA_CREATE_UPCE )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );
   if( pItem )
   {
      hb_zebra_ret( hb_zebra_create_upce( hb_itemGetCPtr( pItem ), hb_itemGetCLen( pItem ), hb_parni( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
