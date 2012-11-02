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
   0x28,  /* 0 */
   0x12,  /* 1 */
   0x22,  /* 2 */
   0x42,  /* 3 */
   0x14,  /* 4 */
   0x24,  /* 5 */
   0x44,  /* 6 */
   0x0A,  /* 7 */
   0x48,  /* 8 */
   0x50,  /* 9 */
   0x15,  /* A 10 */
   0x25,  /* B 11 */
   0x45,  /* C 12 */
   0x29,  /* D 13 */
   0x49,  /* E 14 */
   0x51,  /* F 15 */
   0x16,  /* G 16 */
   0x26,  /* H 17 */
   0x46,  /* I 18 */
   0x2C,  /* J 19 */
   0x58,  /* K 20 */
   0x1A,  /* L 21 */
   0x32,  /* M 22 */
   0x62,  /* N 23 */
   0x34,  /* O 24 */
   0x68,  /* P 25 */
   0x2D,  /* Q 26 */
   0x4D,  /* R 27 */
   0x35,  /* S 28 */
   0x65,  /* T 29 */
   0x69,  /* U 30 */
   0x59,  /* V 31 */
   0x36,  /* W 32 */
   0x66,  /* X 33 */
   0x6C,  /* Y 34 */
   0x5C,  /* Z 35 */
   0x74,  /* - 36 */
   0x2B,  /* . 37 */
   0x4B,  /*   38 */
   0x53,  /* $ 39 */
   0x76,  /* / 40 */
   0x6E,  /* + 41 */
   0x75,  /* % 42 */
   0x64,  /* ($) 43 */
   0x5B,  /* (%) 44 */
   0x6B,  /* (/) 45 */
   0x4C,  /* (+) 46 */
   0x7A   /* Start/Stop 47 */
};

static int _code93_charno( char ch )
{
   static const char * s_symbols = "-. $/+%";

   if( '0' <= ch && ch <= '9' )
      return ch - '0';
   else if( 'A' <= ch && ch <= 'Z' )
      return ch - 'A' + 10;
   else
   {
      const char * ptr = strchr( s_symbols, ch );
      if( ptr && *ptr )
         return ( int ) ( ptr - s_symbols + 36 );
   }
   return -1;
}

PHB_ZEBRA hb_zebra_create_code93( const char * szCode, HB_SIZE nLen, int iFlags )
{
   PHB_ZEBRA  pZebra;
   int        k, i, j, iLen = ( int ) nLen;
   int        csum, ksum;

   HB_SYMBOL_UNUSED( iFlags );

   pZebra = hb_zebra_create();
   pZebra->iType = HB_ZEBRA_TYPE_CODE93;

   j = 0;
   k = 0;
   for( i = 0; i < iLen; i++ )
   {
      if( ( unsigned char ) szCode[ i ] >= 128 )
      {
         pZebra->iError = HB_ZEBRA_ERROR_INVALIDCODE;
         return pZebra;
      }
      if( szCode[ i ] >= ' ' && szCode[ i ] <= 126 )
         j++;

      k += _code93_charno( szCode[ i ] ) >= 0 ? 1 : 2;
   }

   pZebra->szCode = ( char * ) hb_xgrab( j + 1 );
   j = 0;
   for( i = 0; i < iLen; i++ )
   {
      if( szCode[ i ] >= 32 && szCode[ i ] <= 126 )
         pZebra->szCode[ j++ ] = szCode[ i ];
   }
   pZebra->szCode[ j ] = '\0';

   pZebra->pBits = hb_bitbuffer_create();

   /* start */
   hb_bitbuffer_cat_int( pZebra->pBits, 1, 1 );
   hb_bitbuffer_cat_int( pZebra->pBits, s_code[ 47 ], 7 );
   hb_bitbuffer_cat_int( pZebra->pBits, 0, 1 );

   csum = 0;
   ksum = 0;
   k++;
   for( i = 0; i < iLen; i++ )
   {
      int no = _code93_charno( szCode[ i ] );
      if( no >= 0 )
      {
         hb_bitbuffer_cat_int( pZebra->pBits, 1, 1 );
         hb_bitbuffer_cat_int( pZebra->pBits, s_code[ no ], 7 );
         hb_bitbuffer_cat_int( pZebra->pBits, 0, 1 );
         ksum += ( k % 15 ? k % 15 : 15 ) * no;  k--;
         csum += ( k % 20 ? k % 20 : 20 ) * no;
      }
      else
      {
         int no1 = 0, no2 = 0;
         if( szCode[ i ] >= 1 && szCode[ i ] <= 26 )
         {
            no1 = 43; /* ($) */
            no2 = szCode[ i ] - 1 + 10;
         }
         else if( szCode[ i ] >= '!' && szCode[ i ] <= ':' )
         {
            no1 = 45; /* (/) */
            no2 = szCode[ i ] - '!' + 10;
         }
         else if( szCode[ i ] >= 'a' && szCode[ i ] <= 'z' )
         {
            no1 = 46; /* (+) */
            no2 = szCode[ i ] - 'a' + 10;
         }
         else if( szCode[ i ] >= 27 && szCode[ i ] <= 31 )
         {
            no1 = 44; /* (%) */
            no2 = szCode[ i ] - 27 + 10;
         }
         else if( szCode[ i ] >= '[' && szCode[ i ] <= '_' )
         {
            no1 = 44; /* (%) */
            no2 = szCode[ i ] - '[' + 15;
         }
         else if( szCode[ i ] >= '{' && ( unsigned char ) szCode[ i ] <= 127 )
         {
            no1 = 44; /* (%) */
            no2 = szCode[ i ] - '{' + 20;
         }
         else if( szCode[ i ] == '\0' )
         {
            no1 = 44; /* (%) */
            no2 = 30; /* U */
         }
         else if( szCode[ i ] == '@' )
         {
            no1 = 44; /* (%) */
            no2 = 31; /* V */
         }
         else if( szCode[ i ] == '`' )
         {
            no1 = 44; /* (%) */
            no2 = 32; /* W */
         }
         hb_bitbuffer_cat_int( pZebra->pBits, 1, 1 );
         hb_bitbuffer_cat_int( pZebra->pBits, s_code[ no1 ], 7 );
         hb_bitbuffer_cat_int( pZebra->pBits, 0, 1 );
         hb_bitbuffer_cat_int( pZebra->pBits, 1, 1 );
         hb_bitbuffer_cat_int( pZebra->pBits, s_code[ no2 ], 7 );
         hb_bitbuffer_cat_int( pZebra->pBits, 0, 1 );
         ksum += ( k % 15 ? k % 15 : 15 ) * no1;  k--;
         csum += ( k % 20 ? k % 20 : 20 ) * no1;
         ksum += ( k % 15 ? k % 15 : 15 ) * no2;  k--;
         csum += ( k % 20 ? k % 20 : 20 ) * no2;
      }
   }

   /* checksum */
   hb_bitbuffer_cat_int( pZebra->pBits, 1, 1 );
   hb_bitbuffer_cat_int( pZebra->pBits, s_code[ csum % 47 ], 7 );
   hb_bitbuffer_cat_int( pZebra->pBits, 0, 1 );
   ksum += csum % 47;
   hb_bitbuffer_cat_int( pZebra->pBits, 1, 1 );
   hb_bitbuffer_cat_int( pZebra->pBits, s_code[ ksum % 47 ], 7 );
   hb_bitbuffer_cat_int( pZebra->pBits, 0, 1 );

   /* stop */
   hb_bitbuffer_cat_int( pZebra->pBits, 1, 1 );
   hb_bitbuffer_cat_int( pZebra->pBits, s_code[ 47 ], 7 );
   hb_bitbuffer_cat_int( pZebra->pBits, 0, 1 );
   hb_bitbuffer_cat_int( pZebra->pBits, 1, 1 );

   return pZebra;
}


HB_FUNC( HB_ZEBRA_CREATE_CODE93 )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );

   if( pItem )
   {
      hb_zebra_ret( hb_zebra_create_code93( hb_itemGetCPtr( pItem ), hb_itemGetCLen( pItem ), hb_parni( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
