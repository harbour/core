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


/* Usually one character bitmap does not fit into 1 byte, but if we use enough
   good encoding, we can manage to fit :) [Mindaugas] */
static const HB_UCHAR s_code[] =
{
   0x58,   /* 0 */
   0x09,   /* 1 */
   0x0C,   /* 2 */
   0x0D,   /* 3 */
   0x18,   /* 4 */
   0x19,   /* 5 */
   0x1C,   /* 6 */
   0x48,   /* 7 */
   0x49,   /* 8 */
   0x4C,   /* 9 */
   0x21,   /* A */
   0x24,   /* B */
   0x25,   /* C */
   0x30,   /* D */
   0x31,   /* E */
   0x34,   /* F */
   0x60,   /* G */
   0x61,   /* H */
   0x64,   /* I */
   0x70,   /* J */
   0x81,   /* K */
   0x84,   /* L */
   0x85,   /* M */
   0x90,   /* N */
   0x91,   /* O */
   0x94,   /* P */
   0xC0,   /* Q */
   0xC1,   /* R */
   0xC4,   /* S */
   0xD0,   /* T */
   0x03,   /* U */
   0x06,   /* V */
   0x07,   /* W */
   0x12,   /* X */
   0x13,   /* Y */
   0x16,   /* Z */
   0x42,   /* - */
   0x43,   /* . */
   0x46,   /*   */
   0x2A,   /* $ */
   0x8A,   /* / */
   0xA2,   /* + */
   0xA8 }; /* % */

static int _code39_charno( char ch )
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
         return ptr - s_symbols + 36;
   }
   return -1;
}

static void _code39_add( PHB_BITBUFFER pBits, char code, int iFlags, HB_BOOL fLast )
{
   int i, cnt = 0;

   if( iFlags & HB_ZEBRA_FLAG_WIDE2_5 )
   {
      for( i = 0; i < 8; i++ )
      {
         hb_bitbuffer_cat_int( pBits, i & 1 ? 0 : 31, code & 1 ? 5 : 2 );
         cnt += ( code & 1 );
         code >>= 1;
      }
      hb_bitbuffer_cat_int( pBits, 31, cnt < 3 ? 5 : 2 );
      if( ! fLast )
         hb_bitbuffer_cat_int( pBits, 0, 2 );
   }
   else if( iFlags & HB_ZEBRA_FLAG_WIDE3 )
   {
      for( i = 0; i < 8; i++ )
      {
         hb_bitbuffer_cat_int( pBits, i & 1 ? 0 : 31, code & 1 ? 3 : 1 );
         cnt += ( code & 1 );
         code >>= 1;
      }
      hb_bitbuffer_cat_int( pBits, 31, cnt < 3 ? 3 : 1 );
      if( ! fLast )
         hb_bitbuffer_cat_int( pBits, 0, 1 );
   }
   else
   {
      for( i = 0; i < 8; i++ )
      {
         hb_bitbuffer_cat_int( pBits, i & 1 ? 0 : 31, code & 1 ? 2 : 1 );
         cnt += ( code & 1 );
         code >>= 1;
      }
      hb_bitbuffer_cat_int( pBits, 31, cnt < 3 ? 2 : 1 );
      if( ! fLast )
         hb_bitbuffer_cat_int( pBits, 0, 1 );
   }
}

PHB_ZEBRA hb_zebra_create_code39( const char * szCode, HB_SIZE nLen, int iFlags )
{
   PHB_ZEBRA  pZebra;
   int        i, csum, iLen = ( int ) nLen;

   pZebra = hb_zebra_create();
   pZebra->iType = HB_ZEBRA_TYPE_CODE39;

   for( i = 0; i < iLen; i++ )
   {
      if( _code39_charno( szCode[ i ] ) < 0 )
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

   _code39_add( pZebra->pBits, 0x52, iFlags, HB_FALSE );    /* start */

   csum = 0;
   for( i = 0; i < iLen; i++ )
   {
      int no = _code39_charno( szCode[ i ] );
      _code39_add( pZebra->pBits, ( char ) s_code[ no ], iFlags, HB_FALSE );
      csum += no;
   }

   if( iFlags & HB_ZEBRA_FLAG_CHECKSUM )
      _code39_add( pZebra->pBits, ( char ) s_code[ csum % 43 ], iFlags, HB_FALSE );

   _code39_add( pZebra->pBits, 0x52, iFlags, HB_TRUE );    /* stop */
   return pZebra;
}


HB_FUNC( HB_ZEBRA_CREATE_CODE39 )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );
   if( pItem )
   {
      hb_zebra_ret( hb_zebra_create_code39( hb_itemGetCPtr( pItem ), hb_itemGetCLen( pItem ), hb_parni( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
