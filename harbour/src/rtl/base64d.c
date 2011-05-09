/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_BASE64DECODE() function
 *
 * Copyright 2011 Viktor Szakats (harbour.01 syenar.hu)
 * [ base64_decode_* functions are part of the libb64 project, and has
 *   been placed in the public domain. Author: Chris Venter
 *   For details, see http://sourceforge.net/projects/libb64 ]
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

#include "hbapi.h"

/* Warning: this code works only on ASCII based machines */

static signed char base64_decode_value( int value_in )
{
   static const signed char s_decoding[] =
   {
      62, -1, -1, -1, 63, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -2, -1,
      -1, -1,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17,
      18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1, -1, 26, 27, 28, 29, 30, 31,
      32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51
   };

   value_in -= 43;
   if( value_in < 0 || value_in >= ( int ) HB_SIZEOFARRAY( s_decoding ) )
      return -1;

   return s_decoding[ value_in ];
}

static HB_SIZE base64_decode_block( const char * code_in, const HB_SIZE length_in, char * pszPlainttextOut )
{
   const char *         codechar = code_in;
   const char *         code_end = code_in + length_in;
   char *               pszPlainchar = pszPlainttextOut;
   signed char          fragment;

   for( ;; )
   {
      do
      {
         if( codechar == code_end )
            return pszPlainchar - pszPlainttextOut;
         fragment = base64_decode_value( *codechar++ );
      }
      while( fragment < 0 );
      *pszPlainchar = ( fragment & 0x03F ) << 2;

      do
      {
         if( codechar == code_end )
            return pszPlainchar - pszPlainttextOut;
         fragment = base64_decode_value( *codechar++ );
      }
      while( fragment < 0 );
      *pszPlainchar++ |= ( fragment & 0x030 ) >> 4;
      *pszPlainchar    = ( fragment & 0x00F ) << 4;

      do
      {
         if( codechar == code_end )
            return pszPlainchar - pszPlainttextOut;
         fragment = base64_decode_value( *codechar++ );
      }
      while( fragment < 0 );
      *pszPlainchar++ |= ( fragment & 0x03C ) >> 2;
      *pszPlainchar    = ( fragment & 0x003 ) << 6;

      do
      {
         if( codechar == code_end )
            return pszPlainchar - pszPlainttextOut;
         fragment = base64_decode_value( *codechar++ );
      }
      while( fragment < 0 );
      *pszPlainchar++ |= ( fragment & 0x03F );
   }
}

HB_FUNC( HB_BASE64DECODE )
{
   HB_SIZE nSrcLen = hb_parclen( 1 );

   if( nSrcLen > 0 )
   {
      HB_SIZE nDstLen = ( ( ( nSrcLen * 3 ) / 4 ) + 1 ) * sizeof( char );
      char * code = ( char * ) hb_xgrab( nDstLen );

      nDstLen = base64_decode_block( hb_parcx( 1 ), nSrcLen, code );
      hb_retclen_buffer( code, nDstLen );
   }
   else
      hb_retc_null();
}
