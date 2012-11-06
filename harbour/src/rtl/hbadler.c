/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    adler32 checksum function
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 * Algorithm taken from adler32.c Copyright (C) 1995-2002 Mark Adler
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
#include "hbapierr.h"
#include "hbchksum.h"

#define BASE  65521  /* largest prime smaller than 65536 */
#define NMAX  5552   /* largest n such that 255n(n+1)/2 + (n+1)(BASE-1) <= 2^32-1 */

#define LOOP_DO1( buf, i )   { s1 += buf[ i ]; s2 += s1; }
#define LOOP_DO2( buf, i )   { LOOP_DO1( buf, i ) LOOP_DO1( buf, i + 1 ) }
#define LOOP_DO4( buf, i )   { LOOP_DO2( buf, i ) LOOP_DO2( buf, i + 2 ) }
#define LOOP_DO8( buf, i )   { LOOP_DO4( buf, i ) LOOP_DO4( buf, i + 4 ) }
#define LOOP_DO16( buf, i )  { LOOP_DO8( buf, i ) LOOP_DO8( buf, i + 8 ) }

HB_U32 hb_adler32( HB_U32 adler, const void * buf, HB_SIZE len )
{
   HB_U32 s1 = adler & 0xffff;
   HB_U32 s2 = ( adler >> 16 ) & 0xffff;

   if( buf && len )
   {
      const unsigned char * ucbuf = ( const unsigned char * ) buf;
      do
      {
         HB_ISIZ n = len < NMAX ? len : NMAX;
         len -= n;
         if( n >= 16 )
         {
            do
            {
               LOOP_DO16( ucbuf, 0 )
               ucbuf += 16;
               n -= 16;
            }
            while( n >= 16 );
         }
         if( n )
         {
            do
            {
               s1 += *ucbuf++;
               s2 += s1;
            }
            while( --n );
         }
         s1 %= BASE;
         s2 %= BASE;
      }
      while( len );
   }

   return ( s2 << 16 ) | s1;
}

HB_FUNC( HB_ADLER32 )
{
   const char * szString = hb_parc( 1 );

   if( szString )
      hb_retnint( hb_adler32( ( HB_U32 ) hb_parnl( 2 ), szString, hb_parclen( 1 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
