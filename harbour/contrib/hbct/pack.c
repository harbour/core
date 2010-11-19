/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * CT3 CHARPACK() and CHARUNPACK() functions.
 *
 * Copyright 2004 Phil Krylov <phil@newstar.rinet.ru>
 * www - http://www.xharbour.org
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

HB_FUNC( CHARPACK )
{
   HB_SIZE len = hb_parclen( 1 );
   const HB_UCHAR * in = ( const HB_UCHAR * ) hb_parcx( 1 );

   if( hb_parni( 2 ) == 0 )
   {
      HB_UCHAR * out = ( HB_UCHAR * ) hb_xgrab( len * 3 + 2 );
      HB_SIZE n_in = 0, n_out = 0;

      out[ n_out++ ] = 158;
      out[ n_out++ ] = 158;

      while( n_in < len )
      {
         HB_ISIZ n_count = 1, n_max = HB_MIN( 255, len - n_in );
         HB_UCHAR c = in[ n_in ];

         while( n_count < n_max && in[ n_in + n_count ] == c )
            n_count++;
         out[ n_out++ ] = 0;
         out[ n_out++ ] = ( HB_UCHAR ) n_count;
         out[ n_out++ ] = c;
         n_in += n_count;
      }
      if( n_out < len )
         hb_retclen( ( char * ) out, n_out );
      hb_xfree( out );
      if( n_out < len )
         return;
   }
   hb_retclen( ( char * ) in, len );
}

static HB_UCHAR * buf_append( HB_UCHAR * buf, HB_SIZE * buf_size, HB_SIZE count,
                              HB_UCHAR c, HB_SIZE * buf_len )
{
   if( *buf_len + count > *buf_size )
   {
      *buf_size = HB_MAX( *buf_len + count, *buf_size + 32768 );
      buf = ( HB_UCHAR * ) hb_xrealloc( buf, *buf_size );
   }
   memset( buf + *buf_len, c, count );
   *buf_len += count;
   return buf;
}

HB_FUNC( CHARUNPACK )
{
   HB_SIZE len = hb_parclen( 1 );
   const HB_UCHAR * in = ( const HB_UCHAR * ) hb_parcx( 1 );

   if( hb_parni( 2 ) == 0 )
   {
      HB_UCHAR * out;
      HB_SIZE out_len = 0;
      HB_SIZE buf_size = 32768;
      HB_SIZE i;

      if( !( in[ 0 ] == 158 && in[ 1 ] == 158 ) )
      {
         hb_retclen( ( char * ) in, len );
         return;
      }
      out = ( HB_UCHAR * ) hb_xgrab( buf_size );
      for( i = 2; i <= len - 3; i += 3 )
      {
         if( in[ i ] != 0 )
         {
            hb_xfree( out );
            hb_retclen( ( char * ) in, len );
            return;
         }
         out = buf_append( out, &buf_size, in[ i + 1 ], in[ i + 2 ], &out_len );
      }
      hb_retclen( ( char * ) out, out_len );
      hb_xfree( out );
      return;
   }
   hb_retclen( ( char * ) in, len );
}
