/*
 * xHarbour Project source code:
 * BASE64 encoder
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

HB_FUNC( HB_BASE64ENCODE )
{
   HB_SIZE len = hb_parclen( 1 );

   if( len > 0 )
   {
      HB_SIZE lin = hb_parns( 2 );
      HB_SIZE dst = ( 4 * ( ( len + 2 ) / 3 ) + 1 );

      if( lin <= 2 )
         lin = 0;

      if( lin )
         dst += ( ( dst + lin - 1 ) / lin ) * 2;
      dst *= sizeof( char );

      if( dst > len )
      {
         const char * s = hb_parcx( 1 );
         char * t, * p;
         HB_SIZE lln = lin;

         t = p = ( char * ) hb_xgrab( dst );

         while( len-- > 0 )
         {
            #define ADD_EOL()       do { if( --lln == 0 ) { *p++ = '\r'; *p++ = '\n'; lln = lin; } } while( 0 )
            #define ADD_CHAR( c )   do { *p++ = s_b64chars[ ( c ) & 0x3F ]; ADD_EOL(); } while( 0 )
            #define ADD_EQ()        do { *p++ = '='; ADD_EOL(); } while( 0 )
            static const char s_b64chars[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
            int x, y;

            x = *s++;
            ADD_CHAR( x >> 2 );
            if( len-- == 0 )
            {
               ADD_CHAR( x << 4 );
               ADD_EQ();
               ADD_EQ();
               break;
            }

            y = *s++;
            ADD_CHAR( ( x << 4 ) | ( ( y >> 4 ) & 0x0F ) );
            if( len-- == 0 )
            {
               ADD_CHAR( y << 2 );
               ADD_EQ();
               break;
            }

            x = *s++;
            ADD_CHAR( ( y << 2 ) | ( ( x >> 6 ) & 0x03 ) );
            ADD_CHAR( x );
         }

         if( lin && lin != lln )
         {
            *p++ = '\r';
            *p++ = '\n';
         }
         *p = '\0';

         hb_retclen_buffer( t, p - t );
      }
      else
         hb_errRT_BASE( EG_STROVERFLOW, 9999, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
   else
      hb_retc_null();
}
