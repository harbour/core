/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 *
 * www - http://www.harbour-project.org
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

#include <string.h>
#include <limits.h>
#include <stdlib.h>
#include "hbapi.h"

/*
 * base64enc()
 *
 * Encode the data in 's' (length of the data is 'len') in BASE64. The returned
 * string should be freed when not used anymore.
 */

static char * base64enc( char *s, size_t s_len )
{
   char b64chars[] =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
   char * t;
   char * p;
   int x, y;
   int len;

   if( s_len > ( size_t ) INT_MAX )
   {
      return NULL ; /* die("data too long in base64enc()"); */
   }
   len = ( int ) s_len;
   t = ( char * ) hb_xgrab( ( 4 * ( ( len + 2 ) / 3 ) + 1 ) * sizeof( char ) );
   p = t;

   while( len-- > 0 )
   {
      x = *s++;
      *p++ = b64chars[(x >> 2) & 63];
      if( len-- <= 0 )
      {
         *p++ = b64chars[(x << 4) & 63];
         *p++ = '=';
         *p++ = '=';
         break;
      }
      y = *s++;
      *p++ = b64chars[((x << 4) | ((y >> 4) & 15)) & 63];
      if( len-- <= 0 )
      {
         *p++ = b64chars[(y << 2) & 63];
         *p++ = '=';
         break;
      }
      x = *s++;
      *p++ = b64chars[((y << 2) | ((x >> 6) & 3)) & 63];
      *p++ = b64chars[x & 63];
   }
   *p = '\0';

   return t;
}

HB_FUNC( BUILDUSERPASSSTRING )
{
   char * s;
   char * szUser = hb_parcx( 1 );
   char * szPass = hb_parcx( 2 );
   size_t p_len = strlen( szPass );
   size_t u_len = strlen( szUser );

   s = ( char * ) hb_xgrab( ( u_len + p_len + 3 ) * sizeof( char ) );
   s[0] = '\0';
   strcpy( s + 1, szUser );
   strcpy( s + u_len + 2, szPass );

   hb_retcAdopt( s );
}

HB_FUNC( HB_BASE64 )
{
   char * szItem = hb_parc( 1 );
   int nLen = hb_parni( 2 );
   char * szRet = szItem ? base64enc( szItem, nLen ) : NULL;

   if( szRet )
      hb_retc_buffer( szRet );
   else
      hb_retc( NULL );
}
