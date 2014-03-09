/*
 * Harbour Project source code:
 * libcurl 'easy' API - Harbour interface.
 *
 * Copyright 2008-2010 Viktor Szakats (vszakats.net/harbour)
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

#include <curl/curl.h>
#if LIBCURL_VERSION_NUM < 0x070C00
#  include <curl/types.h>
#endif

#include "hbapi.h"
#include "hbapiitm.h"

HB_FUNC( CURL_VERSION )
{
   hb_retc( curl_version() );
}

HB_FUNC( CURL_VERSION_INFO )
{
   curl_version_info_data * data = curl_version_info( CURLVERSION_NOW );

   if( data )
   {
      PHB_ITEM pArray = hb_itemArrayNew( 13 );

      hb_arraySetC(  pArray, 1, data->version );                      /* LIBCURL_VERSION */
      hb_arraySetNI( pArray, 2, data->version_num );                  /* LIBCURL_VERSION_NUM */
      hb_arraySetC(  pArray, 3, data->host );                         /* OS/host/cpu/machine when configured */
      hb_arraySetNI( pArray, 4, data->features );                     /* bitmask, see defines below */
      hb_arraySetC(  pArray, 5, data->ssl_version );                  /* human readable string */
      hb_arraySetNI( pArray, 6, data->ssl_version_num );              /* not used anymore, always 0 */
      hb_arraySetC(  pArray, 7, data->libz_version );                 /* human readable string */
#if defined( CURLVERSION_SECOND )
      hb_arraySetC(  pArray, 9, data->age >= CURLVERSION_SECOND ? data->ares : NULL );
      hb_arraySetNI( pArray, 10, data->age >= CURLVERSION_SECOND ? data->ares_num : 0 );
#else
      hb_arraySetC(  pArray, 9, NULL );
      hb_arraySetNI( pArray, 10, 0 );
#endif
#if defined( CURLVERSION_THIRD )
      hb_arraySetC(  pArray, 11, data->age >= CURLVERSION_THIRD ? data->libidn : NULL );
#else
      hb_arraySetC(  pArray, 11, NULL );
#endif
#if defined( CURLVERSION_FOURTH )
      hb_arraySetNI( pArray, 12, data->age >= CURLVERSION_FOURTH ? data->iconv_ver_num : 0 ); /* Same as '_libiconv_version' if built with HAVE_ICONV */
#else
      hb_arraySetNI( pArray, 12, 0 );
#endif
#if defined( CURLVERSION_FOURTH ) && LIBCURL_VERSION_NUM >= 0x071001
      hb_arraySetC(  pArray, 13, data->age >= CURLVERSION_FOURTH ? data->libssh_version : NULL ); /* human readable string */
#else
      hb_arraySetC(  pArray, 13, NULL );
#endif
      {
         PHB_ITEM pProtocols;
         int      nCount = 0;
         const char * const * prot = data->protocols;

         while( *( prot++ ) )
            nCount++;

         pProtocols = hb_arrayGetItemPtr( pArray, 8 );
         hb_arrayNew( pProtocols, nCount );

         for( prot = data->protocols, nCount = 1; *prot; prot++ )
            hb_arraySetC( pProtocols, nCount++, *prot );
      }

      hb_itemReturnRelease( pArray );
   }
   else
      hb_reta( 0 );
}
