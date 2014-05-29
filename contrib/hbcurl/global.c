/*
 * Harbour Project source code:
 * curl_global_*()
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

#include "hbapi.h"

/* Global initialization/deinitialization */
/* -------------------------------------- */

static void * hb_curl_xgrab( size_t size )
{
   return hb_xgrab( size );
}

static void hb_curl_xfree( void * p )
{
   hb_xfree( p );
}

static void * hb_curl_xrealloc( void * p, size_t size )
{
   return hb_xrealloc( p, size );
}

static char * hb_curl_strdup( const char * s )
{
   return hb_strdup( s );
}

static void * hb_curl_calloc( size_t nelem, size_t elsize )
{
   size_t size = nelem * elsize;
   void * ptr  = hb_xgrab( size );

   memset( ptr, 0, size );

   return ptr;
}

HB_FUNC( CURL_GLOBAL_INIT )
{
#if LIBCURL_VERSION_NUM >= 0x070C00
   hb_retnl( ( long ) curl_global_init_mem( hb_parnldef( 1, CURL_GLOBAL_ALL ),
                                            hb_curl_xgrab,
                                            hb_curl_xfree,
                                            hb_curl_xrealloc,
                                            hb_curl_strdup,
                                            hb_curl_calloc ) );
#else
   hb_retnl( ( long ) curl_global_init( hb_parnldef( 1, CURL_GLOBAL_ALL ) ) );
#endif
}

HB_FUNC( CURL_GLOBAL_CLEANUP )
{
   curl_global_cleanup();
}
