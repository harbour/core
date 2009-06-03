/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * libcurl misc API - Harbour interface.
 *
 * Copyright 2008 Viktor Szakats (harbour.01 syenar.hu)
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

#include "curl/curl.h"
#include "curl/types.h"
#include "curl/multi.h"

#include "hbapi.h"
#include "hbapierr.h"

#if LIBCURL_VERSION_NUM >= 0x070C00

HB_FUNC( CURL_MULTI_STRERROR )
{
   if( ISNUM( 1 ) )
      hb_retc( curl_multi_strerror( ( CURLMcode ) hb_parnl( 1 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

#endif

#if LIBCURL_VERSION_NUM >= 0x070C00

HB_FUNC( CURL_SHARE_STRERROR )
{
   if( ISNUM( 1 ) )
      hb_retc( curl_share_strerror( ( CURLSHcode ) hb_parnl( 1 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

#endif

/* Harbour interface (session independent) */

/* NOTE: Obsolete, superceded by curl_easy_escape() */
HB_FUNC( CURL_ESCAPE )
{
   if( ISCHAR( 1 ) )
   {
      char * buffer = curl_escape( hb_parc( 1 ), hb_parclen( 1 ) );
      hb_retc( buffer );
      curl_free( buffer );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* NOTE: Obsolete, superceded by curl_easy_unescape() */
HB_FUNC( CURL_UNESCAPE )
{
   if( ISCHAR( 1 ) )
   {
      char * buffer = curl_unescape( hb_parc( 1 ), hb_parclen( 1 ) );
      hb_retc( buffer );
      curl_free( buffer );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
