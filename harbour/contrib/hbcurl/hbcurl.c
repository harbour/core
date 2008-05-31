/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * CURL lib low level (Client API) interface code.
 *
 * Copyright 2008 Viktor Szakats <harbour.01 syenar.hu>
 * Copyright 2005 Luiz Rafael Culik Guimaraes <luiz at xharbour.com.br>
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
 * See doc/license.txt for licensing terms.
 *
 */

#include "curl/curl.h"
#include "curl/types.h"
#include "curl/easy.h"

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapifs.h"
#include "hbvm.h"

#include "hbcurl.ch"

#define HB_CURL_OPT_BOOL( n )      ( ISLOG( n ) ? ( long ) hb_parl( n ) : hb_parnl( n ) )
#define HB_CURL_OPT_BOOL_TRUE( n ) ( ISLOG( n ) ? ( long ) hb_parl( n ) : ( ISNUM( 1 ) ? hb_parnl( n ) : 1 ) )

typedef struct _HB_CURL_FTPFILE
{
   BYTE *  name;
   FHANDLE handle;
} HB_CURL_FTPFILE, * PHB_CURL_FTPFILE;

typedef struct _HB_CURL
{
   CURL * curl;

   struct curl_httppost * sHttpPostf;
   struct curl_httppost * sHttpPostl;

   struct curl_slist *    sHttpHeader;
   struct curl_slist *    sQuote;
   struct curl_slist *    sPostQuote;
   struct curl_slist *    sPreQuote;

   HB_CURL_FTPFILE file_ul;
   HB_CURL_FTPFILE file_dl;

   PHB_ITEM pProgressBlock;

} HB_CURL, * PHB_CURL;

/* ---------------------------- */

#ifdef _HB_CURL_REDEF_MEM

void * hb_curl_xgrab( size_t size )
{
   return hb_xgrab( ( ULONG ) size );
}

void hb_curl_xfree( void * p )
{
   hb_xfree( p );
}

void * hb_curl_xrealloc( void * p, size_t size )
{
   return hb_xrealloc( p, ( ULONG ) size );
}

char * hb_curl_strdup( const char * s )
{
   return hb_strdup( s );
}

void * hb_curl_calloc( size_t nelem, size_t elsize )
{
   return hb_xgrab( ( ULONG ) ( nelem * elsize ) );
}

#endif

HB_FUNC( CURL_GLOBAL_INIT )
{
#ifdef _HB_CURL_REDEF_MEM
   /* TOFIX: Doesn't work, as hb_xgrab() is getting pointers not allocated by our allocators. */
   hb_retnl( ( long ) curl_global_init_mem( ISNUM( 1 ) ? hb_parnl( 1 ) : CURL_GLOBAL_ALL,
                                            hb_curl_xgrab,
                                            hb_curl_xfree,
                                            hb_curl_xrealloc,
                                            hb_curl_strdup,
                                            hb_curl_calloc ) );
#else
   hb_retnl( ( long ) curl_global_init( ISNUM( 1 ) ? hb_parnl( 1 ) : CURL_GLOBAL_ALL ) );
#endif
}

HB_FUNC( CURL_GLOBAL_CLEANUP )
{
   curl_global_cleanup();
}

/* ---------------------------- */

size_t hb_curl_read_callback( void * buffer, size_t size, size_t nmemb, void * Cargo )
{
   if( Cargo )
   {
      PHB_CURL_FTPFILE pfile_ul = ( PHB_CURL_FTPFILE ) Cargo;
      size_t ret;

      if( pfile_ul->handle == FS_ERROR )
      {
         pfile_ul->handle = hb_fsOpen( pfile_ul->name, FO_READ );
         
         if( pfile_ul->handle == FS_ERROR )
            return -1;
      }

      ret = ( size_t ) hb_fsReadLarge( pfile_ul->handle, ( BYTE * ) buffer, size * nmemb );

      return hb_fsError() ? CURL_READFUNC_ABORT : ret;
   }

   return -1;
}

size_t hb_curl_write_callback( void * buffer, size_t size, size_t nmemb, void * Cargo )
{
   if( Cargo )
   {
      PHB_CURL_FTPFILE pfile_dl = ( PHB_CURL_FTPFILE ) Cargo;

      if( pfile_dl->handle == FS_ERROR )
      {
         pfile_dl->handle = hb_fsCreate( pfile_dl->name, FC_NORMAL );
         
         if( pfile_dl->handle == FS_ERROR )
            return -1;
      }

      return hb_fsWriteLarge( pfile_dl->handle, ( BYTE * ) buffer, size * nmemb );
   }

   return -1;
}

int hb_curl_progress_callback( void * Cargo,
                               double dltotal,
                               double dlnow,
                               double ultotal,
                               double ulnow )
{
   if( Cargo )
   {
      PHB_ITEM p1 = hb_itemPutND( NULL, ulnow   > 0 ? ulnow   : dlnow   );
      PHB_ITEM p2 = hb_itemPutND( NULL, ultotal > 0 ? ultotal : dltotal );
      
      BOOL bResult = hb_itemGetL( hb_vmEvalBlockV( ( PHB_ITEM ) Cargo, 2, p1, p2 ) );
      
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );

      /* Abort */
      if( bResult )
         return 1;
   }

   return 0;
}

static PHB_CURL PHB_CURL_New( void )
{
   PHB_CURL hb_curl = ( PHB_CURL ) hb_xgrab( sizeof( HB_CURL ) );

   memset( ( void * ) hb_curl, 0, sizeof( HB_CURL ) );

   hb_curl->curl = curl_easy_init();

   return hb_curl;
}

static void PHB_CURL_free( PHB_CURL hb_curl )
{
   curl_easy_setopt( hb_curl->curl, CURLOPT_READFUNCTION, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_READDATA, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_WRITEFUNCTION, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_WRITEDATA, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_PROGRESSFUNCTION, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_PROGRESSDATA, NULL );

   if( hb_curl->sHttpPostf )
      curl_formfree( hb_curl->sHttpPostf );

   if( hb_curl->sHttpPostl )
      curl_formfree( hb_curl->sHttpPostl );

   if( hb_curl->sHttpHeader )
      curl_slist_free_all( hb_curl->sHttpHeader );

   if( hb_curl->sQuote )
      curl_slist_free_all( hb_curl->sQuote );

   if( hb_curl->sPostQuote )
      curl_slist_free_all( hb_curl->sPostQuote );

   if( hb_curl->sPreQuote )
      curl_slist_free_all( hb_curl->sPreQuote );

   if( hb_curl->file_ul.name )
   {
      hb_xfree( hb_curl->file_ul.name );

      if( hb_curl->file_ul.handle != FS_ERROR )
         hb_fsClose( hb_curl->file_ul.handle );
   }

   if( hb_curl->file_dl.name )
   {
      hb_xfree( hb_curl->file_dl.name );

      if( hb_curl->file_dl.handle != FS_ERROR )
         hb_fsClose( hb_curl->file_dl.handle );
   }

   if( hb_curl->pProgressBlock )
      hb_itemRelease( hb_curl->pProgressBlock );

   curl_easy_cleanup( hb_curl->curl );

   hb_xfree( hb_curl );
}

static HB_GARBAGE_FUNC( PHB_CURL_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && * ph )
   {
      /* Destroy the object */
      PHB_CURL_free( ( PHB_CURL ) * ph );

      /* set pointer to NULL to avoid multiple freeing */
      * ph = NULL;
   }
}

static PHB_CURL PHB_CURL_par( int iParam )
{
/*
   TOFIX: Use this.

   void ** ph = ( void ** ) hb_parptrGC( PHB_CURL_release, iParam );

   return ph ? ( PHB_CURL ) * ph : NULL;
*/
   return ( PHB_CURL ) hb_parptr( iParam );
}

HB_FUNC( CURL_EASY_INIT )
{
/*
   TOFIX: Use this.

   void ** ph = ( void ** ) hb_gcAlloc( sizeof( PHB_CURL ), PHB_CURL_release );

   * ph = ( void * ) PHB_CURL_New();

   hb_retptrGC( ph );
*/
   hb_retptr( ( void * ) PHB_CURL_New() );
}

HB_FUNC( CURL_EASY_CLEANUP )
{
   PHB_CURL hb_curl = PHB_CURL_par( 1 );

   if( hb_curl )
      PHB_CURL_free( hb_curl );
}

HB_FUNC( CURL_EASY_PERFORM )
{
   PHB_CURL hb_curl = PHB_CURL_par( 1 );

   if( hb_curl )
      hb_retnl( curl_easy_perform( hb_curl->curl ) );
}

HB_FUNC( CURL_EASY_SETOPT )
{
   PHB_CURL hb_curl = PHB_CURL_par( 1 );
   CURLcode res = CURLE_UNSUPPORTED_PROTOCOL;

   if( hb_curl )
   {
      switch( hb_parni( 2 ) )
      {
      case HB_CURLOPT_INFILESIZE:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_INFILESIZE, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_INFILESIZE_LARGE:
         /* TOFIX */
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_INFILESIZE_LARGE, ( curl_off_t ) hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_RESUME_FROM:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_RESUME_FROM, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_RESUME_FROM_LARGE:
         /* TOFIX */
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_RESUME_FROM_LARGE, ( curl_off_t ) hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_MAXFILESIZE:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAXFILESIZE, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_MAXFILESIZE_LARGE:
         /* TOFIX */
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAXFILESIZE_LARGE, ( curl_off_t ) hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_POSTFIELDSIZE:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_POSTFIELDSIZE, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_POSTFIELDSIZE_LARGE:
         /* TOFIX */
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_POSTFIELDSIZE_LARGE, ( curl_off_t ) hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_PORT:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_PORT, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_TIMEOUT:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_TIMEOUT, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_LOW_SPEED_TIME:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_LOW_SPEED_TIME, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_CRLF:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_CRLF, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_SSLVERSION:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLVERSION, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_TIMECONDITION:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_TIMECONDITION, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_TIMEVALUE:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_TIMEVALUE, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_VERBOSE:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_VERBOSE, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_HEADER:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_HEADER, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_NOPROGRESS:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_NOPROGRESS, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_FAILONERROR:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_FAILONERROR, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_UPLOAD:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_UPLOAD, HB_CURL_OPT_BOOL_TRUE( 3 ) ? 1 : 0 );
         break;

      case HB_CURLOPT_POST:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_POST, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_FTPLISTONLY: /* CURLOPT_DIRLISTONLY */
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTPLISTONLY, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_FTPAPPEND: /* CURLOPT_APPEND */
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTPAPPEND, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_NETRC:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_NETRC, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_FOLLOWLOCATION:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_FOLLOWLOCATION, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_TRANSFERTEXT:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_TRANSFERTEXT, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_PUT:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_PUT, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_AUTOREFERER:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_AUTOREFERER, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_PROXYPORT:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYPORT, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_HTTPPROXYTUNNEL:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPPROXYTUNNEL, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_SSL_VERIFYPEER:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_VERIFYPEER, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_MAXREDIRS:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAXREDIRS, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_FILETIME:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_FILETIME, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_MAXCONNECTS:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAXCONNECTS, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_CLOSEPOLICY:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_CLOSEPOLICY, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_FRESH_CONNECT:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_FRESH_CONNECT, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_FORBID_REUSE:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_FORBID_REUSE, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_CONNECTTIMEOUT:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_CONNECTTIMEOUT, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_HTTPGET:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPGET, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_SSL_VERIFYHOST:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_VERIFYHOST, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_HTTP_VERSION:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTP_VERSION, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_FTP_USE_EPSV:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_USE_EPSV, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_SSLENGINE_DEFAULT:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLENGINE_DEFAULT, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_DNS_USE_GLOBAL_CACHE:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_DNS_USE_GLOBAL_CACHE, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_DNS_CACHE_TIMEOUT:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_DNS_CACHE_TIMEOUT, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_COOKIESESSION:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_COOKIESESSION, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_BUFFERSIZE:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_BUFFERSIZE, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_NOSIGNAL:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_NOSIGNAL, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_PROXYTYPE:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYTYPE, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_UNRESTRICTED_AUTH:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_UNRESTRICTED_AUTH, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_FTP_USE_EPRT:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_USE_EPRT, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_HTTPAUTH:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPAUTH, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_FTP_CREATE_MISSING_DIRS:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_CREATE_MISSING_DIRS, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_PROXYAUTH:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYAUTH, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_FTP_RESPONSE_TIMEOUT:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_RESPONSE_TIMEOUT, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_IPRESOLVE:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_IPRESOLVE, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_FTP_SSL: /* USE_SSL */
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_SSL, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_TCP_NODELAY:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_TCP_NODELAY, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_FTPSSLAUTH:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTPSSLAUTH, hb_parnl( 3 ) );
         break;

      case HB_CURLOPT_IGNORE_CONTENT_LENGTH:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_IGNORE_CONTENT_LENGTH, HB_CURL_OPT_BOOL( 3 ) );
         break;

      case HB_CURLOPT_URL:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_URL, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_PROXY:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_USERPWD:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_USERPWD, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_PROXYUSERPWD:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYUSERPWD, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_RANGE:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_RANGE, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_ERRORBUFFER:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_ERRORBUFFER, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_POSTFIELDS:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_POSTFIELDS, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_REFERER:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_REFERER, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_FTPPORT:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTPPORT, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_USERAGENT:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_USERAGENT, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_COOKIE:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_COOKIE, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_SSLCERT:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLCERT, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_SSLKEYPASSWD:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLKEYPASSWD, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_WRITEHEADER:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_WRITEHEADER, hb_parcx( 3 ) ); /* pointer or file * */
         break;

      case HB_CURLOPT_COOKIEFILE:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_COOKIEFILE, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_CUSTOMREQUEST:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_CUSTOMREQUEST, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_STDERR:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_STDERR, hb_parcx( 3 ) ); /* File * */
         break;

      case HB_CURLOPT_WRITEINFO: /* verificar */
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_WRITEINFO, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_PROGRESSDATA:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROGRESSDATA, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_INTERFACE:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_INTERFACE, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_KRB4LEVEL:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_KRB4LEVEL, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_CAINFO:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_CAINFO, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_TELNETOPTIONS:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_TELNETOPTIONS, hb_parcx( 3 ) ); /* use curl_slist */
         break;

      case HB_CURLOPT_RANDOM_FILE:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_RANDOM_FILE, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_EGDSOCKET:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_EGDSOCKET, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_COOKIEJAR:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_COOKIEJAR, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_SSL_CIPHER_LIST:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_CIPHER_LIST, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_SSLCERTTYPE:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLCERTTYPE, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_SSLKEY:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLKEY, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_SSLKEYTYPE:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLKEYTYPE, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_SSLENGINE:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLENGINE, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_DEBUGDATA:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_DEBUGDATA, hb_parcx( 3 ) ); /* use pointer */
         break;

      case HB_CURLOPT_CAPATH:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_CAPATH, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_ENCODING:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_ENCODING, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_PRIVATE:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_PRIVATE, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_HTTP200ALIASES:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTP200ALIASES, hb_parcx( 3 ) ); /* use struct curl_slist structs */
         break;

      case HB_CURLOPT_SSL_CTX_DATA:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_CTX_DATA, hb_parcx( 3 ) ); /* use pointer */
         break;

      case HB_CURLOPT_NETRC_FILE:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_NETRC_FILE, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_IOCTLDATA:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_IOCTLDATA, hb_parcx( 3 ) );  /* pointer */
         break;

      case HB_CURLOPT_FTP_ACCOUNT:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_ACCOUNT, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_COOKIELIST:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_COOKIELIST, hb_parcx( 3 ) );
         break;

      case HB_CURLOPT_HTTPPOST:
         {
            PHB_ITEM pHttpPost = hb_param( 3, HB_IT_ARRAY );
            ULONG ulPos;
            ULONG ulArrayPos = hb_arrayLen( pHttpPost );

            for( ulPos = 0; ulPos < ulArrayPos; ulPos++ )
            {
               PHB_ITEM pArray = hb_arrayGetItemPtr( pHttpPost, ulPos + 1 );

               curl_formadd( &hb_curl->sHttpPostf,
                             &hb_curl->sHttpPostl,
                             CURLFORM_COPYNAME, hb_arrayGetCPtr( pArray, 1 ),
                             CURLFORM_FILE, hb_arrayGetCPtr( pArray, 2 ),
                             CURLFORM_END );
            }

            res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPPOST, hb_curl->sHttpPostf );
         }
         break;

      case HB_CURLOPT_HTTPHEADER:
         {
            PHB_ITEM pHttpHeaders = hb_param( 3, HB_IT_ARRAY );
            ULONG ulPos;
            ULONG ulArrayPos = hb_arrayLen( pHttpHeaders );

            for( ulPos = 0; ulPos < ulArrayPos; ulPos++ )
               curl_slist_append( hb_curl->sHttpHeader, hb_arrayGetCPtr( pHttpHeaders, ulPos + 1 ) );

            res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPHEADER, hb_curl->sHttpHeader );
         }
         break;

      case HB_CURLOPT_QUOTE:
         {
            PHB_ITEM pHttpHeaders = hb_param( 3, HB_IT_ARRAY );
            ULONG ulPos;
            ULONG ulArrayPos = hb_arrayLen( pHttpHeaders );

            for( ulPos = 0; ulPos < ulArrayPos; ulPos++ )
               curl_slist_append( hb_curl->sQuote, hb_arrayGetCPtr( pHttpHeaders, ulPos + 1 ) );

            res = curl_easy_setopt( hb_curl->curl, CURLOPT_QUOTE, hb_curl->sQuote );
         }
         break;

      case HB_CURLOPT_PREQUOTE:
         {
            PHB_ITEM pHttpHeaders = hb_param( 3, HB_IT_ARRAY );
            ULONG ulPos;
            ULONG ulArrayPos = hb_arrayLen( pHttpHeaders );

            for( ulPos = 0; ulPos < ulArrayPos; ulPos++ )
               curl_slist_append( hb_curl->sQuote, hb_arrayGetCPtr( pHttpHeaders, ulPos + 1 ) );

            res = curl_easy_setopt( hb_curl->curl, CURLOPT_PREQUOTE, hb_curl->sPreQuote );
         }
         break;

      case HB_CURLOPT_POSTQUOTE:
         {
            PHB_ITEM pHttpHeaders = hb_param( 3, HB_IT_ARRAY );
            ULONG ulPos;
            ULONG ulArrayPos = hb_arrayLen( pHttpHeaders );

            for( ulPos = 0; ulPos < ulArrayPos; ulPos++ )
               curl_slist_append( hb_curl->sPostQuote, hb_arrayGetCPtr( pHttpHeaders, ulPos + 1 ) );

            res = curl_easy_setopt( hb_curl->curl, CURLOPT_POSTQUOTE, hb_curl->sPostQuote );
         }
         break;

      /* Harbour special ones */

      case HB_CURLOPT_SETPROGRESS:
         {
            PHB_ITEM pProgressBlock = hb_param( 3, HB_IT_BLOCK );

            if( hb_curl->pProgressBlock )
            {
               curl_easy_setopt( hb_curl->curl, CURLOPT_PROGRESSFUNCTION, NULL );
               curl_easy_setopt( hb_curl->curl, CURLOPT_PROGRESSDATA, NULL );

               hb_itemRelease( hb_curl->pProgressBlock );
               hb_curl->pProgressBlock = NULL;
            }

            if( pProgressBlock )
            {
               hb_curl->pProgressBlock = hb_itemNew( NULL );

               hb_itemCopy( hb_curl->pProgressBlock, pProgressBlock );

               curl_easy_setopt( hb_curl->curl, CURLOPT_PROGRESSFUNCTION, hb_curl_progress_callback );
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROGRESSDATA, ( void * ) hb_curl->pProgressBlock );
            }
         }
         break;

      case HB_CURLOPT_SETUPLOADFILE:
         {
            if( hb_curl->file_ul.name )
            {
               hb_xfree( hb_curl->file_ul.name );
               hb_curl->file_ul.name = NULL;

               if( hb_curl->file_ul.handle != FS_ERROR )
               {
                  hb_fsClose( hb_curl->file_ul.handle );
                  hb_curl->file_ul.handle = FS_ERROR;
               }
            }

            hb_curl->file_ul.name = ( BYTE * ) hb_strdup( hb_parc( 3 ) );
            hb_curl->file_ul.handle = FS_ERROR;

            curl_easy_setopt( hb_curl->curl, CURLOPT_READFUNCTION, hb_curl_read_callback );
            res = curl_easy_setopt( hb_curl->curl, CURLOPT_READDATA, ( void * ) &hb_curl->file_ul );
         }
         break;

      case HB_CURLOPT_CLOSEUPLOADFILE:
         {
            if( hb_curl->file_ul.name )
            {
               hb_xfree( hb_curl->file_ul.name );
               hb_curl->file_ul.name = NULL;

               if( hb_curl->file_ul.handle != FS_ERROR )
               {
                  hb_fsClose( hb_curl->file_ul.handle );
                  hb_curl->file_ul.handle = FS_ERROR;
    
                  res = CURLE_OK;
               }
               else
                  res = ( CURLcode ) -1;
            }
            else
               res = CURLE_OK;
         }
         break;

      case HB_CURLOPT_SETDOWNLOADFILE:
         {
            if( hb_curl->file_dl.name )
            {
               hb_xfree( hb_curl->file_dl.name );
               hb_curl->file_dl.name = NULL;

               if( hb_curl->file_dl.handle != FS_ERROR )
               {
                  hb_fsClose( hb_curl->file_dl.handle );
                  hb_curl->file_dl.handle = FS_ERROR;
               }
            }

            hb_curl->file_dl.name = ( BYTE * ) hb_strdup( hb_parc( 3 ) );
            hb_curl->file_dl.handle = FS_ERROR;

            curl_easy_setopt( hb_curl->curl, CURLOPT_WRITEFUNCTION, hb_curl_write_callback );
            res = curl_easy_setopt( hb_curl->curl, CURLOPT_WRITEDATA, ( void * ) &hb_curl->file_dl );
         }
         break;

      case HB_CURLOPT_CLOSEDOWNLOADFILE:
         {
            if( hb_curl->file_dl.name )
            {
               hb_xfree( hb_curl->file_dl.name );
               hb_curl->file_dl.name = NULL;

               if( hb_curl->file_dl.handle != FS_ERROR )
               {
                  hb_fsClose( hb_curl->file_dl.handle );
                  hb_curl->file_dl.handle = FS_ERROR;
    
                  res = CURLE_OK;
               }
               else
                  res = ( CURLcode ) -1;
            }
            else
               res = CURLE_OK;
         }
         break;

      case HB_CURLOPT_DOWNLOAD:
         res = curl_easy_setopt( hb_curl->curl, CURLOPT_UPLOAD, HB_CURL_OPT_BOOL_TRUE( 3 ) ? 0 : 1 );
         break;
      }
   }

   hb_retnl( res );
}

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

      hb_arraySetC(  pArray,  1, data->version );                     /* LIBCURL_VERSION */                    
      hb_arraySetNI( pArray,  2, data->version_num );                 /* LIBCURL_VERSION_NUM */                
      hb_arraySetC(  pArray,  3, data->host );                        /* OS/host/cpu/machine when configured */
      hb_arraySetNI( pArray,  4, data->features );                    /* bitmask, see defines below */         
      hb_arraySetC(  pArray,  5, data->ssl_version );                 /* human readable string */     
      hb_arraySetNI( pArray,  6, data->ssl_version_num );             /* not used anymore, always 0 */
      hb_arraySetC(  pArray,  7, data->libz_version );                /* human readable string */
      hb_arraySetC(  pArray,  9, data->age >= CURLVERSION_SECOND ? data->ares : NULL );
      hb_arraySetNI( pArray, 10, data->age >= CURLVERSION_SECOND ? data->ares_num : 0 );
      hb_arraySetC(  pArray, 11, data->age >= CURLVERSION_THIRD  ? data->libidn : NULL );
      hb_arraySetNI( pArray, 12, data->age >= CURLVERSION_FOURTH ? data->iconv_ver_num : 0 ); /* Same as '_libiconv_version' if built with HAVE_ICONV */
      hb_arraySetC(  pArray, 13, data->age >= CURLVERSION_FOURTH ? data->libssh_version : NULL ); /* human readable string */

      {
         PHB_ITEM pProtocols;
         int nProtCount = 0;
         const char * const * prot = data->protocols;
   
         while( *( prot++ ) )
            nProtCount++;
   
         pProtocols = hb_arrayGetItemPtr( pArray, 8 );
         hb_arrayNew( pProtocols, nProtCount );
   
         for( prot = data->protocols, nProtCount = 1; *prot; prot++ )
            hb_arraySetC( pProtocols, nProtCount++, *prot );
      }

      hb_itemReturnRelease( pArray );
   }
   else
      hb_reta( 0 );
}

HB_FUNC( CURL_EASY_STRERROR )
{
   hb_retc( curl_easy_strerror( ( CURLcode ) hb_parnl( 1 ) ) );
}

HB_FUNC( CURL_SHARE_STRERROR )
{
   hb_retc( curl_share_strerror( ( CURLSHcode ) hb_parnl( 1 ) ) );
}

HB_FUNC( CURL_EASY_ESCAPE )
{
   PHB_CURL hb_curl = PHB_CURL_par( 1 );

   if( hb_curl )
   {
      /* TOFIX: buffer is allocated using CURL's alloc. */
      hb_retc_buffer( curl_easy_escape( hb_curl->curl, hb_parcx( 1 ), hb_parclen( 1 ) ) );
   }
   else
      hb_retc( NULL );
}

HB_FUNC( CURL_EASY_UNESCAPE )
{
   PHB_CURL hb_curl = PHB_CURL_par( 1 );

   if( hb_curl )
   {
      int nLen = 0;
      /* TOFIX: buffer is allocated using CURL's alloc. */
      char * buffer = curl_easy_unescape( hb_curl->curl, hb_parcx( 1 ), hb_parclen( 1 ), &nLen );

      hb_retclen_buffer( buffer, nLen );
   }
   else
      hb_retc( NULL );
}
