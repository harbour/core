/*
 * libcurl 'easy' API - Harbour interface.
 *
 * Copyright 2008-2015 Viktor Szakats (vszakats.net/harbour)
 * originally based on:
 * Copyright 2005 Luiz Rafael Culik Guimaraes <luiz at xharbour.com.br>
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
#if LIBCURL_VERSION_NUM < 0x070A03
   #include <curl/easy.h>
#endif
#if LIBCURL_VERSION_NUM < 0x070C00
   #include <curl/types.h>
#endif

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbvm.h"
#include "hbhash.h"

#include "hbcurl.ch"

#define HB_CURL_OPT_BOOL( n )      ( HB_ISLOG( n ) ? ( long ) hb_parl( n ) : hb_parnldef( n, 1 ) )
#define HB_CURL_OPT_LARGENUM( n )  ( ( curl_off_t ) hb_parnint( n ) )

/* NOTE: Harbour requires libcurl 7.17.0 or upper.
         This was the version where curl_easy_setopt() started to
         make copies of passed strings, which we currently require.
         Update: This requirement is now sorted out by local string
                 buffering logic used with pre-7.17.0 versions of
                 libcurl.
         [vszakats] */

#if LIBCURL_VERSION_NUM < 0x071100
   #ifndef HB_CURL_HASH_STRINGS
   #define HB_CURL_HASH_STRINGS
   #endif
#endif

/* Fall back to return simple error if special abort signal is not available. */
#if ! defined( CURL_READFUNC_ABORT ) /* Introduced in LIBCURL_VERSION_NUM >= 0x070C01 */
   #define CURL_READFUNC_ABORT  ( ( size_t ) -1 )
#endif

#define HB_CURL_ONFREE_NOOP     0
#define HB_CURL_ONFREE_CLOSE    1
#define HB_CURL_ONFREE_DETACH   2

typedef struct _HB_CURL
{
   CURL * curl;

   struct curl_httppost * pHTTPPOST_First;
   struct curl_httppost * pHTTPPOST_Last;
   struct curl_slist *    pHTTPHEADER;
   struct curl_slist *    pHTTP200ALIASES;
   struct curl_slist *    pQUOTE;
   struct curl_slist *    pPOSTQUOTE;
   struct curl_slist *    pPREQUOTE;
   struct curl_slist *    pTELNETOPTIONS;
   struct curl_slist *    pMAIL_RCPT;
   struct curl_slist *    pRESOLVE;
   struct curl_slist *    pPROXYHEADER;
   struct curl_slist *    pCONNECT_TO;

   char *   ul_name;
   PHB_FILE ul_file;
   int      ul_onfree;

   char *   dl_name;
   PHB_FILE dl_file;
   int      dl_onfree;

   unsigned char * ul_ptr;
   size_t          ul_len;
   size_t          ul_pos;

   unsigned char * dl_ptr;
   size_t          dl_len;
   size_t          dl_pos;

   PHB_ITEM pXferInfoCallback;
   PHB_ITEM pDebugCallback;

   PHB_HASH_TABLE pHash;

} HB_CURL, * PHB_CURL;


/* functions to keep passed string values accessible even if HVM
 * destroy them. It's necessary for old CURL versions which do not
 * make own copy of passed strings
 */

#ifdef HB_CURL_HASH_STRINGS

#define HB_CURL_HASH_TABLE_SIZE  509UL

/* returns a hash key */
static HB_HASH_FUNC( hb_curl_HashKey )    /* HB_SIZE func( const void * Value, const void * Cargo ) */
{
   HB_SIZE      ulSum  = 0;
   const char * szName = ( const char * ) Value;

   while( *szName )
      ulSum += *szName++;

   HB_SYMBOL_UNUSED( HashPtr );
   HB_SYMBOL_UNUSED( Cargo );

   return ulSum % HB_CURL_HASH_TABLE_SIZE;
}

/* deletes a string */
static HB_HASH_FUNC( hb_curl_HashDel )
{
   hb_xfree( ( void * ) Value );
   HB_SYMBOL_UNUSED( HashPtr );
   HB_SYMBOL_UNUSED( Cargo );
   return 1;
}

/* compares two strings */
static HB_HASH_FUNC( hb_curl_HashCmp )
{
   HB_SYMBOL_UNUSED( HashPtr );
   return strcmp( ( const char * ) Value, ( const char * ) Cargo );
}

static const char * hb_curl_StrHashNew( PHB_CURL hb_curl, const char * szValue )
{
   if( szValue )
   {
      char * szHash;

      if( ! hb_curl->pHash )
         hb_curl->pHash = hb_hashTableCreate( HB_CURL_HASH_TABLE_SIZE,
                                              hb_curl_HashKey, hb_curl_HashDel, hb_curl_HashCmp );

      szHash = ( char * ) hb_hashTableFind( hb_curl->pHash, szValue );
      if( ! szHash )
      {
         szHash = hb_strdup( szValue );
         hb_hashTableAdd( hb_curl->pHash, szHash, szHash );
      }
      return szHash;
   }
   else
      return NULL;
}

   #define hb_curl_StrHash( c, s )  hb_curl_StrHashNew( ( c ), ( s ) )

#else

   #define hb_curl_StrHash( c, s )  ( s )

#endif  /* HB_CURL_HASH_STRINGS */

/* Callbacks */

static size_t hb_curl_read_dummy_callback( void * buffer, size_t size, size_t nmemb, void * Cargo )
{
   HB_SYMBOL_UNUSED( buffer );
   HB_SYMBOL_UNUSED( size );
   HB_SYMBOL_UNUSED( nmemb );
   HB_SYMBOL_UNUSED( Cargo );

   return 0;
}

static size_t hb_curl_read_file_callback( void * buffer, size_t size, size_t nmemb, void * Cargo )
{
   PHB_CURL hb_curl = ( PHB_CURL ) Cargo;

   if( hb_curl )
   {
      if( hb_curl->ul_file == NULL && hb_curl->ul_name )
      {
         hb_curl->ul_file = hb_fileExtOpen( hb_curl->ul_name, NULL,
                                            FO_READ | FO_SHARED | FO_PRIVATE |
                                            FXO_SHARELOCK | FXO_NOSEEKPOS,
                                            NULL, NULL );

         if( hb_curl->ul_file == NULL )
            return ( size_t ) -1;

         hb_curl->ul_onfree = HB_CURL_ONFREE_CLOSE;
      }

      if( hb_curl->ul_file )
      {
         size_t ret = ( size_t ) hb_fileRead( hb_curl->ul_file, buffer, size * nmemb, -1 );

         return ( ret == ( size_t ) FS_ERROR || hb_fsError() ) ? CURL_READFUNC_ABORT : ret;
      }
   }

   return ( size_t ) -1;
}

static size_t hb_curl_read_buff_callback( void * buffer, size_t size, size_t nmemb, void * Cargo )
{
   PHB_CURL hb_curl = ( PHB_CURL ) Cargo;

   if( hb_curl )
   {
      size_t nTodo = size * nmemb;
      size_t nLeft = hb_curl->ul_len - hb_curl->ul_pos;

      if( nTodo > nLeft )
         nTodo = nLeft;

      hb_xmemcpy( buffer, hb_curl->ul_ptr + hb_curl->ul_pos, nTodo );

      hb_curl->ul_pos += nTodo;

      return nTodo;
   }

   return ( size_t ) -1;
}

static size_t hb_curl_write_file_callback( void * buffer, size_t size, size_t nmemb, void * Cargo )
{
   PHB_CURL hb_curl = ( PHB_CURL ) Cargo;

   if( hb_curl )
   {
      if( hb_curl->dl_file == NULL && hb_curl->dl_name )
      {
         hb_curl->dl_file = hb_fileExtOpen( hb_curl->dl_name, NULL,
                                            FO_WRITE | FO_EXCLUSIVE | FO_PRIVATE |
                                            FXO_TRUNCATE | FXO_SHARELOCK | FXO_NOSEEKPOS,
                                            NULL, NULL );

         if( hb_curl->dl_file == NULL )
            return ( size_t ) -1;

         hb_curl->dl_onfree = HB_CURL_ONFREE_CLOSE;
      }

      if( hb_curl->dl_file )
      {
         HB_SIZE nDone;
         if( ( nDone = hb_fileWrite( hb_curl->dl_file, buffer, size * nmemb, -1 ) ) != ( HB_SIZE ) FS_ERROR )
            return ( size_t ) nDone;
      }
   }

   return ( size_t ) -1;
}

#define HB_CURL_DL_BUFF_SIZE_INIT  ( CURL_MAX_WRITE_SIZE * 4 )
#define HB_CURL_DL_BUFF_SIZE_INCR  ( CURL_MAX_WRITE_SIZE * 4 )

static size_t hb_curl_write_buff_callback( void * buffer, size_t size, size_t nmemb, void * Cargo )
{
   PHB_CURL hb_curl = ( PHB_CURL ) Cargo;

   if( hb_curl )
   {
      size_t nTodo = size * nmemb;
      size_t nLeft = hb_curl->dl_len - hb_curl->dl_pos;

      if( nTodo > nLeft )
      {
         hb_curl->dl_len += HB_CURL_DL_BUFF_SIZE_INCR;
         hb_curl->dl_ptr  = ( unsigned char * ) hb_xrealloc( hb_curl->dl_ptr, hb_curl->dl_len );
      }

      hb_xmemcpy( hb_curl->dl_ptr + hb_curl->dl_pos, buffer, nTodo );

      hb_curl->dl_pos += nTodo;

      return nTodo;
   }

   return ( size_t ) -1;
}

#if LIBCURL_VERSION_NUM >= 0x072000
static int hb_curl_xferinfo_callback( void * Cargo, curl_off_t dltotal, curl_off_t dlnow, curl_off_t ultotal, curl_off_t ulnow )
{
   int result = 0;

   if( Cargo )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPushEvalSym();
         hb_vmPush( ( PHB_ITEM ) Cargo );
         hb_vmPushNumInt( ( HB_FOFFSET ) ( ulnow > 0 ? ulnow : dlnow ) );
         hb_vmPushNumInt( ( HB_FOFFSET ) ( ultotal > 0 ? ultotal : dltotal ) );
         hb_vmSend( 2 );

         if( hb_parl( -1 ) )
            result = 1;  /* Abort */

         hb_vmRequestRestore();
      }
   }

   return result;
}
#else
static int hb_curl_progress_callback( void * Cargo, double dltotal, double dlnow, double ultotal, double ulnow )
{
   int result = 0;

   if( Cargo )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPushEvalSym();
         hb_vmPush( ( PHB_ITEM ) Cargo );
         hb_vmPushDouble( ulnow > 0 ? ulnow : dlnow, HB_DEFAULT_DECIMALS );
         hb_vmPushDouble( ultotal > 0 ? ultotal : dltotal, HB_DEFAULT_DECIMALS );
         hb_vmSend( 2 );

         if( hb_parl( -1 ) )
            result = 1;  /* Abort */

         hb_vmRequestRestore();
      }
   }

   return result;
}
#endif

#if LIBCURL_VERSION_NUM >= 0x070906
static int hb_curl_debug_callback( CURL * curl, curl_infotype type, char * data, size_t size, void * Cargo )
{
   HB_SYMBOL_UNUSED( curl );

   if( Cargo )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPushEvalSym();
         hb_vmPush( ( PHB_ITEM ) Cargo );
         hb_vmPushInteger( ( int ) type );
         hb_vmPushString( data, ( HB_SIZE ) size );
         hb_vmSend( 2 );

         hb_vmRequestRestore();
      }
   }

   return 0;
}
#endif

/* Helpers */

static void hb_curl_form_free( struct curl_httppost ** ptr )
{
   if( ptr && *ptr )
   {
      curl_formfree( *ptr );
      *ptr = NULL;
   }
}

static void hb_curl_slist_free( struct curl_slist ** ptr )
{
   if( ptr && *ptr )
   {
      curl_slist_free_all( *ptr );
      *ptr = NULL;
   }
}

static void hb_curl_ul_free( PHB_CURL hb_curl )
{
   if( hb_curl )
   {
      if( hb_curl->ul_name )
      {
         hb_xfree( hb_curl->ul_name );
         hb_curl->ul_name = NULL;
      }

      if( hb_curl->ul_file )
      {
         if( hb_curl->ul_onfree == HB_CURL_ONFREE_CLOSE )
            hb_fileClose( hb_curl->ul_file );
         else if( hb_curl->ul_onfree == HB_CURL_ONFREE_DETACH )
            hb_fileDetach( hb_curl->ul_file );

         hb_curl->ul_file = NULL;
      }

      hb_curl->ul_onfree = HB_CURL_ONFREE_NOOP;

      if( hb_curl->ul_ptr )
      {
         hb_xfree( hb_curl->ul_ptr );
         hb_curl->ul_ptr = NULL;
         hb_curl->ul_len = 0;
         hb_curl->ul_pos = 0;
      }
   }
}

static void hb_curl_dl_free( PHB_CURL hb_curl )
{
   if( hb_curl )
   {
      if( hb_curl->dl_name )
      {
         hb_xfree( hb_curl->dl_name );
         hb_curl->dl_name = NULL;
      }

      if( hb_curl->dl_file )
      {
         if( hb_curl->dl_onfree == HB_CURL_ONFREE_CLOSE )
            hb_fileClose( hb_curl->dl_file );
         else if( hb_curl->dl_onfree == HB_CURL_ONFREE_DETACH )
            hb_fileDetach( hb_curl->dl_file );

         hb_curl->dl_file = NULL;
      }

      hb_curl->dl_onfree = HB_CURL_ONFREE_NOOP;

      if( hb_curl->dl_ptr )
      {
         hb_xfree( hb_curl->dl_ptr );
         hb_curl->dl_ptr = NULL;
         hb_curl->dl_len = 0;
         hb_curl->dl_pos = 0;
      }
   }
}

/* Constructor/Destructor */

static void PHB_CURL_free( PHB_CURL hb_curl, HB_BOOL bFree )
{
   curl_easy_setopt( hb_curl->curl, CURLOPT_READFUNCTION, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_READDATA, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_WRITEFUNCTION, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_WRITEDATA, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_PROGRESSFUNCTION, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_PROGRESSDATA, NULL );

   /* Some extra safety. Set these to NULL, before freeing their pointers. */
   curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPPOST, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPHEADER, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_HTTP200ALIASES, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_QUOTE, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_POSTQUOTE, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_PREQUOTE, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_TELNETOPTIONS, NULL );
#if LIBCURL_VERSION_NUM >= 0x071400
   curl_easy_setopt( hb_curl->curl, CURLOPT_MAIL_RCPT, NULL );
#endif
#if LIBCURL_VERSION_NUM >= 0x071503
   curl_easy_setopt( hb_curl->curl, CURLOPT_RESOLVE, NULL );
#endif
#if LIBCURL_VERSION_NUM >= 0x072500
   curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYHEADER, NULL );
#endif
#if LIBCURL_VERSION_NUM >= 0x073100
   curl_easy_setopt( hb_curl->curl, CURLOPT_CONNECT_TO, NULL );
#endif

   hb_curl_form_free( &hb_curl->pHTTPPOST_First );
   hb_curl->pHTTPPOST_Last = NULL;
   hb_curl_slist_free( &hb_curl->pHTTPHEADER );
   hb_curl_slist_free( &hb_curl->pHTTP200ALIASES );
   hb_curl_slist_free( &hb_curl->pQUOTE );
   hb_curl_slist_free( &hb_curl->pPOSTQUOTE );
   hb_curl_slist_free( &hb_curl->pPREQUOTE );
   hb_curl_slist_free( &hb_curl->pTELNETOPTIONS );
   hb_curl_slist_free( &hb_curl->pMAIL_RCPT );
   hb_curl_slist_free( &hb_curl->pRESOLVE );
   hb_curl_slist_free( &hb_curl->pPROXYHEADER );
   hb_curl_slist_free( &hb_curl->pCONNECT_TO );

   hb_curl_ul_free( hb_curl );
   hb_curl_dl_free( hb_curl );

   if( hb_curl->pXferInfoCallback )
   {
      hb_itemRelease( hb_curl->pXferInfoCallback );
      hb_curl->pXferInfoCallback = NULL;
   }

   if( hb_curl->pDebugCallback )
   {
      hb_itemRelease( hb_curl->pDebugCallback );
      hb_curl->pDebugCallback = NULL;
   }

   if( hb_curl->pHash )
   {
      hb_hashTableKill( hb_curl->pHash );
      hb_curl->pHash = NULL;
   }

   if( bFree )
   {
      curl_easy_cleanup( hb_curl->curl );
      hb_xfree( hb_curl );
   }
#if LIBCURL_VERSION_NUM >= 0x070C01
   else
      curl_easy_reset( hb_curl->curl );
#endif
}

/* NOTE: Will create a new one. If 'from' is specified, the new one
         will be based on the 'from' one. */

static PHB_CURL PHB_CURL_create( CURL * from )
{
   CURL * curl = from ? curl_easy_duphandle( from ) : curl_easy_init();

   if( curl )
   {
      PHB_CURL hb_curl = ( PHB_CURL ) hb_xgrabz( sizeof( HB_CURL ) );

      hb_curl->curl = curl;

      return hb_curl;
   }
   else
      return NULL;
}

static HB_GARBAGE_FUNC( PHB_CURL_release )
{
   PHB_CURL * hb_curl_ptr = ( PHB_CURL * ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( hb_curl_ptr && *hb_curl_ptr )
   {
      /* Destroy the object */
      PHB_CURL_free( *hb_curl_ptr, HB_TRUE );
      *hb_curl_ptr = NULL;
   }
}

static HB_GARBAGE_FUNC( PHB_CURL_mark )
{
   PHB_CURL * hb_curl_ptr = ( PHB_CURL * ) Cargo;

   if( hb_curl_ptr && *hb_curl_ptr )
   {
      PHB_CURL hb_curl = *hb_curl_ptr;

      if( hb_curl->pXferInfoCallback )
         hb_gcMark( hb_curl->pXferInfoCallback );

      if( hb_curl->pDebugCallback )
         hb_gcMark( hb_curl->pDebugCallback );
   }
}

static const HB_GC_FUNCS s_gcCURLFuncs =
{
   PHB_CURL_release,
   PHB_CURL_mark
};


static void PHB_CURL_ret( PHB_CURL from )
{
   void ** ph = ( void ** ) hb_gcAllocate( sizeof( PHB_CURL ), &s_gcCURLFuncs );

   *ph = PHB_CURL_create( from );

   hb_retptrGC( ph );
}

static void * PHB_CURL_is( int iParam )
{
   return hb_parptrGC( &s_gcCURLFuncs, iParam );
}

static PHB_CURL PHB_CURL_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcCURLFuncs, iParam );

   return ph ? ( PHB_CURL ) *ph : NULL;
}

/* Harbour interface */

HB_FUNC( CURL_EASY_INIT )
{
   PHB_CURL_ret( NULL );
}

HB_FUNC( CURL_EASY_DUPLICATE )
{
   if( PHB_CURL_is( 1 ) )
      PHB_CURL_ret( PHB_CURL_par( 1 ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( CURL_EASY_CLEANUP )
{
   if( PHB_CURL_is( 1 ) )
   {
      void ** ph = ( void ** ) hb_parptrGC( &s_gcCURLFuncs, 1 );

      if( ph && *ph )
      {
         /* Destroy the object */
         PHB_CURL_free( ( PHB_CURL ) *ph, HB_TRUE );
         *ph = NULL;
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( CURL_EASY_RESET )
{
   if( PHB_CURL_is( 1 ) )
   {
      PHB_CURL hb_curl = PHB_CURL_par( 1 );

      if( hb_curl )
         PHB_CURL_free( hb_curl, HB_FALSE );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( CURL_EASY_PAUSE )
{
   if( PHB_CURL_is( 1 ) )
   {
#if LIBCURL_VERSION_NUM >= 0x071200
      PHB_CURL hb_curl = PHB_CURL_par( 1 );

      hb_retnl( hb_curl ? ( long ) curl_easy_pause( hb_curl->curl, hb_parni( 2 ) ) : HB_CURLE_ERROR );
#else
      hb_retnl( HB_CURLE_ERROR );
#endif
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( CURL_EASY_PERFORM )
{
   if( PHB_CURL_is( 1 ) )
   {
      PHB_CURL hb_curl = PHB_CURL_par( 1 );

      hb_retnl( hb_curl ? ( long ) curl_easy_perform( hb_curl->curl ) : HB_CURLE_ERROR );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* NOTE: curl_easy_send( curl, cBuffer, @nSentBytes ) -> nResult */
HB_FUNC( CURL_EASY_SEND )
{
   if( PHB_CURL_is( 1 ) )
   {
      CURLcode res = ( CURLcode ) HB_CURLE_ERROR;
#if LIBCURL_VERSION_NUM >= 0x071202
      PHB_CURL hb_curl = PHB_CURL_par( 1 );

      if( hb_curl )
      {
         size_t size = 0;

         res = curl_easy_send( hb_curl->curl, hb_parcx( 2 ), ( size_t ) hb_parclen( 2 ), &size );

         hb_storns( size, 3 );
      }
#endif
      hb_retnl( ( long ) res );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* NOTE: curl_easy_recv( curl, @cBuffer ) -> nResult */
HB_FUNC( CURL_EASY_RECV )
{
   if( PHB_CURL_is( 1 ) )
   {
      CURLcode res = ( CURLcode ) HB_CURLE_ERROR;
#if LIBCURL_VERSION_NUM >= 0x071202
      PHB_CURL hb_curl = PHB_CURL_par( 1 );

      if( hb_curl )
      {
         size_t size = ( size_t ) hb_parclen( 2 );
         void * buffer;

         if( size < 1024 )
            size = 1024;

         buffer = hb_xgrab( size + 1 );

         res = curl_easy_recv( hb_curl->curl, buffer, size, &size );

         if( ! hb_storclen_buffer( ( char * ) buffer, size, 2 ) )
            hb_xfree( buffer );
      }
#endif
      hb_retnl( ( long ) res );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( CURL_EASY_SETOPT )
{
   if( PHB_CURL_is( 1 ) && HB_ISNUM( 2 ) )
   {
      PHB_CURL hb_curl = PHB_CURL_par( 1 );
      CURLcode res     = ( CURLcode ) HB_CURLE_ERROR;

      if( hb_curl )
      {
         switch( hb_parni( 2 ) )
         {
            /* Behavior */

            case HB_CURLOPT_VERBOSE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_VERBOSE, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_HEADER:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_HEADER, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_NOPROGRESS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_NOPROGRESS, HB_CURL_OPT_BOOL( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x070A00
            case HB_CURLOPT_NOSIGNAL:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_NOSIGNAL, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071500
            case HB_CURLOPT_WILDCARDMATCH:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_WILDCARDMATCH, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif

            /* Callback */

            /* These are hidden on the Harbour level: */
            /* HB_CURLOPT_WRITEFUNCTION */
            /* HB_CURLOPT_WRITEDATA */
            /* HB_CURLOPT_READFUNCTION */
            /* HB_CURLOPT_READDATA */
#if LIBCURL_VERSION_NUM >= 0x070C03
            /* HB_CURLOPT_IOCTLFUNCTION */
            /* HB_CURLOPT_IOCTLDATA */
#endif
            /* HB_CURLOPT_SEEKFUNCTION */
            /* HB_CURLOPT_SEEKDATA */
            /* HB_CURLOPT_SOCKOPTFUNCTION */
            /* HB_CURLOPT_SOCKOPTDATA */
            /* HB_CURLOPT_OPENSOCKETFUNCTION */
            /* HB_CURLOPT_OPENSOCKETDATA */
            /* HB_CURLOPT_PROGRESSFUNCTION */
            /* HB_CURLOPT_PROGRESSDATA */
            /* HB_CURLOPT_HEADERFUNCTION */
            /* HB_CURLOPT_HEADERDATA / CURLOPT_WRITEHEADER */
#if LIBCURL_VERSION_NUM >= 0x070906
            /* HB_CURLOPT_DEBUGFUNCTION */
            /* HB_CURLOPT_DEBUGDATA */
#endif
#if LIBCURL_VERSION_NUM >= 0x070B00
            /* HB_CURLOPT_SSL_CTX_FUNCTION */
            /* HB_CURLOPT_SSL_CTX_DATA */
#endif
            /* HB_CURLOPT_CONV_TO_NETWORK_FUNCTION */
            /* HB_CURLOPT_CONV_FROM_NETWORK_FUNCTION */
            /* HB_CURLOPT_CONV_FROM_UTF8_FUNCTION */

            /* Error */

            /* HB_CURLOPT_ERRORBUFFER */
            /* HB_CURLOPT_STDERR */

            case HB_CURLOPT_FAILONERROR:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FAILONERROR, HB_CURL_OPT_BOOL( 3 ) );
               break;

            /* Network */

            /* This is the only option that must be set before curl_easy_perform() is called. */
            case HB_CURLOPT_URL:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_URL, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_PROXY:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_PROXYPORT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYPORT, hb_parnl( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x070A00
            case HB_CURLOPT_PROXYTYPE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYTYPE, hb_parnl( 3 ) );
               break;
#endif
            case HB_CURLOPT_HTTPPROXYTUNNEL:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPPROXYTUNNEL, HB_CURL_OPT_BOOL( 3 ) );
               break;

            case HB_CURLOPT_INTERFACE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_INTERFACE, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x070F02
            case HB_CURLOPT_LOCALPORT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_LOCALPORT, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_LOCALPORTRANGE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_LOCALPORTRANGE, hb_parnl( 3 ) );
               break;
#endif
            case HB_CURLOPT_DNS_CACHE_TIMEOUT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_DNS_CACHE_TIMEOUT, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_DNS_USE_GLOBAL_CACHE: /* OBSOLETE */
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_DNS_USE_GLOBAL_CACHE, HB_CURL_OPT_BOOL( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x070A00
            case HB_CURLOPT_BUFFERSIZE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_BUFFERSIZE, hb_parnl( 3 ) );
               break;
#endif
            case HB_CURLOPT_PORT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PORT, hb_parnl( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x070B02
            case HB_CURLOPT_TCP_NODELAY:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TCP_NODELAY, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x073100
            case HB_CURLOPT_TCP_FASTOPEN:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TCP_FASTOPEN, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071300
            case HB_CURLOPT_ADDRESS_SCOPE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_ADDRESS_SCOPE, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072D00
            case HB_CURLOPT_DEFAULT_PROTOCOL:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_DEFAULT_PROTOCOL, hb_parc( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071304
            case HB_CURLOPT_PROTOCOLS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROTOCOLS, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_REDIR_PROTOCOLS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_REDIR_PROTOCOLS, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_NOPROXY:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_NOPROXY, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
#if LIBCURL_VERSION_NUM < 0x073100
            case HB_CURLOPT_SOCKS5_GSSAPI_SERVICE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SOCKS5_GSSAPI_SERVICE, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
#endif
            case HB_CURLOPT_SOCKS5_GSSAPI_NEC:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SOCKS5_GSSAPI_NEC, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072B00
#if LIBCURL_VERSION_NUM >= 0x073100
            case HB_CURLOPT_SOCKS5_GSSAPI_SERVICE:
#endif
            case HB_CURLOPT_PROXY_SERVICE_NAME:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_SERVICE_NAME, hb_parc( 3 ) );
               break;
            case HB_CURLOPT_SERVICE_NAME:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SERVICE_NAME, hb_parc( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071900
            case HB_CURLOPT_TCP_KEEPALIVE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TCP_KEEPALIVE, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_TCP_KEEPIDLE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TCP_KEEPIDLE, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_TCP_KEEPINTVL:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TCP_KEEPINTVL, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072800
            case HB_CURLOPT_UNIX_SOCKET_PATH:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_UNIX_SOCKET_PATH, hb_parc( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072A00
            case HB_CURLOPT_PATH_AS_IS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PATH_AS_IS, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072B00
            case HB_CURLOPT_PIPEWAIT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PIPEWAIT, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif

            /* Names and passwords options (Authentication) */

            case HB_CURLOPT_NETRC:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_NETRC, hb_parnl( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x070A09
            case HB_CURLOPT_NETRC_FILE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_NETRC_FILE, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
#endif
            case HB_CURLOPT_USERPWD:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_USERPWD, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x071301
            case HB_CURLOPT_USERNAME:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_USERNAME, hb_parc( 3 ) );
               break;
            case HB_CURLOPT_PASSWORD:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PASSWORD, hb_parc( 3 ) );
               break;
#endif
            case HB_CURLOPT_PROXYUSERPWD:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYUSERPWD, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x071301
            case HB_CURLOPT_PROXYUSERNAME:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYUSERNAME, hb_parc( 3 ) );
               break;
            case HB_CURLOPT_PROXYPASSWORD:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYPASSWORD, hb_parc( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070A06
            case HB_CURLOPT_HTTPAUTH:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPAUTH, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070A07
            case HB_CURLOPT_PROXYAUTH:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYAUTH, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072100
            case HB_CURLOPT_XOAUTH2_BEARER:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_XOAUTH2_BEARER, hb_parc( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072200
            case HB_CURLOPT_LOGIN_OPTIONS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_LOGIN_OPTIONS, hb_parc( 3 ) );
               break;
#endif

            /* HTTP options */

            case HB_CURLOPT_AUTOREFERER:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_AUTOREFERER, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_ACCEPT_ENCODING:
#if LIBCURL_VERSION_NUM >= 0x071506
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_ACCEPT_ENCODING, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
#else
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_ENCODING, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
#endif
               break;
#if LIBCURL_VERSION_NUM >= 0x071506
            case HB_CURLOPT_TRANSFER_ENCODING:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TRANSFER_ENCODING, hb_parnl( 3 ) );
               break;
#endif
            case HB_CURLOPT_FOLLOWLOCATION:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FOLLOWLOCATION, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_UNRESTRICTED_AUTH:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_UNRESTRICTED_AUTH, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_MAXREDIRS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAXREDIRS, hb_parnl( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x071101
            case HB_CURLOPT_POSTREDIR:
#if LIBCURL_VERSION_NUM >= 0x071301
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_POSTREDIR, HB_CURL_OPT_BOOL( 3 ) );
#else
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_POST301, HB_CURL_OPT_BOOL( 3 ) );
#endif
               break;
#endif
            case HB_CURLOPT_PUT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PUT, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_POST:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_POST, HB_CURL_OPT_BOOL( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x071101
            case HB_CURLOPT_POSTFIELDS:
            case HB_CURLOPT_COPYPOSTFIELDS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_COPYPOSTFIELDS, hb_parc( 3 ) );
               break;
#endif
            case HB_CURLOPT_POSTFIELDSIZE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_POSTFIELDSIZE, hb_parnl( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x070B01
            case HB_CURLOPT_POSTFIELDSIZE_LARGE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_POSTFIELDSIZE_LARGE, HB_CURL_OPT_LARGENUM( 3 ) );
               break;
#endif
            case HB_CURLOPT_HTTPPOST:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               if( pArray )
               {
                  HB_SIZE ulPos;
                  HB_SIZE ulArrayLen = hb_arrayLen( pArray );

                  for( ulPos = 0; ulPos < ulArrayLen; ++ulPos )
                  {
                     PHB_ITEM pSubArray = hb_arrayGetItemPtr( pArray, ulPos + 1 );

                     if( pSubArray && HB_IS_ARRAY( pSubArray ) && hb_arrayLen( pSubArray ) >= 2 )
                        curl_formadd( &hb_curl->pHTTPPOST_First,
                                      &hb_curl->pHTTPPOST_Last,
                                      CURLFORM_COPYNAME, hb_arrayGetCPtr( pSubArray, 1 ),
                                      CURLFORM_NAMELENGTH, ( long ) hb_arrayGetCLen( pSubArray, 1 ),
                                      CURLFORM_FILE, hb_curl_StrHash( hb_curl, hb_arrayGetCPtr( pSubArray, 2 ) ),
                                      CURLFORM_END );
                  }

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPPOST, hb_curl->pHTTPPOST_First );
               }
               break;
            }
            case HB_CURLOPT_HTTPPOST_CONTENT:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               if( pArray )
               {
                  HB_SIZE ulPos;
                  HB_SIZE ulArrayLen = hb_arrayLen( pArray );

                  for( ulPos = 0; ulPos < ulArrayLen; ++ulPos )
                  {
                     PHB_ITEM pSubArray = hb_arrayGetItemPtr( pArray, ulPos + 1 );

                     if( pSubArray && HB_IS_ARRAY( pSubArray ) && hb_arrayLen( pSubArray ) >= 2 )
#if defined( CURLFORM_CONTENTLEN )
                        curl_formadd( &hb_curl->pHTTPPOST_First,
                                      &hb_curl->pHTTPPOST_Last,
                                      CURLFORM_COPYNAME, hb_arrayGetCPtr( pSubArray, 1 ),
                                      CURLFORM_NAMELENGTH, ( long ) hb_arrayGetCLen( pSubArray, 1 ),
                                      CURLFORM_COPYCONTENTS, hb_arrayGetCPtr( pSubArray, 2 ),
                                      CURLFORM_CONTENTLEN, ( curl_off_t ) hb_arrayGetCLen( pSubArray, 2 ),
                                      CURLFORM_END );
#else
                        curl_formadd( &hb_curl->pHTTPPOST_First,
                                      &hb_curl->pHTTPPOST_Last,
                                      CURLFORM_COPYNAME, hb_arrayGetCPtr( pSubArray, 1 ),
                                      CURLFORM_NAMELENGTH, ( long ) hb_arrayGetCLen( pSubArray, 1 ),
                                      CURLFORM_COPYCONTENTS, hb_arrayGetCPtr( pSubArray, 2 ),
                                      CURLFORM_CONTENTSLENGTH, ( long ) hb_arrayGetCLen( pSubArray, 2 ),
                                      CURLFORM_END );
#endif
                  }

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPPOST, hb_curl->pHTTPPOST_First );
               }
               break;
            }
            case HB_CURLOPT_HTTPPOST_FORM:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               if( pArray )
               {
                  HB_SIZE ulPos;
                  HB_SIZE ulArrayLen = hb_arrayLen( pArray );

                  for( ulPos = 0; ulPos < ulArrayLen; ++ulPos )
                  {
                     PHB_ITEM pSubArray = hb_arrayGetItemPtr( pArray, ulPos + 1 );

                     if( pSubArray && HB_IS_ARRAY( pSubArray ) && hb_arrayLen( pSubArray ) >= 3 )
                     {
                        switch( hb_arrayGetNI( pSubArray, 1 ) )
                        {
                           case HB_CURLOPT_HTTPPOST_FORM_CONTENT:
#if defined( CURLFORM_CONTENTLEN )
                              curl_formadd( &hb_curl->pHTTPPOST_First,
                                            &hb_curl->pHTTPPOST_Last,
                                            CURLFORM_COPYNAME, hb_arrayGetCPtr( pSubArray, 2 ),
                                            CURLFORM_NAMELENGTH, ( long ) hb_arrayGetCLen( pSubArray, 2 ),
                                            CURLFORM_COPYCONTENTS, hb_arrayGetCPtr( pSubArray, 3 ),
                                            CURLFORM_CONTENTLEN, ( curl_off_t ) hb_arrayGetCLen( pSubArray, 3 ),
                                            CURLFORM_END );
#else
                              curl_formadd( &hb_curl->pHTTPPOST_First,
                                            &hb_curl->pHTTPPOST_Last,
                                            CURLFORM_COPYNAME, hb_arrayGetCPtr( pSubArray, 2 ),
                                            CURLFORM_NAMELENGTH, ( long ) hb_arrayGetCLen( pSubArray, 2 ),
                                            CURLFORM_COPYCONTENTS, hb_arrayGetCPtr( pSubArray, 3 ),
                                            CURLFORM_CONTENTSLENGTH, ( long ) hb_arrayGetCLen( pSubArray, 3 ),
                                            CURLFORM_END );
#endif

                              break;
                           case HB_CURLOPT_HTTPPOST_FORM_FILE:
                              curl_formadd( &hb_curl->pHTTPPOST_First,
                                            &hb_curl->pHTTPPOST_Last,
                                            CURLFORM_COPYNAME, hb_arrayGetCPtr( pSubArray, 2 ),
                                            CURLFORM_NAMELENGTH, ( long ) hb_arrayGetCLen( pSubArray, 2 ),
                                            CURLFORM_FILE, hb_curl_StrHash( hb_curl, hb_arrayGetCPtr( pSubArray, 3 ) ),
                                            CURLFORM_END );
                              break;
                        }
                     }
                  }

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPPOST, hb_curl->pHTTPPOST_First );
               }
               break;
            }
            case HB_CURLOPT_REFERER:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_REFERER, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_USERAGENT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_USERAGENT, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_HTTPHEADER:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPHEADER, NULL );
               hb_curl_slist_free( &hb_curl->pHTTPHEADER );

               if( pArray )
               {
                  HB_SIZE ulPos;
                  HB_SIZE ulArrayLen = hb_arrayLen( pArray );

                  for( ulPos = 0; ulPos < ulArrayLen; ++ulPos )
                     hb_curl->pHTTPHEADER = curl_slist_append( hb_curl->pHTTPHEADER, hb_arrayGetCPtr( pArray, ulPos + 1 ) );

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPHEADER, hb_curl->pHTTPHEADER );
               }
               break;
            }
#if LIBCURL_VERSION_NUM >= 0x072500
            case HB_CURLOPT_HEADEROPT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_HEADEROPT, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_PROXYHEADER:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYHEADER, NULL );
               hb_curl_slist_free( &hb_curl->pPROXYHEADER );

               if( pArray )
               {
                  HB_SIZE ulPos;
                  HB_SIZE ulArrayLen = hb_arrayLen( pArray );

                  for( ulPos = 0; ulPos < ulArrayLen; ++ulPos )
                     hb_curl->pPROXYHEADER = curl_slist_append( hb_curl->pPROXYHEADER, hb_arrayGetCPtr( pArray, ulPos + 1 ) );

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYHEADER, hb_curl->pPROXYHEADER );
               }
               break;
            }
#endif
#if LIBCURL_VERSION_NUM >= 0x073100
            case HB_CURLOPT_CONNECT_TO:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               curl_easy_setopt( hb_curl->curl, CURLOPT_CONNECT_TO, NULL );
               hb_curl_slist_free( &hb_curl->pCONNECT_TO );

               if( pArray )
               {
                  HB_SIZE ulPos;
                  HB_SIZE ulArrayLen = hb_arrayLen( pArray );

                  for( ulPos = 0; ulPos < ulArrayLen; ++ulPos )
                     hb_curl->pCONNECT_TO = curl_slist_append( hb_curl->pCONNECT_TO, hb_arrayGetCPtr( pArray, ulPos + 1 ) );

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_CONNECT_TO, hb_curl->pCONNECT_TO );
               }
               break;
            }
#endif
#if LIBCURL_VERSION_NUM >= 0x070A03
            case HB_CURLOPT_HTTP200ALIASES:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               curl_easy_setopt( hb_curl->curl, CURLOPT_HTTP200ALIASES, NULL );
               hb_curl_slist_free( &hb_curl->pHTTP200ALIASES );

               if( pArray )
               {
                  HB_SIZE ulPos;
                  HB_SIZE ulArrayLen = hb_arrayLen( pArray );

                  for( ulPos = 0; ulPos < ulArrayLen; ++ulPos )
                     hb_curl->pHTTP200ALIASES = curl_slist_append( hb_curl->pHTTP200ALIASES, hb_arrayGetCPtr( pArray, ulPos + 1 ) );

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTP200ALIASES, hb_curl->pHTTP200ALIASES );
               }
               break;
            }
#endif
            case HB_CURLOPT_COOKIE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_COOKIE, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_COOKIEFILE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_COOKIEFILE, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_COOKIEJAR:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_COOKIEJAR, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_COOKIESESSION:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_COOKIESESSION, HB_CURL_OPT_BOOL( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x070E01
            case HB_CURLOPT_COOKIELIST:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_COOKIELIST, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
#endif
            case HB_CURLOPT_HTTPGET:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPGET, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_HTTP_VERSION:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTP_VERSION, hb_parnl( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x070E01
            case HB_CURLOPT_IGNORE_CONTENT_LENGTH:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_IGNORE_CONTENT_LENGTH, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071002
            case HB_CURLOPT_HTTP_CONTENT_DECODING:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTP_CONTENT_DECODING, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_HTTP_TRANSFER_DECODING:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTP_TRANSFER_DECODING, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072400
            case HB_CURLOPT_EXPECT_100_TIMEOUT_MS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_EXPECT_100_TIMEOUT_MS, hb_parnl( 3 ) );
               break;
#endif

               /* SMTP options */

#if LIBCURL_VERSION_NUM >= 0x071400
            case HB_CURLOPT_MAIL_FROM:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAIL_FROM, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_MAIL_RCPT:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               curl_easy_setopt( hb_curl->curl, CURLOPT_MAIL_RCPT, NULL );
               hb_curl_slist_free( &hb_curl->pMAIL_RCPT );

               if( pArray )
               {
                  HB_SIZE ulPos;
                  HB_SIZE ulArrayLen = hb_arrayLen( pArray );

                  for( ulPos = 0; ulPos < ulArrayLen; ++ulPos )
                     hb_curl->pMAIL_RCPT = curl_slist_append( hb_curl->pMAIL_RCPT, hb_arrayGetCPtr( pArray, ulPos + 1 ) );

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAIL_RCPT, hb_curl->pMAIL_RCPT );
               }
               break;
            }
#endif
#if LIBCURL_VERSION_NUM >= 0x071900
            case HB_CURLOPT_MAIL_AUTH:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAIL_AUTH, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
#endif

               /* TFTP options */

#if LIBCURL_VERSION_NUM >= 0x071304
            case HB_CURLOPT_TFTP_BLKSIZE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TFTP_BLKSIZE, hb_parnl( 3 ) );
               break;
#endif

            /* FTP options */

            case HB_CURLOPT_FTPPORT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTPPORT, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_QUOTE:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               curl_easy_setopt( hb_curl->curl, CURLOPT_QUOTE, NULL );
               hb_curl_slist_free( &hb_curl->pQUOTE );

               if( pArray )
               {
                  HB_SIZE ulPos;
                  HB_SIZE ulArrayLen = hb_arrayLen( pArray );

                  for( ulPos = 0; ulPos < ulArrayLen; ++ulPos )
                     hb_curl->pQUOTE = curl_slist_append( hb_curl->pQUOTE, hb_arrayGetCPtr( pArray, ulPos + 1 ) );

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_QUOTE, hb_curl->pQUOTE );
               }
               break;
            }
            case HB_CURLOPT_POSTQUOTE:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               curl_easy_setopt( hb_curl->curl, CURLOPT_POSTQUOTE, NULL );
               hb_curl_slist_free( &hb_curl->pPOSTQUOTE );

               if( pArray )
               {
                  HB_SIZE ulPos;
                  HB_SIZE ulArrayLen = hb_arrayLen( pArray );

                  for( ulPos = 0; ulPos < ulArrayLen; ++ulPos )
                     hb_curl->pPOSTQUOTE = curl_slist_append( hb_curl->pPOSTQUOTE, hb_arrayGetCPtr( pArray, ulPos + 1 ) );

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_POSTQUOTE, hb_curl->pPOSTQUOTE );
               }
               break;
            }
            case HB_CURLOPT_PREQUOTE:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               curl_easy_setopt( hb_curl->curl, CURLOPT_PREQUOTE, NULL );
               hb_curl_slist_free( &hb_curl->pPREQUOTE );

               if( pArray )
               {
                  HB_SIZE ulPos;
                  HB_SIZE ulArrayLen = hb_arrayLen( pArray );

                  for( ulPos = 0; ulPos < ulArrayLen; ++ulPos )
                     hb_curl->pQUOTE = curl_slist_append( hb_curl->pPREQUOTE, hb_arrayGetCPtr( pArray, ulPos + 1 ) );

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_PREQUOTE, hb_curl->pPREQUOTE );
               }
               break;
            }
            case HB_CURLOPT_DIRLISTONLY: /* HB_CURLOPT_FTPLISTONLY */
#if LIBCURL_VERSION_NUM > 0x071004
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_DIRLISTONLY, HB_CURL_OPT_BOOL( 3 ) );
#else
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTPLISTONLY, HB_CURL_OPT_BOOL( 3 ) );
#endif
               break;
            case HB_CURLOPT_APPEND: /* HB_CURLOPT_FTPAPPEND */
#if LIBCURL_VERSION_NUM > 0x071004
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_APPEND, HB_CURL_OPT_BOOL( 3 ) );
#else
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTPAPPEND, HB_CURL_OPT_BOOL( 3 ) );
#endif
               break;
#if LIBCURL_VERSION_NUM >= 0x070A05
            case HB_CURLOPT_FTP_USE_EPRT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_USE_EPRT, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
            case HB_CURLOPT_FTP_USE_EPSV:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_USE_EPSV, HB_CURL_OPT_BOOL( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x071400
            case HB_CURLOPT_FTP_USE_PRET:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_USE_PRET, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070A07
            case HB_CURLOPT_FTP_CREATE_MISSING_DIRS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_CREATE_MISSING_DIRS, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070A08
            case HB_CURLOPT_FTP_RESPONSE_TIMEOUT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_RESPONSE_TIMEOUT, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070F05
            case HB_CURLOPT_FTP_ALTERNATIVE_TO_USER:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_ALTERNATIVE_TO_USER, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070E02
            case HB_CURLOPT_FTP_SKIP_PASV_IP:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_SKIP_PASV_IP, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
            case HB_CURLOPT_USE_SSL:
#if LIBCURL_VERSION_NUM > 0x071004
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_USE_SSL, hb_parnl( 3 ) );
#elif LIBCURL_VERSION_NUM >= 0x070B00
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_SSL, hb_parnl( 3 ) );
#endif
               break;
#if LIBCURL_VERSION_NUM >= 0x070C02
            case HB_CURLOPT_FTPSSLAUTH:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTPSSLAUTH, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071001
            case HB_CURLOPT_FTP_SSL_CCC:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_SSL_CCC, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070D00
            case HB_CURLOPT_FTP_ACCOUNT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_ACCOUNT, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070F01
            case HB_CURLOPT_FTP_FILEMETHOD:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_FILEMETHOD, hb_parnl( 3 ) );
               break;
#endif

               /* RTSP */

#if LIBCURL_VERSION_NUM >= 0x071400
            case HB_CURLOPT_RTSP_REQUEST:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_RTSP_REQUEST, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_RTSP_SESSION_ID:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_RTSP_SESSION_ID, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_RTSP_STREAM_URI:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_RTSP_STREAM_URI, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_RTSP_TRANSPORT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_RTSP_TRANSPORT, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_RTSP_CLIENT_CSEQ:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_RTSP_CLIENT_CSEQ, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_RTSP_SERVER_CSEQ:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_RTSP_SERVER_CSEQ, hb_parnl( 3 ) );
               break;
#endif

            /* Protocol */

            case HB_CURLOPT_TRANSFERTEXT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TRANSFERTEXT, HB_CURL_OPT_BOOL( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x071200
            case HB_CURLOPT_PROXY_TRANSFER_MODE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_TRANSFER_MODE, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
            case HB_CURLOPT_CRLF:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_CRLF, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_RANGE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_RANGE, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_RESUME_FROM:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_RESUME_FROM, hb_parnl( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x070B00
            case HB_CURLOPT_RESUME_FROM_LARGE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_RESUME_FROM_LARGE, HB_CURL_OPT_LARGENUM( 3 ) );
               break;
#endif
            case HB_CURLOPT_CUSTOMREQUEST:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_CUSTOMREQUEST, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_FILETIME:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FILETIME, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_NOBODY:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_NOBODY, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_INFILESIZE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_INFILESIZE, hb_parnl( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x070B00
            case HB_CURLOPT_INFILESIZE_LARGE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_INFILESIZE_LARGE, HB_CURL_OPT_LARGENUM( 3 ) );
               break;
#endif
            case HB_CURLOPT_UPLOAD:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_UPLOAD, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_DOWNLOAD: /* Harbour extension */
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_UPLOAD, ! HB_CURL_OPT_BOOL( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x070A08
            case HB_CURLOPT_MAXFILESIZE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAXFILESIZE, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070B00
            case HB_CURLOPT_MAXFILESIZE_LARGE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAXFILESIZE_LARGE, HB_CURL_OPT_LARGENUM( 3 ) );
               break;
#endif
            case HB_CURLOPT_TIMECONDITION:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TIMECONDITION, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_TIMEVALUE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TIMEVALUE, hb_parnl( 3 ) );
               break;

            /* Connection */

            case HB_CURLOPT_TIMEOUT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TIMEOUT, hb_parnl( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x071002
            case HB_CURLOPT_TIMEOUT_MS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TIMEOUT_MS, hb_parnl( 3 ) );
               break;
#endif
            case HB_CURLOPT_LOW_SPEED_LIMIT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_LOW_SPEED_LIMIT, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_LOW_SPEED_TIME:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_LOW_SPEED_TIME, hb_parnl( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x070F05
            case HB_CURLOPT_MAX_SEND_SPEED_LARGE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAX_SEND_SPEED_LARGE, HB_CURL_OPT_LARGENUM( 3 ) );
               break;
            case HB_CURLOPT_MAX_RECV_SPEED_LARGE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAX_RECV_SPEED_LARGE, HB_CURL_OPT_LARGENUM( 3 ) );
               break;
#endif
            case HB_CURLOPT_MAXCONNECTS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAXCONNECTS, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_CLOSEPOLICY: /* OBSOLETE, does nothing. */
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
#if LIBCURL_VERSION_NUM >= 0x071002
            case HB_CURLOPT_CONNECTTIMEOUT_MS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_CONNECTTIMEOUT_MS, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070A08
            case HB_CURLOPT_IPRESOLVE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_IPRESOLVE, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070F02
            case HB_CURLOPT_CONNECT_ONLY:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_CONNECT_ONLY, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071503
            case HB_CURLOPT_RESOLVE:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               curl_easy_setopt( hb_curl->curl, CURLOPT_RESOLVE, NULL );
               hb_curl_slist_free( &hb_curl->pRESOLVE );

               if( pArray )
               {
                  HB_SIZE ulPos;
                  HB_SIZE ulArrayLen = hb_arrayLen( pArray );

                  for( ulPos = 0; ulPos < ulArrayLen; ++ulPos )
                     hb_curl->pRESOLVE = curl_slist_append( hb_curl->pRESOLVE, hb_arrayGetCPtr( pArray, ulPos + 1 ) );

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_RESOLVE, hb_curl->pRESOLVE );
               }
               break;
            }
#endif
#if LIBCURL_VERSION_NUM >= 0x071800
            case HB_CURLOPT_DNS_SERVERS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_DNS_SERVERS, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_ACCEPTTIMEOUT_MS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_ACCEPTTIMEOUT_MS, hb_parnl( 3 ) );
               break;
#endif

            /* SSL and Security */

            case HB_CURLOPT_SSLCERT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLCERT, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x070903
            case HB_CURLOPT_SSLCERTTYPE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLCERTTYPE, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
#endif
            case HB_CURLOPT_SSLKEY:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLKEY, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_SSLKEYTYPE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLKEYTYPE, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_KEYPASSWD:
#if LIBCURL_VERSION_NUM > 0x071004
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_KEYPASSWD, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
#elif LIBCURL_VERSION_NUM > 0x070902
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLKEYPASSWD, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
#else
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLCERTPASSWD, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
#endif
               break;
            case HB_CURLOPT_SSLENGINE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLENGINE, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_SSLENGINE_DEFAULT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLENGINE_DEFAULT, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_SSLVERSION:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLVERSION, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_SSL_VERIFYPEER:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_VERIFYPEER, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_CAINFO:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_CAINFO, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_CAPATH:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_CAPATH, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_RANDOM_FILE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_RANDOM_FILE, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_EGDSOCKET:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_EGDSOCKET, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_SSL_VERIFYHOST:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_VERIFYHOST, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_SSL_CIPHER_LIST:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_CIPHER_LIST, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_SSL_SESSIONID_CACHE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_SESSIONID_CACHE, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_KRBLEVEL: /* HB_CURLOPT_KRB4LEVEL */
#if LIBCURL_VERSION_NUM > 0x071003
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_KRBLEVEL, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
#else
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_KRB4LEVEL, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
#endif
               break;
#if LIBCURL_VERSION_NUM >= 0x071300
            case HB_CURLOPT_CRLFILE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_CRLFILE, hb_parc( 3 ) );
               break;
            case HB_CURLOPT_ISSUERCERT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_ISSUERCERT, hb_parc( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071301
            case HB_CURLOPT_CERTINFO:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_CERTINFO, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071600
            case HB_CURLOPT_GSSAPI_DELEGATION:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_GSSAPI_DELEGATION, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071900
            case HB_CURLOPT_SSL_OPTIONS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_OPTIONS, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072400
            case HB_CURLOPT_SSL_ENABLE_ALPN:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_ENABLE_ALPN, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_SSL_ENABLE_NPN:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_ENABLE_NPN, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072700
            case HB_CURLOPT_PINNEDPUBLICKEY:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PINNEDPUBLICKEY, hb_parc( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072900
            case HB_CURLOPT_SSL_VERIFYSTATUS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_VERIFYSTATUS, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072A00
            case HB_CURLOPT_SSL_FALSESTART:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_FALSESTART, hb_parnl( 3 ) );
               break;
#endif

               /* SSH options */

#if LIBCURL_VERSION_NUM >= 0x071001
            case HB_CURLOPT_SSH_AUTH_TYPES:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSH_AUTH_TYPES, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071101
            case HB_CURLOPT_SSH_HOST_PUBLIC_KEY_MD5:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSH_HOST_PUBLIC_KEY_MD5, hb_parc( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071001
            case HB_CURLOPT_SSH_PUBLIC_KEYFILE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSH_PUBLIC_KEYFILE, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
            case HB_CURLOPT_SSH_PRIVATE_KEYFILE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSH_PRIVATE_KEYFILE, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071306
            case HB_CURLOPT_SSH_KNOWNHOSTS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSH_KNOWNHOSTS, hb_curl_StrHash( hb_curl, hb_parc( 3 ) ) );
               break;
#endif

               /* Other options */

#if LIBCURL_VERSION_NUM >= 0x070A03
            case HB_CURLOPT_PRIVATE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PRIVATE, hb_parptr( 3 ) );
               break;
#endif

               /* HB_CURLOPT_SHARE */

#if LIBCURL_VERSION_NUM >= 0x071004
            case HB_CURLOPT_NEW_FILE_PERMS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_NEW_FILE_PERMS, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_NEW_DIRECTORY_PERMS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_NEW_DIRECTORY_PERMS, hb_parnl( 3 ) );
               break;
#endif

            /* Telnet options */

            case HB_CURLOPT_TELNETOPTIONS:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               curl_easy_setopt( hb_curl->curl, CURLOPT_TELNETOPTIONS, NULL );
               hb_curl_slist_free( &hb_curl->pTELNETOPTIONS );

               if( pArray )
               {
                  HB_SIZE ulPos;
                  HB_SIZE ulArrayLen = hb_arrayLen( pArray );

                  for( ulPos = 0; ulPos < ulArrayLen; ++ulPos )
                     hb_curl->pTELNETOPTIONS = curl_slist_append( hb_curl->pTELNETOPTIONS, hb_arrayGetCPtr( pArray, ulPos + 1 ) );

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_TELNETOPTIONS, hb_curl->pTELNETOPTIONS );
               }
               break;
            }

            /* Undocumented */

            /* HB_CURLOPT_WRITEINFO */

            /* Harbour specials */

            case HB_CURLOPT_XFERINFOBLOCK:
            {
               PHB_ITEM pCallback = hb_param( 3, HB_IT_EVALITEM );

               if( hb_curl->pXferInfoCallback )
               {
#if LIBCURL_VERSION_NUM >= 0x072000
                  curl_easy_setopt( hb_curl->curl, CURLOPT_XFERINFOFUNCTION, NULL );
                  curl_easy_setopt( hb_curl->curl, CURLOPT_XFERINFODATA, NULL );
#else
                  curl_easy_setopt( hb_curl->curl, CURLOPT_PROGRESSFUNCTION, NULL );
                  curl_easy_setopt( hb_curl->curl, CURLOPT_PROGRESSDATA, NULL );
#endif

                  hb_itemRelease( hb_curl->pXferInfoCallback );
                  hb_curl->pXferInfoCallback = NULL;
               }

               if( pCallback )
               {
                  hb_curl->pXferInfoCallback = hb_itemNew( pCallback );
                  /* unlock the item so GC will not mark them as used */
                  hb_gcUnlock( hb_curl->pXferInfoCallback );

#if LIBCURL_VERSION_NUM >= 0x072000
                  curl_easy_setopt( hb_curl->curl, CURLOPT_XFERINFOFUNCTION, hb_curl_xferinfo_callback );
                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_XFERINFODATA, hb_curl->pXferInfoCallback );
#else
                  curl_easy_setopt( hb_curl->curl, CURLOPT_PROGRESSFUNCTION, hb_curl_progress_callback );
                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROGRESSDATA, hb_curl->pXferInfoCallback );
#endif
               }
               break;
            }

            case HB_CURLOPT_DEBUGBLOCK:
            {
#if LIBCURL_VERSION_NUM >= 0x070906
               PHB_ITEM pCallback = hb_param( 3, HB_IT_EVALITEM );

               if( hb_curl->pDebugCallback )
               {
                  curl_easy_setopt( hb_curl->curl, CURLOPT_DEBUGFUNCTION, NULL );
                  curl_easy_setopt( hb_curl->curl, CURLOPT_DEBUGDATA, NULL );

                  hb_itemRelease( hb_curl->pDebugCallback );
                  hb_curl->pDebugCallback = NULL;
               }

               if( pCallback )
               {
                  hb_curl->pDebugCallback = hb_itemNew( pCallback );
                  /* unlock the item so GC will not mark them as used */
                  hb_gcUnlock( hb_curl->pDebugCallback );

                  curl_easy_setopt( hb_curl->curl, CURLOPT_DEBUGFUNCTION, hb_curl_debug_callback );
                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_DEBUGDATA, hb_curl->pDebugCallback );
               }
#endif
               break;
            }

            case HB_CURLOPT_UL_FILE_SETUP:
            {
               HB_BOOL lSuccess = HB_TRUE;

               hb_curl_ul_free( hb_curl );

               if( HB_ISCHAR( 3 ) )
                  hb_curl->ul_name = hb_strdup( hb_parc( 3 ) );
               else if( hb_fileParamGet( 3 ) )
                  hb_curl->ul_file = hb_fileParamGet( 3 );
               else if( HB_ISNUM( 3 ) )
               {
                  hb_curl->ul_file   = hb_fileFromHandle( hb_numToHandle( hb_parnint( 3 ) ) );
                  hb_curl->ul_onfree = HB_CURL_ONFREE_DETACH;
               }
               else
                  lSuccess = HB_FALSE;

               if( lSuccess )
               {
                  curl_easy_setopt( hb_curl->curl, CURLOPT_READFUNCTION, hb_curl_read_file_callback );
                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_READDATA, hb_curl );
               }
               break;
            }
            case HB_CURLOPT_UL_FILE_CLOSE:
               hb_curl_ul_free( hb_curl );
               res = CURLE_OK;
               break;

            case HB_CURLOPT_DL_FILE_SETUP:
            {
               HB_BOOL lSuccess = HB_TRUE;

               hb_curl_dl_free( hb_curl );

               if( HB_ISCHAR( 3 ) )
                  hb_curl->dl_name = hb_strdup( hb_parc( 3 ) );
               else if( hb_fileParamGet( 3 ) )
                  hb_curl->dl_file = hb_fileParamGet( 3 );
               else if( HB_ISNUM( 3 ) )
               {
                  hb_curl->dl_file   = hb_fileFromHandle( hb_numToHandle( hb_parnint( 3 ) ) );
                  hb_curl->dl_onfree = HB_CURL_ONFREE_DETACH;
               }
               else
                  lSuccess = HB_FALSE;

               if( lSuccess )
               {
                  curl_easy_setopt( hb_curl->curl, CURLOPT_WRITEFUNCTION, hb_curl_write_file_callback );
                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_WRITEDATA, hb_curl );
               }
               break;
            }
            case HB_CURLOPT_DL_FILE_CLOSE:
               hb_curl_dl_free( hb_curl );
               res = CURLE_OK;
               break;

            case HB_CURLOPT_UL_BUFF_SETUP:
               hb_curl_ul_free( hb_curl );

               if( HB_ISCHAR( 3 ) )
               {
                  hb_curl->ul_pos = 0;
                  hb_curl->ul_len = hb_parclen( 3 );
                  hb_curl->ul_ptr = ( unsigned char * ) hb_xgrab( hb_curl->ul_len + 1 );

                  hb_xmemcpy( hb_curl->ul_ptr, hb_parc( 3 ), hb_curl->ul_len );

                  curl_easy_setopt( hb_curl->curl, CURLOPT_READFUNCTION, hb_curl_read_buff_callback );
                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_READDATA, hb_curl );
               }
               break;

            case HB_CURLOPT_DL_BUFF_SETUP:
               hb_curl_dl_free( hb_curl );

               hb_curl->dl_pos = 0;
               hb_curl->dl_len = hb_parnldef( 3, HB_CURL_DL_BUFF_SIZE_INIT );
               hb_curl->dl_ptr = ( unsigned char * ) hb_xgrab( hb_curl->dl_len + 1 );

               curl_easy_setopt( hb_curl->curl, CURLOPT_WRITEFUNCTION, hb_curl_write_buff_callback );
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_WRITEDATA, hb_curl );
               break;

            case HB_CURLOPT_DL_BUFF_GET:
               hb_storclen( ( char * ) hb_curl->dl_ptr, hb_curl->dl_pos, 3 );
               if( hb_curl->dl_ptr )
                  res = CURLE_OK;
               break;

            case HB_CURLOPT_UL_NULL_SETUP:
               hb_curl_ul_free( hb_curl );

               curl_easy_setopt( hb_curl->curl, CURLOPT_READFUNCTION, hb_curl_read_dummy_callback );
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_READDATA, hb_curl );
               break;
         }
      }

      hb_retnl( ( long ) res );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Harbour extension. */
HB_FUNC( CURL_EASY_DL_BUFF_GET )
{
   if( PHB_CURL_is( 1 ) )
   {
      PHB_CURL hb_curl = PHB_CURL_par( 1 );

      if( hb_curl )
         hb_retclen( ( char * ) hb_curl->dl_ptr, hb_curl->dl_pos );
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

#define HB_CURL_INFO_TYPE_INVALID    0
#define HB_CURL_INFO_TYPE_STR        1
#define HB_CURL_INFO_TYPE_PTR        2
#define HB_CURL_INFO_TYPE_PTR_SLIST  3
#define HB_CURL_INFO_TYPE_LONG       4
#define HB_CURL_INFO_TYPE_DOUBLE     5
#define HB_CURL_INFO_TYPE_SLIST      6

#define HB_CURL_EASY_GETINFO( hb_curl, n, p )  ( hb_curl ? curl_easy_getinfo( hb_curl->curl, n, p ) : ( CURLcode ) HB_CURLE_ERROR )

/* NOTE: curl_easy_getinfo( curl, x, @nError ) -> xValue */
HB_FUNC( CURL_EASY_GETINFO )
{
   if( PHB_CURL_is( 1 ) && HB_ISNUM( 2 ) )
   {
      PHB_CURL hb_curl = PHB_CURL_par( 1 );
      CURLcode res     = ( CURLcode ) HB_CURLE_ERROR;

      int type = HB_CURL_INFO_TYPE_INVALID;

      char * ret_string = NULL;
      char * ret_ptr    = NULL;
      long   ret_long   = 0;
      struct curl_slist * ret_slist = NULL;
      double ret_double = 0.0;

      switch( hb_parni( 2 ) )
      {
         case HB_CURLINFO_EFFECTIVE_URL:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_EFFECTIVE_URL, &ret_string );
            type = HB_CURL_INFO_TYPE_STR;
            break;
         case HB_CURLINFO_RESPONSE_CODE:
#if LIBCURL_VERSION_NUM > 0x070A07
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_RESPONSE_CODE, &ret_long );
#else
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_HTTP_CODE, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_HTTP_CONNECTCODE:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_HTTP_CONNECTCODE, &ret_long );
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_FILETIME:
#if LIBCURL_VERSION_NUM >= 0x070500
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_FILETIME, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_TOTAL_TIME:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_TOTAL_TIME, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
            break;
         case HB_CURLINFO_NAMELOOKUP_TIME:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_NAMELOOKUP_TIME, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
            break;
         case HB_CURLINFO_CONNECT_TIME:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_CONNECT_TIME, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
            break;
         case HB_CURLINFO_PRETRANSFER_TIME:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_PRETRANSFER_TIME, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
            break;
         case HB_CURLINFO_STARTTRANSFER_TIME:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_STARTTRANSFER_TIME, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
            break;
         case HB_CURLINFO_REDIRECT_TIME:
#if LIBCURL_VERSION_NUM >= 0x070907
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_REDIRECT_TIME, &ret_double );
#endif
            type = HB_CURL_INFO_TYPE_DOUBLE;
            break;
         case HB_CURLINFO_REDIRECT_COUNT:
#if LIBCURL_VERSION_NUM >= 0x070907
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_REDIRECT_COUNT, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_REDIRECT_URL:
#if LIBCURL_VERSION_NUM >= 0x071202
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_REDIRECT_URL, &ret_string );
#endif
            type = HB_CURL_INFO_TYPE_STR;
            break;
         case HB_CURLINFO_SIZE_UPLOAD:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_SIZE_UPLOAD, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
            break;
         case HB_CURLINFO_SIZE_DOWNLOAD:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_SIZE_DOWNLOAD, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
            break;
         case HB_CURLINFO_SPEED_DOWNLOAD:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_SPEED_DOWNLOAD, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
            break;
         case HB_CURLINFO_SPEED_UPLOAD:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_SPEED_UPLOAD, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
            break;
         case HB_CURLINFO_HEADER_SIZE:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_HEADER_SIZE, &ret_long );
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_REQUEST_SIZE:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_REQUEST_SIZE, &ret_long );
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_SSL_VERIFYRESULT:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_SSL_VERIFYRESULT, &ret_long );
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_SSL_ENGINES:
#if LIBCURL_VERSION_NUM >= 0x071203
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_SSL_ENGINES, &ret_slist );
#endif
            type = HB_CURL_INFO_TYPE_SLIST;
            break;
         case HB_CURLINFO_CONTENT_LENGTH_DOWNLOAD:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_CONTENT_LENGTH_DOWNLOAD, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
            break;
         case HB_CURLINFO_CONTENT_LENGTH_UPLOAD:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_CONTENT_LENGTH_UPLOAD, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
            break;
         case HB_CURLINFO_CONTENT_TYPE:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_CONTENT_TYPE, &ret_string );
            type = HB_CURL_INFO_TYPE_STR;
            break;
         case HB_CURLINFO_PRIVATE:
#if LIBCURL_VERSION_NUM >= 0x070A03
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_PRIVATE, &ret_ptr );
#endif
            type = HB_CURL_INFO_TYPE_PTR;
            break;
         case HB_CURLINFO_HTTPAUTH_AVAIL:
#if LIBCURL_VERSION_NUM >= 0x070A08
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_HTTPAUTH_AVAIL, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_PROXYAUTH_AVAIL:
#if LIBCURL_VERSION_NUM >= 0x070A08
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_PROXYAUTH_AVAIL, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_OS_ERRNO:
#if LIBCURL_VERSION_NUM >= 0x070C02
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_OS_ERRNO, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_NUM_CONNECTS:
#if LIBCURL_VERSION_NUM >= 0x070C03
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_NUM_CONNECTS, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_COOKIELIST:
#if LIBCURL_VERSION_NUM >= 0x070E01
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_COOKIELIST, &ret_slist );
#endif
            type = HB_CURL_INFO_TYPE_SLIST;
            break;
         case HB_CURLINFO_ACTIVESOCKET:
#if LIBCURL_VERSION_NUM >= 0x072D00
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_ACTIVESOCKET, &ret_slist );
#endif
            type = HB_CURL_INFO_TYPE_PTR_SLIST;
            break;
         case HB_CURLINFO_LASTSOCKET:  /* NOTE: Not compatible with 64-bit Windows builds */
#if LIBCURL_VERSION_NUM >= 0x070F02
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_LASTSOCKET, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_FTP_ENTRY_PATH:
#if LIBCURL_VERSION_NUM >= 0x070F04
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_FTP_ENTRY_PATH, &ret_string );
#endif
            type = HB_CURL_INFO_TYPE_STR;
            break;
         case HB_CURLINFO_PRIMARY_IP:
#if LIBCURL_VERSION_NUM >= 0x071300
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_PRIMARY_IP, &ret_string );
#endif
            type = HB_CURL_INFO_TYPE_STR;
            break;
         case HB_CURLINFO_APPCONNECT_TIME:
#if LIBCURL_VERSION_NUM >= 0x071300
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_APPCONNECT_TIME, &ret_double );
#endif
            type = HB_CURL_INFO_TYPE_DOUBLE;
            break;
         case HB_CURLINFO_CERTINFO:
#if LIBCURL_VERSION_NUM >= 0x071301
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_CERTINFO, &ret_slist );
#endif
            type = HB_CURL_INFO_TYPE_SLIST;
            break;
         case HB_CURLINFO_CONDITION_UNMET:
#if LIBCURL_VERSION_NUM >= 0x071304
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_CONDITION_UNMET, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_RTSP_SESSION_ID:
#if LIBCURL_VERSION_NUM >= 0x071400
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_RTSP_SESSION_ID, &ret_string );
#endif
            type = HB_CURL_INFO_TYPE_STR;
            break;
         case HB_CURLINFO_RTSP_CLIENT_CSEQ:
#if LIBCURL_VERSION_NUM >= 0x071400
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_RTSP_CLIENT_CSEQ, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_RTSP_SERVER_CSEQ:
#if LIBCURL_VERSION_NUM >= 0x071400
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_RTSP_SERVER_CSEQ, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_RTSP_CSEQ_RECV:
#if LIBCURL_VERSION_NUM >= 0x071400
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_RTSP_CSEQ_RECV, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_PRIMARY_PORT:
#if LIBCURL_VERSION_NUM >= 0x071500
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_PRIMARY_PORT, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_LOCAL_IP:
#if LIBCURL_VERSION_NUM >= 0x071500
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_LOCAL_IP, &ret_string );
#endif
            type = HB_CURL_INFO_TYPE_STR;
            break;
         case HB_CURLINFO_LOCAL_PORT:
#if LIBCURL_VERSION_NUM >= 0x071500
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_LOCAL_PORT, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_HTTP_VERSION:
#if LIBCURL_VERSION_NUM >= 0x073200
            res = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_HTTP_VERSION, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
      }

      switch( type )
      {
         case HB_CURL_INFO_TYPE_STR:
            hb_retc( ret_string );
            break;
         case HB_CURL_INFO_TYPE_PTR:
            hb_retptr( ret_ptr );
            break;
         case HB_CURL_INFO_TYPE_PTR_SLIST:
            hb_retptr( ret_slist );
            break;
         case HB_CURL_INFO_TYPE_LONG:
            hb_retnl( ret_long );
            break;
         case HB_CURL_INFO_TYPE_DOUBLE:
            hb_retnd( ret_double );
            break;
         case HB_CURL_INFO_TYPE_SLIST:
            if( ret_slist )
            {
               PHB_ITEM pArray;
               int      nCount;
               struct curl_slist * walk_ret_slist;

               /* Count */
               for( walk_ret_slist = ret_slist, nCount = 0; walk_ret_slist->next; nCount++ )
                  walk_ret_slist = walk_ret_slist->next;

               /* Fill */
               pArray = hb_itemArrayNew( nCount );
               for( walk_ret_slist = ret_slist, nCount = 1; walk_ret_slist->next; )
               {
                  hb_arraySetC( pArray, nCount++, walk_ret_slist->data );
                  walk_ret_slist = walk_ret_slist->next;
               }
               hb_itemReturnRelease( pArray );

               curl_slist_free_all( ret_slist );
            }
            else
               hb_reta( 0 );
            break;
      }

      hb_stornl( ( long ) res, 3 );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( CURL_EASY_ESCAPE )
{
   if( PHB_CURL_is( 1 ) )
   {
#if LIBCURL_VERSION_NUM >= 0x070F04
      PHB_CURL hb_curl = PHB_CURL_par( 1 );

      if( hb_curl )
      {
         char * buffer = curl_easy_escape( hb_curl->curl, hb_parcx( 2 ), ( int ) hb_parclen( 2 ) );
         hb_retc( buffer );
         curl_free( buffer );
      }
      else
#endif
      hb_retc_null();
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( CURL_EASY_UNESCAPE )
{
   if( PHB_CURL_is( 1 ) )
   {
#if LIBCURL_VERSION_NUM >= 0x070F04
      PHB_CURL hb_curl = PHB_CURL_par( 1 );

      if( hb_curl )
      {
         int    nLen   = 0;
         char * buffer = curl_easy_unescape( hb_curl->curl, hb_parcx( 2 ), ( int ) hb_parclen( 2 ), &nLen );
         hb_retclen( buffer, nLen );
         curl_free( buffer );
      }
      else
#endif
      hb_retc_null();
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( CURL_EASY_STRERROR )
{
   if( HB_ISNUM( 1 ) )
#if LIBCURL_VERSION_NUM >= 0x070C00
      hb_retc( curl_easy_strerror( ( CURLcode ) hb_parnl( 1 ) ) );
#else
      hb_retc_null();
#endif
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
