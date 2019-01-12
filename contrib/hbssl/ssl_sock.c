/*
 * Harbour extended socket filter with SSL encryption
 *
 * Copyright 2015 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

/* this has to be declared before hbsocket.h is included */
#define _HB_SOCKEX_IMPLEMENTATION_

#include "hbssl.h"

#include "hbapiitm.h"
#include "hbvm.h"
#include "hbdate.h"
#include "hbinit.h"

typedef struct _HB_SSLSTREAM
{
   SSL *       ssl;
   PHB_ITEM    pSSL;
   HB_BOOL     blocking;
}
HB_SSLSTREAM;

const char * hb_ssl_socketErrorStr( int iError )
{
   if( iError >= HB_SSL_SOCK_ERROR_BASE )
   {
      switch( iError - HB_SSL_SOCK_ERROR_BASE )
      {
         case SSL_ERROR_NONE:
            return "SSL_ERROR_NONE";
         case SSL_ERROR_ZERO_RETURN:
            return "SSL_ERROR_ZERO_RETURN";
         case SSL_ERROR_WANT_READ:
            return "SSL_ERROR_WANT_READ";
         case SSL_ERROR_WANT_WRITE:
            return "SSL_ERROR_WANT_WRITE";
         case SSL_ERROR_WANT_CONNECT:
            return "SSL_ERROR_WANT_CONNECT";
         case SSL_ERROR_WANT_ACCEPT:
            return "SSL_ERROR_WANT_ACCEPT";
         case SSL_ERROR_WANT_X509_LOOKUP:
            return "SSL_ERROR_WANT_X509_LOOKUP";
         case SSL_ERROR_SYSCALL:
            return "SSL_ERROR_SYSCALL";
         case SSL_ERROR_SSL:
            return "SSL_ERROR_SSL";
      }
   }

   return hb_socketErrorStr( iError );
}

long hb_ssl_socketRead( PHB_SSLSTREAM pStream, HB_SOCKET sd,
                        void * buffer, long len, HB_MAXINT timeout )
{
   long lRead = -1;
   int iToRead = -1;
   HB_MAXUINT timer;

   #if 0
   sd = SSL_get_rfd( pStream->ssl );
   #endif

#if LONG_MAX > INT_MAX
   if( len > INT_MAX )
      len = INT_MAX;
#endif

#if 0
   while( ERR_get_error() != 0 ) { /* eat pending errors */ }
#endif

   if( pStream->blocking ? timeout >= 0 : timeout < 0 )
   {
      if( hb_socketSetBlockingIO( sd, timeout < 0 ) >= 0 )
         pStream->blocking = ! pStream->blocking;
   }

   timer = hb_timerInit( timeout );

   if( len > 0 )
   {
      iToRead = SSL_pending( pStream->ssl );
      if( iToRead <= 0 )
      {
         iToRead = timeout < 0 ? 1 : hb_socketSelectRead( sd, timeout );
         if( iToRead > 0 )
            iToRead = ( int ) len;
         else if( iToRead == 0 )
            hb_socketSetError( HB_SOCKET_ERR_TIMEOUT );
      }
      else if( iToRead > len )
         iToRead = ( int ) len;
   }

   while( iToRead > 0 )
   {
      lRead = SSL_read( pStream->ssl, buffer, iToRead );
      if( lRead > 0 )
         hb_socketSetError( 0 );
      else
      {
         int iError = SSL_get_error( pStream->ssl, ( int ) lRead );
         switch( iError )
         {
            case SSL_ERROR_ZERO_RETURN:
               hb_socketSetError( HB_SOCKET_ERR_PIPE );
               lRead = 0;
               break;
            case SSL_ERROR_WANT_READ:
            case SSL_ERROR_WANT_WRITE:
               if( hb_vmRequestQuery() == 0 )
               {
                  if( timeout > 0 )
                  {
                     if( ( timeout = hb_timerTest( timeout, &timer ) ) != 0 )
                     {
                        if( iError == SSL_ERROR_WANT_READ )
                           iError = hb_socketSelectRead( sd, timeout );
                        else
                           iError = hb_socketSelectWrite( sd, timeout );
                        if( iError > 0 )
                           continue;
                        else if( iError < 0 )
                           break;
                     }
                  }
                  hb_socketSetError( HB_SOCKET_ERR_TIMEOUT );
                  break;
               }
               /* fallthrough */
            default:
               hb_socketSetError( HB_SSL_SOCK_ERROR_BASE + iError );
         }
      }
      break;
   }

   return lRead;
}

long hb_ssl_socketWrite( PHB_SSLSTREAM pStream, HB_SOCKET sd,
                         const void * buffer, long len, HB_MAXINT timeout,
                         long * plast )
{
   long lWritten = 0, lWr = 0;
   HB_MAXUINT timer;

   #if 0
   sd = SSL_get_wfd( pStream->ssl );
   #endif

#if LONG_MAX > INT_MAX
   if( len > INT_MAX )
      len = INT_MAX;
#endif

#if 0
   while( ERR_get_error() != 0 ) { /* eat pending errors */ }
#endif

   if( pStream->blocking ? timeout >= 0 : timeout < 0 )
   {
      if( hb_socketSetBlockingIO( sd, timeout < 0 ) >= 0 )
         pStream->blocking = ! pStream->blocking;
   }

   timer = hb_timerInit( timeout );

   while( len > 0 )
   {
      lWr = SSL_write( pStream->ssl, buffer, ( int ) len );

      if( plast )
         *plast = lWr;

      if( lWr > 0 )
      {
         lWritten += lWr;
         len -= lWr;
         buffer = ( const char * ) buffer + lWr;
         hb_socketSetError( 0 );
      }
      else
      {
         int iError = SSL_get_error( pStream->ssl, ( int ) lWr );
         switch( iError )
         {
            case SSL_ERROR_WANT_READ:
            case SSL_ERROR_WANT_WRITE:
               if( hb_vmRequestQuery() == 0 )
               {
                  if( timeout > 0 )
                  {
                     if( ( timeout = hb_timerTest( timeout, &timer ) ) != 0 )
                     {
                        if( iError == SSL_ERROR_WANT_READ )
                           iError = hb_socketSelectRead( sd, timeout );
                        else
                           iError = hb_socketSelectWrite( sd, timeout );
                        if( iError > 0 )
                           continue;
                     }
                     else
                        iError = 0;
                  }
                  else
                     iError = 0;
                  if( lWritten == 0 && iError == 0 )
                     hb_socketSetError( HB_SOCKET_ERR_TIMEOUT );
                  break;
               }
               /* fallthrough */
            default:
               hb_socketSetError( HB_SSL_SOCK_ERROR_BASE + iError );
         }
         break;
      }
   }

   return lWritten != 0 ? lWritten : lWr;
}

void hb_ssl_socketClose( PHB_SSLSTREAM pStream )
{
   SSL_shutdown( pStream->ssl );
   if( pStream->pSSL )
      hb_itemRelease( pStream->pSSL );
   else
      SSL_free( pStream->ssl );
   hb_xfree( pStream );
}

PHB_SSLSTREAM hb_ssl_socketNew( HB_SOCKET sd, SSL * ssl, HB_BOOL fServer,
                                HB_MAXINT timeout, PHB_ITEM pSSL,
                                int * piResult )
{
   PHB_SSLSTREAM pStream;
   HB_MAXUINT timer;
   int iResult;

   pStream = ( HB_SSLSTREAM * ) hb_xgrabz( sizeof( HB_SSLSTREAM ) );

   pStream->ssl = ssl;
   pStream->pSSL = pSSL ? hb_itemNew( pSSL ) : NULL;
   pStream->blocking = timeout < 0;
   if( hb_socketSetBlockingIO( sd, pStream->blocking ) < 0 )
      pStream->blocking = ! pStream->blocking;

   SSL_set_mode( ssl, HB_SSL_MODE_AUTO_RETRY );
   iResult = SSL_set_fd( ssl, sd );  /* Truncates `sd` on win64. OpenSSL bug: https://rt.openssl.org/Ticket/Display.html?id=1928&user=guest&pass=guest */

   timer = hb_timerInit( timeout );

   while( iResult == 1 )
   {
      if( fServer )
         iResult = SSL_accept( ssl );
      else
         iResult = SSL_connect( ssl );

      if( iResult != 1 && hb_vmRequestQuery() == 0 )
      {
         int iError = SSL_get_error( ssl, iResult );
         if( iError == SSL_ERROR_WANT_READ ||
             iError == SSL_ERROR_WANT_WRITE )
         {
            if( timeout < 0 )
            {
               iResult = 1;
               continue;
            }
            else if( timeout > 0 )
            {
               if( ( timeout = hb_timerTest( timeout, &timer ) ) != 0 )
               {
                  if( iError == SSL_ERROR_WANT_READ )
                     iError = hb_socketSelectRead( sd, timeout );
                  else
                     iError = hb_socketSelectWrite( sd, timeout );
                  if( iError > 0 )
                  {
                     iResult = 1;
                     continue;
                  }
               }
            }
            hb_socketSetError( HB_SOCKET_ERR_TIMEOUT );
         }
      }
      break;
   }

   if( iResult != 1 )
   {
      hb_ssl_socketClose( pStream );
      pStream = NULL;
   }
   else
      pStream->blocking = hb_socketSetBlockingIO( sd, HB_FALSE ) < 0;

   if( piResult )
      *piResult = iResult;

   return pStream;
}

/* socket filter */

static SSL * s_SSL_itemGet( PHB_ITEM pItem, PHB_ITEM * pSSL, HB_BOOL * pfFree )
{
   SSL * ssl = NULL;

   if( pItem )
   {
      PHB_ITEM pRelease = NULL;

      if( HB_IS_EVALITEM( pItem ) )
         pItem = pRelease = hb_itemDo( pItem, 0 );

      ssl = hb_SSL_itemGet( pItem );
      if( ssl == NULL )
      {
         SSL_CTX * ssl_ctx = hb_SSL_CTX_itemGet( pItem );
         if( ssl_ctx )
         {
            ssl = SSL_new( ssl_ctx );
            if( pRelease )
               hb_itemRelease( pRelease );
            pItem = pRelease = NULL;
         }
      }
      if( ssl )
      {
         * pSSL = pItem;
         * pfFree = pRelease != NULL;
      }
      else if( pRelease )
         hb_itemRelease( pRelease );
   }
   return ssl;
}

#define HB_SSLSOCK_GET( p )   ( ( PHB_SSLSTREAM ) p->cargo )
#define HB_SSLSOCK_READAHEAD  0x40

static PHB_SOCKEX s_sockexNew( HB_SOCKET sd, PHB_ITEM pParams )
{
   PHB_SOCKEX pSock;
   HB_BOOL fServer = HB_FALSE, fFree = HB_FALSE;
   HB_MAXINT timeout = -1;
   PHB_ITEM pSSL = NULL;
   SSL * ssl = NULL;

   if( pParams && HB_IS_HASH( pParams ) )
   {
      PHB_ITEM pItem;

      if( ssl == NULL )
         ssl = s_SSL_itemGet( hb_hashGetCItemPtr( pParams, "ssl" ), &pSSL, &fFree );
      if( ssl == NULL )
         ssl = s_SSL_itemGet( hb_hashGetCItemPtr( pParams, "ctx" ), &pSSL, &fFree );
      if( ssl == NULL )
         ssl = s_SSL_itemGet( hb_hashGetCItemPtr( pParams, "key" ), &pSSL, &fFree );

      if( ( pItem = hb_hashGetCItemPtr( pParams, "timeout" ) ) != NULL &&
          HB_IS_NUMERIC( pItem ) )
         timeout = hb_itemGetNInt( pItem );
      if( ( pItem = hb_hashGetCItemPtr( pParams, "server" ) ) != NULL &&
          HB_IS_LOGICAL( pItem ) )
         fServer = hb_itemGetL( pItem );
      else if( ( pItem = hb_hashGetCItemPtr( pParams, "client" ) ) != NULL &&
               HB_IS_LOGICAL( pItem ) )
         fServer = ! hb_itemGetL( pItem );
   }

   pSock = hb_sockexNewSSL( sd, ssl, fServer, timeout, pSSL );
   if( pSock )
      hb_socekxParamsInit( pSock, pParams );
   if( fFree )
      hb_itemRelease( pSSL );

   return pSock;
}

/* this wrapper does not support multilevel filtering so
   it destroys previous wrappers if any and create new one.
 */
static PHB_SOCKEX s_sockexNext( PHB_SOCKEX pSock, PHB_ITEM pParams )
{
   PHB_SOCKEX pSockNew = NULL;

   if( pSock && pSock->sd != HB_NO_SOCKET )
   {
      pSockNew = s_sockexNew( pSock->sd, pParams );
      if( pSockNew )
         hb_sockexClose( pSock, HB_FALSE );
   }

   return pSockNew;
}

static int s_sockexClose( PHB_SOCKEX pSock, HB_BOOL fClose )
{
   int iResult;

   if( pSock->cargo )
      hb_ssl_socketClose( HB_SSLSOCK_GET( pSock ) );

   iResult = hb_sockexRawClear( pSock, fClose );
   hb_xfree( pSock );

   return iResult;
}

static long s_sockexRead( PHB_SOCKEX pSock, void * data, long len, HB_MAXINT timeout )
{
   long lRead = HB_MIN( pSock->inbuffer, len );

   if( lRead > 0 )
   {
      memcpy( data, pSock->buffer + pSock->posbuffer, lRead );
      pSock->inbuffer -= lRead;
      if( pSock->inbuffer )
         pSock->posbuffer += lRead;
      else
         pSock->posbuffer = 0;
      return lRead;
   }
   else if( pSock->sd == HB_NO_SOCKET )
   {
      hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
      return -1;
   }
   return hb_ssl_socketRead( HB_SSLSOCK_GET( pSock ), pSock->sd, data, len, timeout );
}

static long s_sockexWrite( PHB_SOCKEX pSock, const void * data, long len, HB_MAXINT timeout )
{
   if( pSock->sd == HB_NO_SOCKET )
   {
      hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
      return -1;
   }
   return hb_ssl_socketWrite( HB_SSLSOCK_GET( pSock ), pSock->sd, data, len, timeout, NULL );
}

static long s_sockexFlush( PHB_SOCKEX pSock, HB_MAXINT timeout, HB_BOOL fSync )
{
   HB_SYMBOL_UNUSED( pSock );
   HB_SYMBOL_UNUSED( timeout );
   HB_SYMBOL_UNUSED( fSync );

   return 0;
}

static int s_sockexCanRead( PHB_SOCKEX pSock, HB_BOOL fBuffer, HB_MAXINT timeout )
{
   if( pSock->inbuffer )
      return 1;
   else if( pSock->sd == HB_NO_SOCKET )
   {
      hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
      return -1;
   }
   else if( SSL_pending( HB_SSLSOCK_GET( pSock )->ssl ) )
   {
      long len;

      if( pSock->buffer == NULL )
      {
         if( pSock->readahead <= 0 )
            pSock->readahead = HB_SSLSOCK_READAHEAD;
         pSock->buffer = ( HB_BYTE * ) hb_xgrab( pSock->readahead );
      }
      len = hb_ssl_socketRead( HB_SSLSOCK_GET( pSock ), pSock->sd,
                               pSock->buffer, pSock->readahead, 0 );
      if( len > 0 )
      {
         pSock->inbuffer = len;
         len = 1;
      }
      return ( int ) len;
   }
   return fBuffer ? 0 : hb_socketSelectRead( pSock->sd, timeout );
}

static int s_sockexCanWrite( PHB_SOCKEX pSock, HB_BOOL fBuffer, HB_MAXINT timeout )
{
   if( pSock->sd == HB_NO_SOCKET )
   {
      hb_socketSetError( HB_SOCKET_ERR_INVALIDHANDLE );
      return -1;
   }
   return fBuffer ? 0 : hb_socketSelectWrite( pSock->sd, timeout );
}

static char * s_sockexName( PHB_SOCKEX pSock )
{
   return hb_strdup( pSock->pFilter->pszName );
}

static const char * s_sockexErrorStr( PHB_SOCKEX pSock, int iError )
{
   HB_SYMBOL_UNUSED( pSock );

   return hb_ssl_socketErrorStr( iError );
}

static const HB_SOCKET_FILTER s_sockFilter =
{
   "SSL",
   s_sockexNew,
   s_sockexNext,
   s_sockexClose,
   s_sockexRead,
   s_sockexWrite,
   s_sockexFlush,
   s_sockexCanRead,
   s_sockexCanWrite,
   s_sockexName,
   s_sockexErrorStr
};

PHB_SOCKEX hb_sockexNewSSL( HB_SOCKET sd, SSL * ssl, HB_BOOL fServer,
                            HB_MAXINT timeout, PHB_ITEM pSSL )
{
   PHB_SOCKEX pSock = NULL;

   if( sd != HB_NO_SOCKET && ssl )
   {
      PHB_SSLSTREAM pStream = hb_ssl_socketNew( sd, ssl, fServer, timeout, pSSL, NULL );
      if( pStream )
      {
         pSock = ( PHB_SOCKEX ) hb_xgrabz( sizeof( HB_SOCKEX ) );
         pSock->sd = sd;
         pSock->fRedirAll = HB_TRUE;
         pSock->fShutDown = HB_TRUE;
         pSock->pFilter = &s_sockFilter;
         pSock->cargo = ( void * ) pStream;
      }
   }

   return pSock;
}

static void s_sslSocketNew( HB_BOOL fServer )
{
   HB_SOCKET sd = hb_socketParam( 1 );

   if( sd != HB_NO_SOCKET )
   {
      PHB_SOCKEX pSock = NULL;
      SSL * ssl = hb_SSL_par( 2 );

      if( ssl )
         pSock = hb_sockexNewSSL( sd, ssl, fServer, hb_parnintdef( 3, - 1 ), hb_param( 2, HB_IT_ANY ) );
      else if( HB_ISHASH( 2 ) )
         pSock = hb_sockexNew( sd, s_sockFilter.pszName, hb_param( 2, HB_IT_ANY ) );
      else
         hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );

      if( pSock )
      {
         PHB_ITEM pSockItm = hb_param( 1, HB_IT_POINTER );

         if( HB_ISBYREF( 1 ) && hb_sockexItemReplace( pSockItm, pSock ) )
            hb_itemReturn( pSockItm );
         else
         {
            hb_socketItemClear( pSockItm );
            hb_sockexItemPut( hb_param( -1, HB_IT_ANY ), pSock );
         }
      }
   }
}

/* hb_socketNewSSL_connect( [@]<pSocket>, <pSSL> [, <nTimeout> ] ) */
HB_FUNC( HB_SOCKETNEWSSL_CONNECT )
{
   s_sslSocketNew( HB_FALSE );
}

/* hb_socketNewSSL_accept( [@]<pSocket>, <pSSL> [, <nTimeout> ] ) */
HB_FUNC( HB_SOCKETNEWSSL_ACCEPT )
{
   s_sslSocketNew( HB_TRUE );
}

HB_CALL_ON_STARTUP_BEGIN( _hb_sslsock_init_ )
   hb_sockexRegister( &s_sockFilter );
HB_CALL_ON_STARTUP_END( _hb_sslsock_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_sslsock_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY  HB_DATASEG_FUNC( _hb_sslsock_init_ )
   #include "hbiniseg.h"
#endif
