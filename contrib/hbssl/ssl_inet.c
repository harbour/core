/*
 * Harbour Project source code:
 *    SSL encryption for Harbour hb_inet*() connections
 *
 * Copyright 2014 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#define _HB_ZNET_INTERNAL_

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbsocket.h"
#include "hbdate.h"
#include "hbznet.h"
#include "hbssl.h"

#define HB_ZNET_SSL_ERROR_BASE      100

typedef struct _HB_ZNETSTREAM
{
   SSL *       ssl;
   PHB_ITEM    pSSL;
   int         error;
   HB_BOOL     blocking;
}
HB_ZNETSTREAM;

static long hb_inetReadSSL( PHB_ZNETSTREAM pStream, HB_SOCKET sd,
                            void * buffer, long len, HB_MAXINT timeout )
{
   HB_MAXUINT timer = timeout <= 0 ? 0 : hb_dateMilliSeconds();
   long lRead = -1;
   int iToRead = -1;

   /* sd = SSL_get_rfd( pStream->ssl ); */

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
         pStream->blocking = !pStream->blocking;
   }

   pStream->error = 0;

   if( len > 0 )
   {
      iToRead = SSL_pending( pStream->ssl );
      if( iToRead <= 0 )
      {
         iToRead = timeout < 0 ? 1 : hb_socketSelectRead( sd, timeout );
         if( iToRead <= 0 )
            pStream->error = iToRead == 0 ?
                             HB_SOCKET_ERR_TIMEOUT : hb_socketGetError();
         else
            iToRead = ( int ) len;
      }
      else if( iToRead > len )
         iToRead = ( int ) len;
   }

   while( iToRead > 0 )
   {
      lRead = SSL_read( pStream->ssl, buffer, iToRead );
      if( lRead > 0 )
         pStream->error = 0;
      else
      {
         int iError = SSL_get_error( pStream->ssl, ( int ) lRead );
         switch( iError )
         {
            case SSL_ERROR_ZERO_RETURN:
               pStream->error = HB_SOCKET_ERR_PIPE;
               lRead = 0;
               break;
            case SSL_ERROR_WANT_READ:
            case SSL_ERROR_WANT_WRITE:
               if( timeout > 0 )
               {
                  HB_MAXUINT timecurr = hb_dateMilliSeconds();
                  if( timecurr > timer )
                     timeout -= timecurr - timer;
                  if( timeout > 0 )
                  {
                     timer = timecurr;
                     if( iError == SSL_ERROR_WANT_READ )
                        iError = hb_socketSelectRead( sd, timeout );
                     else
                        iError = hb_socketSelectWrite( sd, timeout );
                     if( iError > 0 )
                        continue;
                     else if( iError < 0 )
                     {
                        pStream->error = hb_socketGetError();
                        break;
                     }
                  }
               }
               pStream->error = HB_SOCKET_ERR_TIMEOUT;
               break;
            default:
               pStream->error = HB_ZNET_SSL_ERROR_BASE + iError;
         }
      }
      break;
   }

   return lRead;
}

static long hb_inetWriteSSL( PHB_ZNETSTREAM pStream, HB_SOCKET sd,
                             const void * buffer, long len, HB_MAXINT timeout,
                             long * plast )
{
   HB_MAXUINT timer = timeout <= 0 ? 0 : hb_dateMilliSeconds();
   long lWritten = 0, lWr = 0;

   /* sd = SSL_get_wfd( pStream->ssl ); */

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
         pStream->blocking = !pStream->blocking;
   }

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
         pStream->error = 0;
      }
      else
      {
         int iError = SSL_get_error( pStream->ssl, ( int ) lWr );
         switch( iError )
         {
            case SSL_ERROR_WANT_READ:
            case SSL_ERROR_WANT_WRITE:
               if( timeout > 0 )
               {
                  HB_MAXUINT timecurr = hb_dateMilliSeconds();
                  if( timecurr > timer )
                     timeout -= timecurr - timer;
                  if( timeout > 0 )
                  {
                     timer = timecurr;
                     if( iError == SSL_ERROR_WANT_READ )
                        iError = hb_socketSelectRead( sd, timeout );
                     else
                        iError = hb_socketSelectWrite( sd, timeout );
                     if( iError > 0 )
                        continue;
                     if( lWritten == 0 )
                        pStream->error = iError < 0 ? hb_socketGetError() :
                                                      HB_SOCKET_ERR_TIMEOUT;
                     break;
                  }
               }
               if( lWritten == 0 )
                  pStream->error = HB_SOCKET_ERR_TIMEOUT;
               break;
            default:
               pStream->error = HB_ZNET_SSL_ERROR_BASE + iError;
         }
         break;
      }
   }

   return lWritten != 0 ? lWritten : lWr;
}

static long hb_inetFlushSSL( PHB_ZNETSTREAM pStream, HB_SOCKET sd,
                             HB_MAXINT timeout )
{
   HB_SYMBOL_UNUSED( pStream );
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( timeout );

   return 0;
}

static void hb_inetCloseSSL( PHB_ZNETSTREAM pStream )
{
   SSL_shutdown( pStream->ssl );
   hb_itemRelease( pStream->pSSL );
   hb_xfree( pStream );
}

static int hb_inetErrorSSL( PHB_ZNETSTREAM pStream )
{
   return pStream->error;
}

static const char * hb_inetErrStrSSL( PHB_ZNETSTREAM pStream, int iError )
{
   HB_SYMBOL_UNUSED( pStream );

   if( iError >= HB_ZNET_SSL_ERROR_BASE )
   {
      switch( iError - HB_ZNET_SSL_ERROR_BASE )
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

static void hb_inetStartSSL( HB_BOOL fServer )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_POINTER );
   HB_SOCKET sd = hb_znetInetFD( pItem, HB_TRUE );

   if( sd != HB_NO_SOCKET )
   {
      if( hb_SSL_is( 2 ) )
      {
         int iResult = -2;
         SSL * ssl = hb_SSL_par( 2 );

         if( ssl )
         {
            PHB_ZNETSTREAM pStream = ( HB_ZNETSTREAM * ) hb_xgrabz( sizeof( HB_ZNETSTREAM ) );
            HB_MAXINT timeout;
            HB_MAXUINT timer;

            timeout = HB_ISNUM( 3 ) ? hb_parnint( 3 ) :
                                      hb_znetInetTimeout( pItem, HB_FALSE );
            timer = timeout <= 0 ? 0 : hb_dateMilliSeconds();

            pStream->ssl = ssl;
            pStream->pSSL = hb_itemNew( hb_param( 2, HB_IT_POINTER ) );
            pStream->blocking = timeout < 0;
            if( hb_socketSetBlockingIO( sd, pStream->blocking ) < 0 )
               pStream->blocking = !pStream->blocking;

            SSL_set_mode( ssl, HB_SSL_MODE_AUTO_RETRY );
            iResult = SSL_set_fd( ssl, sd );
            while( iResult == 1 )
            {
               if( fServer )
                  iResult = SSL_accept( ssl );
               else
                  iResult = SSL_connect( ssl );

               if( iResult != 1 )
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
                        HB_MAXUINT timecurr = hb_dateMilliSeconds();
                        if( timecurr > timer )
                        {
                           timeout -= timecurr - timer;
                           timer = timecurr;
                        }

                        if( timeout > 0 )
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
                     iResult = -5;
                     break;
                  }
                  iResult = -4;
               }
               else if( hb_znetInetInitialize( pItem, pStream,
                                               hb_inetReadSSL, hb_inetWriteSSL,
                                               hb_inetFlushSSL, hb_inetCloseSSL,
                                               hb_inetErrorSSL, hb_inetErrStrSSL ) )
                  break;
               else
                  iResult = -3;
            }
            if( iResult != 1 )
               hb_inetCloseSSL( pStream );
            else
               pStream->blocking = hb_socketSetBlockingIO( sd, HB_FALSE ) < 0;
         }
         hb_retni( iResult );
      }
      else
         hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

/* hb_inetSSL_connect( <pSocket>, <pSSL> [, <nTimeout> ] ) */
HB_FUNC( HB_INETSSL_CONNECT )
{
   hb_inetStartSSL( HB_FALSE );
}

/* hb_inetSSL_accept( <pSocket>, <pSSL> [, <nTimeout> ] ) */
HB_FUNC( HB_INETSSL_ACCEPT )
{
   hb_inetStartSSL( HB_TRUE );
}
