/*
 * SSL encryption for Harbour hb_inet*() connections
 *
 * Copyright 2014 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#define _HB_ZNET_INTERNAL_

#include "hbssl.h"

#include "hbapiitm.h"
#include "hbdate.h"
#include "hbznet.h"

static long hb_inetReadSSL( PHB_ZNETSTREAM pStream, HB_SOCKET sd,
                            void * buffer, long len, HB_MAXINT timeout )
{
   return hb_ssl_socketRead( ( PHB_SSLSTREAM ) pStream, sd, buffer, len, timeout );
}

static long hb_inetWriteSSL( PHB_ZNETSTREAM pStream, HB_SOCKET sd,
                             const void * buffer, long len, HB_MAXINT timeout,
                             long * plast )
{
   return hb_ssl_socketWrite( ( PHB_SSLSTREAM ) pStream, sd, buffer, len, timeout, plast );
}

static void hb_inetCloseSSL( PHB_ZNETSTREAM pStream )
{
   hb_ssl_socketClose( ( PHB_SSLSTREAM ) pStream );
}

static long hb_inetFlushSSL( PHB_ZNETSTREAM pStream, HB_SOCKET sd,
                             HB_MAXINT timeout, HB_BOOL fSync )
{
   HB_SYMBOL_UNUSED( pStream );
   HB_SYMBOL_UNUSED( sd );
   HB_SYMBOL_UNUSED( timeout );
   HB_SYMBOL_UNUSED( fSync );

   return 0;
}

static int hb_inetErrorSSL( PHB_ZNETSTREAM pStream )
{
   HB_SYMBOL_UNUSED( pStream );

   return hb_socketGetError();
}

static const char * hb_inetErrStrSSL( PHB_ZNETSTREAM pStream, int iError )
{
   HB_SYMBOL_UNUSED( pStream );

   return hb_ssl_socketErrorStr( iError );
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
            HB_MAXINT timeout = HB_ISNUM( 3 ) ? hb_parnint( 3 ) :
                                hb_znetInetTimeout( pItem, HB_FALSE );
            PHB_SSLSTREAM pStream = hb_ssl_socketNew( sd, ssl, fServer, timeout,
                                                      hb_param( 2, HB_IT_POINTER ),
                                                      &iResult );
            if( pStream )
            {
               if( ! hb_znetInetInitialize( pItem, ( PHB_ZNETSTREAM ) pStream,
                                            hb_inetReadSSL, hb_inetWriteSSL,
                                            hb_inetFlushSSL, hb_inetCloseSSL,
                                            hb_inetErrorSSL, hb_inetErrStrSSL ) )
               {
                  hb_ssl_socketClose( pStream );
                  iResult = -3;
               }
            }
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
