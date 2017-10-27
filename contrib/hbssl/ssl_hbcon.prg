/*
 * OpenSSL API (SSL) - Harbour extensions
 *
 * Copyright 2016 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

/* hb_SSL_new() --> <pSSL> */
FUNCTION hb_SSL_new()

   STATIC s_onceControl

   /* initialize SSL library */
   hb_threadOnce( @s_onceControl, {|| SSL_init(), ;
                  RAND_seed( hb_randStr( 20 ) + hb_TToS( hb_DateTime() ) ) } )

   /* create a new SSL structure for a connection */
   RETURN SSL_new( SSL_CTX_new() )

/* hb_SSL_connect_socket( <pSocket>, [ <nTimeOut> ], [ @<cInfo> ] ) --> <lConnected> */
FUNCTION hb_SSL_connect_socket( pSocket, nTimeout, cInfo )

   LOCAL nErr
   LOCAL ssl

   ssl := hb_SSL_new()
   IF ! Empty( pSocket := hb_socketNewSSL_connect( @pSocket, ssl, nTimeout ) )
      cInfo := "SSL connected with " + SSL_get_cipher( ssl ) + " encryption."
      RETURN .T.
   ENDIF

   nErr := ERR_get_error()
   cInfo := hb_StrFormat( "SSL connection error [%d] %s", ;
                          nErr, ERR_error_string( nErr ) )
   RETURN .F.

/* hb_SSL_connect_inet( <pSocket>, [ <nTimeOut> ], [ @<cInfo> ] ) --> <lConnected> */
FUNCTION hb_SSL_connect_inet( pInetSock, nTimeout, cInfo )

   LOCAL nResult, nErr
   LOCAL ssl

   ssl := hb_SSL_new()
   IF ( nResult := hb_inetSSL_CONNECT( pInetSock, ssl, nTimeout ) ) == 1
      cInfo := "SSL connected with " + SSL_get_cipher( ssl ) + " encryption."
      RETURN .T.
   ENDIF

   nErr := ERR_get_error()
   cInfo := hb_StrFormat( "SSL connection error [%d:%d] %s", ;
                          nResult, nErr, ERR_error_string( nErr ) )
   RETURN .F.
