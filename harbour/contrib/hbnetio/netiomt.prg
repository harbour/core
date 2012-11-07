/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration code for alternative RDD IO API which uses own
 *    very simple TCP/IP file server with RPC support
 *    All files which names starts 'net:' are redirected to this API.
 *    This is code for simple MT server which is activated by:
 *       NETIO_MTSERVER( [<nPort>], [<cIfAddr>], [<cRootDir>], [<xRPC>],
 *                       [<cPasswd>], [<nCompressionLevel>], [<nStrategy>],
 *                       [<sSrvFunc>] )
 *                                              -> <pListenSocket> | NIL
 *    and can be stopped by:
 *       NETIO_SERVERSTOP( <pListenSocket>, .T. )
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "error.ch"

FUNCTION netio_MTServer( nPort, cIfAddr, cRootDir, xRPC, ;
      cPasswd, nCompressLevel, nStrategy, ;
      sSrvFunc )

   LOCAL pListenSocket, lRPC
   LOCAL oError

   IF sSrvFunc == NIL
      sSrvFunc := @netio_Server()
   ENDIF

   IF hb_mtvm()

      SWITCH ValType( xRPC )
      CASE "S"
      CASE "H"
         lRPC := .T.
         EXIT
      CASE "L"
         lRPC := xRPC
         EXIT
      OTHERWISE
         xRPC := NIL
      ENDSWITCH

      pListenSocket := netio_Listen( nPort, cIfAddr, cRootDir, lRPC )
      IF ! Empty( pListenSocket )
         hb_threadDetach( hb_threadStart( @netio_srvloop(), pListenSocket, ;
            xRPC, sSrvFunc, ;
            cPasswd, nCompressLevel, nStrategy ) )
      ENDIF
   ELSE
      oError := ErrorNew()

      oError:severity    := ES_ERROR
      oError:genCode     := EG_UNSUPPORTED
      oError:subSystem   := "HBNETIO"
      oError:subCode     := 0
      oError:description := hb_langErrMsg( EG_UNSUPPORTED )
      oError:canRetry    := .F.
      oError:canDefault  := .F.
      oError:fileName    := ""
      oError:osCode      := 0

      Eval( ErrorBlock(), oError )
   ENDIF

   RETURN pListenSocket


STATIC FUNCTION NETIO_SRVLOOP( pListenSocket, xRPC, sSrvFunc, ... )

   LOCAL pConnectionSocket

   WHILE .T.
      pConnectionSocket := netio_Accept( pListenSocket,, ... )
      IF Empty( pConnectionSocket )
         EXIT
      ENDIF
      IF xRPC != NIL
         netio_RPCFilter( pConnectionSocket, xRPC )
      ENDIF
      hb_threadDetach( hb_threadStart( sSrvFunc, pConnectionSocket ) )
      pConnectionSocket := NIL
   ENDDO

   RETURN NIL
