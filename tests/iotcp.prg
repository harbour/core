/*
 * Harbour Project source code:
 *    Harbour FILE IO redirector: IOTCP
 *    example of IOUSR usage
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

#include "hbiousr.ch"
#include "hbsocket.ch"
#include "fileio.ch"
#include "error.ch"

ANNOUNCE HB_IOTCP

#define IOTCP_SOCKET          1
#define IOTCP_EOF             2
#define IOTCP_TIMEOUT         3

#define IOTCP_ERRORBASE       10000

#define IOTCP_NEW( sd, tout ) { sd, .F., tout }

/*
 * NOTE: cDefExt, cPaths and oError can be NIL
 */
STATIC FUNCTION IOTCP_Open( cFile, cDefExt, nFlags, cPaths, oError )

   LOCAL nTimeout := 10000, nError, hSock := NIL, aFile := NIL
   LOCAL cAddr, cRest, nPort := 0, aAddr
   LOCAL nAt

   HB_SYMBOL_UNUSED( cDefExt )
   HB_SYMBOL_UNUSED( cPaths )

   /* strip "tcp:" prefix */
   cAddr := SubStr( cFile, 5 )

   /* take host, port and timeout from "<host>:<port>[:<timeout>]" string */
   IF ( nAt := At( ":", cAddr ) ) > 1
      cRest := SubStr( cAddr, nAt + 1 )
      cAddr := Left( cAddr, nAt - 1 )
      IF ( nPort := Val( cRest ) ) > 0
         WHILE IsDigit( cRest )
            cRest := SubStr( cRest, 2 )
         ENDDO
         IF cRest = ":"
            cRest := SubStr( cRest, 2 )
            IF IsDigit( cRest ) .AND. ( nTimeout := Val( cRest ) ) > 0
               WHILE IsDigit( cRest )
                  cRest := SubStr( cRest, 2 )
               ENDDO
               IF cRest = ":"
                  cRest := ""
               ENDIF
            ENDIF
         ENDIF
         IF ! cRest == ""
            nPort := 0
         ENDIF
      ENDIF
   ENDIF

   IF nPort != 0
      IF ! Empty( aAddr := hb_socketResolveINetAddr( cAddr, nPort ) ) .AND. ;
         ! Empty( hSock := hb_socketOpen() )
         hb_socketSetKeepAlive( hSock, .T. )
         IF hb_socketConnect( hSock, aAddr, nTimeout )
            IF ! Empty( hSock )
               SWITCH hb_bitAnd( nFlags, hb_bitOr( FO_READ, FO_WRITE, FO_READWRITE ) )
               CASE FO_READ
                  hb_socketShutdown( hSock, HB_SOCKET_SHUT_WR )
                  EXIT
               CASE FO_WRITE
                  hb_socketShutdown( hSock, HB_SOCKET_SHUT_RD )
                  EXIT
               ENDSWITCH
               aFile := IOTCP_NEW( hSock, nTimeout )
            ENDIF
         ENDIF
         IF aFile == NIL
            nError := hb_socketGetError()
            hb_socketClose( hSock )
         ENDIF
      ENDIF
      IF nError == 0 .AND. aFile == NIL
         nError := hb_socketGetError()
      ENDIF
   ELSE
      nError := HB_SOCKET_ERR_WRONGADDR
   ENDIF

   IOUSR_SetError( nError, IOTCP_ERRORBASE )

   IF oError != NIL
      oError:filename := cFile
      IF aFile == NIL
         oError:osCode := nError
         oError:genCode := EG_OPEN
      ENDIF
   ENDIF

   RETURN aFile /* if aFile == NIL indicates error */


STATIC FUNCTION IOTCP_Close( aFile )

   hb_socketClose( aFile[ IOTCP_SOCKET ] )
   IOUSR_SetError( hb_socketGetError(), IOTCP_ERRORBASE )

   RETURN NIL


STATIC FUNCTION IOTCP_Read( aFile, /*@*/ cData, nLen, nTimeout )

   LOCAL nRead := 0, nError

   IF ! aFile[ IOTCP_EOF ]
      IF nTimeout == -1
         nTimeout := aFile[ IOTCP_TIMEOUT ]
      ENDIF
      nRead := hb_socketRecv( aFile[ IOTCP_SOCKET ], @cData, nLen, 0, nTimeout )
      nError := hb_socketGetError()
      IF nRead <= 0
         SWITCH nError
         CASE HB_SOCKET_ERR_TIMEOUT
         CASE HB_SOCKET_ERR_AGAIN
         CASE HB_SOCKET_ERR_TRYAGAIN
            EXIT
         OTHERWISE
            aFile[ IOTCP_EOF ] := .F.
         ENDSWITCH
         nRead := 0
      ENDIF
      IOUSR_SetError( nError, IOTCP_ERRORBASE )
   ENDIF

   RETURN nRead


STATIC FUNCTION IOTCP_Write( aFile, cData, nLen, nTimeout )

   IF nTimeout == -1
      nTimeout := aFile[ IOTCP_TIMEOUT ]
   ENDIF
   nLen := hb_socketSend( aFile[ IOTCP_SOCKET ], cData, nLen, 0, nTimeout )
   IOUSR_SetError( hb_socketGetError(), IOTCP_ERRORBASE )

   RETURN iif( nLen < 0, 0, nLen )


STATIC FUNCTION IOTCP_Eof( aFile )
   RETURN aFile[ IOTCP_EOF ]


STATIC FUNCTION IOTCP_Configure( aFile, nIndex, xValue )

   HB_SYMBOL_UNUSED( aFile )
   HB_SYMBOL_UNUSED( nIndex )
   HB_SYMBOL_UNUSED( xValue )

   RETURN .F.


STATIC FUNCTION IOTCP_Handle( aFile )

   IOUSR_SetError( 0, IOTCP_ERRORBASE )

   RETURN hb_socketGetFD( aFile[ IOTCP_SOCKET ] )


INIT PROCEDURE CLIPINIT

   LOCAL aMethods[ IOUSR_METHODCOUNT ]

   aMethods[ IOUSR_OPEN      ] := @IOTCP_Open()
   aMethods[ IOUSR_CLOSE     ] := @IOTCP_Close()
   aMethods[ IOUSR_READ      ] := @IOTCP_Read()
   aMethods[ IOUSR_WRITE     ] := @IOTCP_Write()
   aMethods[ IOUSR_EOF       ] := @IOTCP_Eof()
   aMethods[ IOUSR_CONFIGURE ] := @IOTCP_Configure()
   aMethods[ IOUSR_HANDLE    ] := @IOTCP_Handle()

   IOUSR_Register( aMethods, "tcp:" )

   RETURN



/* test code */

REQUEST HB_IOTCP

PROCEDURE Main( cAddr )

   LOCAL hFile, cData, cSend, cEOL, nLen

   IF Empty( cAddr )
      cAddr := "tcp:smtp.gmail.com:25:10000"
   ENDIF

   ? "open:", cAddr
   IF Empty( hFile := hb_vfOpen( cAddr, FO_READWRITE ) )
      ? "Open error:", FError()
   ELSE
      cData := Space( 1024 )
      cEOL := e"\r\n"
      IF ( nLen := hb_vfRead( hFile, @cData,, 10000 ) ) > 0
         ? "<<", StrTran( hb_BLeft( cData, nLen ), cEOL, cEOL + "<< " )
      ENDIF
      cSend := "EHLO" + cEOL
      nLen := hb_vfWrite( hFile, cSend,, 1000 )
      ? ">>", StrTran( hb_BLeft( cSend, nLen ), cEOL, cEOL + ">> " )
      IF nLen != hb_BLen( cSend )
         ? "WRITE ERROR:", FError()
      ENDIF
      IF ( nLen := hb_vfRead( hFile, @cData,, 10000 ) ) > 0
         ? "<<", StrTran( hb_BLeft( cData, nLen ), cEOL, cEOL + "<< " )
      ENDIF
      cSend := "QUIT" + cEOL
      nLen := hb_vfWrite( hFile, cSend,, 1000 )
      ? ">>", StrTran( hb_BLeft( cSend, nLen ), cEOL, cEOL + ">> " )
      IF nLen != hb_BLen( cSend )
         ? "WRITE ERROR:", FError()
      ENDIF
      IF ( nLen := hb_vfRead( hFile, @cData,, 10000 ) ) > 0
         ? "<<", StrTran( hb_BLeft( cData, nLen ), cEOL, cEOL + "<< " )
      ENDIF
      hb_vfClose( hFile )
      ? "closed, error:", FError()
   ENDIF
   ?
   WAIT

   RETURN
