/*
 * xHarbour Project source code:
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 * Copyright 2009 Viktor Szakats (harbour syenar.net) (SSL support)
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

/* 2004-01-13
      Enhanced tip cliente to conenct to secure smtp servers by Luiz Rafael Culik
   2007-03-29, Hannes Ziegler
      Adapted all :new() method(s) so that TIPClient becomes the
      abstract super class for TIPClientFtp, TIPClientHttp, TIPClientPop and TIPClientSmtp

      Added Methods :INetErrorDesc(), :lastErrorCode() and :lastErrorMessage()
      Removed method :data() since it calls an undeclared method :getOk()
      :data() is used in TIPClientSmtp

      Fixed bug in :readToFile()
   2007-06-01, Toninho@fwi
      Added data ::nWrite to work like ::nRead
   2009-06-29, Luiz Rafael Culik (luiz at xharbour dot com dot br)
      Added support for proxy connection
*/

#include "hbclass.ch"

#include "error.ch"
#include "fileio.ch"

#if defined( _SSL_DEBUG_TEMP )
#  include "simpleio.ch"
#endif

#include "hbssl.ch"
#undef __HBEXTREQ__
#include "hbssl.hbx"

#define RCV_BUF_SIZE Int( ::InetRcvBufSize( ::SocketCon ) / 2 )
#define SND_BUF_SIZE Int( ::InetSndBufSize( ::SocketCon ) / 2 )

/**
* Inet Client class
*/
CREATE CLASS TIPClient

   CLASS VAR bInitSocks  INIT .F.
   CLASS VAR cCRLF       INIT tip_CRLF()

   VAR oUrl                      /* url to wich to connect */
   VAR oCredentials              /* credential needed to access the service */
   VAR nStatus           INIT 0  /* basic status */
   VAR SocketCon
   VAR bTrace

   VAR nDefaultRcvBuffSize
   VAR nDefaultSndBuffSize

   VAR nLength           INIT -1 /* Input stream length */
   VAR nRead             INIT 0  /* Input stream data read by the app*/
   VAR nLastRead         INIT 0  /* Last physical read amount */

   VAR nDefaultPort
   VAR nConnTimeout
   VAR bInitialized      INIT .F.

   VAR cReply
   VAR nAccessMode
   VAR nWrite            INIT 0
   VAR nLastWrite        INIT 0

   VAR bEof              INIT .F.
   VAR isOpen            INIT .F.

   VAR exGauge              /* Gauge control; it can be a codeblock or a function pointer. */

   VAR lTLS              INIT .F.

   VAR lHasSSL           INIT tip_SSL()
   VAR ssl_ctx
   VAR ssl
   VAR nSSLError         INIT 0

   VAR Cargo

   /* Data for proxy connection */
   VAR cProxyHost
   VAR nProxyPort        INIT 0
   VAR cProxyUser
   VAR cProxyPassword

   METHOD New( oUrl, xTrace, oCredentials )
   METHOD Open( cUrl )

   METHOD EnableTLS( lEnable )

   METHOD Read( nLen )
   METHOD ReadToFile( cFile, nMode, nSize )
   METHOD Write( cData, nLen, bCommit )
   METHOD Commit()
   METHOD WriteFromFile( cFile )
   METHOD Reset()
   METHOD Close()

   /* METHOD Data( cData ) */                   // commented: calls undeclared METHOD :getOk

   METHOD SetProxy( cProxyHost, nProxyPort, cProxyUser, cProxyPassword )

   METHOD lastErrorCode() INLINE ::nLastError
   METHOD lastErrorMessage( SocketCon ) INLINE ::inetErrorDesc( SocketCon )

   METHOD InetRcvBufSize( SocketCon, nSizeBuff )
   METHOD InetSndBufSize( SocketCon, nSizeBuff )

   METHOD InetTimeOut( SocketCon, nConnTimeout )

   METHOD HasSSL() INLINE ::lHasSSL

   PROTECTED:

   VAR nLastError INIT 0

   METHOD OpenProxy( cServer, nPort, cProxy, nProxyPort, cResp, cUserName, cPassWord, cUserAgent )
   METHOD ReadHTTPProxyResponse( sResponse )

   /* Methods to log data if needed */
   METHOD inetRecv( SocketCon, cStr1, len )
   METHOD inetRecvLine( SocketCon, nRet, size )
   METHOD inetRecvAll( SocketCon, cRet, size )
   METHOD inetCount( SocketCon )
   METHOD inetSendAll( SocketCon, cData, nLen )
   METHOD inetErrorCode( SocketCon )
   METHOD inetErrorDesc( SocketCon )
   METHOD inetConnect( cServer, nPort, SocketCon )

   METHOD Log( ... )

ENDCLASS

METHOD New( oUrl, xTrace, oCredentials ) CLASS TIPClient

   LOCAL oErr
   LOCAL oLog

   LOCAL aProtoAccepted := { "ftp", "http", "pop", "smtp" }
   LOCAL aProtoAcceptedSSL := iif( ::lHasSSL, { "ftps", "https", "pop3s", "pops", "smtps" }, {} )

   IF HB_ISSTRING( xTrace ) .OR. ;
      ( HB_ISLOGICAL( xTrace ) .AND. xTrace )
      oLog := TIPLog():New( iif( HB_ISSTRING( xTrace ), xTrace, NIL ) )
      ::bTrace := {| cMsg | iif( PCount() > 0, oLog:Add( cMsg ), oLog:Close() ) }
   ELSEIF HB_ISEVALITEM( xTrace )
      ::bTrace := xTrace
   ENDIF

   IF HB_ISSTRING( oUrl )
      oUrl := TUrl():New( oUrl )
   ENDIF

   IF AScan( aProtoAccepted   , {| tmp | tmp == oURL:cProto } ) == 0 .AND. ;
      AScan( aProtoAcceptedSSL, {| tmp | tmp == oURL:cProto } ) == 0

      oErr := ErrorNew()
      oErr:Args          := { Self, oURL:cProto }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "unsupported protocol"
      oErr:GenCode       := EG_UNSUPPORTED
      oErr:Operation     := ::className() + ":new()"
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1081
      oErr:SubSystem     := "BASE"
      Eval( ErrorBlock(), oErr )
   ENDIF

   IF ! ::bInitSocks
      hb_inetInit()
      IF ::lHasSSL
         SSL_init()
         RAND_seed( Time() + hb_UserName() + DToS( Date() ) + hb_DirBase() + NetName() )
      ENDIF
      ::bInitSocks := .T.
   ENDIF

   IF ::lHasSSL
      IF oURL:cProto == "ftps" .OR. ;
         oURL:cProto == "https" .OR. ;
         oURL:cProto == "pop3s" .OR. oURL:cProto == "pops" .OR. ;
         oURL:cProto == "smtps"
         ::EnableTLS( .T. )
      ENDIF
   ENDIF

   ::oUrl         := oUrl
   ::oCredentials := oCredentials

   RETURN self

METHOD Open( cUrl ) CLASS TIPClient

   LOCAL nPort
   LOCAL cResp

   IF HB_ISSTRING( cUrl )
      ::oUrl := TUrl():New( cUrl )
   ENDIF

   IF ::oUrl:nPort == -1
      nPort := ::nDefaultPort
   ELSE
      nPort := ::oUrl:nPort
   ENDIF

   ::SocketCon := hb_inetCreate()

   ::InetTimeOut( ::SocketCon )

   IF ! Empty( ::cProxyHost )
      cResp := ""
      IF ! ::OpenProxy( ::oUrl:cServer, nPort, ::cProxyHost, ::nProxyPort, @cResp, ::cProxyUser, ::cProxyPassword, "Mozilla/3.0 compatible" )
         RETURN .F.
      ENDIF
   ELSE
      ::inetConnect( ::oUrl:cServer, nPort, ::SocketCon )

      IF ::inetErrorCode( ::SocketCon ) != 0
         RETURN .F.
      ENDIF
   ENDIF
   ::isOpen := .T.

   RETURN .T.

METHOD EnableTLS( lEnable ) CLASS TIPClient

   LOCAL lSuccess

   IF ::lTLS == lEnable
      RETURN .T.
   ENDIF

   IF lEnable
      IF ::lHasSSL
         ::ssl_ctx := SSL_CTX_new()
         ::ssl := SSL_new( ::ssl_ctx )
         ::lTLS := .T.
         lSuccess := .T.
      ELSE
         lSuccess := .F.
      ENDIF
   ELSE
      IF ::lHasSSL
         ::lTLS := .F.
         lSuccess := .T.
      ELSE
         lSuccess := .T.
      ENDIF
   ENDIF

   RETURN lSuccess

METHOD OpenProxy( cServer, nPort, cProxy, nProxyPort, cResp, cUserName, cPassWord, cUserAgent ) CLASS TIPClient

   LOCAL cRequest
   LOCAL lRet := .F.
   LOCAL tmp

   ::inetConnect( cProxy, nProxyPort, ::SocketCon )

   IF ( tmp := ::inetErrorCode( ::SocketCon ) ) == 0
      cRequest := "CONNECT " + cServer + ":" + hb_ntos( nPort ) + " HTTP/1.1" + Chr( 13 ) + Chr( 10 )
      IF ! Empty( cUserAgent )
         cRequest += "User-agent: " + cUserAgent + Chr( 13 ) + Chr( 10 )
      ENDIF
      IF ! Empty( cUserName )
         cRequest += "Proxy-authorization: Basic " + hb_base64Encode( cUserName + ":" + cPassWord ) + Chr( 13 ) + Chr( 10 )
      ENDIF
      cRequest += Chr( 13 ) + Chr( 10 )
      ::inetSendAll( ::SocketCon, cRequest )
      cResp := ""
      IF ::ReadHTTPProxyResponse( @cResp )
         tmp := At( " ", cResp )
         IF tmp > 0 .AND. Val( SubStr( cResp, tmp + 1 ) ) == 200
            lRet := .T.
         ENDIF
      ENDIF
      IF ! lRet
         ::close()
      ENDIF
   ELSE
      cResp := hb_ntos( tmp )
      lRet := .F.
   ENDIF

   RETURN lRet

METHOD ReadHTTPProxyResponse( /* @ */ sResponse ) CLASS TIPClient

   LOCAL bMoreDataToRead := .T.
   LOCAL nLength, nData
   LOCAL szResponse

   DO WHILE bMoreDataToRead

      szResponse := Space( 1 )
      nData := ::inetRecv( ::SocketCon, @szResponse, hb_BLen( szResponse ) )
      IF nData == 0
         RETURN .F.
      ENDIF
      sResponse += szResponse

      nLength := hb_BLen( sResponse )
      IF nLength >= 4
         bMoreDataToRead := !( ;
            hb_BSubStr( sResponse, nLength - 3, 1 ) == Chr( 13 ) .AND. ;
            hb_BSubStr( sResponse, nLength - 2, 1 ) == Chr( 10 ) .AND. ;
            hb_BSubStr( sResponse, nLength - 1, 1 ) == Chr( 13 ) .AND. ;
            hb_BSubStr( sResponse, nLength, 1 ) == Chr( 10 ) )
      ENDIF
   ENDDO

   RETURN .T.

METHOD Close() CLASS TIPClient

   LOCAL nRet := -1

   IF ! Empty( ::SocketCon )

      nRet := hb_inetClose( ::SocketCon )

      IF ::lHasSSL .AND. ::lTLS
         SSL_shutdown( ::ssl )
         ::ssl := NIL
         ::ssl_ctx := NIL
      ENDIF

      ::SocketCon := NIL
      ::isOpen := .F.
   ENDIF

   IF HB_ISEVALITEM( ::bTrace )
      /* Call with no parameter to signal end of logging session */
      Eval( ::bTrace )
   ENDIF

   RETURN nRet

METHOD Reset() CLASS TIPClient

   ::bInitialized := .F.
   ::bEof := .F.

   RETURN .T.

METHOD Commit() CLASS TIPClient
   RETURN .T.

METHOD Read( nLen ) CLASS TIPClient

   LOCAL cStr0
   LOCAL cStr1

   IF ::nLength > 0 .AND. ::nLength == ::nRead
      RETURN NIL
   ENDIF

   IF Empty( nLen ) .OR. nLen < 0 .OR. ( ::nLength > 0 .AND. nLen > ::nLength - ::nRead )
      nLen := ::nLength - ::nRead
   ENDIF

   IF Empty( nLen ) .OR. nLen < 0
      // read till end of stream
      cStr1 := Space( RCV_BUF_SIZE )
      cStr0 := ""
      ::nLastRead := ::inetRecv( ::SocketCon, @cStr1, RCV_BUF_SIZE )
      DO WHILE ::nLastRead > 0
         ::nRead += ::nLastRead
         cStr0 += hb_BLeft( cStr1, ::nLastRead )
         ::nLastRead := ::inetRecv( ::SocketCon, @cStr1, RCV_BUF_SIZE )
      ENDDO
      ::bEof := .T.
   ELSE
      // read an amount of data
      cStr0 := Space( nLen )

      IF ::lTLS
         IF ::lHasSSL
            /* Getting around implementing the hack used in non-SSL branch for now.
               IMO the proper fix would have been done to hb_inetRecvAll(). [vszakats] */
            ::nLastRead := ::inetRecvAll( ::SocketCon, @cStr0, nLen )
         ENDIF
      ELSE
         // S.R. if len of file is less than RCV_BUF_SIZE hb_inetRecvAll return 0
         //      ::nLastRead := ::InetRecvAll( ::SocketCon, @cStr0, nLen )
         ::inetRecvAll( ::SocketCon, @cStr0, nLen )
         ::nLastRead := ::inetCount( ::SocketCon )
      ENDIF
      ::nRead += ::nLastRead

      IF ::nLastRead != nLen
         ::bEof := .T.
         cStr0 := hb_BLeft( cStr0, ::nLastRead )
         // S.R.         RETURN NIL
      ENDIF

      IF ::nRead == ::nLength
         ::bEof := .T.
      ENDIF
   ENDIF

   RETURN cStr0

METHOD ReadToFile( cFile, nMode, nSize ) CLASS TIPClient

   LOCAL nFout
   LOCAL cData
   LOCAL nSent

   hb_default( @nMode, FC_NORMAL )

   nSent := 0

   IF ! Empty( ::exGauge )
      hb_ExecFromArray( ::exGauge, { nSent, nSize, Self } )
   ENDIF

   ::nRead   := 0
   ::nStatus := 1

   DO WHILE ::inetErrorCode( ::SocketCon ) == 0 .AND. ! ::bEof
      cData := ::Read( RCV_BUF_SIZE )
      IF cData == NIL
         IF nFout != NIL
            FClose( nFout )
         ENDIF
         RETURN ::inetErrorCode( ::SocketCon ) == 0
      ENDIF
      IF nFout == NIL
         nFout := FCreate( cFile, nMode )
         IF nFout == F_ERROR
            ::nStatus := 0
            RETURN .F.
         ENDIF
      ENDIF

      IF FWrite( nFout, cData ) != hb_BLen( cData )
         FClose( nFout )
         RETURN .F.
      ENDIF

      nSent += hb_BLen( cData )
      IF ! Empty( ::exGauge )
         hb_ExecFromArray( ::exGauge, { nSent, nSize, Self } )
      ENDIF

   ENDDO

   IF nSent > 0
      ::Commit()
   ENDIF

   ::nStatus := 2
   FClose( nFout )
   IF ::inetErrorCode( ::SocketCon ) != 0
      RETURN .F.
   ENDIF

   RETURN .T.

METHOD WriteFromFile( cFile ) CLASS TIPClient

   LOCAL nFin
   LOCAL cData
   LOCAL nLen
   LOCAL nSize, nSent, nBufSize

   ::nWrite  := 0
   ::nStatus := 0
   nFin := FOpen( cFile, FO_READ )
   IF nFin == F_ERROR
      RETURN .F.
   ENDIF
   nSize := FSeek( nFin, 0, FS_END )
   FSeek( nFin, 0 )

   nBufSize := SND_BUF_SIZE

   // allow initialization of the gauge
   nSent := 0
   IF ! Empty( ::exGauge )
      hb_ExecFromArray( ::exGauge, { nSent, nSize, Self } )
   ENDIF

   ::nStatus := 1
   cData := Space( nBufSize )
   nLen := FRead( nFin, @cData, nBufSize )
   DO WHILE nLen > 0
      IF ::Write( @cData, nLen ) != nLen
         FClose( nFin )
         RETURN .F.
      ENDIF
      nSent += nLen
      IF ! Empty( ::exGauge )
         hb_ExecFromArray( ::exGauge, { nSent, nSize, Self } )
      ENDIF
      nLen := FRead( nFin, @cData, nBufSize )
   ENDDO

   // it may happen that the file has length 0
   IF nSent > 0
      ::Commit()
   ENDIF

   ::nStatus := 2
   FClose( nFin )

   RETURN .T.

#if 0

/* HZ: METHOD :getOk() is not declared in TIPClient */
METHOD Data( cData ) CLASS TIPClient
   ::InetSendall( ::SocketCon, "DATA" + ::cCRLF )
   IF ! ::GetOk()
      RETURN .F.
   ENDIF
   ::InetSendall(::SocketCon, cData + ::cCRLF + "." + ::cCRLF )
   RETURN ::GetOk()

#endif

METHOD Write( cData, nLen, bCommit ) CLASS TIPClient

   IF Empty( nLen )
      nLen := hb_BLen( cData )
   ENDIF

   ::nLastWrite := ::inetSendAll( ::SocketCon, cData, nLen )

   IF ! Empty( bCommit ) .AND. bCommit
      ::Commit()
   ENDIF

   ::nWrite += ::nLastWrite

   RETURN ::nLastWrite

METHOD inetSendAll( SocketCon, cData, nLen ) CLASS TIPClient

   LOCAL nRet

   IF Empty( nLen )
      nLen := hb_BLen( cData )
   ENDIF

   IF ::lTLS
      IF ::lHasSSL
#if defined( _SSL_DEBUG_TEMP )
         ? "SSL_write()", cData
#endif
         nRet := SSL_write( ::ssl, cData, nLen )
         ::nSSLError := iif( nRet < 0, nRet, 0 )
      ELSE
         nRet := 0
      ENDIF
   ELSE
      nRet := hb_inetSendAll( SocketCon, cData, nLen )
   ENDIF

   IF HB_ISEVALITEM( ::bTrace )
      ::Log( SocketCon, nlen, cData, nRet )
   ENDIF

   RETURN nRet

METHOD inetCount( SocketCon ) CLASS TIPClient

   LOCAL nRet := hb_inetCount( SocketCon )

   IF HB_ISEVALITEM( ::bTrace )
      ::Log( SocketCon, nRet )
   ENDIF

   RETURN nRet

METHOD inetRecv( SocketCon, cStr1, len ) CLASS TIPClient

   LOCAL nRet

   IF ::lTLS
      IF ::lHasSSL
#if defined( _SSL_DEBUG_TEMP )
         ? "SSL_read()"
#endif
         nRet := SSL_read( ::ssl, @cStr1, len )
         ::nSSLError := iif( nRet < 0, nRet, 0 )
      ELSE
         nRet := 0
      ENDIF
   ELSE
      nRet := hb_inetRecv( SocketCon, @cStr1, len )
   ENDIF

   IF HB_ISEVALITEM( ::bTrace )
      ::Log( SocketCon, "", len, iif( nRet >= 0, cStr1, nRet ) )
   ENDIF

   RETURN nRet

METHOD inetRecvLine( SocketCon, nRet, size ) CLASS TIPClient

   LOCAL cRet

   IF ::lTLS
      IF ::lHasSSL
         nRet := hb_SSL_read_line( ::ssl, @cRet, size, ::nConnTimeout )
#if defined( _SSL_DEBUG_TEMP )
         ? "hb_SSL_read_line()", cRet
#endif
         IF nRet == 0 .OR. Empty( cRet )
            cRet := NIL
         ENDIF
         ::nSSLError := iif( nRet < 0, nRet, 0 )
      ELSE
         cRet := ""
         nRet := 0
      ENDIF
   ELSE
      cRet := hb_inetRecvLine( SocketCon, @nRet, size )
   ENDIF

   IF HB_ISEVALITEM( ::bTrace )
      ::Log( SocketCon, "", size, cRet )
   ENDIF

   RETURN cRet

METHOD inetRecvAll( SocketCon, cRet, size ) CLASS TIPClient

   LOCAL nRet

   IF ::lTLS
      IF ::lHasSSL
         nRet := hb_SSL_read_all( ::ssl, @cRet, size, ::nConnTimeout )
#if defined( _SSL_DEBUG_TEMP )
         ? "hb_SSL_read_all()", cRet
#endif
         IF nRet == 0 .OR. Empty( cRet )
            cRet := NIL
         ENDIF
         ::nSSLError := iif( nRet < 0, nRet, 0 )
      ELSE
         cRet := ""
         nRet := 0
      ENDIF
   ELSE
      nRet := hb_inetRecvAll( SocketCon, @cRet, size )
   ENDIF

   IF HB_ISEVALITEM( ::bTrace )
      ::Log( SocketCon, "", size, iif( nRet >= 0, cRet, nRet ) )
   ENDIF

   RETURN nRet

METHOD inetErrorCode( SocketCon ) CLASS TIPClient

   LOCAL nRet

   IF ::lTLS
      IF ::lHasSSL
         nRet := iif( ::nSSLError == 0, 0, SSL_get_error( ::ssl, ::nSSLError ) )
      ELSE
         nRet := 0
      ENDIF
   ELSE
      nRet := hb_inetErrorCode( SocketCon )
   ENDIF

   ::nLastError := nRet

   IF HB_ISEVALITEM( ::bTrace )
      ::Log( SocketCon, nRet )
   ENDIF

   RETURN nRet

METHOD inetErrorDesc( SocketCon ) CLASS TIPClient

   LOCAL cMsg := ""

   hb_default( @SocketCon, ::SocketCon )

   IF ! Empty( SocketCon )
      IF ::lTLS
         IF ::lHasSSL .AND. ::nSSLError != 0
            cMsg := ERR_error_string( SSL_get_error( ::ssl, ::nSSLError ) )
         ENDIF
      ELSE
         cMsg := hb_inetErrorDesc( SocketCon )
      ENDIF
   ENDIF

   RETURN cMsg

/* BROKEN, should test number of parameters and act accordingly, see doc\inet.txt */
METHOD inetConnect( cServer, nPort, SocketCon ) CLASS TIPClient

   hb_inetConnect( cServer, nPort, SocketCon )

   /* IMPORTANT: if internet connection is off and address is not resolved and it is SSL compliant, then RTE must be avoided [pritpal] */
   IF hb_inetStatus( SocketCon ) == -1
      RETURN NIL
   ENDIF

   IF ! Empty( ::nDefaultSndBuffSize )
      ::InetSndBufSize( SocketCon, ::nDefaultSndBuffSize )
   ENDIF

   IF ! Empty( ::nDefaultRcvBuffSize )
      ::InetRcvBufSize( SocketCon, ::nDefaultRcvBuffSize )
   ENDIF

   IF ::lHasSSL .AND. ::lTLS
      SSL_set_mode( ::ssl, HB_SSL_MODE_AUTO_RETRY )
      SSL_set_fd( ::ssl, hb_inetFD( SocketCon ) )
      SSL_connect( ::ssl )
      /* TODO: Add error handling */
   ENDIF

   IF HB_ISEVALITEM( ::bTrace )
      ::Log( cServer, nPort, SocketCon )
   ENDIF

   RETURN NIL

/* Methods to manage buffers */
METHOD InetRcvBufSize( SocketCon, nSizeBuff ) CLASS TIPClient

   IF ! Empty( nSizeBuff )
      hb_inetSetRcvBufSize( SocketCon, nSizeBuff )
   ENDIF

   RETURN hb_inetGetRcvBufSize( SocketCon )

METHOD InetSndBufSize( SocketCon, nSizeBuff ) CLASS TIPClient

   IF ! Empty( nSizeBuff )
      hb_inetSetSndBufSize( SocketCon, nSizeBuff )
   ENDIF

   RETURN hb_inetGetSndBufSize( SocketCon )

METHOD InetTimeOut( SocketCon, nConnTimeout ) CLASS TIPClient

   IF HB_ISNUMERIC( nConnTimeout )
      ::nConnTimeout := nConnTimeout
   ENDIF
   IF HB_ISNUMERIC( ::nConnTimeout )
      RETURN hb_inetTimeout( SocketCon, ::nConnTimeout )
   ENDIF

   RETURN NIL

/* Called from another method with list of parameters and, as last parameter, return code
   of function being logged.
   Example, I want to log MyFunc( a, b, c ) which returns m,
            ::Log( a, b, c, m )
*/
METHOD Log( ... ) CLASS TIPClient

   LOCAL xVar
   LOCAL cMsg

   IF HB_ISEVALITEM( ::bTrace )

      cMsg := DToS( Date() ) + "-" + Time() + Space( 2 ) + ;
         SubStr( ProcName( 1 ), RAt( ":", ProcName( 1 ) ) ) + ;
         "( "

      FOR EACH xVar IN hb_AParams()

         // Preserves CRLF on result
         IF xVar:__enumIndex() < PCount()
            cMsg += StrTran( StrTran( AllTrim( hb_CStr( xVar ) ), Chr( 13 ), "<cr>" ), Chr( 10 ), "<lf>" )
         ELSE
            cMsg += hb_CStr( xVar )
         ENDIF

         cMsg += iif( xVar:__enumIndex() < PCount() - 1, ", ", "" )

         IF xVar:__enumIndex() == PCount() - 1
            cMsg += " )" + hb_eol() + ">> "
         ELSEIF xVar:__enumIndex() == PCount()
            cMsg += " <<" + hb_eol() + hb_eol()
         ENDIF
      NEXT

      Eval( ::bTrace, cMsg )
   ENDIF

   RETURN Self

METHOD SetProxy( cProxyHost, nProxyPort, cProxyUser, cProxyPassword ) CLASS TIPClient

   ::cProxyHost     := cProxyHost
   ::nProxyPort     := nProxyPort
   ::cProxyUser     := cProxyUser
   ::cProxyPassword := cProxyPassword

   RETURN Self

FUNCTION tip_SSL()
   RETURN hb_IsFunction( "__HBEXTERN__HBSSL__" )
