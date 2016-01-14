/*
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 * Copyright 2009 Viktor Szakats (vszakats.net/harbour) (SSL support)
 * Copyright 2015 Jean Lefebvre (TLS support) 
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
   2015-01-29, Jean Lefebvre
      added FUNCTION ActivateSSL(Self) 
      to be used here and into smtpcli (needed to post activate SSL as request in TLS)  
      Also, changed all reference to TLS to SSL 
*/

#include "hbclass.ch"

#include "error.ch"
#include "fileio.ch"

#if defined( _SSL_DEBUG_TEMP )
   #include "simpleio.ch"
#endif

#include "hbssl.ch"
#undef __HBEXTREQ__
#include "hbssl.hbx"

#define RCV_BUF_SIZE Int( ::InetRcvBufSize( ::SocketCon ) / 2 )
#define SND_BUF_SIZE Int( ::InetSndBufSize( ::SocketCon ) / 2 )

/* Inet Client class */
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

   VAR lSSL              INIT .F.

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

   METHOD EnableSSL( lEnable )

   METHOD Read( nLen )
   METHOD ReadToFile( cFile, nMode, nSize )
   METHOD Write( cData, nLen, lCommit )
   METHOD Commit()
   METHOD WriteFromFile( cFile )
   METHOD Reset()
   METHOD Close()

   METHOD SetProxy( cProxyHost, nProxyPort, cProxyUser, cProxyPassword )

   METHOD lastErrorCode() INLINE ::nLastError
   METHOD lastErrorMessage( SocketCon ) INLINE ::inetErrorDesc( SocketCon )

   METHOD InetRcvBufSize( SocketCon, nSizeBuff )
   METHOD InetSndBufSize( SocketCon, nSizeBuff )

   METHOD InetTimeOut( SocketCon, nConnTimeout )

   METHOD HasSSL() INLINE ::lHasSSL

   PROTECTED:

   VAR nLastError INIT 0

   METHOD OpenProxy( cServer, nPort, cProxy, nProxyPort, cResp, cUserName, cPassword, cUserAgent )
   METHOD ReadHTTPProxyResponse( cResponse )

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
   LOCAL lSSL

   IF HB_ISSTRING( xTrace ) .OR. hb_defaultValue( xTrace, .F. )
      oLog := TIPLog():New( iif( HB_ISSTRING( xTrace ), xTrace, NIL ) )
      ::bTrace := {| cMsg | iif( PCount() > 0, oLog:Add( cMsg ), oLog:Close() ) }
   ELSEIF HB_ISEVALITEM( xTrace )
      ::bTrace := xTrace
   ENDIF

   IF HB_ISSTRING( oUrl )
      oUrl := TUrl():New( oUrl )
   ENDIF

   SWITCH oURL:cProto
   CASE "ftp"
   CASE "http"
   CASE "pop"
   CASE "smtp"
      lSSL := .F.
      EXIT
   CASE "ftps"
   CASE "https"
   CASE "pop3s"
   CASE "pops"
   CASE "smtps"
      IF ::lHasSSL
         lSSL := .T.
         EXIT
      ENDIF
   OTHERWISE
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
   ENDSWITCH

   IF ! ::bInitSocks
      hb_inetInit()
      IF ::lHasSSL
         SSL_init()
         RAND_seed( hb_randStr( 20 ) + hb_TToS( hb_DateTime() ) + NetName() )
      ENDIF
      ::bInitSocks := .T.
   ENDIF

   IF ::lHasSSL .AND. lSSL
      ::EnableSSL( .T. )
   ENDIF

   ::oUrl         := oUrl
   ::oCredentials := oCredentials

   RETURN Self

METHOD Open( cUrl ) CLASS TIPClient

   LOCAL nPort

   IF HB_ISSTRING( cUrl )
      ::oUrl := TUrl():New( cUrl )
   ENDIF

   nPort := iif( ::oUrl:nPort == -1, ::nDefaultPort, ::oUrl:nPort )

   ::SocketCon := hb_inetCreate()

   ::InetTimeOut( ::SocketCon )

   IF Empty( ::cProxyHost )
      ::inetConnect( ::oUrl:cServer, nPort, ::SocketCon )
      IF ::inetErrorCode( ::SocketCon ) != 0
         RETURN .F.
      ENDIF
   ELSEIF ! ::OpenProxy( ::oUrl:cServer, nPort, ::cProxyHost, ::nProxyPort,, ::cProxyUser, ::cProxyPassword, "Mozilla/3.0 compatible" )
      RETURN .F.
   ENDIF
   ::isOpen := .T.

   RETURN .T.

METHOD EnableSSL( lEnable ) CLASS TIPClient

   LOCAL lSuccess

   IF ::lSSL == lEnable
      RETURN .T.
   ENDIF

   IF lEnable
      IF ::lHasSSL
         ::ssl_ctx := SSL_CTX_new()
         ::ssl := SSL_new( ::ssl_ctx )
         ::lSSL := .T.
         lSuccess := .T.
      ELSE
         lSuccess := .F.
      ENDIF
   ELSE
      IF ::lHasSSL
         ::lSSL := .F.
         lSuccess := .T.
      ELSE
         lSuccess := .T.
      ENDIF
   ENDIF

   RETURN lSuccess

METHOD OpenProxy( cServer, nPort, cProxy, nProxyPort, cResp, cUserName, cPassword, cUserAgent ) CLASS TIPClient

   LOCAL cRequest
   LOCAL lRet := .F.
   LOCAL tmp

   ::inetConnect( cProxy, nProxyPort, ::SocketCon )

   IF ( tmp := ::inetErrorCode( ::SocketCon ) ) == 0
      cRequest := ;
         "CONNECT " + cServer + ":" + hb_ntos( nPort ) + " HTTP/1.1" + Chr( 13 ) + Chr( 10 ) + ;
         "Proxy-Connection: Keep-Alive" + Chr( 13 ) + Chr( 10 )
      IF HB_ISSTRING( cUserAgent ) .AND. ! Empty( cUserAgent )
         cRequest += "User-Agent: " + cUserAgent + Chr( 13 ) + Chr( 10 )
      ENDIF
      IF HB_ISSTRING( cUserName ) .AND. ! Empty( cUserName )
         cRequest += "Proxy-Authorization: Basic " + hb_base64Encode( cUserName + ":" + hb_defaultValue( cPassword, "" ) ) + Chr( 13 ) + Chr( 10 )
      ENDIF
      cRequest += Chr( 13 ) + Chr( 10 )
      ::inetSendAll( ::SocketCon, cRequest )
      cResp := ""
      IF ::ReadHTTPProxyResponse( @cResp ) .AND. ;
         ( tmp := At( " ", cResp ) ) > 0 .AND. ;
         Val( SubStr( cResp, tmp + 1 ) ) == 200
         lRet := .T.
      ELSE
         ::close()
      ENDIF
   ELSE
      cResp := hb_ntos( tmp )
   ENDIF

   RETURN lRet

METHOD ReadHTTPProxyResponse( /* @ */ cResponse ) CLASS TIPClient

   LOCAL cBuffer

   DO WHILE .T.
      cBuffer := Space( 1 )
      IF ::inetRecv( ::SocketCon, @cBuffer, hb_BLen( cBuffer ) ) <= 0
         RETURN .F.
      ENDIF
      cResponse += cBuffer
      IF hb_BRight( cResponse, 4 ) == e"\r\n\r\n"
         EXIT
      ENDIF
   ENDDO

   RETURN .T.

METHOD Close() CLASS TIPClient

   LOCAL nRet

   IF Empty( ::SocketCon )
      nRet := -1
   ELSE
      nRet := hb_inetClose( ::SocketCon )

      IF ::lSSL .AND. ::lHasSSL
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

   IF ! HB_ISNUMERIC( nLen ) .OR. nLen <= 0 .OR. ( ::nLength > 0 .AND. nLen > ::nLength - ::nRead )
      nLen := ::nLength - ::nRead
   ENDIF

   IF nLen <= 0
      // read till end of stream
      cStr1 := Space( RCV_BUF_SIZE )
      cStr0 := ""
      DO WHILE ( ::nLastRead := ::inetRecv( ::SocketCon, @cStr1, RCV_BUF_SIZE ) ) > 0
         ::nRead += ::nLastRead
         cStr0 += hb_BLeft( cStr1, ::nLastRead )
      ENDDO
      ::bEof := .T.
   ELSE
      // read an amount of data
      cStr0 := Space( nLen )

      IF ::lSSL .AND. ::lHasSSL
         /* Getting around implementing the hack used in non-SSL branch for now.
            IMO the proper fix would have been done to hb_inetRecvAll(). [vszakats] */
         ::nLastRead := ::inetRecvAll( ::SocketCon, @cStr0, nLen )
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
#if 0
         RETURN NIL
#endif
      ENDIF

      IF ::nRead == ::nLength
         ::bEof := .T.
      ENDIF
   ENDIF

   RETURN cStr0

METHOD ReadToFile( /* @ */ cFile, nMode, nSize ) CLASS TIPClient

   LOCAL nFOut
   LOCAL cData
   LOCAL nSent := 0

   LOCAL lToMemory := hb_PIsByRef( 1 )

   hb_default( @nSize, 0 )

   IF lToMemory
      cFile := ""
   ENDIF

   IF HB_ISEVALITEM( ::exGauge )
      Eval( ::exGauge, nSent, nSize, Self )
   ENDIF

   ::nRead   := 0
   ::nStatus := 1

   DO WHILE ::inetErrorCode( ::SocketCon ) == 0 .AND. ! ::bEof
      IF ( cData := ::Read( RCV_BUF_SIZE ) ) == NIL
         IF nFOut != NIL
            FClose( nFOut )
         ENDIF
         RETURN ::inetErrorCode( ::SocketCon ) == 0
      ENDIF
      IF ! lToMemory .AND. nFOut == NIL
         IF ( nFOut := FCreate( cFile, nMode ) ) == F_ERROR
            ::nStatus := 0
            RETURN .F.
         ENDIF
      ENDIF

      IF lToMemory
         cFile += cData
      ELSEIF FWrite( nFOut, cData ) != hb_BLen( cData )
         FClose( nFOut )
         RETURN .F.
      ENDIF

      nSent += hb_BLen( cData )

      IF HB_ISEVALITEM( ::exGauge )
         Eval( ::exGauge, nSent, nSize, Self )
      ENDIF
   ENDDO

   IF nSent > 0
      ::Commit()
   ENDIF

   ::nStatus := 2
   IF nFOut != NIL
      FClose( nFOut )
   ENDIF

   RETURN ::inetErrorCode( ::SocketCon ) == 0

METHOD WriteFromFile( cFile ) CLASS TIPClient

   LOCAL nFIn
   LOCAL cData
   LOCAL nLen
   LOCAL nSize, nSent, nBufSize

   ::nWrite  := 0
   ::nStatus := 0
   IF ( nFIn := FOpen( cFile ) ) == F_ERROR
      RETURN .F.
   ENDIF
   nSize := FSeek( nFIn, 0, FS_END )
   FSeek( nFIn, 0 )

   nBufSize := SND_BUF_SIZE

   // allow initialization of the gauge
   nSent := 0

   IF HB_ISEVALITEM( ::exGauge )
      Eval( ::exGauge, nSent, nSize, Self )
   ENDIF

   ::nStatus := 1
   cData := Space( nBufSize )
   DO WHILE ( nLen := FRead( nFIn, @cData, nBufSize ) ) > 0
      IF ::Write( @cData, nLen ) != nLen
         FClose( nFIn )
         RETURN .F.
      ENDIF
      nSent += nLen
      IF HB_ISEVALITEM( ::exGauge )
         Eval( ::exGauge, nSent, nSize, Self )
      ENDIF
   ENDDO

   // it may happen that the file has length 0
   IF nSent > 0
      ::Commit()
   ENDIF

   ::nStatus := 2
   FClose( nFIn )

   RETURN .T.

METHOD Write( cData, nLen, lCommit ) CLASS TIPClient

   IF ! HB_ISNUMERIC( nLen ) .OR. nLen <= 0
      nLen := hb_BLen( cData )
   ENDIF

   ::nLastWrite := ::inetSendAll( ::SocketCon, cData, nLen )

   IF hb_defaultValue( lCommit, .F. )
      ::Commit()
   ENDIF

   ::nWrite += ::nLastWrite

   RETURN ::nLastWrite

METHOD inetSendAll( SocketCon, cData, nLen ) CLASS TIPClient

   LOCAL nRet

   IF ! HB_ISNUMERIC( nLen ) .OR. nLen <= 0
      nLen := hb_BLen( cData )
   ENDIF

   IF ::lSSL
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

   IF ::lSSL
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

   IF ::lSSL
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

   IF ::lSSL
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

   IF ::lSSL
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

   hb_default( @SocketCon, ::SocketCon )

   IF ! Empty( SocketCon )
      IF ::lSSL
         IF ::lHasSSL .AND. ::nSSLError != 0
            RETURN ERR_error_string( SSL_get_error( ::ssl, ::nSSLError ) )
         ENDIF
      ELSE
         RETURN hb_inetErrorDesc( SocketCon )
      ENDIF
   ENDIF

   RETURN ""

/* BROKEN, should test number of parameters and act accordingly, see doc/inet.txt */
METHOD inetConnect( cServer, nPort, SocketCon ) CLASS TIPClient

   hb_inetConnect( cServer, nPort, SocketCon )

   /* IMPORTANT: if internet connection is off and address is not
                 resolved and it is SSL compliant, then RTE must
                 be avoided [pritpal] */
   IF hb_inetStatus( SocketCon ) == -1
      RETURN NIL
   ENDIF

   IF hb_defaultValue( ::nDefaultSndBuffSize, 0 ) > 0
      ::InetSndBufSize( SocketCon, ::nDefaultSndBuffSize )
   ENDIF

   IF hb_defaultValue( ::nDefaultRcvBuffSize, 0 ) > 0
      ::InetRcvBufSize( SocketCon, ::nDefaultRcvBuffSize )
   ENDIF

   IF ::lSSL .AND. ::lHasSSL
      __tip_SSLConnectFD( ::ssl, SocketCon )
   ENDIF

   IF HB_ISEVALITEM( ::bTrace )
      ::Log( cServer, nPort, SocketCon )
   ENDIF

   RETURN NIL

/* Methods to manage buffers */
METHOD InetRcvBufSize( SocketCon, nSizeBuff ) CLASS TIPClient

   IF HB_ISNUMERIC( nSizeBuff ) .AND. nSizeBuff > 0
      hb_inetSetRcvBufSize( SocketCon, nSizeBuff )
   ENDIF

   RETURN hb_inetGetRcvBufSize( SocketCon )

METHOD InetSndBufSize( SocketCon, nSizeBuff ) CLASS TIPClient

   IF HB_ISNUMERIC( nSizeBuff ) .AND. nSizeBuff > 0
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
            ::Log( a, b, c, m ) */
METHOD Log( ... ) CLASS TIPClient

   LOCAL xVar
   LOCAL cMsg

   IF HB_ISEVALITEM( ::bTrace )

      cMsg := DToS( Date() ) + "-" + Time() + Space( 2 ) + ;
         SubStr( ProcName( 1 ), RAt( ":", ProcName( 1 ) ) ) + ;
         "( "

      FOR EACH xVar IN hb_AParams()

         // Preserves CRLF on result
         IF xVar:__enumIsLast()
            cMsg += hb_CStr( xVar )
         ELSE
            cMsg += hb_StrReplace( AllTrim( hb_CStr( xVar ) ), Chr( 13 ) + Chr( 10 ), { "<cr>", "<lf>" } )
         ENDIF

         IF xVar:__enumIsLast()
            cMsg += " <<" + hb_eol() + hb_eol()
         ELSEIF xVar:__enumIndex() == PCount() - 1
            cMsg += " )" + hb_eol() + ">> "
         ELSE
            cMsg += ", "
         ENDIF
      NEXT

      Eval( ::bTrace, cMsg )
   ENDIF

   RETURN Self

METHOD SetProxy( cProxyHost, nProxyPort, cProxyUser, cProxyPassword ) CLASS TIPClient

   IF __clsParent( cProxyHost:classH(), "TURL" )
      ::cProxyHost     := cProxyHost:cServer
      ::nProxyPort     := cProxyHost:nPort
      ::cProxyUser     := cProxyHost:cUserid
      ::cProxyPassword := cProxyHost:cPassword
   ELSE
      ::cProxyHost     := hb_defaultValue( cProxyHost, "" )
      ::nProxyPort     := hb_defaultValue( nProxyPort, 0 )
      ::cProxyUser     := hb_defaultValue( cProxyUser, "" )
      ::cProxyPassword := hb_defaultValue( cProxyPassword, "" )
   ENDIF

   RETURN Self

FUNCTION tip_SSL()
   RETURN hb_IsFunction( "__HBEXTERN__HBSSL__" )

FUNCTION __tip_SSLConnectFD( ssl, SocketCon )

   SSL_set_mode( ssl, HB_SSL_MODE_AUTO_RETRY )
   SSL_set_fd( ssl, hb_inetFD( SocketCon ) )
   SSL_connect( ssl )

   /* TODO: Add error handling */

   RETURN .T.
