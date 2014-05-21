/*
 * Copyright 2009 Mindaugas Kavaliauskas <dbtopas / at / dbtopas.lt>
 * www - http://harbour-project.org
 */

#include "directry.ch"
#include "error.ch"

#include "hbclass.ch"
#include "hbsocket.ch"
#include "hbthread.ch"
#include "hbver.ch"

#include "hbssl.ch"
#include "hbssl.hbx"

#pragma -km+

/*
  Docs:

  RFC 1945 - Hypertext Transfer Protocol -- HTTP/1.0
  RFC 2616 - Hypertext Transfer Protocol -- HTTP/1.1
  HTTP Made Really Easy (http://www.jmarshall.com/easy/http/)
*/

#define THREAD_COUNT_PREALLOC    3
#define THREAD_COUNT_MAX         50
#define SESSION_TIMEOUT          600

#define CR_LF                    ( Chr( 13 ) + Chr( 10 ) )

THREAD STATIC t_cResult, t_nStatusCode, t_aHeader, t_aSessionData

MEMVAR server, get, post, cookie, session, httpd

CREATE CLASS UHttpd MODULE FRIENDLY

   EXPORTED:
   METHOD Run( hConfig )
   METHOD Stop()

   VAR cError INIT ""

   HIDDEN:
   VAR hConfig

   VAR aFirewallFilter

   VAR hmtxQueue
   VAR hmtxLog
   VAR hmtxSession

   VAR hListen
   VAR hSSLCtx
   VAR hSession

   VAR lStop

   VAR lHasSSL INIT hb_IsFunction( "__HBEXTERN__HBSSL__" )

   METHOD LogAccess()
   METHOD LogError( cError )

ENDCLASS

FUNCTION UHttpdNew()
   RETURN UHttpd()

METHOD Run( hConfig ) CLASS UHttpd

   LOCAL hSocket, nI, aI, xValue, aThreads, nJobs, nWorkers

   IF ! hb_mtvm()
      ::cError := "Multithread support required"
      RETURN .F.
   ENDIF

   ::hConfig := { ;
      "SSL"                  => .F., ;
      "Port"                 => 80, ;
      "BindAddress"          => "0.0.0.0", ;
      "LogAccess"            => {|| NIL }, ;
      "LogError"             => {|| NIL }, ;
      "Trace"                => {|| NIL }, ;
      "Idle"                 => {|| NIL }, ;
      "Mount"                => { => }, ;
      "PrivateKeyFilename"   => "", ;
      "CertificateFilename"  => "", ;
      "FirewallFilter"       => "0.0.0.0/0" }

   FOR EACH xValue IN hConfig
      IF !( xValue:__enumKey $ ::hConfig ) .OR. !( ValType( xValue ) == ValType( ::hConfig[ xValue:__enumKey ] ) )
         ::cError := "Invalid config option '" + xValue:__enumKey + "'"
         RETURN .F.
      ENDIF
      ::hConfig[ xValue:__enumKey ] := xValue
   NEXT


   IF ::hConfig[ "SSL" ]
      IF ::lHasSSL
         SSL_init()
         DO WHILE RAND_status() != 1
            RAND_add( Str( hb_Random(), 18, 15 ) + Str( hb_MilliSeconds(), 20 ), 1 )
         ENDDO

         ::hSSLCtx := SSL_CTX_new( HB_SSL_CTX_NEW_METHOD_SSLV23_SERVER )
         SSL_CTX_set_options( ::hSSLCtx, HB_SSL_OP_NO_TLSv1 )
         IF SSL_CTX_use_PrivateKey_file( ::hSSLCtx, ::hConfig[ "PrivateKeyFilename" ], HB_SSL_FILETYPE_PEM ) != 1
            ::cError := "Invalid private key file"
            RETURN .F.
         ENDIF
         IF SSL_CTX_use_certificate_file( ::hSSLCtx, ::hConfig[ "CertificateFilename" ], HB_SSL_FILETYPE_PEM ) != 1
            ::cError := "Invalid certificate file"
            RETURN .F.
         ENDIF
      ELSE
         ::cError := "SSL not supported"
         RETURN .F.
      ENDIF
   ENDIF

   IF ::hConfig[ "Port" ] < 1 .OR. ::hConfig[ "Port" ] > 65535
      ::cError := "Invalid port number"
      RETURN .F.
   ENDIF

   IF ParseFirewallFilter( ::hConfig[ "FirewallFilter" ], @aI )
      ::aFirewallFilter := aI
   ELSE
      ::cError := "Invalid firewall filter"
      RETURN .F.
   ENDIF

   ::hmtxQueue   := hb_mutexCreate()
   ::hmtxLog     := hb_mutexCreate()
   ::hmtxSession := hb_mutexCreate()

   IF Empty( ::hListen := hb_socketOpen() )
      ::cError := "Socket create error: " + hb_socketErrorString()
      RETURN .F.
   ENDIF

   IF ! hb_socketBind( ::hListen, { HB_SOCKET_AF_INET, ::hConfig[ "BindAddress" ], ::hConfig[ "Port" ] } )
      ::cError := "Bind error: " + hb_socketErrorString()
      hb_socketClose( ::hListen )
      RETURN .F.
   ENDIF

   IF ! hb_socketListen( ::hListen )
      ::cError := "Listen error: " + hb_socketErrorString()
      hb_socketClose( ::hListen )
      RETURN .F.
   ENDIF

   aThreads := {}
   FOR nI := 1 TO THREAD_COUNT_PREALLOC
      AAdd( aThreads, hb_threadStart( HB_THREAD_INHERIT_PUBLIC, @ProcessConnection(), Self ) )
   NEXT

   ::lStop := .F.
   ::hSession := { => }

   DO WHILE .T.
      IF Empty( hSocket := hb_socketAccept( ::hListen,, 1000 ) )
         IF hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT
            Eval( ::hConfig[ "Idle" ], Self )
            IF ::lStop
               EXIT
            ENDIF
         ELSE
            ::LogError( "[error] Accept error: " + hb_socketErrorString() )
         ENDIF
      ELSE
         Eval( ::hConfig[ "Trace" ], "New connection", hSocket )
         IF hb_mutexQueueInfo( ::hmtxQueue, @nWorkers, @nJobs ) .AND. ;
               Len( aThreads ) < THREAD_COUNT_MAX .AND. ;
               nJobs >= nWorkers
            AAdd( aThreads, hb_threadStart( HB_THREAD_INHERIT_PUBLIC, @ProcessConnection(), Self ) )
         ENDIF
         hb_mutexNotify( ::hmtxQueue, hSocket )
      ENDIF
   ENDDO
   hb_socketClose( ::hListen )

   /* End child threads */
   AEval( aThreads, {|| hb_mutexNotify( ::hmtxQueue, NIL ) } )
   AEval( aThreads, {| h | hb_threadJoin( h ) } )

   RETURN .T.

METHOD Stop() CLASS UHttpd

   Eval( ::hConfig[ "Trace" ], "stopping" )
   ::lStop := .T.

   RETURN NIL

METHOD LogError( cError ) CLASS UHttpd

   hb_mutexLock( ::hmtxLog )
   Eval( ::hConfig[ "LogError" ], DToS( Date() ) + " " + Time() + " " + cError )
   hb_mutexUnlock( ::hmtxLog )

   RETURN NIL

METHOD LogAccess() CLASS UHttpd

   LOCAL cDate := DToS( Date() ), cTime := Time()

   hb_mutexLock( ::hmtxLog )
   Eval( ::hConfig[ "LogAccess" ], ;
      server[ "REMOTE_ADDR" ] + " - - [" + Right( cDate, 2 ) + "/" + ;
      { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" }[ Val( SubStr( cDate, 5, 2 ) ) ] + ;
      "/" + Left( cDate, 4 ) + ":" + cTime + ' +0000] "' + server[ "REQUEST_ALL" ] + '" ' + ;
      hb_ntos( t_nStatusCode ) + " " + hb_ntos( Len( t_cResult ) ) + ;
      ' "' + server[ "HTTP_REFERER" ] + '" "' + server[ "HTTP_USER_AGENT" ] + ;
      '"' )
   hb_mutexUnlock( ::hmtxLog )

   RETURN NIL

STATIC FUNCTION IPAddr2Num( cIP )

   LOCAL aA, n1, n2, n3, n4

   aA := hb_regex( "^(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$", cIP )
   IF Len( aA ) == 5 .AND. ( n1 := Val( aA[ 2 ] ) ) <= 255 .AND. ( n2 := Val( aA[ 3 ] ) ) <= 255 .AND. ;
         ( n3 := Val( aA[ 4 ] ) ) <= 255 .AND. ( n4 := Val( aA[ 5 ] ) ) <= 255
      RETURN ( ( ( n1 * 256 ) + n2 ) * 256 + n3 ) * 256 + n4
   ENDIF

   RETURN NIL

STATIC FUNCTION ParseFirewallFilter( cFilter, aFilter )

   LOCAL cExpr, nI, cI, nPrefix, nAddr, nAddr2, nPos, nPos2, lDeny, aDeny, aI

   aFilter := { => }
   hb_HKeepOrder( aFilter, .F. )
   aDeny := {}
   FOR EACH cExpr IN hb_ATokens( cFilter, " " )
      IF ! Empty( cExpr )
         IF ( lDeny := hb_LeftEq( cExpr, "!" ) )
            cExpr := SubStr( cExpr, 2 )
         ENDIF
         IF ( nI := At( "/", cExpr ) ) > 0
            cI := SubStr( cExpr, nI + 1 )
            cExpr := Left( cExpr, nI - 1 )
            IF "." $ cI
               IF ( nI := IPAddr2Num( cI ) ) == NIL
                  RETURN .F.
               ENDIF
               nPrefix := 32
               DO WHILE hb_bitAnd( nI, 1 ) == 0
                  nPrefix--
                  nI := hb_bitShift( nI, -1 )
               ENDDO
               IF nI + 1 != hb_bitShift( 1, nPrefix )
                  RETURN .F.
               ENDIF
            ELSE
               nPrefix := Val( cI )
               IF nPrefix < 0 .OR. nPrefix > 32 .OR. !( hb_ntos( nPrefix ) == cI )
                  RETURN .F.
               ENDIF
            ENDIF
         ELSE
            nPrefix := 32
         ENDIF
         IF ( nAddr := IPAddr2Num( cExpr ) ) == NIL
            RETURN .F.
         ENDIF
         nPrefix := 0x100000000 - hb_bitShift( 1, 32 - nPrefix )

         // Remove unnecessary network address part
         nAddr := hb_bitAnd( nAddr, nPrefix )
         nAddr2 := hb_bitOr( nAddr, hb_bitXor( nPrefix, 0xFFFFFFFF ) )

         IF lDeny
            AAdd( aDeny, { nAddr, nAddr2 } )
         ELSE
            // Add to filter
            hb_HHasKey( aFilter, nAddr, @nPos )
            IF nPos == 0 .OR. hb_HValueAt( aFilter, nPos ) + 1 < nAddr
               // Does not overlap/glue with nPos
               // So, add new interval
               aFilter[ nAddr ] := nAddr2
               nPos++
            ENDIF
            hb_HHasKey( aFilter, nAddr2 + 1, @nPos2 )
            // Merge and delete inner subintervals
            aFilter[ hb_HKeyAt( aFilter, nPos ) ] := Max( hb_HValueAt( aFilter, nPos2 ), nAddr2 )
            DO WHILE nPos2-- > nPos
               hb_HDelAt( aFilter, nPos + 1 )
            ENDDO
         ENDIF
      ENDIF
   NEXT

   FOR EACH aI IN aDeny
      nAddr := aI[ 1 ]
      nAddr2 := aI[ 2 ]

      // Delete from filter
      hb_HHasKey( aFilter, nAddr, @nPos )
      IF nPos == 0 .OR. hb_HValueAt( aFilter, nPos ) < nAddr
         nPos++
      ENDIF
      IF nPos > Len( aFilter )
         LOOP
      ENDIF

      hb_HHasKey( aFilter, nAddr2, @nPos2 )
      IF nPos2 > 0 .AND. hb_HValueAt( aFilter, nPos2 ) > nAddr2
         aFilter[ nAddr2 + 1 ] := hb_HValueAt( aFilter, nPos2 )
      ENDIF
      IF nAddr > hb_HKeyAt( aFilter, nPos )
         aFilter[ hb_HKeyAt( aFilter, nPos ) ] := nAddr - 1
         nPos++
      ENDIF
      DO WHILE nPos2-- >= nPos
         hb_HDelAt( aFilter, nPos )
      ENDDO
   NEXT

   RETURN .T.

STATIC FUNCTION MY_SSL_READ( hConfig, hSSL, hSocket, cBuf, nTimeout, nError )

   LOCAL nErr, nLen

   nLen := SSL_read( hSSL, @cBuf )
   IF nLen < 0
      SWITCH nErr := SSL_get_error( hSSL, nLen )
      CASE HB_SSL_ERROR_WANT_READ
         nErr := hb_socketSelectRead( hSocket, nTimeout )
         IF nErr < 0
            nError := hb_socketGetError()
         ELSE // Both cases: data received and timeout
            nError := HB_SOCKET_ERR_TIMEOUT
         ENDIF
         RETURN -1
      CASE HB_SSL_ERROR_WANT_WRITE
         nErr := hb_socketSelectWrite( hSocket, nTimeout )
         IF nErr < 0
            nError := hb_socketGetError()
         ELSE // Both cases: data sent and timeout
            nError := HB_SOCKET_ERR_TIMEOUT
         ENDIF
         RETURN -1
      OTHERWISE
         Eval( hConfig[ "Trace" ], "SSL_read() error", nErr )
         nError := 1000 + nErr
         RETURN -1
      ENDSWITCH
   ENDIF

   RETURN nLen

STATIC FUNCTION MY_SSL_WRITE( hConfig, hSSL, hSocket, cBuf, nTimeout, nError )

   LOCAL nErr, nLen

   nLen := SSL_write( hSSL, cBuf )
   IF nLen <= 0
      SWITCH nErr := SSL_get_error( hSSL, nLen )
      CASE HB_SSL_ERROR_WANT_READ
         nErr := hb_socketSelectRead( hSocket, nTimeout )
         IF nErr < 0
            nError := hb_socketGetError()
            RETURN -1
         ELSE  // Both cases: data received and timeout
            RETURN 0
         ENDIF
      CASE HB_SSL_ERROR_WANT_WRITE
         nErr := hb_socketSelectWrite( hSocket, nTimeout )
         IF nErr < 0
            nError := hb_socketGetError()
            RETURN -1
         ELSE  // Both cases: data sent and timeout
            RETURN 0
         ENDIF
      OTHERWISE
         Eval( hConfig[ "Trace" ], "SSL_write() error", nErr )
         nError := 1000 + nErr
         RETURN -1
      ENDSWITCH
   ENDIF

   RETURN nLen

STATIC FUNCTION MY_SSL_ACCEPT( hConfig, hSSL, hSocket, nTimeout )

   LOCAL nErr

   nErr := SSL_accept( hSSL )
   IF nErr > 0
      RETURN 0
   ELSEIF nErr < 0
      SWITCH nErr := SSL_get_error( hSSL, nErr )
      CASE HB_SSL_ERROR_WANT_READ
         nErr := hb_socketSelectRead( hSocket, nTimeout )
         IF nErr < 0
            nErr := hb_socketGetError()
         ELSE
            nErr := HB_SOCKET_ERR_TIMEOUT
         ENDIF
         EXIT
      CASE HB_SSL_ERROR_WANT_WRITE
         nErr := hb_socketSelectWrite( hSocket, nTimeout )
         IF nErr < 0
            nErr := hb_socketGetError()
         ELSE
            nErr := HB_SOCKET_ERR_TIMEOUT
         ENDIF
         EXIT
      OTHERWISE
         Eval( hConfig[ "Trace" ], "SSL_accept() error", nErr )
         nErr := 1000 + nErr
      ENDSWITCH
   ELSE /* nErr == 0 */
      nErr := SSL_get_error( hSSL, nErr )
      Eval( hConfig[ "Trace" ], "SSL_accept() shutdown error", nErr )
      nErr := 1000 + nErr
   ENDIF

   RETURN nErr

STATIC FUNCTION ProcessConnection( oServer )

   LOCAL hSocket, cRequest, aI, nLen, nErr, nTime, nReqLen, cBuf, aServer
   LOCAL hSSL

   ErrorBlock( {| o | UErrorHandler( o, oServer ) } )

   PRIVATE server, get, post, cookie, session, httpd

   httpd := oServer

   /* main worker thread loop */
   DO WHILE .T.
      hb_mutexSubscribe( oServer:hmtxQueue,, @hSocket )
      IF hSocket == NIL
         EXIT
      ENDIF

      /* Prepare server variable and clone it for every query,
         because request handler script can ruin variable value */
      aServer := { => }
      aServer[ "HTTPS" ] := oServer:hConfig[ "SSL" ]
      IF ! Empty( aI := hb_socketGetPeerName( hSocket ) )
         aServer[ "REMOTE_ADDR" ] := aI[ 2 ]
         aServer[ "REMOTE_HOST" ] := aServer[ "REMOTE_ADDR" ] // no reverse DNS
         aServer[ "REMOTE_PORT" ] := aI[ 3 ]
      ENDIF
      IF ! Empty( aI := hb_socketGetSockName( hSocket ) )
         aServer[ "SERVER_ADDR" ] := aI[ 2 ]
         aServer[ "SERVER_PORT" ] := aI[ 3 ]
      ENDIF

      /* Firewall */
      nLen := IPAddr2Num( aServer[ "REMOTE_ADDR" ] )
      hb_HHasKey( oServer:aFirewallFilter, nLen, @nErr )
      IF nErr > 0 .AND. nLen <= hb_HValueAt( oServer:aFirewallFilter, nErr )
         Eval( oServer:hConfig[ "Trace" ], "Firewall denied", aServer[ "REMOTE_ADDR" ] )
         hb_socketShutdown( hSocket )
         hb_socketClose( hSocket )
         LOOP
      ENDIF

      IF oServer:lHasSSL .AND. oServer:hConfig[ "SSL" ]
         hSSL := SSL_new( oServer:hSSLCtx )
         SSL_set_mode( hSSL, hb_bitOr( SSL_get_mode( hSSL ), HB_SSL_MODE_ENABLE_PARTIAL_WRITE ) )
         hb_socketSetBlockingIO( hSocket, .F. )
         SSL_set_fd( hSSL, hb_socketGetFD( hSocket ) )

         nTime := hb_MilliSeconds()
         DO WHILE .T.
            IF ( nErr := MY_SSL_ACCEPT( oServer:hConfig, hSSL, hSocket, 1000 ) ) == 0
               EXIT
            ELSE
               IF nErr == HB_SOCKET_ERR_TIMEOUT
                  IF ( hb_MilliSeconds() - nTime ) > 1000 * 30 .OR. oServer:lStop
                     Eval( oServer:hConfig[ "Trace" ], "SSL accept timeout", hSocket )
                     EXIT
                  ENDIF
               ELSE
                  Eval( oServer:hConfig[ "Trace" ], "SSL accept error:", nErr, hb_socketErrorString( nErr ) )
                  EXIT
               ENDIF
            ENDIF
         ENDDO

         IF nErr != 0
            Eval( oServer:hConfig[ "Trace" ], "Close connection1", hSocket )
            hb_socketShutdown( hSocket )
            hb_socketClose( hSocket )
            LOOP
         ENDIF

         aServer[ "SSL_CIPHER" ] := SSL_get_cipher( hSSL )
         aServer[ "SSL_PROTOCOL" ] := SSL_get_version( hSSL )
         aServer[ "SSL_CIPHER_USEKEYSIZE" ] := SSL_get_cipher_bits( hSSL, @nErr )
         aServer[ "SSL_CIPHER_ALGKEYSIZE" ] := nErr
         aServer[ "SSL_VERSION_LIBRARY" ] := SSLeay_version( HB_SSLEAY_VERSION )
         aServer[ "SSL_SERVER_I_DN" ] := X509_name_oneline( X509_get_issuer_name( SSL_get_certificate( hSSL ) ) )
         aServer[ "SSL_SERVER_S_DN" ] := X509_name_oneline( X509_get_subject_name( SSL_get_certificate( hSSL ) ) )
      ENDIF

      /* loop for processing connection */

      /* Set cRequest to empty string here. This enables request pipelining */
      cRequest := ""
      DO WHILE ! oServer:lStop

         /* receive query header */
         nLen := 1
         nTime := hb_MilliSeconds()
         cBuf := Space( 4096 )
         DO WHILE !( CR_LF + CR_LF $ cRequest )
            IF oServer:lHasSSL .AND. oServer:hConfig[ "SSL" ]
               nLen := MY_SSL_READ( oServer:hConfig, hSSL, hSocket, @cBuf, 1000, @nErr )
            ELSE
               IF ( nLen := hb_socketRecv( hSocket, @cBuf,,, 1000 ) ) < 0
                  nErr := hb_socketGetError()
               ENDIF
            ENDIF

            IF nLen > 0
               cRequest += hb_BLeft( cBuf, nLen )
            ELSEIF nLen == 0
               /* connection closed */
               EXIT
            ELSE
               /* nLen == -1  socket error */
               IF nErr == HB_SOCKET_ERR_TIMEOUT
                  IF ( hb_MilliSeconds() - nTime ) > 1000 * 30 .OR. oServer:lStop
                     Eval( oServer:hConfig[ "Trace" ], "receive timeout", hSocket )
                     EXIT
                  ENDIF
               ELSE
                  Eval( oServer:hConfig[ "Trace" ], "receive error:", nErr, hb_socketErrorString( nErr ) )
                  EXIT
               ENDIF
            ENDIF
         ENDDO

         IF nLen <= 0 .OR. oServer:lStop
            EXIT
         ENDIF

         // PRIVATE
         server := hb_HClone( aServer )
         get := { => }
         post := { => }
         cookie := { => }
         session := NIL

         t_cResult := ""
         t_aHeader := {}
         t_nStatusCode := 200
         t_aSessionData := NIL

         Eval( oServer:hConfig[ "Trace" ], Left( cRequest, At( CR_LF + CR_LF, cRequest ) + 1 ) )

         nReqLen := ParseRequestHeader( @cRequest )
         IF nReqLen == NIL
            USetStatusCode( 400 )
            UAddHeader( "Connection", "close" )
         ELSE

            /* receive query body */
            nLen := 1
            nTime := hb_MilliSeconds()
            cBuf := Space( 4096 )
            DO WHILE Len( cRequest ) < nReqLen
               IF oServer:lHasSSL .AND. oServer:hConfig[ "SSL" ]
                  nLen := MY_SSL_READ( oServer:hConfig, hSSL, hSocket, @cBuf, 1000, @nErr )
               ELSE
                  IF ( nLen := hb_socketRecv( hSocket, @cBuf,,, 1000 ) ) < 0
                     nErr := hb_socketGetError()
                  ENDIF
               ENDIF

               IF nLen > 0
                  cRequest += hb_BLeft( cBuf, nLen )
               ELSEIF nLen == 0
                  /* connection closed */
                  EXIT
               ELSE
                  /* nLen == -1  socket error */
                  IF nErr == HB_SOCKET_ERR_TIMEOUT
                     IF ( hb_MilliSeconds() - nTime ) > 1000 * 120 .OR. oServer:lStop
                        Eval( oServer:hConfig[ "Trace" ], "receive timeout", hSocket )
                        EXIT
                     ENDIF
                  ELSE
                     Eval( oServer:hConfig[ "Trace" ], "receive error:", nErr, hb_socketErrorString( nErr ) )
                     EXIT
                  ENDIF
               ENDIF
            ENDDO

            IF nLen <= 0 .OR. oServer:lStop
               EXIT
            ENDIF

            Eval( oServer:hConfig[ "Trace" ], cRequest )
            ParseRequestBody( Left( cRequest, nReqLen ) )
            cRequest := SubStr( cRequest, nReqLen + 1 )

            /* Deal with supported protocols and methods */
            IF ! hb_LeftEq( server[ "SERVER_PROTOCOL" ], "HTTP/" )
               USetStatusCode( 400 ) /* Bad request */
               UAddHeader( "Connection", "close" )
            ELSEIF !( SubStr( server[ "SERVER_PROTOCOL" ], 6 ) $ "1.0 1.1" )
               USetStatusCode( 505 ) /* HTTP version not supported */
            ELSEIF !( server[ "REQUEST_METHOD" ] $ "GET POST" )
               USetStatusCode( 501 ) /* Not implemented */
            ELSE
               IF server[ "SERVER_PROTOCOL" ] == "HTTP/1.1"
                  IF Lower( server[ "HTTP_CONNECTION" ] ) == "close"
                     UAddHeader( "Connection", "close" )
                  ELSE
                     UAddHeader( "Connection", "keep-alive" )
                  ENDIF
               ENDIF

               /* Do the job */
               ProcessRequest( oServer )
               dbCloseAll()
            ENDIF
         ENDIF /* request header ok */

         // Send response
         cBuf := MakeResponse( oServer:hConfig )

         DO WHILE hb_BLen( cBuf ) > 0 .AND. ! oServer:lStop
            IF oServer:lHasSSL .AND. oServer:hConfig[ "SSL" ]
               nLen := MY_SSL_WRITE( oServer:hConfig, hSSL, hSocket, cBuf, 1000, @nErr )
            ELSE
               nLen := hb_socketSend( hSocket, cBuf,,, 1000 )
               IF nLen < 0
                  nErr := hb_socketGetError()
               ENDIF
            ENDIF

            IF nLen < 0
               Eval( oServer:hConfig[ "Trace" ], "send error:", nErr, hb_socketErrorString( nErr ) )
               EXIT
            ELSEIF nLen > 0
               cBuf := hb_BSubStr( cBuf, nLen + 1 )
            ENDIF
         ENDDO

         IF oServer:lStop
            EXIT
         ENDIF

         oServer:LogAccess()

         IF Lower( UGetHeader( "Connection" ) ) == "close" .OR. server[ "SERVER_PROTOCOL" ] == "HTTP/1.0"
            EXIT
         ENDIF
      ENDDO

      IF oServer:lHasSSL
         hSSL := NIL
      ENDIF

      Eval( oServer:hConfig[ "Trace" ], "Close connection1", hSocket )
      hb_socketShutdown( hSocket )
      hb_socketClose( hSocket )
   ENDDO

   RETURN 0

STATIC PROCEDURE ProcessRequest( oServer )

   LOCAL nI, hMount, cMount, cPath, bEval, xRet, nT := hb_MilliSeconds()

   // Search mounting table
   hMount := oServer:hConfig[ "Mount" ]
   cMount := server[ "SCRIPT_NAME" ]
   IF cMount $ hMount
      cPath := ""
   ELSE
      nI := Len( cMount )
      DO WHILE ( nI := hb_RAt( "/", cMount,, nI ) ) > 0
         IF ( Left( cMount, nI ) + "*" ) $ hMount
            Eval( oServer:hConfig[ "Trace" ], "HAS", Left( cMount, nI ) + "*" )
            cMount := Left( cMount, nI ) + "*"
            cPath := SubStr( server[ "SCRIPT_NAME" ], nI + 1 )
            EXIT
         ENDIF
         IF --nI == 0
            EXIT
         ENDIF
      ENDDO
   ENDIF

   IF cPath != NIL
      bEval := hMount[ cMount ]
      BEGIN SEQUENCE WITH {| oErr | UErrorHandler( oErr, oServer ) }
         xRet := Eval( bEval, cPath )
         DO CASE
         CASE HB_ISSTRING( xRet )
            UWrite( xRet )
         CASE HB_ISHASH( xRet )
            UWrite( UParse( xRet ) )
         ENDCASE
      RECOVER
         USetStatusCode( 500 )
         UAddHeader( "Connection", "close" )
      END SEQUENCE
      dbCloseAll()
      // Unlock session
      IF t_aSessionData != NIL
         session := NIL
         hb_mutexUnlock( t_aSessionData[ 1 ] )
         t_aSessionData := NIL
      ENDIF
   ELSE
      USetStatusCode( 404 )
   ENDIF
   Eval( oServer:hConfig[ "Trace" ], "ProcessRequest time:", hb_ntos( hb_MilliSeconds() - nT ), "ms" )

   RETURN

STATIC FUNCTION ParseRequestHeader( cRequest )

   LOCAL aRequest, aLine, nI, nJ, cI, nK, nContentLength := 0

   nI := At( CR_LF + CR_LF, cRequest )
   aRequest := hb_ATokens( Left( cRequest, nI - 1 ), CR_LF )
   cRequest := SubStr( cRequest, nI + 4 )

   aLine := hb_ATokens( aRequest[ 1 ], " " )

   server[ "REQUEST_ALL" ] := aRequest[ 1 ]
   IF Len( aLine ) == 3 .AND. hb_LeftEq( aLine[ 3 ], "HTTP/" )
      server[ "REQUEST_METHOD" ] := aLine[ 1 ]
      server[ "REQUEST_URI" ] := aLine[ 2 ]
      server[ "SERVER_PROTOCOL" ] := aLine[ 3 ]
   ELSE
      server[ "REQUEST_METHOD" ] := aLine[ 1 ]
      server[ "REQUEST_URI" ] := iif( Len( aLine ) >= 2, aLine[ 2 ], "" )
      server[ "SERVER_PROTOCOL" ] := iif( Len( aLine ) >= 3, aLine[ 3 ], "" )
      RETURN NIL
   ENDIF

   // Fix invalid queries: bind to root
   IF ! hb_LeftEq( server[ "REQUEST_URI" ], "/" )
      server[ "REQUEST_URI" ] := "/" + server[ "REQUEST_URI" ]
   ENDIF

   IF ( nI := At( "?", server[ "REQUEST_URI" ] ) ) > 0
      server[ "SCRIPT_NAME" ] := Left( server[ "REQUEST_URI" ], nI - 1 )
      server[ "QUERY_STRING" ] := SubStr( server[ "REQUEST_URI" ], nI + 1 )
   ELSE
      server[ "SCRIPT_NAME" ] := server[ "REQUEST_URI" ]
      server[ "QUERY_STRING" ] := ""
   ENDIF

   server[ "HTTP_ACCEPT" ] := ""
   server[ "HTTP_ACCEPT_CHARSET" ] := ""
   server[ "HTTP_ACCEPT_ENCODING" ] := ""
   server[ "HTTP_ACCEPT_LANGUAGE" ] := ""
   server[ "HTTP_CONNECTION" ] := ""
   server[ "HTTP_HOST" ] := ""
   server[ "HTTP_KEEP_ALIVE" ] := ""
   server[ "HTTP_REFERER" ] := ""
   server[ "HTTP_USER_AGENT" ] := ""

   FOR nI := 2 TO Len( aRequest )
      IF aRequest[ nI ] == ""
         EXIT
      ELSEIF ( nJ := At( ":", aRequest[ nI ] ) ) > 0
         cI := AllTrim( SubStr( aRequest[ nI ], nJ + 1 ) )
         SWITCH Upper( Left( aRequest[ nI ], nJ - 1 ) )
         CASE "COOKIE"
            server[ "HTTP_COOKIE" ] := cI
            IF ( nK := At( ";", cI ) ) == 0
               nK := Len( RTrim( cI ) )
            ENDIF
            cI := Left( cI, nK )
            IF ( nK := At( "=", cI ) ) > 0
               /* cookie names are case insensitive, uppercase it */
               cookie[ Upper( Left( cI, nK - 1 ) ) ] := SubStr( cI, nK + 1 )
            ENDIF
            EXIT
         CASE "CONTENT-LENGTH"
            nContentLength := Val( cI )
            EXIT
         CASE "CONTENT-TYPE"
            server[ "CONTENT_TYPE" ] := cI
            EXIT
         OTHERWISE
            server[ "HTTP_" + StrTran( Upper( Left( aRequest[ nI ], nJ - 1 ) ), "-", "_" ) ] := cI
            EXIT
         ENDSWITCH
      ENDIF
   NEXT
   IF !( server[ "QUERY_STRING" ] == "" )
      FOR EACH cI IN hb_ATokens( server[ "QUERY_STRING" ], "&" )
         IF ( nI := At( "=", cI ) ) > 0
            get[ UUrlDecode( Left( cI, nI - 1 ) ) ] := UUrlDecode( SubStr( cI, nI + 1 ) )
         ELSE
            get[ UUrlDecode( cI ) ] := NIL
         ENDIF
      NEXT
   ENDIF

   RETURN nContentLength

STATIC FUNCTION ParseRequestBody( cRequest )

   LOCAL nI, cPart, cEncoding

   IF "CONTENT_TYPE" $ server .AND. ;
      hb_LeftEq( server[ "CONTENT_TYPE" ], "application/x-www-form-urlencoded" )

      IF ( nI := At( "CHARSET=", Upper( server[ "CONTENT_TYPE" ] ) ) ) > 0
         cEncoding := Upper( SubStr( server[ "CONTENT_TYPE" ], nI + 8 ) )
      ENDIF
      IF !( cRequest == "" )
         IF cEncoding == "UTF-8"
            FOR EACH cPart IN hb_ATokens( cRequest, "&" )
               IF ( nI := At( "=", cPart ) ) > 0
                  post[ hb_UTF8ToStr( UUrlDecode( Left( cPart, nI - 1 ) ) ) ] := hb_UTF8ToStr( UUrlDecode( SubStr( cPart, nI + 1 ) ) )
               ELSE
                  post[ hb_UTF8ToStr( UUrlDecode( cPart ) ) ] := NIL
               ENDIF
            NEXT
         ELSE
            FOR EACH cPart IN hb_ATokens( cRequest, "&" )
               IF ( nI := At( "=", cPart ) ) > 0
                  post[ UUrlDecode( Left( cPart, nI - 1 ) ) ] := UUrlDecode( SubStr( cPart, nI + 1 ) )
               ELSE
                  post[ UUrlDecode( cPart ) ] := NIL
               ENDIF
            NEXT
         ENDIF
      ENDIF
   ENDIF

   RETURN NIL

STATIC FUNCTION MakeResponse( hConfig )

   LOCAL cRet, cStatus

   IF UGetHeader( "Content-Type" ) == NIL
      UAddHeader( "Content-Type", "text/html" )
   ENDIF
   UAddHeader( "Date", HttpDateFormat( hb_DateTime() ) )

   cRet := iif( server[ "SERVER_PROTOCOL" ] == "HTTP/1.0", "HTTP/1.0 ", "HTTP/1.1 " )
   SWITCH t_nStatusCode
   CASE 100 ; cStatus := "100 Continue"                        ;  EXIT
   CASE 101 ; cStatus := "101 Switching Protocols"             ;  EXIT
   CASE 200 ; cStatus := "200 OK"                              ;  EXIT
   CASE 201 ; cStatus := "201 Created"                         ;  EXIT
   CASE 202 ; cStatus := "202 Accepted"                        ;  EXIT
   CASE 203 ; cStatus := "203 Non-Authoritative Information"   ;  EXIT
   CASE 204 ; cStatus := "204 No Content"                      ;  EXIT
   CASE 205 ; cStatus := "205 Reset Content"                   ;  EXIT
   CASE 206 ; cStatus := "206 Partial Content"                 ;  EXIT
   CASE 300 ; cStatus := "300 Multiple Choices"                ;  EXIT
   CASE 301 ; cStatus := "301 Moved Permanently"               ;  EXIT
   CASE 302 ; cStatus := "302 Found"                           ;  EXIT
   CASE 303 ; cStatus := "303 See Other"                       ;  EXIT
   CASE 304 ; cStatus := "304 Not Modified"                    ;  EXIT
   CASE 305 ; cStatus := "305 Use Proxy"                       ;  EXIT
   CASE 307 ; cStatus := "307 Temporary Redirect"              ;  EXIT
   CASE 400 ; cStatus := "400 Bad Request"                     ;  EXIT
   CASE 401 ; cStatus := "401 Unauthorized"                    ;  EXIT
   CASE 402 ; cStatus := "402 Payment Required"                ;  EXIT
   CASE 403 ; cStatus := "403 Forbidden"                       ;  EXIT
   CASE 404 ; cStatus := "404 Not Found"                       ;  EXIT
   CASE 405 ; cStatus := "405 Method Not Allowed"              ;  EXIT
   CASE 406 ; cStatus := "406 Not Acceptable"                  ;  EXIT
   CASE 407 ; cStatus := "407 Proxy Authentication Required"   ;  EXIT
   CASE 408 ; cStatus := "408 Request Timeout"                 ;  EXIT
   CASE 409 ; cStatus := "409 Conflict"                        ;  EXIT
   CASE 410 ; cStatus := "410 Gone"                            ;  EXIT
   CASE 411 ; cStatus := "411 Length Required"                 ;  EXIT
   CASE 412 ; cStatus := "412 Precondition Failed"             ;  EXIT
   CASE 413 ; cStatus := "413 Request Entity Too Large"        ;  EXIT
   CASE 414 ; cStatus := "414 Request-URI Too Long"            ;  EXIT
   CASE 415 ; cStatus := "415 Unsupprted Media Type"           ;  EXIT
   CASE 416 ; cStatus := "416 Requested Range Not Satisfiable" ;  EXIT
   CASE 417 ; cStatus := "417 Expectation Failed"              ;  EXIT
   CASE 500 ; cStatus := "500 Internal Server Error"           ;  EXIT
   CASE 501 ; cStatus := "501 Not Implemented"                 ;  EXIT
   CASE 502 ; cStatus := "502 Bad Gateway"                     ;  EXIT
   CASE 503 ; cStatus := "503 Service Unavailable"             ;  EXIT
   CASE 504 ; cStatus := "504 Gateway Timeout"                 ;  EXIT
   CASE 505 ; cStatus := "505 HTTP Version Not Supported"      ;  EXIT
   OTHERWISE; cStatus := "500 Internal Server Error"
   ENDSWITCH

   cRet += cStatus + CR_LF
   IF t_nStatusCode != 200
      t_cResult := "<html><body><h1>" + cStatus + "</h1></body></html>"
   ENDIF
   UAddHeader( "Content-Length", hb_ntos( Len( t_cResult ) ) )
   AEval( t_aHeader, {| x | cRet += x[ 1 ] + ": " + x[ 2 ] + CR_LF } )
   cRet += CR_LF
   Eval( hConfig[ "Trace" ], cRet )
   cRet += t_cResult

   RETURN cRet

STATIC FUNCTION HttpDateFormat( tDate )

   LOCAL nOffset := hb_UTCOffset()

   RETURN ;
      { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" }[ DoW( tDate ) ] + ", " + ;
      StrZero( Day( tDate ), 2 ) + " " + ;
      { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" }[ Month( tDate ) ] + " " + ;
      StrZero( Year( tDate ), 4 ) + " " + ;
      hb_TToC( tDate, "", "HH:MM:SS" ) + " " + ;
      hb_StrFormat( "UTC%1$s%2$02d%3$02d", ;
         iif( nOffset < 0, "-", "+" ), ;
         Int( Abs( nOffset ) / 3600 ), ;
         Int( ( Abs( nOffset ) / 3600 ) - Int( Abs( nOffset ) / 3600 ) ) * 60 )

STATIC FUNCTION HttpDateUnformat( cDate, tDate )

   LOCAL nMonth, tI

   // TODO: support outdated compatibility format RFC2616
   IF Len( cDate ) == 29 .AND. Right( cDate, 4 ) == " GMT" .AND. SubStr( cDate, 4, 2 ) == ", "
      IF ( nMonth := AScan( { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" }, SubStr( cDate, 9, 3 ) ) ) > 0
         tI := hb_SToT( SubStr( cDate, 13, 4 ) + PadL( nMonth, 2, "0" ) + SubStr( cDate, 6, 2 ) + StrTran( SubStr( cDate, 18, 8 ), ":" ) )
         IF ! Empty( tI )
            tDate := tI + hb_UTCOffset() / ( 3600 * 24 )
            RETURN .T.
         ENDIF
      ENDIF
   ENDIF

   RETURN .F.

STATIC FUNCTION UErrorHandler( oErr, oServer )

   Eval( oServer:hConfig[ "Trace" ], "UErrorHandler" )
   IF oErr:genCode == EG_ZERODIV
      RETURN 0
   ELSEIF oErr:genCode == EG_LOCK
      RETURN .T.
   ELSEIF ( oErr:genCode == EG_OPEN .AND. oErr:osCode == 32 .OR. ;
         oErr:genCode == EG_APPENDLOCK ) .AND. oErr:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF
   oServer:LogError( GetErrorDesc( oErr ) )
   IF oErr != NIL // Dummy check to avoid unreachable code warning for RETURN NIL
      Break( oErr )
   ENDIF

   RETURN NIL

STATIC FUNCTION GetErrorDesc( oErr )

   LOCAL cRet, nI, cI, aPar, nJ, xI

   cRet := "ERRORLOG ============================================================" + hb_eol() + ;
      "Error: " + oErr:subsystem + "/" + ErrDescCode( oErr:genCode ) + "(" + hb_ntos( oErr:genCode ) + ") " + ;
      hb_ntos( oErr:subcode ) + hb_eol()
   IF ! Empty( oErr:filename );      cRet += "File: " + oErr:filename + hb_eol()
   ENDIF
   IF ! Empty( oErr:description );   cRet += "Description: " + oErr:description + hb_eol()
   ENDIF
   IF ! Empty( oErr:operation );     cRet += "Operation: " + oErr:operation + hb_eol()
   ENDIF
   IF ! Empty( oErr:osCode );        cRet += "OS error: " + hb_ntos( oErr:osCode ) + hb_eol()
   ENDIF
   IF HB_ISARRAY( oErr:args )
      cRet += "Arguments:" + hb_eol()
      AEval( oErr:args, {| X, Y | cRet += Str( Y, 5 ) + ": " + hb_CStr( X ) + hb_eol() } )
   ENDIF
   cRet += hb_eol()

   cRet += "Stack:" + hb_eol()
   nI := 2
#if 0
   DO WHILE ! Empty( ProcName( ++nI ) )
      cRet += "    " + ProcName( nI ) + "(" + hb_ntos( ProcLine( nI ) ) + ")" + hb_eol()
   ENDDO
#else
   DO WHILE ! Empty( ProcName( ++nI ) )
      cI := "    " + ProcName( nI ) + "(" + hb_ntos( ProcLine( nI ) ) + ")"
      cI := PadR( cI, Max( 32, Len( cI ) + 1 ) )
      cI += "("
      aPar := __dbgVMParLList( nI )
      FOR EACH nJ IN aPar
         cI += cvt2str( nJ )
         IF ! nJ:__enumIsLast()
            cI += ", "
         ENDIF
      NEXT
      cI += ")"
      nJ := Len( aPar )
      DO WHILE ! HB_ISSYMBOL( xI := __dbgVMVarLGet( nI, ++nJ ) )
         cI += ", " + cvt2str( xI )
      ENDDO
      xI := NIL
      cRet += cI + hb_eol()
   ENDDO
#endif
   cRet += hb_eol()

   cRet += "Executable:  " + hb_ProgName() + hb_eol()
   cRet += "Versions:" + hb_eol()
   cRet += "  OS: " + OS() + hb_eol()
   cRet += "  Harbour: " + Version() + ", " + hb_Version( HB_VERSION_BUILD_DATE_STR ) + hb_eol()
   cRet += hb_eol()

   IF oErr:genCode != EG_MEM
      cRet += "Database areas:" + hb_eol()
      cRet += "    Current: " + hb_ntos( Select() ) + "  " + Alias() + hb_eol()

      BEGIN SEQUENCE WITH {| o | Break( o ) }
         IF Used()
            cRet += ;
               "    Filter: " + dbFilter() + hb_eol() + ;
               "    Relation: " + dbRelation() + hb_eol() + ;
               "    Index expression: " + ordKey( ordSetFocus() ) + hb_eol() + ;
               hb_eol()
            BEGIN SEQUENCE WITH {| o | Break( o ) }
               FOR nI := 1 TO FCount()
                  cRet += Str( nI, 6 ) + " " + PadR( FieldName( nI ), 14 ) + ": " + hb_ValToExp( FieldGet( nI ) ) + hb_eol()
               NEXT
            RECOVER
               cRet += "!!! Error reading database fields !!!" + hb_eol()
            END SEQUENCE
            cRet += hb_eol()
         ENDIF
      RECOVER
         cRet += "!!! Error accessing current workarea !!!" + hb_eol()
      END SEQUENCE

      FOR nI := 1 TO 250
         BEGIN SEQUENCE WITH {| o | Break( o ) }
            IF Used()
               dbSelectArea( nI )
               cRet += Str( nI, 6 ) + " " + rddName() + " " + PadR( Alias(), 15 ) + " " + ;
                  Str( RecNo() ) + "/" + Str( LastRec() ) + ;
                  iif( Empty( ordSetFocus() ), "", " Index " + ordSetFocus() + "(" + hb_ntos( ordNumber() ) + ")" ) + hb_eol()
               dbCloseArea()
            ENDIF
         RECOVER
            cRet += "!!! Error accessing workarea number: " + Str( nI, 4 ) + "!!!" + hb_eol()
         END SEQUENCE
      NEXT
      cRet += hb_eol()
   ENDIF

   RETURN cRet

STATIC FUNCTION ErrDescCode( nCode )

   LOCAL cI := NIL

   IF nCode > 0 .AND. nCode <= 41
      cI := { ;
         "ARG"     , "BOUND"    , "STROVERFLOW", "NUMOVERFLOW", "ZERODIV" , "NUMERR"     , "SYNTAX"  , "COMPLEXITY" , ; //  1,  2,  3,  4,  5,  6,  7,  8
         NIL       , NIL        , "MEM"        , "NOFUNC"     , "NOMETHOD", "NOVAR"      , "NOALIAS" , "NOVARMETHOD", ; //  9, 10, 11, 12, 13, 14, 15, 16
         "BADALIAS", "DUPALIAS" , NIL          , "CREATE"     , "OPEN"    , "CLOSE"      , "READ"    , "WRITE"      , ; // 17, 18, 19, 20, 21, 22, 23, 24
         "PRINT"   , NIL        , NIL          , NIL          , NIL       , "UNSUPPORTED", "LIMIT"   , "CORRUPTION" , ; // 25, 26 - 29, 30, 31, 32
         "DATATYPE", "DATAWIDTH", "NOTABLE"    , "NOORDER"    , "SHARED"  , "UNLOCKED"   , "READONLY", "APPENDLOCK" , ; // 33, 34, 35, 36, 37, 38, 39, 40
         "LOCK"    }[ nCode ]                                                                                           // 41
   ENDIF

   RETURN iif( cI == NIL, "", "EG_" + cI )

STATIC FUNCTION cvt2str( xI, lLong )

   LOCAL cValtype, cI, xJ

   lLong := ! Empty( lLong )

   SWITCH cValtype := ValType( xI )
   CASE "U"
      RETURN iif( lLong, "[U]:NIL", "NIL" )
   CASE "N"
      RETURN iif( lLong, "[N]:" + hb_ntos( xI ), hb_ntos( xI ) )
   CASE "M"
   CASE "C"
      IF Len( xI ) <= 260
         RETURN iif( lLong, "[" + cValtype + hb_ntos( Len( xI ) ) + "]:", "" ) + '"' + xI + '"'
      ELSE
         RETURN iif( lLong, "[" + cValtype + hb_ntos( Len( xI ) ) + "]:", "" ) + '"' + Left( xI, 100 ) + '"...'
      ENDIF
   CASE "A"
      RETURN "[A" + hb_ntos( Len( xI ) ) + "]"
   CASE "H"
      RETURN "[H" + hb_ntos( Len( xI ) ) + "]"
   CASE "O"
      cI := ""
      IF __objHasMsg( xI, "ID" )
         xJ := xI:ID
         IF ! HB_ISOBJECT( xJ )
            cI += ",ID=" + cvt2str( xJ )
         ENDIF
      ENDIF
      IF __objHasMsg( xI, "nID" )
         xJ := xI:nID
         IF ! HB_ISOBJECT( xJ )
            cI += ",NID=" + cvt2str( xJ )
         ENDIF
      ENDIF
      IF __objHasMsg( xI, "xValue" )
         xJ := xI:xValue
         IF ! HB_ISOBJECT( xJ )
            cI += ",XVALUE=" + cvt2str( xJ )
         ENDIF
      ENDIF
      RETURN "[O:" + xI:ClassName() + cI + "]"
   CASE "D"
      RETURN iif( lLong, "[D]:", "" ) + hb_DToC( xI, "yyyy-mm-dd" )
   CASE "L"
      RETURN iif( lLong, "[L]:", "" ) + iif( xI, ".T.", ".F." )
   CASE "P"
      RETURN iif( lLong, "[P]:", "" ) + "0p" + hb_NumToHex( xI )
   ENDSWITCH

   RETURN "[" + cValtype + "]" // BS,etc


/********************************************************************
  Public functions
********************************************************************/

PROCEDURE USetStatusCode( nStatusCode )

   t_nStatusCode := nStatusCode

   RETURN

FUNCTION UGetHeader( cType )

   LOCAL nI

   IF ( nI := AScan( t_aHeader, {| x | Upper( x[ 1 ] ) == Upper( cType ) } ) ) > 0
      RETURN t_aHeader[ nI, 2 ]
   ENDIF

   RETURN NIL

PROCEDURE UAddHeader( cType, cValue )

   LOCAL nI

   IF ( nI := AScan( t_aHeader, {| x | Upper( x[ 1 ] ) == Upper( cType ) } ) ) > 0
      t_aHeader[ nI, 2 ] := cValue
   ELSE
      AAdd( t_aHeader, { cType, cValue } )
   ENDIF

   RETURN

PROCEDURE URedirect( cURL, nCode )

   USetStatusCode( hb_defaultValue( nCode, 303 ) )
   UAddHeader( "Location", cURL )

   RETURN

PROCEDURE UWrite( cString )

   t_cResult += cString

   RETURN

STATIC PROCEDURE USessionCreateInternal()

   LOCAL cSID, hMtx

   cSID := hb_MD5( DToS( Date() ) + Time() + Str( hb_Random(), 15, 12 ) )
   hMtx := hb_mutexCreate()
   hb_mutexLock( hMtx )
   t_aSessionData := httpd:hSession[ cSID ] := { hMtx, { "_unique" => hb_MD5( Str( hb_Random(), 15, 12 ) ) }, hb_MilliSeconds() + SESSION_TIMEOUT * 1000, cSID }
   session := t_aSessionData[ 2 ]
   UAddHeader( "Set-Cookie", "SESSID=" + cSID + "; path=/" )

   RETURN

STATIC PROCEDURE USessionDestroyInternal()

   hb_HDel( httpd:hSession, t_aSessionData[ 4 ] )
   hb_mutexUnlock( t_aSessionData[ 1 ] )
   UAddHeader( "Set-Cookie", "SESSID=" + t_aSessionData[ 4 ] + "; path=/; Max-Age=0" )

   RETURN

PROCEDURE USessionStart()

   LOCAL cSID

   IF "SESSID" $ cookie
      cSID := cookie[ "SESSID" ]
   ENDIF

   hb_mutexLock( httpd:hmtxSession )
   IF cSID == NIL .OR. !( cSID $ httpd:hSession )
      // Session does not exist
      USessionCreateInternal()
   ELSE

      // Session exists
      t_aSessionData := httpd:hSession[ cSID ]
      IF hb_mutexLock( t_aSessionData[ 1 ], 0 )

         // No concurent sessions
         IF t_aSessionData[ 3 ] > hb_MilliSeconds()
            t_aSessionData[ 3 ] := hb_MilliSeconds() + SESSION_TIMEOUT * 1000
            session := t_aSessionData[ 2 ]
         ELSE
            USessionDestroyInternal()
            USessionCreateInternal()
         ENDIF
      ELSE

         // Concurent process exists
         hb_mutexUnlock( httpd:hmtxSession )

         // Wait for session
         hb_mutexLock( t_aSessionData[ 1 ] )

         // Check if session is not destroyed
         hb_mutexLock( httpd:hmtxSession )
         IF cSID $ httpd:hSession
            // Session exists
            IF t_aSessionData[ 3 ] > hb_MilliSeconds()
               t_aSessionData[ 3 ] := hb_MilliSeconds() + SESSION_TIMEOUT * 1000
               session := t_aSessionData[ 2 ]
            ELSE
               USessionDestroyInternal()
               USessionCreateInternal()
            ENDIF
         ELSE
            // Session was destroyed by concurent process
            USessionCreateInternal()
         ENDIF
      ENDIF
   ENDIF
   hb_mutexUnlock( httpd:hmtxSession )

   RETURN

PROCEDURE USessionStop()

   session := NIL
   hb_mutexUnlock( t_aSessionData[ 1 ] )
   t_aSessionData := NIL

   RETURN

PROCEDURE USessionDestroy()

   hb_mutexLock( httpd:hmtxSession )
   USessionDestroyInternal()
   USessionStop()
   hb_mutexUnlock( httpd:hmtxSession )

   RETURN

FUNCTION UOsFileName( cFileName )

   IF !( hb_ps() == "/" )
      RETURN StrTran( cFileName, "/", hb_ps() )
   ENDIF

   RETURN cFileName

FUNCTION UHtmlEncode( cString )

   LOCAL nI, cI, cRet := ""

   FOR nI := 1 TO Len( cString )
      cI := SubStr( cString, nI, 1 )
      DO CASE
      CASE cI == "<" ; cRet += "&lt;"
      CASE cI == ">" ; cRet += "&gt;"
      CASE cI == "&" ; cRet += "&amp;"
      CASE cI == '"' ; cRet += "&quot;"
      OTHERWISE      ; cRet += cI
      ENDCASE
   NEXT

   RETURN cRet

FUNCTION UUrlEncode( cString )

   LOCAL nI, cI, cRet := ""

   FOR nI := 1 TO Len( cString )
      cI := SubStr( cString, nI, 1 )
      DO CASE
      CASE cI == " "
         cRet += "+"
      CASE Asc( cI ) >= 127 .OR. Asc( cI ) <= 31 .OR. cI $ '=&%+'
         cRet += "%" + hb_StrToHex( cI )
      OTHERWISE
         cRet += cI
      ENDCASE
   NEXT

   RETURN cRet

FUNCTION UUrlDecode( cString )

   LOCAL nI := 1

   cString := StrTran( cString, "+", " " )
   DO WHILE nI <= Len( cString ) .AND. ( nI := hb_At( "%", cString, nI ) ) > 0
      IF Upper( SubStr( cString, nI + 1, 1 ) ) $ "0123456789ABCDEF" .AND. ;
         Upper( SubStr( cString, nI + 2, 1 ) ) $ "0123456789ABCDEF"
         cString := Stuff( cString, nI, 3, hb_HexToStr( SubStr( cString, nI + 1, 2 ) ) )
      ENDIF
      nI++
   ENDDO

   RETURN cString

FUNCTION ULink( cText, cUrl )
   RETURN '<a href="' + cUrl + '">' + UHtmlEncode( cText ) + '</a>'

FUNCTION UUrlChecksum( cUrl )
   RETURN cUrl + iif( "?" $ cUrl, "&", "?" ) + "_ucs=" + hb_MD5( session[ "_unique" ] + cUrl + session[ "_unique" ] )

FUNCTION UUrlValidate( cUrl )

   LOCAL nI

   hb_default( @cURL, server[ "REQUEST_URI" ] )

   IF ( nI := At( "?_ucs=", cUrl ) ) == 0
      nI := At( "&_ucs=", cUrl )
   ENDIF

   RETURN hb_MD5( session[ "_unique" ] + Left( cUrl, nI - 1 ) + session[ "_unique" ] ) == SubStr( cUrl, nI + 6 )

PROCEDURE UProcFiles( cFileName, lIndex )

   LOCAL aDir, aF, cI, tDate, tHDate

   cFileName := StrTran( cFileName, "//", "/" )

   // Security
   IF "/../" $ cFileName
      USetStatusCode( 403 )
      RETURN
   ENDIF

   IF hb_FileExists( UOsFileName( cFileName ) )
      IF "HTTP_IF_MODIFIED_SINCE" $ server .AND. ;
         HttpDateUnformat( server[ "HTTP_IF_MODIFIED_SINCE" ], @tHDate ) .AND. ;
         hb_FGetDateTime( UOsFileName( cFileName ), @tDate ) .AND. ;
         tDate <= tHDate

         USetStatusCode( 304 )

      ELSEIF "HTTP_IF_UNMODIFIED_SINCE" $ server .AND. ;
         HttpDateUnformat( server[ "HTTP_IF_UNMODIFIED_SINCE" ], @tHDate ) .AND. ;
         hb_FGetDateTime( UOsFileName( cFileName ), @tDate ) .AND. ;
         tDate > tHDate

         USetStatusCode( 412 )
      ELSE
         SWITCH Lower( hb_FNameExt( cFileName ) )
         CASE ".css";  cI := "text/css"; EXIT
         CASE ".htm"
         CASE ".html"; cI := "text/html"; EXIT
         CASE ".txt"
         CASE ".text"
         CASE ".asc"
         CASE ".c"
         CASE ".h"
         CASE ".cpp"
         CASE ".hpp"
         CASE ".log";  cI := "text/plain"; EXIT
         CASE ".rtf";  cI := "text/rtf"; EXIT
         CASE ".xml";  cI := "text/xml"; EXIT
         CASE ".bmp";  cI := "image/bmp"; EXIT
         CASE ".gif";  cI := "image/gif"; EXIT
         CASE ".jpg"
         CASE ".jpe"
         CASE ".jpeg"; cI := "image/jpeg"; EXIT
         CASE ".png";  cI := "image/png";   EXIT
         CASE ".tif"
         CASE ".tiff"; cI := "image/tiff"; EXIT
         CASE ".djv"
         CASE ".djvu"; cI := "image/vnd.djvu"; EXIT
         CASE ".ico";  cI := "image/x-icon"; EXIT
         CASE ".xls";  cI := "application/excel"; EXIT
         CASE ".doc";  cI := "application/msword"; EXIT
         CASE ".pdf";  cI := "application/pdf"; EXIT
         CASE ".ps"
         CASE ".eps";  cI := "application/postscript"; EXIT
         CASE ".ppt";  cI := "application/powerpoint"; EXIT
         CASE ".bz2";  cI := "application/x-bzip2"; EXIT
         CASE ".gz";   cI := "application/x-gzip"; EXIT
         CASE ".tgz";  cI := "application/x-gtar"; EXIT
         CASE ".js";   cI := "application/x-javascript"; EXIT
         CASE ".tar";  cI := "application/x-tar"; EXIT
         CASE ".tex";  cI := "application/x-tex"; EXIT
         CASE ".zip";  cI := "application/zip"; EXIT
         CASE ".midi"; cI := "audio/midi"; EXIT
         CASE ".mp3";  cI := "audio/mpeg"; EXIT
         CASE ".wav";  cI := "audio/x-wav"; EXIT
         CASE ".qt"
         CASE ".mov";  cI := "video/quicktime"; EXIT
         CASE ".avi";  cI := "video/x-msvideo"; EXIT
         OTHERWISE ;   cI := "application/octet-stream"
         ENDSWITCH

         UAddHeader( "Content-Type", cI )

         IF hb_FGetDateTime( UOsFileName( cFileName ), @tDate )
            UAddHeader( "Last-Modified", HttpDateFormat( tDate ) )
         ENDIF

         UWrite( hb_MemoRead( UOsFileName( cFileName ) ) )
      ENDIF
   ELSEIF hb_DirExists( UOsFileName( cFileName ) )
      IF !( Right( cFileName, 1 ) == "/" )
         URedirect( "http://" + server[ "HTTP_HOST" ] + server[ "SCRIPT_NAME" ] + "/" )
         RETURN
      ENDIF
      IF AScan( { "index.html", "index.htm" }, ;
            {| x | iif( hb_FileExists( UOsFileName( cFileName + X ) ), ( cFileName += X, .T. ), .F. ) } ) > 0
         UAddHeader( "Content-Type", "text/html" )
         UWrite( hb_MemoRead( UOsFileName( cFileName ) ) )
         RETURN
      ENDIF
      IF ! hb_defaultValue( lIndex, .F. )
         USetStatusCode( 403 )
         RETURN
      ENDIF

      UAddHeader( "Content-Type", "text/html" )

      aDir := hb_Directory( UOsFileName( cFileName ), "D" )
      IF "s" $ get
         DO CASE
         CASE get[ "s" ] == "s"
            ASort( aDir,,, {| X, Y | iif( X[ F_ATTR ] == "D", iif( Y[ F_ATTR ] == "D", X[ F_NAME ] < Y[ F_NAME ], .T. ), ;
               iif( Y[ F_ATTR ] == "D", .F., X[ F_SIZE ] < Y[ F_SIZE ] ) ) } )
         CASE get[ "s" ] == "m"
            ASort( aDir,,, {| X, Y | iif( X[ F_ATTR ] == "D", iif( Y[ F_ATTR ] == "D", X[ F_NAME ] < Y[ F_NAME ], .T. ), ;
               iif( Y[ F_ATTR ] == "D", .F., X[ F_DATE ] < Y[ F_DATE ] ) ) } )
         OTHERWISE
            ASort( aDir,,, {| X, Y | iif( X[ F_ATTR ] == "D", iif( Y[ F_ATTR ] == "D", X[ F_NAME ] < Y[ F_NAME ], .T. ), ;
               iif( Y[ F_ATTR ] == "D", .F., X[ F_NAME ] < Y[ F_NAME ] ) ) } )
         ENDCASE
      ELSE
         ASort( aDir,,, {| X, Y | iif( X[ F_ATTR ] == "D", iif( Y[ F_ATTR ] == "D", X[ F_NAME ] < Y[ F_NAME ], .T. ), ;
            iif( Y[ F_ATTR ] == "D", .F., X[ F_NAME ] < Y[ F_NAME ] ) ) } )
      ENDIF

      UWrite( '<html><body><h1>Index of ' + server[ "SCRIPT_NAME" ] + '</h1><pre>' )
      UWrite( '<a href="?s=n">Name</a>' )
      UWrite( '<a href="?s=m">Modified</a>' )
      UWrite( '<a href="?s=s">Size</a>' + CR_LF + '<hr>' )
      FOR EACH aF IN aDir
         IF hb_LeftEq( aF[ F_NAME ], "." )
         ELSEIF "D" $ aF[ F_ATTR ]
            UWrite( '[DIR] <a href="' + aF[ F_NAME ] + '/">' + aF[ F_NAME ] + '</a>' + Space( 50 - Len( aF[ F_NAME ] ) ) + ;
               hb_TToC( aF[ F_DATE ] ) + CR_LF )
         ELSE
            UWrite( '      <a href="' + aF[ F_NAME ] + '">' + aF[ F_NAME ] + '</a>' + Space( 50 - Len( aF[ F_NAME ] ) ) + ;
               hb_TToC( aF[ F_DATE ] ) + "  " + hb_ntos( aF[ F_SIZE ] ) + CR_LF )
         ENDIF
      NEXT
      UWrite( "<hr></pre></body></html>" )
   ELSE
      USetStatusCode( 404 )
   ENDIF

   RETURN

PROCEDURE UProcInfo()

   LOCAL cI

   UWrite( '<h1>Info</h1>' )

   UWrite( '<h2>Platform</h2>' )
   UWrite( '<table border=1 cellspacing=0>' )
   UWrite( '<tr><td>OS</td><td>' + UHtmlEncode( OS() ) + '</td></tr>' )
   UWrite( '<tr><td>Harbour</td><td>' + UHtmlEncode( Version() ) + '</td></tr>' )
   UWrite( '<tr><td>Build date</td><td>' + UHtmlEncode( hb_Version( HB_VERSION_BUILD_DATE_STR ) ) + '</td></tr>' )
   UWrite( '<tr><td>Compiler</td><td>' + UHtmlEncode( hb_Compiler() ) + '</td></tr>' )
   UWrite( '</table>' )

   UWrite( '<h2>Capabilities</h2>' )
   UWrite( '<table border=1 cellspacing=0>' )
   cI := ""
   AEval( rddList(), {| X | cI += iif( Empty( cI ), "", ", " ) + X } )
   UWrite( '<tr><td>RDD</td><td>' + UHtmlEncode( cI ) + '</td></tr>' )
   UWrite( '</table>' )

   UWrite( '<h2>Variables</h2>' )

   UWrite( '<h3>server</h3>' )
   UWrite( '<table border=1 cellspacing=0>' )
   AEval( ASort( hb_HKeys( server ) ), {| X | UWrite( '<tr><td>' + X + '</td><td>' + UHtmlEncode( hb_CStr( server[ X ] ) ) + '</td></tr>' ) } )
   UWrite( '</table>' )

   IF ! Empty( get )
      UWrite( '<h3>get</h3>' )
      UWrite( '<table border=1 cellspacing=0>' )
      AEval( ASort( hb_HKeys( get ) ), {| X | UWrite( '<tr><td>' + X + '</td><td>' + UHtmlEncode( hb_CStr( get[ X ] ) ) + '</td></tr>' ) } )
      UWrite( '</table>' )
   ENDIF

   IF ! Empty( post )
      UWrite( '<h3>post</h3>' )
      UWrite( '<table border=1 cellspacing=0>' )
      AEval( ASort( hb_HKeys( post ) ), {| X | UWrite( '<tr><td>' + X + '</td><td>' + UHtmlEncode( hb_CStr( post[ X ] ) ) + '</td></tr>' ) } )
      UWrite( '</table>' )
   ENDIF

   RETURN

FUNCTION UParse( aData, cFileName, hConfig )
   RETURN parse_data( aData, compile_file( cFileName, hConfig ), hConfig )

STATIC FUNCTION parse_data( aData, aCode, hConfig )

   LOCAL aInstr, aData2, cRet, xValue, aValue, cExtend := ""

   DO WHILE cExtend != NIL
      cExtend := NIL
      cRet := ""
      FOR EACH aInstr IN aCode
         SWITCH aInstr[ 1 ]
         CASE "txt"
            cRet += aInstr[ 2 ]
            EXIT

         CASE "="
            IF aInstr[ 2 ] $ aData
               xValue := aData[ aInstr[ 2 ] ]
               DO CASE
               CASE HB_ISSTRING( xValue )    ; cRet += UHtmlEncode( xValue )
               CASE HB_ISNUMERIC( xValue )   ; cRet += UHtmlEncode( Str( xValue ) )
               CASE HB_ISDATE( xValue )      ; cRet += UHtmlEncode( DToC( xValue ) )
               CASE HB_ISTIMESTAMP( xValue ) ; cRet += UHtmlEncode( hb_TToC( xValue ) )
               CASE HB_ISOBJECT( xValue )    ; cRet += UHtmlEncode( xValue:Output() )
               OTHERWISE
                  Eval( hConfig[ "Trace" ], hb_StrFormat( "Template error: invalid type '%1$s'", ValType( xValue ) ) )
               ENDCASE
            ELSE
               Eval( hConfig[ "Trace" ], hb_StrFormat( "Template error: variable '%1$s' not found", aInstr[ 2 ] ) )
            ENDIF
            EXIT

         CASE ":"
            IF aInstr[ 2 ] $ aData
               xValue := aData[ aInstr[ 2 ] ]
               DO CASE
               CASE HB_ISSTRING( xValue )    ; cRet += xValue
               CASE HB_ISNUMERIC( xValue )   ; cRet += Str( xValue )
               CASE HB_ISDATE( xValue )      ; cRet += DToC( xValue )
               CASE HB_ISTIMESTAMP( xValue ) ; cRet += hb_TToC( xValue )
               CASE HB_ISOBJECT( xValue )    ; cRet += xValue:Output()
               OTHERWISE
                  Eval( hConfig[ "Trace" ], hb_StrFormat( "Template error: invalid type '%1$s'", ValType( xValue ) ) )
               ENDCASE
            ELSE
               Eval( hConfig[ "Trace" ], hb_StrFormat( "Template error: variable '%1$s' not found", aInstr[ 2 ] ) )
            ENDIF
            EXIT

         CASE "if"
            xValue := iif( aInstr[ 2 ] $ aData, aData[ aInstr[ 2 ] ], NIL )
            IF ! Empty( xValue )
               cRet += parse_data( aData, aInstr[ 3 ], hConfig )
            ELSE
               cRet += parse_data( aData, aInstr[ 4 ], hConfig )
            ENDIF
            EXIT

         CASE "loop"
            IF aInstr[ 2 ] $ aData .AND. HB_ISARRAY( aValue := aData[ aInstr[ 2 ] ] )
               FOR EACH xValue IN aValue
                  aData2 := hb_HClone( aData )
                  hb_HEval( xValue, {| k, v | aData2[ aInstr[ 2 ] + "." + k ] := v } )
                  aData2[ aInstr[ 2 ] + ".__index" ] := xValue:__enumIndex
                  cRet += parse_data( aData2, aInstr[ 3 ], hConfig )
                  aData2 := NIL
               NEXT
            ELSE
               Eval( hConfig[ "Trace" ], hb_StrFormat( "Template error: loop variable '%1$s' not found", aInstr[ 2 ] ) )
            ENDIF
            EXIT

         CASE "extend"
            cExtend := aInstr[ 2 ]
            EXIT

         CASE "include"
            cRet += parse_data( aData, compile_file( aInstr[ 2 ], hConfig ), hConfig )
            EXIT
         ENDSWITCH
      NEXT
      IF cExtend != NIL
         aData[ "" ] := cRet
         cRet := ""
         aCode := compile_file( cExtend, hConfig )
      ENDIF
   ENDDO

   RETURN cRet

STATIC FUNCTION compile_file( cFileName, hConfig )

   LOCAL nPos, cTpl, aCode := {}

   hb_default( @cFileName, MEMVAR->server[ "SCRIPT_NAME" ] )

   cFileName := UOsFileName( hb_DirBase() + "tpl/" + cFileName + ".tpl" )
   IF hb_FileExists( cFileName )
      cTpl := hb_MemoRead( cFileName )
      BEGIN SEQUENCE
         IF ( nPos := compile_buffer( cTpl, 1, aCode ) ) < Len( cTpl ) + 1
            Break( nPos )
         ENDIF
      RECOVER USING nPos
         Eval( hConfig[ "Trace" ], hb_StrFormat( "Template error: syntax at %1$s(%2$d,%3$d)", cFileName, SubStrCount( Chr( 10 ), cTpl,, nPos ) + 1, nPos - hb_RAt( Chr( 10 ), cTpl,, nPos - 1 ) ) )
         aCode := {}
      END SEQUENCE
   ELSE
      Eval( hConfig[ "Trace" ], hb_StrFormat( "Template error: file '%1$s' not found", cFileName ) )
   ENDIF

   RETURN aCode

STATIC FUNCTION compile_buffer( cTpl, nStart, aCode )

   LOCAL nI, nS, nE, cTag, cParam

   DO WHILE ( nS := hb_At( "{{", cTpl, nStart ) ) > 0
      IF nS > nStart
         AAdd( aCode, { "txt", SubStr( cTpl, nStart, nS - nStart ) } )
      ENDIF
      IF ( nE := hb_At( "}}", cTpl, nS ) ) > 0
         IF ( nI := hb_At( " ", cTpl, nS, nE ) ) == 0
            nI := nE
         ENDIF
         cTag := SubStr( cTpl, nS + 2, nI - nS - 2 )
         cParam := SubStr( cTpl, nI + 1, nE - nI - 1 )

         SWITCH cTag
         CASE "="
         CASE ":"
            AAdd( aCode, { cTag, cParam } )
            nStart := nE + 2
            EXIT

         CASE "if"
            AAdd( aCode, { "if", cParam, {}, {} } )
            nI := compile_buffer( cTpl, nE + 2, ATail( aCode )[ 3 ] )
            IF SubStr( cTpl, nI, 8 ) == "{{else}}"
               nI := compile_buffer( cTpl, nI + 8, ATail( aCode )[ 4 ] )
            ENDIF
            IF SubStr( cTpl, nI, 9 ) == "{{endif}}"
               nStart := nI + 9
            ELSE
               Break( nI )
            ENDIF
            EXIT

         CASE "loop"
            AAdd( aCode, { "loop", cParam, {} } )
            nI := compile_buffer( cTpl, nE + 2, ATail( aCode )[ 3 ] )
            IF SubStr( cTpl, nI, 11 ) == "{{endloop}}"
               nStart := nI + 11
            ELSE
               Break( nI )
            ENDIF
            EXIT

         CASE "extend"
            AAdd( aCode, { "extend", cParam } )
            nStart := nE + 2
            EXIT

         CASE "include"
            AAdd( aCode, { "include", cParam } )
            nStart := nE + 2
            EXIT

         OTHERWISE
            RETURN nS

         ENDSWITCH
      ELSE
         Break( nS )
      ENDIF
   ENDDO
   IF nStart < Len( cTpl )
      AAdd( aCode, { "txt", SubStr( cTpl, nStart ) } )
   ENDIF

   RETURN Len( cTpl ) + 1

STATIC FUNCTION SubStrCount( cSub, cString, nStart, nEnd )

   LOCAL nCount := 0

   hb_default( @nStart, 1 )

   DO WHILE ( nStart := hb_At( cSub, cString, nStart, nEnd ) ) > 0
      nCount++
      nStart++
   ENDDO

   RETURN nCount
