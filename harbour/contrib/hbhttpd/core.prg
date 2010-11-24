/*
 * $Id$
 */

#include "hbclass.ch"
#include "common.ch"
#include "error.ch"

#include "hbsocket.ch"

#pragma -km+

/*
  Docs:

  RFC 1945 - Hypertext Transfer Protocol -- HTTP/1.0
  RFC 2616 - Hypertext Transfer Protocol -- HTTP/1.1
  HTTP Made Really Easy (http://www.jmarshall.com/easy/http/)
*/


#define THREAD_COUNT_PREALLOC    0
#define THREAD_COUNT_MAX        50
#define SESSION_TIMEOUT        600

#define CR_LF                       ( Chr( 13 ) + Chr( 10 ) )

THREAD STATIC t_cResult, t_nStatusCode, t_aHeader, t_lSessionDestroy

MEMVAR server, get, post, cookie, session


CREATE CLASS UHttpd
   /* Settings */
   DATA nPort        INIT 80
   DATA cBindAddress INIT "0.0.0.0"
   DATA bLogAccess   INIT {|| NIL }
   DATA bLogError    INIT {|| NIL }
   DATA bIdle        INIT {|| NIL }
   DATA aMount       INIT { => }

   /* Results */
   DATA cError       INIT ""

   /* Private */
   DATA hmtxQueue
   DATA hmtxLog
   DATA hmtxSession

   DATA hListen
   DATA aSession

   DATA lStop

   METHOD RUN()
   METHOD Stop()

   /* Private */
   METHOD LogAccess()
   METHOD LogError( cError )

ENDCLASS

FUNCTION UHttpdNew()

   RETURN UHttpd()

METHOD RUN() CLASS UHttpd

   LOCAL hSocket, nI, aThreads
   LOCAL nWaiters

   IF ! HB_MTVM()
      Self:cError := "Multithread support required"
      RETURN .F.
   ENDIF

   IF Self:nPort < 1 .OR. Self:nPort > 65535
      Self:cError := "Invalid port number"
      RETURN .F.
   ENDIF

   Self:hmtxQueue   := hb_mutexCreate()
   Self:hmtxLog     := hb_mutexCreate()
   Self:hmtxSession := hb_mutexCreate()

   IF Empty( Self:hListen := hb_socketOpen() )
      Self:cError :=  "Socket create error " + hb_ntos( hb_socketGetError() )
      RETURN .F.
   ENDIF

   IF !hb_socketBind( Self:hListen, { HB_SOCKET_AF_INET, Self:cBindAddress, Self:nPort } )
      Self:cError :=  "Bind error " + hb_ntos( hb_socketGetError() )
      hb_socketClose( Self:hListen )
      RETURN .F.
   ENDIF

   IF !hb_socketListen( Self:hListen )
      Self:cError :=  "Listen error " + hb_ntos( hb_socketGetError() )
      hb_socketClose( Self:hListen )
      RETURN .F.
   ENDIF

   aThreads := {}
   FOR nI := 1 TO THREAD_COUNT_PREALLOC
      AAdd( aThreads, hb_threadStart( @ProcessConnection(), Self ) )
   NEXT

   Self:lStop := .F.
   Self:aSession := { => }

   DO WHILE .T.
      IF Empty( hSocket := hb_socketAccept( Self:hListen,, 1000 ) )
         IF hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT
            Eval( Self:bIdle, Self )
            IF Self:lStop;  EXIT
            ENDIF
         ELSE
            Self:LogError( "[error] Accept error " + hb_ntos( hb_socketGetError() ) )
         ENDIF
      ELSE
         hb_mutexQueueInfo( Self:hmtxQueue, @nWaiters )
         ? "New connection", hSocket
         ? "Waiters:", nWaiters
         IF nWaiters < 2 .AND. Len( aThreads ) < THREAD_COUNT_MAX
         /*
            We need two threads in worst case. If first thread becomes a sessioned
            thread, the second one will continue to serve sessionless requests for
            the same connection. We create two threads here to avoid free thread count
            check (and aThreads variable sync) in ProcessRequest().
         */
            AAdd( aThreads, hb_threadStart( @ProcessConnection(), Self ) )
            AAdd( aThreads, hb_threadStart( @ProcessConnection(), Self ) )
         ENDIF
         hb_mutexNotify( Self:hmtxQueue, { hSocket, "" } )
      ENDIF
   ENDDO
   hb_socketClose( Self:hListen )

   /* End child threads */
   hb_mutexLock( Self:hmtxSession )
   HB_HEVAL( Self:aSession, {|k, v| hb_mutexNotify( v[2], NIL ), HB_SYMBOL_UNUSED( k ) } )
   hb_mutexUnlock( Self:hmtxSession )
   AEval( aThreads, {|| hb_mutexNotify( Self:hmtxQueue, NIL ) } )
   AEval( aThreads, {|h| hb_threadJoin( h ) } )

   RETURN .T.

METHOD Stop() CLASS UHttpd

   Self:lStop := .T.

   RETURN NIL

METHOD LogError( cError ) CLASS UHttpd

   hb_mutexLock( Self:hmtxLog )
   Eval( Self:bLogError, DToS( Date() ) + " " + Time() + " " + cError )
   hb_mutexUnlock( Self:hmtxLog )

   RETURN NIL

METHOD LogAccess() CLASS UHttpd

   LOCAL cDate := DToS( Date() ), cTime := Time()

   hb_mutexLock( Self:hmtxLog )
   Eval( Self:bLogAccess, ;
      server["REMOTE_ADDR"] + " - - [" + Right( cDate, 2 ) + "/" + ;
      { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" }[VAL(SUBSTR(cDate, 5, 2))] + ;
      "/" + Left( cDate, 4 ) + ":" + cTime + ' +0000] "' + server["REQUEST_ALL"] + '" ' + ;
      hb_ntos( t_nStatusCode ) + " " + hb_ntos( Len( t_cResult ) ) + ;
      ' "' + server["HTTP_REFERER"] + '" "' + server["HTTP_USER_AGENT"] + ;
      '"' )
   hb_mutexUnlock( Self:hmtxLog )

   RETURN NIL

STATIC FUNCTION ProcessConnection( oServer )

   LOCAL hSocket, cRequest, aI, nLen, nReqLen, cBuf

   PRIVATE server, get, post, cookie

   DO WHILE .T.
      hb_mutexSubscribe( oServer:hmtxQueue, , @aI )
      IF aI == NIL
         EXIT
      ENDIF

      hSocket := aI[1]
      cRequest := aI[2]

      BEGIN SEQUENCE

         /* receive query header */
         cRequest := ""
         nLen := 1
         DO WHILE At( CR_LF + CR_LF, cRequest ) == 0 .AND. nLen > 0
            cBuf := Space( 4096 )
            IF ( nLen := hb_socketRecv( hSocket, @cBuf,,, 10000 ) ) > 0  /* Timeout */
               cRequest += Left( cBuf, nLen )
            ELSE
               IF nLen == - 1 .AND. hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT
                  nLen := 0
                  ? "recv() timeout", hSocket
               ENDIF
            ENDIF
         ENDDO

         IF nLen == - 1
            ? "recv() error:", hb_socketGetError()
         ELSEIF nLen == 0 /* connection closed */
         ELSE

            // PRIVATE
            server := { => }
            get := { => }
            post := { => }
            cookie := { => }

            t_cResult := ""
            t_aHeader := {}
            t_nStatusCode := 200

            IF !Empty( aI := hb_socketGetPeerName( hSocket ) )
               server["REMOTE_ADDR"] := aI[HB_SOCKET_ADINFO_ADDRESS]
               server["REMOTE_HOST"] := server["REMOTE_ADDR"]  // no reverse DNS
               server["REMOTE_PORT"] := aI[HB_SOCKET_ADINFO_PORT]
            ENDIF

            IF !Empty( aI := hb_socketGetSockName( hSocket ) )
               server["SERVER_ADDR"] := aI[HB_SOCKET_ADINFO_ADDRESS]
               server["SERVER_PORT"] := aI[HB_SOCKET_ADINFO_PORT]
            ENDIF

            ? Left( cRequest, At( CR_LF + CR_LF, cRequest ) + 1 )

            nReqLen := ParseRequestHeader( @cRequest )
            IF nReqLen == NIL
               USetStatusCode( 400 )
            ELSE

               /* receive query body */
               DO WHILE Len( cRequest ) < nReqLen .AND. nLen > 0
                  cBuf := Space( 4096 )
                  IF ( nLen := hb_socketRecv( hSocket, @cBuf,,, 500 ) ) > 0
                     cRequest += Left( cBuf, nLen )
                  ENDIF
               ENDDO

               IF nLen == - 1
                  ? "recv() error:", hb_socketGetError()
               ELSEIF nLen == 0 /* connection closed */
               ELSE
                  ? cRequest
                  ParseRequestBody( Left( cRequest, nReqLen ) )
                  cRequest := SubStr( cRequest, nReqLen + 1 )

                  /* Deal with supported protocols and methods */
                  IF server["SERVER_PROTOCOL"] $ "HTTP/1.0 HTTP/1.1"
                     IF !( server["REQUEST_METHOD"] $ "GET POST" )
                        USetStatusCode( 501 )
                     ELSE
                        IF server["SERVER_PROTOCOL"] == "HTTP/1.1"
                           IF Lower( server["HTTP_CONNECTION"] ) == "close"
                              UAddHeader( "Connection", "close" )
                           ELSE
                              UAddHeader( "Connection", "keep-alive" )
                           ENDIF
                        ENDIF
                        IF ! ProcessRequest( oServer, hSocket );  BREAK
                        ENDIF
                     ENDIF
                  ELSE /* We do not support another protocols */
                     USetStatusCode( 400 )
                  ENDIF
               ENDIF
            ENDIF

            SendResponse( oServer, hSocket )

            IF Lower( UGetHeader( "Connection" ) ) == "close" .OR. server["SERVER_PROTOCOL"] == "HTTP/1.0"
            ELSE
               hb_mutexNotify( oServer:hmtxQueue, { hSocket, cRequest } )
               BREAK
            ENDIF
         ENDIF
         ? "Close connection1", hSocket
         hb_socketShutdown( hSocket )
         hb_socketClose( hSocket )
      END SEQUENCE
   ENDDO
   dbCloseAll()

   RETURN 0

STATIC FUNCTION ProcessRequest( oServer, hSocket, cBuffer )

   LOCAL nI, cMount, cPath, cSID, hmtx, aData, bEval

   PRIVATE session

   // Search mounting table
   cMount := server["SCRIPT_NAME"]
   IF HB_HHasKey( oServer:aMount, cMount )
      cPath := ""
   ELSE
      nI := Len( cMount )
      DO WHILE ( nI := HB_RAT( "/", cMount,, nI ) ) > 0
         IF HB_HHasKey( oServer:aMount, Left( cMount, nI ) + "*" )
            cMount := Left( cMount, nI ) + "*"
            cPath := SubStr( server["SCRIPT_NAME"], nI + 1 )
            EXIT
         ENDIF
         nI --
      ENDDO
   ENDIF

   IF cMount != NIL
      bEval := oServer:aMount[cMount, 1]

      IF oServer:aMount[cMount, 2]
         /* sessioned */
         IF HB_HHasKey( cookie, "SESSID" );  cSID := cookie["SESSID"]
         ENDIF

         hb_mutexLock( oServer:hmtxSession )
         IF cSID == NIL .OR. ! HB_HHasKey( oServer:aSession, cSID )

            /* create new session */

            cSID := HB_MD5( DToS( Date() ) + Time() + Str( HB_RANDOM(), 15, 12 ) )
            hmtx := hb_mutexCreate()
            oServer:aSession[cSID] := { hb_threadSelf(), hmtx, { => } }

            // PRIVATE
            session := oServer:aSession[cSID, 3]

            hb_mutexUnlock( oServer:hmtxSession )

            DO WHILE .T.
               t_cResult := ""
               t_aHeader := {}
               t_nStatusCode := 200
               t_lSessionDestroy := .F.
               BEGIN SEQUENCE WITH {|oErr| UErrorHandler( oErr, oServer ) }
                  Eval( bEval, cPath )
               RECOVER
                  USetStatusCode( 500 )
               END SEQUENCE

               IF t_lSessionDestroy
                  UAddHeader( "Set-Cookie", "SESSID=" + cSID + "; path=/; Max-Age=0" )
               ELSE
                  UAddHeader( "Set-Cookie", "SESSID=" + cSID + "; path=/" )
               ENDIF

               IF server["SERVER_PROTOCOL"] == "HTTP/1.1"
                  IF Lower( server["HTTP_CONNECTION"] ) == "close"
                     UAddHeader( "Connection", "close" )
                  ELSE
                     UAddHeader( "Connection", "keep-alive" )
                  ENDIF
               ENDIF

               SendResponse( oServer, hSocket )

               IF t_lSessionDestroy
                  /* Destroy session before closing socket, since graceful close requires some time */
                  hb_mutexLock( oServer:hmtxSession )
                  HB_HDel( oServer:aSession, cSID )
                  hb_mutexUnlock( oServer:hmtxSession )
               ENDIF

               IF Lower( UGetHeader( "Connection" ) ) == "close" .OR. server["SERVER_PROTOCOL"] == "HTTP/1.0"
                  ? "Close connection2", hSocket
                  hb_socketShutdown( hSocket )
                  hb_socketClose( hSocket )
               ELSE
                  /* pass connection to common queue */
                  hb_mutexNotify( oServer:hmtxQueue, { hSocket, cBuffer } )
               ENDIF

               IF t_lSessionDestroy
                  EXIT
               ENDIF

               IF ! hb_mutexSubscribe( hmtx, SESSION_TIMEOUT, @aData ) .OR. aData == NIL
                  ? "Session exit"
                  hb_mutexLock( oServer:hmtxSession )
                  HB_HDel( oServer:aSession, cSID )
                  hb_mutexUnlock( oServer:hmtxSession )
                  EXIT
               ENDIF
               hSocket := aData[1]
               cBuffer := aData[2]
               bEval := aData[3]
               cPath := aData[4]
               server := aData[5]
               get := aData[6]
               post := aData[7]
               cookie := aData[8]
               session := aData[9]
               aData := NIL
            ENDDO

            /* close databases and release variables */
            dbCloseAll()
            server := NIL
            get := NIL
            post := NIL
            cookie := NIL
            session := NIL
         ELSE
            /* session already exists */
            ? "session pries", server["SCRIPT_NAME"]
            hb_mutexNotify( oServer:aSession[cSID, 2], { hSocket, cBuffer, oServer:aMount[cMount, 1], cPath, server, get, post, cookie, oServer:aSession[cSID, 3] } )
            hb_mutexUnlock( oServer:hmtxSession )
         ENDIF

         RETURN .F.
      ELSE
         /* not sessioned */
         BEGIN SEQUENCE WITH {|oErr| UErrorHandler( oErr, oServer ) }
            Eval( bEval, cPath )
         RECOVER
            USetStatusCode( 500 )
         END SEQUENCE
      ENDIF
   ELSE
      USetStatusCode( 404 )
   ENDIF

   RETURN .T.

STATIC FUNCTION ParseRequestHeader( cRequest )

   LOCAL aRequest, aLine, nI, nJ, cI, nK, nContentLength := 0

   aRequest := uhttpd_split( CR_LF, cRequest )
   aLine := uhttpd_split( " ", aRequest[1] )

   server["REQUEST_ALL"] := aRequest[1]
   IF Len( aLine ) == 3 .AND. Left( aLine[3], 5 ) == "HTTP/"
      server["REQUEST_METHOD"] := aLine[1]
      server["REQUEST_URI"] := aLine[2]
      server["SERVER_PROTOCOL"] := aLine[3]
   ELSE
      server["REQUEST_METHOD"] := aLine[1]
      server["REQUEST_URI"] := iif( Len( aLine ) >= 2, aLine[2], "" )
      server["SERVER_PROTOCOL"] := iif( Len( aLine ) >= 3, aLine[3], "" )
      RETURN 0
   ENDIF

   // Fix invalid queries: bind to root
   IF ! ( Left( server["REQUEST_URI"], 1 ) == "/" )
      server["REQUEST_URI"] := "/" + server["REQUEST_URI"]
   ENDIF

   IF ( nI := At( "?", server["REQUEST_URI"] ) ) > 0
      server["SCRIPT_NAME"] := Left( server["REQUEST_URI"], nI - 1 )
      server["QUERY_STRING"] := SubStr( server["REQUEST_URI"], nI + 1 )
   ELSE
      server["SCRIPT_NAME"] := server["REQUEST_URI"]
      server["QUERY_STRING"] := ""
   ENDIF

   server["HTTP_ACCEPT"] := ""
   server["HTTP_ACCEPT_CHARSET"] := ""
   server["HTTP_ACCEPT_ENCODING"] := ""
   server["HTTP_ACCEPT_LANGUAGE"] := ""
   server["HTTP_CONNECTION"] := ""
   server["HTTP_HOST"] := ""
   server["HTTP_KEEP_ALIVE"] := ""
   server["HTTP_REFERER"] := ""
   server["HTTP_USER_AGENT"] := ""
   server["HTTP_CONTENT_TYPE"] := ""

   FOR nI := 2 TO Len( aRequest )
      IF aRequest[nI] == ""
         EXIT
      ELSEIF ( nJ := At( ":", aRequest[nI] ) ) > 0
         cI := AllTrim( SubStr( aRequest[nI], nJ + 1 ) )
         SWITCH Upper( Left( aRequest[nI], nJ - 1 ) )
         CASE "COOKIE"
            IF ( nK := At( ";", cI ) ) == 0
               nK := Len( RTrim( cI ) )
            ENDIF
            cI := Left( cI, nK )
            IF ( nK := At( "=", cI ) ) > 0
               /* cookie names are case insensitive, uppercase it */
               cookie[ UPPER( LEFT( cI, nK - 1 ) ) ] := SubStr( cI, nK + 1 )
            ENDIF
            EXIT
         CASE "CONTENT-LENGTH"
            nContentLength := Val( cI )
            EXIT
            OTHERWISE
            server[ "HTTP_" + STRTRAN( UPPER( LEFT( aRequest[nI], nJ - 1 ) ), "-", "_" ) ] := cI
            EXIT
         ENDSWITCH
      ENDIF
   NEXT
   IF !( server["QUERY_STRING"] == "" )
      FOR EACH cI IN uhttpd_split( "&", server["QUERY_STRING"] )
         IF ( nI := At( "=", cI ) ) > 0
            get[UUrlDecode(LEFT(cI, nI - 1))] := UUrlDecode( SubStr( cI, nI + 1 ) )
         ELSE
            get[UUrlDecode(cI)] := NIL
         ENDIF
      NEXT
   ENDIF
   cRequest := SubStr( cRequest, At( CR_LF + CR_LF, cRequest ) + 4 )

   RETURN nContentLength

STATIC FUNCTION ParseRequestBody( cRequest )

   LOCAL nI, cPart

   IF server["HTTP_CONTENT_TYPE"] == "application/x-www-form-urlencoded"
      FOR EACH cPart IN uhttpd_split( "&", cRequest )
         IF ( nI := At( "=", cPart ) ) > 0
            post[UUrlDecode(LEFT(cPart, nI - 1))] := UUrlDecode( SubStr( cPart, nI + 1 ) )
         ELSE
            post[UUrlDecode(cPart)] := NIL
         ENDIF
      NEXT
   ENDIF

   RETURN NIL

STATIC FUNCTION MakeResponse()

   LOCAL cRet

   IF UGetHeader( "Content-Type" ) == NIL
      UAddHeader( "Content-Type", "text/html" )
   ENDIF
   UAddHeader( "Date", HttpDateFormat( HB_DATETIME() ) )

   cRet := iif( server["SERVER_PROTOCOL"] == "HTTP/1.0", "HTTP/1.0 ", "HTTP/1.1 " )
   SWITCH t_nStatusCode
   CASE 200
      cRet += "200 OK"
      EXIT
   CASE 301
      cRet += "301 Moved Permanently"
      t_cResult := "<html><body><h1>301 Moved Permanently</h1></body></html>"
      EXIT
   CASE 302
      cRet += "302 Found"
      t_cResult := "<html><body><h1>302 Found</h1></body></html>"
      EXIT
   CASE 303
      cRet += "303 See Other"
      t_cResult := "<html><body><h1>303 See Other</h1></body></html>"
      EXIT
   CASE 304
      cRet += "304 Not Modified"
      t_cResult := "<html><body><h1>304 Not Modified</h1></body></html>"
      EXIT
   CASE 400
      cRet += "400 Bad Request"
      t_cResult := "<html><body><h1>400 Bad Request</h1></body></html>"
      UAddHeader( "Connection", "close" )
      EXIT
   CASE 401
      cRet += "401 Unauthorized"
      t_cResult := "<html><body><h1>401 Unauthorized</h1></body></html>"
      EXIT
   CASE 402
      cRet += "402 Payment Required"
      t_cResult := "<html><body><h1>402 Payment Required</h1></body></html>"
      EXIT
   CASE 403
      cRet += "403 Forbidden"
      t_cResult := "<html><body><h1>403 Forbidden</h1></body></html>"
      EXIT
   CASE 404
      cRet += "404 Not Found"
      t_cResult := "<html><body><h1>404 Not Found</h1></body></html>"
      EXIT
   CASE 412
      cRet += "412 Precondition Failed"
      t_cResult := "<html><body><h1>412 Precondition Failed</h1></body></html>"
      EXIT
   CASE 500
      cRet += "500 Internal Server Error"
      t_cResult := "<html><body><h1>500 Internal Server Error</h1></body></html>"
      EXIT
   CASE 501
      cRet += "501 Not Implemented"
      t_cResult := "<html><body><h1>501 Not Implemented</h1></body></html>"
      UAddHeader( "Connection", "close" )
      EXIT
      OTHERWISE
      cRet += "500 Internal Server Error"
      t_cResult := "<html><body><h1>500 Internal Server Error</h1></body></html>"
      UAddHeader( "Connection", "close" )
   ENDSWITCH
   cRet += CR_LF
   UAddHeader( "Content-Length", hb_ntos( Len( t_cResult ) ) )
   AEval( t_aHeader, {|x| cRet += x[1] + ": " + x[2] + CR_LF } )
   cRet += CR_LF
   ? cRet
   cRet += t_cResult

   RETURN cRet

STATIC PROCEDURE SendResponse( oServer, hSocket )

   LOCAL cSend, nLen

   cSend := MakeResponse()

   //  ? cSend

   DO WHILE Len( cSend ) > 0
      IF ( nLen := hb_socketSend( hSocket, cSend ) ) == - 1
         ? "send() error:", hb_socketGetError(), hSocket
         EXIT
      ELSEIF nLen > 0
         cSend := SubStr( cSend, nLen + 1 )
      ENDIF
   ENDDO
   oServer:LogAccess()

   RETURN

STATIC FUNCTION HttpDateFormat( tDate )

   RETURN { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" }[DOW(tDate)] + ", " + ;
      PadL( Day( tDate ), 2, "0" ) + " " + ;
      { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" }[MONTH(tDate)] + ;
      " " + PadL( Year( tDate ), 4, "0" ) + HB_TTOC( tDate, "", "HH:MM:SS" ) + " GMT" // TOFIX: time zone

STATIC FUNCTION HttpDateUnformat( cDate, tDate )

   LOCAL nMonth

   // TODO: support outdated compatibility format RFC2616
   IF Len( cDate ) == 29 .AND. Right( cDate, 4 ) == " GMT" .AND. SubStr( cDate, 4, 2 ) == ", "
      nMonth := ASCAN( { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", ;
         "Oct", "Nov", "Dec" }, SubStr( cDate, 9, 3 ) )
      IF nMonth > 0
         tDate := HB_STOT( SubStr( cDate, 13, 4 ) + PadL( nMonth, 2, "0" ) + SubStr( cDate, 6, 2 ) + ;
            StrTran( SubStr( cDate, 18, 8 ), ":" ) )
         RETURN ! Empty( tDate )
      ENDIF
   ENDIF

   RETURN .F.

STATIC FUNCTION UErrorHandler( oErr, oServer )

   IF     oErr:genCode == EG_ZERODIV;  RETURN 0
   ELSEIF oErr:genCode == EG_LOCK;     RETURN .T.
   ELSEIF ( oErr:genCode == EG_OPEN .AND. oErr:osCode == 32 .OR. ;
         oErr:genCode == EG_APPENDLOCK ) .AND. oErr:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF
   oServer:LogError( GetErrorDesc( oErr ) )
   BREAK( oErr )

   RETURN NIL

STATIC FUNCTION GetErrorDesc( oErr )

   LOCAL cRet, nI

   cRet := "ERRORLOG ============================================================" + hb_eol() + ;
      "Error: " + oErr:subsystem + "/" + ErrDescCode( oErr:genCode ) + "(" + hb_ntos( oErr:genCode ) + ") " + ;
      hb_ntos( oErr:subcode ) + hb_eol()
   IF !Empty( oErr:filename );      cRet += "File: " + oErr:filename + hb_eol()
   ENDIF
   IF !Empty( oErr:description );   cRet += "Description: " + oErr:description + hb_eol()
   ENDIF
   IF !Empty( oErr:operation );     cRet += "Operacija: " + oErr:operation + hb_eol()
   ENDIF
   IF !Empty( oErr:osCode );        cRet += "OS error: " + hb_ntos( oErr:osCode ) + hb_eol()
   ENDIF
   IF hb_isArray( oErr:args )
      cRet += "Arguments:" + hb_eol()
      AEval( oErr:args, {|X, Y| cRet += Str( Y, 5 ) + ": " + HB_CStr( X ) + hb_eol() } )
   ENDIF
   cRet += hb_eol()

   cRet += "Stack:" + hb_eol()
   nI := 2
   DO WHILE ! Empty( ProcName( ++ nI ) )
      cRet += "    " + ProcName( nI ) + "(" + hb_ntos( ProcLine( nI ) ) + ")" + hb_eol()
   ENDDO
   cRet += hb_eol()

   cRet += "Executable:  " + HB_PROGNAME() + hb_eol()
   cRet += "Versions:" + hb_eol()
   cRet += "  OS: " + OS() + hb_eol()
   cRet += "  Harbour: " + Version() + ", " + HB_BUILDDATE() + hb_eol()
   cRet += hb_eol()

   IF oErr:genCode != EG_MEM
      cRet += "Database areas:" + hb_eol()
      cRet += "    Current: " + hb_ntos( Select() ) + "  " + Alias() + hb_eol()

      BEGIN SEQUENCE WITH {|o| BREAK( o ) }
         IF !Empty( Alias() )
            cRet += "    Filter: " + dbFilter() + hb_eol()
            cRet += "    Relation: " + dbRelation() + hb_eol()
            cRet += "    Index expression: " + OrdKey( OrdSetFocus() ) + hb_eol()
            cRet += hb_eol()
            BEGIN SEQUENCE WITH {|o| BREAK( o ) }
               FOR nI := 1 TO FCount()
                  cRet += Str( nI, 6 ) + " " + PadR( FieldName( nI ), 14 ) + ": " + HB_VALTOEXP( FieldGet( nI ) ) + hb_eol()
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
         BEGIN SEQUENCE WITH {|o| BREAK( o ) }
            IF ! Empty( Alias( nI ) )
               dbSelectArea( nI )
               cRet += Str( nI, 6 ) + " " + rddName() + " " + PadR( Alias(), 15 ) + ;
                  Str( RecNo() ) + "/" + Str( LastRec() ) + ;
                  iif( Empty( OrdSetFocus() ), "", " Index " + OrdSetFocus() + "(" + hb_ntos( OrdNumber() ) + ")" ) + hb_eol()
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
      cI := {;
         "ARG"     , "BOUND"    , "STROVERFLOW", "NUMOVERFLOW", "ZERODIV" , "NUMERR"     , "SYNTAX"  , "COMPLEXITY" , ; //  1,  2,  3,  4,  5,  6,  7,  8
         NIL       , NIL        , "MEM"        , "NOFUNC"     , "NOMETHOD", "NOVAR"      , "NOALIAS" , "NOVARMETHOD", ; //  9, 10, 11, 12, 13, 14, 15, 16
         "BADALIAS", "DUPALIAS" , NIL          , "CREATE"     , "OPEN"    , "CLOSE"      , "READ"    , "WRITE"      , ; // 17, 18, 19, 20, 21, 22, 23, 24
         "PRINT"   , NIL        , NIL          , NIL          , NIL       , "UNSUPPORTED", "LIMIT"   , "CORRUPTION" , ; // 25, 26 - 29, 30, 31, 32
         "DATATYPE", "DATAWIDTH", "NOTABLE"    , "NOORDER"    , "SHARED"  , "UNLOCKED"   , "READONLY", "APPENDLOCK" , ; // 33, 34, 35, 36, 37, 38, 39, 40
         "LOCK"    }[nCode]                                                                                            // 41
   ENDIF

   RETURN iif( cI == NIL, "", "EG_" + cI )


/********************************************************************
  Public functions
********************************************************************/

PROCEDURE USetStatusCode( nStatusCode )

   t_nStatusCode := nStatusCode

   RETURN

FUNCTION UGetHeader( cType )

   LOCAL nI

   IF ( nI := ASCAN( t_aHeader, {|x| Upper(x[1] ) == Upper(cType ) } ) ) > 0
      RETURN t_aHeader[nI, 2]
   ENDIF

   RETURN NIL

PROCEDURE UAddHeader( cType, cValue )

   LOCAL nI

   IF ( nI := ASCAN( t_aHeader, {|x| Upper(x[1] ) == Upper(cType ) } ) ) > 0
      t_aHeader[nI, 2] := cValue
   ELSE
      AAdd( t_aHeader, { cType, cValue } )
   ENDIF

   RETURN

PROCEDURE URedirect( cURL, nCode )

   IF nCode == NIL;  nCode := 303
   ENDIF
   USetStatusCode( nCode )
   UAddHeader( "Location", cURL )

   RETURN

PROCEDURE USessionDestroy()

   t_lSessionDestroy := .T.

   RETURN

PROCEDURE UWrite( cString )

   t_cResult += cString

   RETURN

FUNCTION UOsFileName( cFileName )

   IF hb_ps() != "/"
      RETURN StrTran( cFileName, "/", hb_ps() )
   ENDIF

   RETURN cFileName

FUNCTION UHtmlEncode( cString )

   LOCAL nI, cI, cRet := ""

   FOR nI := 1 TO Len( cString )
      cI := SubStr( cString, nI, 1 )
      IF cI == "<"
         cRet += "&lt;"
      ELSEIF cI == ">"
         cRet += "&gt;"
      ELSEIF cI == "&"
         cRet += "&amp;"
      ELSEIF cI == '"'
         cRet += "&quot;"
      ELSE
         cRet += cI
      ENDIF
   NEXT

   RETURN cRet

FUNCTION UUrlEncode( cString )

   LOCAL nI, cI, cRet := ""

   FOR nI := 1 TO Len( cString )
      cI := SubStr( cString, nI, 1 )
      IF cI == " "
         cRet += "+"
      ELSEIF Asc( cI ) >= 127 .OR. Asc( cI ) <= 31 .OR. cI $ '=&%+'
         cRet += "%" + HB_StrToHex( cI )
      ELSE
         cRet += cI
      ENDIF
   NEXT

   RETURN cRet

FUNCTION UUrlDecode( cString )

   LOCAL nI

   cString := StrTran( cString, "+", " " )
   nI := 1
   DO WHILE nI <= Len( cString )
      nI := HB_AT( "%", cString, nI )
      IF nI == 0;  EXIT
      ENDIF
      IF Upper( SubStr( cString, nI + 1, 1 ) ) $ "0123456789ABCDEF" .AND. ;
            Upper( SubStr( cString, nI + 2, 1 ) ) $ "0123456789ABCDEF"
         cString := Stuff( cString, nI, 3, HB_HexToStr( SubStr(cString, nI + 1, 2 ) ) )
      ENDIF
      nI++
   ENDDO

   RETURN cString

FUNCTION ULink( cText, cURL )

   RETURN '<a href="' + cURL + '">' + UHtmlEncode( cText ) + '</a>'

PROCEDURE UProcFiles( cFileName, lIndex )

   LOCAL aDir, aF, nI, cI, tDate, tHDate

   DEFAULT lIndex TO .F.

   cFileName := StrTran( cFileName, "//", "/" )

   // Security
   IF "/../" $ cFileName
      USetStatusCode( 403 )
      RETURN
   ENDIF

   IF HB_FileExists( uOSFileName( cFileName ) )
      IF HB_HHasKey( server, "HTTP_IF_MODIFIED_SINCE" ) .AND. ;
            HttpDateUnformat( server["HTTP_IF_MODIFIED_SINCE"], @tHDate ) .AND. ;
            HB_FGETDATETIME( UOsFileName( cFileName ), @tDate ) .AND. ;
            ( tDate <= tHDate )
         USetStatusCode( 304 )
      ELSEIF HB_HHasKey( server, "HTTP_IF_UNMODIFIED_SINCE" ) .AND. ;
            HttpDateUnformat( server["HTTP_IF_UNMODIFIED_SINCE"], @tHDate ) .AND. ;
            HB_FGETDATETIME( UOsFileName( cFileName ), @tDate ) .AND. ;
            ( tDate > tHDate )
         USetStatusCode( 412 )
      ELSE
         IF ( nI := RAt( ".", cFileName ) ) > 0
            SWITCH Lower( SubStr( cFileName, nI + 1 ) )
            CASE "css";                                 cI := "text/css";  EXIT
            CASE "htm";   CASE "html";                  cI := "text/html";  EXIT
            CASE "txt";   CASE "text";  CASE "asc"
            CASE "c";     CASE "h";     CASE "cpp"
            CASE "hpp";   CASE "log";                   cI := "text/plain";  EXIT
            CASE "rtf";                                 cI := "text/rtf";  EXIT
            CASE "xml";                                 cI := "text/xml";  EXIT
            CASE "bmp";                                 cI := "image/bmp";  EXIT
            CASE "gif";                                 cI := "image/gif";  EXIT
            CASE "jpg";   CASE "jpe";   CASE "jpeg";    cI := "image/jpeg";  EXIT
            CASE "png";                                 cI := "image/png";   EXIT
            CASE "tif";   CASE "tiff";                  cI := "image/tiff";  EXIT
            CASE "djv";   CASE "djvu";                  cI := "image/vnd.djvu";  EXIT
            CASE "ico";                                 cI := "image/x-icon";  EXIT
            CASE "xls";                                 cI := "application/excel";  EXIT
            CASE "doc";                                 cI := "application/msword";  EXIT
            CASE "pdf";                                 cI := "application/pdf";  EXIT
            CASE "ps";    CASE "eps";                   cI := "application/postscript";  EXIT
            CASE "ppt";                                 cI := "application/powerpoint";  EXIT
            CASE "bz2";                                 cI := "application/x-bzip2";  EXIT
            CASE "gz";                                  cI := "application/x-gzip";  EXIT
            CASE "tgz";                                 cI := "application/x-gtar";  EXIT
            CASE "js";                                  cI := "application/x-javascript";  EXIT
            CASE "tar";                                 cI := "application/x-tar";  EXIT
            CASE "tex";                                 cI := "application/x-tex";  EXIT
            CASE "zip";                                 cI := "application/zip";  EXIT
            CASE "midi";                                cI := "audio/midi";  EXIT
            CASE "mp3";                                 cI := "audio/mpeg";  EXIT
            CASE "wav";                                 cI := "audio/x-wav";  EXIT
            CASE "qt";    CASE "mov";                   cI := "video/quicktime";  EXIT
            CASE "avi";                                 cI := "video/x-msvideo";  EXIT
            OTHERWISE
               cI := "application/octet-stream"
            ENDSWITCH
         ELSE
            cI := "application/octet-stream"
         ENDIF
         UAddHeader( "Content-Type", cI )

         IF HB_FGETDATETIME( UOsFileName( cFileName ), @tDate )
            UAddHeader( "Last-Modified", HttpDateFormat( tDate ) )
         ENDIF

         UWrite( HB_MEMOREAD( UOsFileName(cFileName ) ) )
      ENDIF
   ELSEIF HB_DirExists( UOsFileName( cFileName ) )
      IF Right( cFileName, 1 ) != "/"
         URedirect( "http://" + server["HTTP_HOST"] + server["SCRIPT_NAME"] + "/" )
         RETURN
      ENDIF
      IF ASCAN( { "index.html", "index.htm" }, ;
            {|x| iif( HB_FileExists( UOSFileName(cFileName + X ) ), ( cFileName += X, .T. ), .F. ) } ) > 0
         UAddHeader( "Content-Type", "text/html" )
         UWrite( HB_MEMOREAD( UOsFileName(cFileName ) ) )
         RETURN
      ENDIF
      IF ! lIndex
         USetStatusCode( 403 )
         RETURN
      ENDIF

      UAddHeader( "Content-Type", "text/html" )

      aDir := Directory( UOsFileName( cFileName ), "D" )
      IF HB_HHasKey( get, "s" )
         IF get["s"] == "s"
            ASort( aDir, , , {|X, Y| iif( X[5] == "D", iif(Y[5] == "D", X[1] < Y[1], .T. ), ;
               iif( Y[5] == "D", .F. , X[2] < Y[2] ) ) } )
         ELSEIF get["s"] == "m"
            ASort( aDir, , , {|X, Y| iif( X[5] == "D", iif(Y[5] == "D", X[1] < Y[1], .T. ), ;
               iif( Y[5] == "D", .F. , DToS( X[3] ) + X[4] < DToS( Y[3] ) + Y[4] ) ) } )
         ELSE
            ASort( aDir, , , {|X, Y| iif( X[5] == "D", iif(Y[5] == "D", X[1] < Y[1], .T. ), ;
               iif( Y[5] == "D", .F. , X[1] < Y[1] ) ) } )
         ENDIF
      ELSE
         ASort( aDir, , , {|X, Y| iif( X[5] == "D", iif(Y[5] == "D", X[1] < Y[1], .T. ), ;
            iif( Y[5] == "D", .F. , X[1] < Y[1] ) ) } )
      ENDIF

      UWrite( '<html><body><h1>Index of ' + server[ "SCRIPT_NAME" ] + '</h1><pre>      ' )
      UWrite( '<a href="?s=n">Name</a>                                                  ' )
      UWrite( '<a href="?s=m">Modified</a>             ' )
      UWrite( '<a href="?s=s">Size</a>' + CR_LF + '<hr>' )
      FOR EACH aF IN aDir
         IF Left( aF[1], 1 ) == "."
         ELSEIF "D" $ aF[5]
            UWrite( '[DIR] <a href="' + aF[1] + '/">' + aF[1] + '</a>' + Space( 50 - Len(aF[1] ) ) + ;
               DToC( aF[3] ) + ' ' + aF[4] + CR_LF )
         ELSE
            UWrite( '      <a href="' + aF[1] + '">' + aF[1] + '</a>' + Space( 50 - Len(aF[1] ) ) + ;
               DToC( aF[3] ) + ' ' + aF[4] + Str( aF[2], 12 ) + CR_LF )
         ENDIF
      NEXT
      UWrite( "<hr></pre></body></html>" )
   ELSE
      USetStatusCode( 404 )
   ENDIF

   RETURN

PROCEDURE UProcInfo()

   UWrite( '<h1>Info</h1>' )

   UWrite( '<h2>Platform</h2>' )
   UWrite( '<table border=1 cellspacing=0>' )
   UWrite( '<tr><td>OS</td><td>' + UHtmlEncode( OS() ) + '</td></tr>' )
   UWrite( '<tr><td>Harbour</td><td>' + UHtmlEncode( Version() ) + '</td></tr>' )
   UWrite( '<tr><td>Build date</td><td>' + UHtmlEncode( HB_BUILDDATE() ) + '</td></tr>' )
   UWrite( '<tr><td>Compiler</td><td>' + UHtmlEncode( HB_COMPILER() ) + '</td></tr>' )
   UWrite( '</table>' )

   UWrite( '<h2>Capabilities</h2>' )
   UWrite( '<table border=1 cellspacing=0>' )
   UWrite( '<tr><td>RDD</td><td>' + UHtmlEncode( uhttpd_join(", ", rddList() ) ) + '</td></tr>' )
   UWrite( '</table>' )

   UWrite( '<h2>Variables</h2>' )

   UWrite( '<h3>server</h3>' )
   UWrite( '<table border=1 cellspacing=0>' )
   HB_HEval( server, {|k, v| UWrite( '<tr><td>' + k + '</td><td>' + UHtmlEncode(HB_CStr(v ) ) + '</td></tr>' ) } )
   UWrite( '</table>' )

   IF !Empty( get )
      UWrite( '<h3>get</h3>' )
      UWrite( '<table border=1 cellspacing=0>' )
      HB_HEval( get, {|k, v| UWrite( '<tr><td>' + k + '</td><td>' + UHtmlEncode(HB_CStr(v ) ) + '</td></tr>' ) } )
      UWrite( '</table>' )
   ENDIF

   IF !Empty( post )
      UWrite( '<h3>post</h3>' )
      UWrite( '<table border=1 cellspacing=0>' )
      HB_HEval( post, {|k, v| UWrite( '<tr><td>' + k + '</td><td>' + UHtmlEncode(HB_CStr(v ) ) + '</td></tr>' ) } )
      UWrite( '</table>' )
   ENDIF

   RETURN

FUNCTION uhttpd_split( cSeparator, cString )

   LOCAL aRet := {}
   LOCAL nI

   DO WHILE ( nI := At( cSeparator, cString ) ) > 0
      AAdd( aRet, Left( cString, nI - 1 ) )
      cString := SubStr( cString, nI + Len( cSeparator ) )
   ENDDO
   AAdd( aRet, cString )

   RETURN aRet

FUNCTION uhttpd_join( cSeparator, aData )

   LOCAL cRet := ""
   LOCAL nI

   FOR nI := 1 TO Len( aData )

      IF nI > 1
         cRet += cSeparator
      ENDIF

      IF     ValType( aData[ nI ] ) $ "CM" ; cRet += aData[ nI ]
      ELSEIF ValType( aData[ nI ] ) == "N" ; cRet += hb_ntos( aData[ nI ] )
      ELSEIF ValType( aData[ nI ] ) == "D" ; cRet += iif( ! Empty( aData[ nI ] ), DToC( aData[ nI ] ), "" )
      ELSE
      ENDIF
   NEXT

   RETURN cRet
