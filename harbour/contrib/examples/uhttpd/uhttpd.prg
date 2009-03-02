/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    uHTTPD (Micro HTTP server)
 *
 * Copyright 2009 Francesco Saverio Giudice <info / at / fsgiudice.com>
 * Copyright 2008 Mindaugas Kavaliauskas (dbtopas at dbtopas.lt)
 * www - http://www.harbour-project.org
 *
 * Credits:
 *    Based on first version posted from Mindaugas Kavaliauskas on
 *    developers NG on December 15th, 2008 whom give my thanks to have
 *    shared initial work.
 *                                                          Francesco.
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

/*
 * A simple HTTP server.
 *
 * More description to come.
 *
 *
 */

/*
  TODO:
      - security check
        verify to launch .hrb and .exe *only* from cgi-bin
      - optimize code
      - add SSL support
      - check CGIExec() better

*/

// remove comment to activate hb_toOutDebug()
//#define DEBUG_ACTIVE

#ifdef __XHARBOUR__
   #include "hbcompat.ch"
   #xtranslate hb_StrFormat( [<x,...>] ) => hb_sprintf( <x> )
#else
   /* TRY / CATCH / FINALLY / END */
   #xcommand TRY  => BEGIN SEQUENCE WITH {|oErr| Break( oErr )}
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
   #xcommand FINALLY => ALWAYS
#endif
#include "fileio.ch"
#include "common.ch"
#include "inkey.ch"
#include "error.ch"

#include "hbextern.ch"   // need this to use with HRB

#ifdef GD_SUPPORT
  // adding GD support
  REQUEST GDIMAGE, GDIMAGECHAR, GDCHART
  #define APP_GD_SUPPORT "_GD"
  #stdout "Lib GD support enabled"
#else
  #define APP_GD_SUPPORT ""
  #stdout "Lib GD support disabled"
#endif

#ifdef USE_HB_INET
  #define APP_INET_SUPPORT "_INET"
  #stdout "Harbour socket"
#else
  #define APP_INET_SUPPORT ""
  #stdout "Mindaugas socket"
#endif

#define APP_NAME      "uhttpd"
#define APP_VER_NUM   "0.4.1"
#define APP_VERSION   APP_VER_NUM + APP_GD_SUPPORT + APP_INET_SUPPORT

#define AF_INET         2

// default values - they can changes using line command switch
#define START_RUNNING_THREADS   4             // Start threads to serve connections
#define MAX_RUNNING_THREADS    20             // Max running threads

#define START_SERVICE_THREADS   1             // Initial number for service connections
#define MAX_SERVICE_THREADS     3             // Max running threads

#define LISTEN_PORT             8082          // differs from standard 80 port for tests in case
                                              // anyone has a apache/IIS installed
#define FILE_STOP               ".uhttpd.stop"
#define FILE_ACCESS_LOG         "logs\access.log"
#define FILE_ERROR_LOG          "logs\error.log"

#define PAGE_STATUS_REFRESH   5
#define THREAD_MAX_WAIT     ( 60 ) // HOW MUCH TIME THREAD HAS TO WAIT BEFORE FINISH - IN SECONDS
#define CGI_MAX_EXEC_TIME     30

// TOCHECK: Caching of HRB modules (Is this faster than loading HRBBody from file where OS will cache ?)
#define HRB_ACTIVATE_CACHE   .F.   // if .T. caching of HRB modules will be enabled. (NOTE: changes of files will not be loaded until server is active)

#define CR_LF    (CHR(13)+CHR(10))
#define HB_IHASH()   HB_HSETCASEMATCH( {=>}, FALSE )

#ifndef __XHARBOUR__

  #ifdef __PLATFORM__WINDOWS
     REQUEST HB_GT_WVT_DEFAULT
     REQUEST HB_GT_WIN
     REQUEST HB_GT_NUL
  #else
     REQUEST HB_GT_STD_DEFAULT
     REQUEST HB_GT_NUL
  #endif
 #define THREAD_GT hb_gtVersion()

#else

  REQUEST HB_GT_WVT
  REQUEST HB_GT_WIN
  REQUEST HB_GT_NUL

#endif

// dynamic call for HRB support
DYNAMIC HRBMAIN

STATIC s_hmtxQueue, s_hmtxServiceThreads, s_hmtxRunningThreads, s_hmtxLog, s_hmtxConsole, s_hmtxBusy
STATIC s_hmtxHRB, s_hmtxCGIKill

STATIC s_hfileLogAccess, s_hfileLogError, s_cDocumentRoot, s_lIndexes, s_lConsole, s_nPort
STATIC s_cSessionPath
STATIC s_nThreads, s_nStartThreads, s_nMaxThreads
STATIC s_nServiceThreads, s_nStartServiceThreads, s_nMaxServiceThreads
STATIC s_nConnections, s_nMaxConnections, s_nTotConnections
STATIC s_nServiceConnections, s_nMaxServiceConnections, s_nTotServiceConnections
STATIC s_aRunningThreads := {}
STATIC s_aServiceThreads := {}
STATIC s_hHRBModules     := {=>}

#ifdef USE_HB_INET
STATIC s_cLocalAddress, s_nLocalPort
#endif

// ALIASES: now read from ini file
//STATIC s_hScriptAliases    := { "/info" => "/cgi-bin/info.hrb" }
STATIC s_hScriptAliases    := { => }

THREAD STATIC t_cResult, t_nStatusCode, t_aHeader, t_cErrorMsg, t_oSession
THREAD STATIC t_hProc

MEMVAR _SERVER, _GET, _POST, _COOKIE, _SESSION, _REQUEST, _HTTP_REQUEST, m_cPost

ANNOUNCE ERRORSYS

FUNCTION MAIN( ... )
   LOCAL nPort, hListen, hSocket, aRemote, cI, xVal
   LOCAL aThreads, nStartThreads, nMaxThreads, nStartServiceThreads
   LOCAL i, cPar, lStop
   LOCAL cGT, cDocumentRoot, lIndexes, cConfig
   LOCAL lConsole, lScriptAliasMixedCase
   LOCAL nProgress := 0
   LOCAL hDefault, cLogAccess, cLogError, cSessionPath
   LOCAL cCmdPort, cCmdDocumentRoot, lCmdIndexes, nCmdStartThreads, nCmdMaxThreads

   IF !HB_MTVM()
      ? "I need multhread support. Please, recompile me!"
      WAIT
      RETURN 2
   ENDIF

   // ----------------------- Initializations ---------------------------------

   SysSettings()

   ErrorBlock( { | oError | uhttpd_DefError( oError ) } )

   // ----------------------- Parameters defaults -----------------------------

   // defaults not changeble via ini file
   lStop                := FALSE
   cConfig              := EXE_Path() + "\" + APP_NAME + ".ini"
   lConsole             := TRUE
   nStartServiceThreads := START_SERVICE_THREADS

   // Check GT version - if I have started app with //GT:NUL then I have to disable
   // console
   cGT := HB_GTVERSION()
   IF ( cGT == "NUL" )
      lConsole := FALSE
   ENDIF

   // TOCHECK: now not force case insensitive
   //HB_HSETCASEMATCH( s_hScriptAliases, FALSE )

   // ----------------- Line command parameters checking ----------------------

   i := 1
   DO WHILE ( i <= PCount() )

      cPar := hb_PValue( i++ )

      DO CASE
      CASE cPar == "--port"             .OR. cPar == "-p"
         cCmdPort    := hb_PValue( i++ )

      CASE cPar == "--docroot"          .OR. cPar == "-d"
         cCmdDocumentRoot := hb_PValue( i++ )

      CASE cPar == "--indexes"          .OR. cPar == "-i"
         lCmdIndexes := TRUE

      CASE cPar == "--stop"             .OR. cPar == "-s"
         lStop    := TRUE

      CASE cPar == "--config"           .OR. cPar == "-c"
         cConfig     := hb_PValue( i++ )

      CASE cPar == "--start-threads"    .OR. cPar == "-ts"
         nCmdStartThreads := Val( hb_PValue( i++ ) )

      CASE cPar == "--max-threads"      .OR. cPar == "-tm"
         nCmdMaxThreads := Val( hb_PValue( i++ ) )

      CASE cPar == "--help"             .OR. Lower( cPar ) == "-h" .OR. cPar == "-?"
         help()
         RETURN 0

      OTHERWISE
         help()
         RETURN 0
      ENDCASE
   ENDDO

   // -------------------- checking STOP request -------------------------------

   IF lStop
      HB_MEMOWRIT( FILE_STOP, "" )
      RETURN 0
   ELSE
      FERASE( FILE_STOP )
   ENDIF

   // ----------------- Parse ini file ----------------------------------------

   //hb_ToOutDebug( "cConfig = %s\n\r", cConfig )

   hDefault := ParseIni( cConfig )

   // ------------------- Parameters changeable from ini file ----------------

   // All key values MUST be in uppercase
   nPort                 := hDefault[ "MAIN" ][ "PORT" ]
   cDocumentRoot         := hDefault[ "MAIN" ][ "DOCUMENT_ROOT" ]
   lIndexes              := hDefault[ "MAIN" ][ "SHOW_INDEXES" ]
   lScriptAliasMixedCase := hDefault[ "MAIN" ][ "SCRIPTALIASMIXEDCASE" ]
   cSessionPath          := hDefault[ "MAIN" ][ "SESSIONPATH" ]

   cLogAccess            := hDefault[ "LOGFILES" ][ "ACCESS" ]
   cLogError             := hDefault[ "LOGFILES" ][ "ERROR" ]

   nStartThreads         := hDefault[ "THREADS" ][ "START_NUM" ]
   nMaxThreads           := hDefault[ "THREADS" ][ "MAX_NUM" ]

   // ATTENTION: aliases can be in mixed case
   // i.e. we can have /info or /Info that will be different unless lScriptAliasMixedCase will be FALSE
   FOR EACH xVal IN hDefault[ "ALIASES" ]
       IF HB_ISSTRING( xVal )
          hb_HSet( s_hScriptAliases, IIF( lScriptAliasMixedCase, xVal:__enumKey(), Upper( xVal:__enumKey() ) ), xVal )
       ENDIF
   NEXT

   //hb_ToOutDebug( "hDefault = %s\n\r", hb_ValToExp( hDefault ) )
   //hb_ToOutDebug( "s_hScriptAliases = %s\n\r", hb_ValToExp( s_hScriptAliases ) )

   // ------------------- Parameters forced from command line ----------------

   IF cCmdPort != NIL
      nPort := Val( cCmdPort )
   ENDIF

   IF cCmdDocumentRoot != NIL
      cDocumentRoot := cCmdDocumentRoot
   ENDIF

   IF lCmdIndexes != NIL
      lIndexes := lCmdIndexes
   ENDIF

   IF nCmdStartThreads != NIL
      nStartThreads := nCmdStartThreads
   ENDIF

   IF nCmdMaxThreads != NIL
      nMaxThreads := nCmdMaxThreads
   ENDIF

   // -------------------- checking starting values ----------------------------

   IF nPort <= 0 .OR. nPort > 65535
      ? "Invalid port number:", nPort
      WAIT
      RETURN 1
   ENDIF


   IF HB_ISSTRING( cDocumentRoot )
      //cI := STRTRAN( SUBSTR( cDocumentRoot, 2 ), "\", "/" )
      cI := cDocumentRoot
      IF HB_DirExists( cI )
         IF RIGHT( cI, 1 ) == "/" .AND. LEN(cI) > 2 .AND. SUBSTR( cI, LEN( cI ) - 2, 1 ) != ":"
           s_cDocumentRoot := LEFT( cI, LEN( cI ) - 1 )
         ELSE
           s_cDocumentRoot := cI
         ENDIF
      ELSE
         ? "Invalid document root:", cI
         WAIT
         RETURN 3
      ENDIF
   ELSE
      ? "Invalid document root"
      WAIT
      RETURN 3
   ENDIF

   //hb_ToOutDebug( "s_cDocumentRoot = %s, cDocumentRoot = %s\n\r", s_cDocumentRoot, cDocumentRoot )

   IF nMaxThreads <= 0
      nMaxThreads := MAX_RUNNING_THREADS
   ENDIF

   IF nStartThreads < 0
      nStartThreads := 0
   ELSEIF nStartThreads > nMaxThreads
      nStartThreads := nMaxThreads
   ENDIF

   // -------------------- assign STATIC values --------------------------------

   s_lIndexes               := lIndexes
   s_lConsole               := lConsole
   s_nPort                  := nPort
   s_nThreads               := 0
   s_nStartThreads          := nStartThreads
   s_nMaxThreads            := nMaxThreads
   s_nServiceThreads        := 0
   s_nStartServiceThreads   := nStartServiceThreads
   s_nMaxServiceThreads     := MAX_SERVICE_THREADS
   s_nConnections           := 0
   s_nMaxConnections        := 0
   s_nTotConnections        := 0
   s_nServiceConnections    := 0
   s_nMaxServiceConnections := 0
   s_nTotServiceConnections := 0
   s_cSessionPath           := cSessionPath

   // --------------------- Open log files -------------------------------------

   IF ( s_hfileLogAccess := FOPEN( cLogAccess, FO_CREAT + FO_WRITE ) ) == -1
      ? "Can't open access log file"
      WAIT
      RETURN 1
   ENDIF
   FSEEK( s_hfileLogAccess, 0, FS_END )

   IF ( s_hfileLogError := FOPEN( cLogError, FO_CREAT + FO_WRITE ) ) == -1
      ? "Can't open error log file"
      WAIT
      RETURN 1
   ENDIF
   FSEEK( s_hfileLogError, 0, FS_END )

   // --------------------- MAIN PART ------------------------------------------

   SET CURSOR OFF

   // --------------------- define mutexes -------------------------------------

   s_hmtxQueue          := hb_mutexCreate()
   s_hmtxLog            := hb_mutexCreate()
   s_hmtxConsole        := hb_mutexCreate()
   s_hmtxBusy           := hb_mutexCreate()
   s_hmtxRunningThreads := hb_mutexCreate()
   s_hmtxServiceThreads := hb_mutexCreate()
   s_hmtxHRB            := hb_mutexCreate()
   s_hmtxCGIKill        := hb_mutexCreate()

   WriteToConsole( "--- Starting " + APP_NAME + " ---" )

   // --------------------------------------------------------------------------
   // SOCKET CREATION
   // --------------------------------------------------------------------------

#ifdef USE_HB_INET
   hListen := hb_InetServer( nPort )

   IF hb_InetErrorCode( hListen ) != 0
      ? "Bind Error"
   ELSE

     s_nLocalPort    := hb_InetPort( hListen )
     s_cLocalAddress := hb_InetAddress( hListen )

#else
   hListen   := socket_create()
   IF socket_bind( hListen, { AF_INET, "0.0.0.0", nPort } ) == -1
      ? "bind() error", socket_error()
   ELSEIF socket_listen( hListen ) == -1
      ? "listen() error", socket_error()
   ELSE
#endif
      // --------------------------------------------------------------------------------- //
      // Starting Accept connection thread
      // --------------------------------------------------------------------------------- //

      WriteToConsole( "Starting AcceptConnection Thread" )
      aThreads := {}
      AADD( aThreads, hb_threadStart( @AcceptConnections() ) )

      // --------------------------------------------------------------------------------- //
      // main loop
      // --------------------------------------------------------------------------------- //

      WriteToConsole( "Starting main loop" )

      IF s_lConsole
         hb_DispOutAt( 1, 5, APP_NAME + " - web server - v. " + APP_VERSION )
         hb_DispOutAt( 4, 5, "Server listening (Port: " + LTrim( Str( nPort ) ) + ") : ..." )
         hb_DispOutAt( 10, 9, "Waiting." )
      ENDIF

      DO WHILE .T.

         // windows resource releasing - 1 millisecond wait
         WIN_SYSREFRESH( 1 )

         IF s_lConsole

            // Show application infos
            IF hb_mutexLock( s_hmtxBusy )
               hb_DispOutAt( 5, 5, "Threads           : " + Transform( s_nThreads, "9999999999" ) )
               hb_DispOutAt( 6, 5, "Connections       : " + Transform( s_nConnections, "9999999999" ) )
               hb_DispOutAt( 7, 5, "Max Connections   : " + Transform( s_nMaxConnections, "9999999999" ) )
               hb_DispOutAt( 8, 5, "Total Connections : " + Transform( s_nTotConnections, "9999999999" ) )

               hb_DispOutAt( 5, 37, "ServiceThreads    : " + Transform( s_nServiceThreads, "9999999999" ) )
               hb_DispOutAt( 6, 37, "Connections       : " + Transform( s_nServiceConnections, "9999999999" ) )
               hb_DispOutAt( 7, 37, "Max Connections   : " + Transform( s_nMaxServiceConnections, "9999999999" ) )
               hb_DispOutAt( 8, 37, "Total Connections : " + Transform( s_nTotServiceConnections, "9999999999" ) )
               hb_mutexUnlock( s_hmtxBusy )
            ENDIF

            // Show progress
            Progress( @nProgress )
         ENDIF

         // Wait a connection
#ifdef USE_HB_INET
         hb_InetTimeOut( hListen, 50 )
         IF HB_InetDataReady( hListen ) > 0
#else
         IF socket_select( { hListen },,, 50 ) > 0
#endif
            // reset remote values
            aRemote := NIL

            // Accept a remote connection
#ifdef USE_HB_INET
            hSocket := HB_INETACCEPT( hListen )
#else
            hSocket := socket_accept( hListen, @aRemote )
#endif
            IF hSocket == NIL

#ifdef USE_HB_INET
               WriteToConsole( hb_StrFormat( "accept() error" ) )
#else
               WriteToConsole( hb_StrFormat( "accept() error: %s", socket_error() ) )
#endif

            ELSE

               // Send accepted connection to AcceptConnections() thread
               hb_mutexNotify( s_hmtxQueue, hSocket )

            ENDIF

         ELSE

            // Checking if I have to quit
            IF HB_FileExists( FILE_STOP )
               FERASE( FILE_STOP )
               EXIT
            ENDIF

         ENDIF

      ENDDO

      WriteToConsole( "Waiting threads" )
      // Send to thread that they have to stop
      AEVAL( aThreads, {|| hb_mutexNotify( s_hmtxQueue, NIL ) } )
      // Wait threads to end
      AEVAL( aThreads, {|h| hb_threadJoin( h ) } )

   ENDIF

   WriteToConsole( "--- Quitting " + APP_NAME + " ---" )

   // Close socket
#ifdef USE_HB_INET
   hb_InetClose( hListen )
#else
   socket_close( hListen )
#endif

   // Close log files
   FCLOSE( s_hfileLogAccess )
   FCLOSE( s_hfileLogError )

   SET CURSOR ON

   RETURN 0

// --------------------------------------------------------------------------------- //
// THREAD FUNCTIONS
// --------------------------------------------------------------------------------- //

STATIC FUNCTION AcceptConnections()
   LOCAL hSocket
   LOCAL nConnections, nThreads, nMaxThreads, n
   LOCAL nServiceConnections, nServiceThreads, nMaxServiceThreads, nThreadID
   LOCAL pThread

   WriteToConsole( "Starting AcceptConnections()" )

   // Starting initial running threads
   FOR n := 1 TO s_nStartThreads
       pThread := hb_threadStart( @ProcessConnection(), @nThreadID )
       AADD( s_aRunningThreads, { pThread, nThreadID } )
   NEXT

   // Starting initial service threads
   FOR n := 1 TO s_nStartServiceThreads
       pThread := hb_threadStart( @ServiceConnection(), @nThreadID )
       AADD( s_aServiceThreads, { pThread, nThreadID } )
   NEXT

   // Main AcceptConnections loop
   DO WHILE .T.

      // reset socket
      hSocket := NIL

      // releasing resources
      WIN_SYSREFRESH( 1 )

      // Waiting a connection from main application loop
      hb_mutexSubscribe( s_hmtxQueue,, @hSocket )

      // I have a QUIT request
      IF hSocket == NIL

         // Requesting to Running threads to quit (using -1 value)
         AEVAL( s_aRunningThreads, {|| hb_mutexNotify( s_hmtxRunningThreads, -1 ) } )
         // waiting running threads to quit
         AEVAL( s_aRunningThreads, {|h| hb_threadJoin( h[1] ) } )

         // Requesting to Service threads to quit (using -1 value)
         AEVAL( s_aServiceThreads, {|| hb_mutexNotify( s_hmtxServiceThreads, -1 ) } )
         // waiting service threads to quit
         AEVAL( s_aServiceThreads, {|h| hb_threadJoin( h[1] ) } )

         EXIT
      ENDIF

      // Load current state
      IF hb_mutexLock( s_hmtxBusy )
         nConnections       := s_nConnections
         nThreads           := s_nThreads
         nMaxThreads        := s_nMaxThreads
         nServiceConnections:= s_nServiceConnections
         nServiceThreads    := s_nServiceThreads
         nMaxServiceThreads := s_nMaxServiceThreads
         hb_mutexUnlock( s_hmtxBusy )
      ENDIF

      // If I have no more threads to use ...
      IF nConnections > nMaxThreads

         // If I have no more of service threads to use ... (DOS attack ?)
         IF nServiceConnections > nMaxServiceThreads
             // DROP connection
#ifdef USE_HB_INET
            hb_InetClose( hSocket )
#else
            socket_shutdown( hSocket )
            socket_close( hSocket )
#endif

         // If I have no service threads in use ...
         ELSEIF nServiceConnections >= nServiceThreads
            // Add one more
            pThread := hb_threadStart( @ServiceConnection(), @nThreadID )
            AADD( s_aServiceThreads, { pThread, nThreadID } )
         ENDIF
         // Otherwise I send connection to service thread
         hb_mutexNotify( s_hmtxServiceThreads, hSocket )

         LOOP

      // If I have no running threads in use ...
      ELSEIF nConnections >= nThreads
         // Add one more
         pThread := hb_threadStart( @ProcessConnection(), @nThreadID )
         AADD( s_aRunningThreads, { pThread, nThreadID } )
      ENDIF
      // Otherwise I send connection to running thread
      hb_mutexNotify( s_hmtxRunningThreads, hSocket )

   ENDDO

   WriteToConsole( "Quitting AcceptConnections()" )

   RETURN 0

// --------------------------------------------------------------------------------- //
// CONNECTIONS
// --------------------------------------------------------------------------------- //
STATIC FUNCTION ProcessConnection( /*@*/ nThreadId )
   LOCAL hSocket, nLen, cRequest, cSend
   LOCAL nMsecs, nParseTime, nPos

   nThreadId    := hb_threadID()

   ErrorBlock( { | oError | uhttpd_DefError( oError ) } )

   WriteToConsole( "Starting ProcessConnections() " + hb_CStr( nThreadId ) )

   IF hb_mutexLock( s_hmtxBusy )
      s_nThreads++
      hb_mutexUnlock( s_hmtxBusy )
   ENDIF

   // ProcessConnection Loop
   DO WHILE .T.

      // Reset socket
      hSocket := NIL

      // releasing resources
      WIN_SYSREFRESH( 1 )

      // Waiting a connection from AcceptConnections() but up to defined time
      hb_mutexSubscribe( s_hmtxRunningThreads, THREAD_MAX_WAIT, @hSocket )

      // received a -1 value, I have to quit
      IF HB_ISNUMERIC( hSocket )
         EXIT
      // no socket received, thread can graceful quit only if over minimal number
      ELSEIF hSocket == NIL
         IF hb_mutexLock( s_hmtxBusy )
            IF s_nThreads <= s_nStartThreads
               hb_mutexUnlock( s_hmtxBusy )
               LOOP
            ENDIF
            hb_mutexUnlock( s_hmtxBusy )
         ENDIF
         EXIT
      ENDIF

      // Connection accepted
      IF hb_mutexLock( s_hmtxBusy )
         s_nConnections++
         s_nTotConnections++
         s_nMaxConnections := Max( s_nConnections, s_nMaxConnections )
         hb_mutexUnlock( s_hmtxBusy )
      ENDIF

      // Save initial time
      nMsecs := hb_milliseconds()

      BEGIN SEQUENCE

         /* receive query */
         nLen := readRequest( hSocket, @cRequest )

         IF nLen == -1
#ifdef USE_HB_INET
            ? "recv() error:", hb_InetErrorCode( hSocket ), hb_InetErrorDesc( hSocket )
#else
            ? "recv() error:", socket_error()
#endif

         ELSEIF nLen == 0 /* connection closed */
         ELSE

            //hb_ToOutDebug( "cRequest -- INIZIO --\n\r%s\n\rcRequest -- FINE --\n\r", cRequest )

            PRIVATE _SERVER := HB_IHASH(), _GET := HB_IHASH(), _POST := HB_IHASH(), _COOKIE := HB_IHASH(), _SESSION := HB_IHASH()
            PRIVATE _REQUEST := HB_IHASH(), _HTTP_REQUEST := HB_IHASH(), m_cPost
            t_cResult     := ""
            t_aHeader     := {}
            t_nStatusCode := 200
            t_cErrorMsg   := ""

            defineServerAdresses( hSocket )

            IF ParseRequest( cRequest )
               //hb_ToOutDebug( "_SERVER = %s,\n\r _GET = %s,\n\r _POST = %s,\n\r _REQUEST = %s,\n\r _HTTP_REQUEST = %s\n\r", hb_ValToExp( _SERVER ), hb_ValToExp( _GET ), hb_ValToExp( _POST ), hb_ValToExp( _REQUEST ), hb_ValToExp( _HTTP_REQUEST ) )
               define_Env( _SERVER )
               cSend := uproc_default()
            ELSE
               uhttpd_SetStatusCode( 400 )
               cSend := MakeResponse()
            ENDIF

            //hb_ToOutDebug( "cSend = %s\n\r", cSend )

            sendReply( hSocket, cSend )

            WriteToLog( cRequest )

         ENDIF

#ifdef USE_HB_INET
         hb_InetClose( hSocket )
#else
         socket_shutdown( hSocket )
         socket_close( hSocket )
#endif
      END SEQUENCE

      nParseTime := hb_milliseconds() - nMsecs
      WriteToConsole( "Page served in : " + Str( nParseTime/1000, 7, 4 ) + " seconds" )

      IF hb_mutexLock( s_hmtxBusy )
         s_nConnections--
         hb_mutexUnlock( s_hmtxBusy )
      ENDIF

   ENDDO

   WriteToConsole( "Quitting ProcessConnections() " + hb_CStr( nThreadId ) )

   IF hb_mutexLock( s_hmtxBusy )
      s_nThreads--
      IF ( nPos := aScan( s_aRunningThreads, {|h| h[2] == nThreadId } ) > 0 )
         hb_aDel( s_aRunningThreads, nPos, TRUE )
      ENDIF
      hb_mutexUnlock( s_hmtxBusy )
   ENDIF

   RETURN 0

STATIC FUNCTION ServiceConnection( /*@*/ nThreadId )
   LOCAL hSocket, nLen, cRequest, cSend
   LOCAL nMsecs, nParseTime, nPos
   LOCAL nError := 500013

   ErrorBlock( { | oError | uhttpd_DefError( oError ) } )

   nThreadId := hb_threadID()
   //nThreadIdRef := nThreadId

   WriteToConsole( "Starting ServiceConnections() " + hb_CStr( nThreadId ) )

   IF hb_mutexLock( s_hmtxBusy )
      s_nServiceThreads++
      hb_mutexUnlock( s_hmtxBusy )
   ENDIF

   DO WHILE .T.

      // Reset socket
      hSocket := NIL

      // releasing resources
      WIN_SYSREFRESH( 1 )

      // Waiting a connection from AcceptConnections() but up to defined time
      hb_mutexSubscribe( s_hmtxServiceThreads, THREAD_MAX_WAIT, @hSocket )

      // received a -1 value, I have to quit
      IF HB_ISNUMERIC( hSocket )
         EXIT
      // no socket received, thread can graceful quit only if over minimal number
      ELSEIF hSocket == NIL
         IF hb_mutexLock( s_hmtxBusy )
            IF s_nServiceThreads <= s_nStartServiceThreads
               hb_mutexUnlock( s_hmtxBusy )
               LOOP
            ENDIF
            hb_mutexUnlock( s_hmtxBusy )
         ENDIF
         EXIT
      ENDIF

      // Connection accepted
      IF hb_mutexLock( s_hmtxBusy )
         s_nServiceConnections++
         s_nTotServiceConnections++
         s_nMaxServiceConnections := Max( s_nServiceConnections, s_nMaxServiceConnections )
         hb_mutexUnlock( s_hmtxBusy )
      ENDIF

      // Save initial time
      nMsecs := hb_milliseconds()

      BEGIN SEQUENCE

         /* receive query */
         nLen := readRequest( hSocket, @cRequest )

         IF nLen == -1
#ifdef USE_HB_INET
            ? "recv() error:", hb_InetErrorCode( hSocket ), hb_InetErrorDesc( hSocket )
#else
            ? "recv() error:", socket_error()
#endif
         ELSEIF nLen == 0 /* connection closed */
         ELSE

            //hb_ToOutDebug( "cRequest -- INIZIO --\n\r%s\n\rcRequest -- FINE --\n\r", cRequest )

            PRIVATE _SERVER := HB_IHASH(), _GET := HB_IHASH(), _POST := HB_IHASH(), _COOKIE := HB_IHASH(), _SESSION := HB_IHASH()
            PRIVATE _REQUEST := HB_IHASH(), _HTTP_REQUEST := HB_IHASH(), m_cPost
            t_cResult     := ""
            t_aHeader     := {}
            t_nStatusCode := 200
            t_cErrorMsg   := ""

            defineServerAdresses( hSocket )

            IF ParseRequest( cRequest )
               //hb_ToOutDebug( "_SERVER = %s,\n\r _GET = %s,\n\r _POST = %s,\n\r _REQUEST = %s,\n\r _HTTP_REQUEST = %s\n\r", hb_ValToExp( _SERVER ), hb_ValToExp( _GET ), hb_ValToExp( _POST ), hb_ValToExp( _REQUEST ), hb_ValToExp( _HTTP_REQUEST ) )
               define_Env( _SERVER )
            ENDIF
            // Error page served
            uhttpd_SetStatusCode( nError )
            cSend := MakeResponse()

            sendReply( hSocket, cSend )

            WriteToLog( cRequest )

         ENDIF
#ifdef USE_HB_INET
         hb_InetClose( hSocket )
#else
         socket_shutdown( hSocket )
         socket_close( hSocket )
#endif
      END SEQUENCE

      nParseTime := hb_milliseconds() - nMsecs
      WriteToConsole( "Page served in : " + Str( nParseTime/1000, 7, 4 ) + " seconds" )

      IF hb_mutexLock( s_hmtxBusy )
         s_nServiceConnections--
         hb_mutexUnlock( s_hmtxBusy )
      ENDIF

   ENDDO

   WriteToConsole( "Quitting ServiceConnections() " + hb_CStr( nThreadId ) )

   IF hb_mutexLock( s_hmtxBusy )
      s_nServiceThreads--
      IF ( nPos := aScan( s_aServiceThreads, {|h| h[2] == nThreadId } ) > 0 )
         hb_aDel( s_aServiceThreads, nPos, TRUE )
      ENDIF
      hb_mutexUnlock( s_hmtxBusy )
   ENDIF

   RETURN 0

STATIC FUNCTION ParseRequest( cRequest )
   LOCAL aRequest, aLine, nI, nJ, cI
   LOCAL cReq, aVal, cFields, hVars

   // RFC2616
   aRequest := uhttpd_split( CR_LF, cRequest )

   //hb_toOutDebug( "aRequest = %s\n\r", hb_ValToExp( aRequest ) )

   WriteToConsole( aRequest[1] )
   aLine := uhttpd_split( " ", aRequest[1] )
   IF LEN( aLine ) != 3 .OR. ;
      ( aLine[1] != "GET" .AND. aLine[1] != "POST" ) .OR. ; // Sorry, we support GET and POST only
      LEFT( aLine[3], 5 ) != "HTTP/"
      RETURN .F.
   ENDIF

   // define _SERVER var
   _SERVER[ "REQUEST_METHOD"  ] := aLine[1]
   _SERVER[ "REQUEST_URI"     ] := aLine[2]
   _SERVER[ "SERVER_PROTOCOL" ] := aLine[3]

   IF ( nI := AT( "?", _SERVER["REQUEST_URI"] ) ) > 0
      _SERVER[ "SCRIPT_NAME"  ] := LEFT( _SERVER["REQUEST_URI"], nI - 1)
      _SERVER[ "QUERY_STRING" ] := SUBSTR( _SERVER["REQUEST_URI"], nI + 1)
   ELSE
      _SERVER[ "SCRIPT_NAME"  ] := _SERVER["REQUEST_URI"]
      _SERVER[ "QUERY_STRING" ] := ""
   ENDIF

   _SERVER[ "HTTP_ACCEPT"          ] := ""
   _SERVER[ "HTTP_ACCEPT_CHARSET"  ] := ""
   _SERVER[ "HTTP_ACCEPT_ENCODING" ] := ""
   _SERVER[ "HTTP_ACCEPT_LANGUAGE" ] := ""
   _SERVER[ "HTTP_CONNECTION"      ] := ""
   _SERVER[ "HTTP_HOST"            ] := ""
   _SERVER[ "HTTP_KEEP_ALIVE"      ] := ""
   _SERVER[ "HTTP_REFERER"         ] := ""
   _SERVER[ "HTTP_USER_AGENT"      ] := ""
   _SERVER[ "HTTP_CACHE_CONTROL"   ] := ""
   _SERVER[ "HTTP_COOKIE"          ] := ""

   FOR nI := 2 TO LEN( aRequest )
      IF aRequest[nI] == "";  EXIT
      ELSEIF ( nJ := AT( ":", aRequest[nI] ) ) > 0
         cI := LTRIM( SUBSTR( aRequest[nI], nJ + 1))
         SWITCH UPPER( LEFT( aRequest[nI], nJ - 1))
           CASE "ACCEPT"
           CASE "ACCEPT-CHARSET"
           CASE "ACCEPT-ENCODING"
           CASE "ACCEPT-LANGUAGE"
           CASE "CACHE-CONTROL"
           CASE "CONNECTION"
           CASE "COOKIE"
           CASE "KEEP-ALIVE"
           CASE "REFERER"
           CASE "USER-AGENT"
             _SERVER[ "HTTP_" + STRTRAN( UPPER( LEFT( aRequest[nI], nJ - 1 ) ), "-", "_" ) ] := cI
             EXIT
           CASE "HOST"
             aVal := uhttpd_split( ":", aRequest[ nI ] )
             _SERVER[ "HTTP_" + STRTRAN( UPPER( aVal[ 1 ] ), "-", "_")] := AllTrim( aVal[ 2 ] )
             EXIT
           CASE "CONTENT-TYPE"
           CASE "CONTENT-LENGTH"
             _SERVER[ STRTRAN( UPPER( LEFT( aRequest[ nI ], nJ - 1 ) ), "-", "_" ) ] := cI
             EXIT
        ENDSWITCH
      ENDIF
   NEXT

   // Load _HTTP_REQUEST
   FOR EACH cReq IN aRequest
       IF cReq:__enumIndex() == 1 // GET request
          hb_HSet( _HTTP_REQUEST, "HTTP Request", cReq )
       ELSEIF Empty( cReq )
          EXIT
       ELSE
          aVal := uhttpd_split( ":", cReq, 1 )
          hb_HSet( _HTTP_REQUEST, aVal[ 1 ], IIF( Len( aVal ) == 2, AllTrim( aVal[ 2 ] ), NIL ) )
       ENDIF
   NEXT

   //hb_toOutDebug( "_HTTP_REQUEST: aRequest = %s, _HTTP_REQUEST = %s\n\r", hb_ValToExp( aRequest ), hb_ValToExp( _HTTP_REQUEST ) )

   // GET
   cFields := _SERVER[ "QUERY_STRING" ]
   IF !Empty( cFields )
      hVars := uhttpd_GetVars( cFields )
      hb_HMerge( _GET, hVars )
      hb_HMerge( _REQUEST, hVars )
   ENDIF

   //hb_toOutDebug( "GET: cFields = %s, hVars = %s, _GET = %s, _REQUEST = %s\n\r", cFields, hb_ValToExp( hVars ), hb_ValToExp( _GET ), hb_ValToExp( _REQUEST ) )

   // POST
   IF "POST" $ Upper( _SERVER[ 'REQUEST_METHOD' ] )
      cFields := aTail( aRequest )
      IF !Empty( cFields )
         hVars := uhttpd_GetVars( cFields )
         hb_HMerge( _POST, hVars )
         hb_HMerge( _REQUEST, hVars )
      ENDIF
      m_cPost := cFields  // TOFIX: Who needs this ?
   ENDIF

   //hb_toOutDebug( "POST: cFields = %s, hVars = %s, _POST = %s, _REQUEST = %s\n\r", cFields, hb_ValToExp( hVars ), hb_ValToExp( _POST ), hb_ValToExp( _REQUEST ) )

   // COOKIES
   cFields := _SERVER[ 'HTTP_COOKIE' ]
   IF !Empty( cFields )
      hVars := uhttpd_GetVars( cFields, ";" )
      hb_HMerge( _COOKIE, hVars )
      hb_HMerge( _REQUEST, hVars )
   ENDIF
   //hb_toOutDebug( "COOKIE: cFields = %s, hVars = %s, _COOKIE = %s, _REQUEST = %s\n\r", cFields, hb_ValToExp( hVars ), hb_ValToExp( _COOKIE ), hb_ValToExp( _REQUEST ) )


   // Complete _SERVER
   _SERVER[ "SERVER_NAME"       ] := uhttpd_split( ":", _HTTP_REQUEST[ "HOST" ], 1 )[ 1 ]
   _SERVER[ "SERVER_SOFTWARE"   ] := APP_NAME + " " + APP_VERSION + " (" + OS() + ")"
   _SERVER[ "SERVER_SIGNATURE"  ] := "<address>" + _SERVER[ "SERVER_SOFTWARE" ] + " Server at " + _SERVER[ "SERVER_NAME" ] + " Port " + _SERVER[ "SERVER_PORT" ] + "</address>"
   _SERVER[ "DOCUMENT_ROOT"     ] := s_cDocumentRoot
   _SERVER[ "SERVER_ADMIN"      ] := "root@localhost"   // TOFIX: put real user
   _SERVER[ "SCRIPT_FILENAME"   ] := STRTRAN( STRTRAN( _SERVER[ "DOCUMENT_ROOT" ] + _SERVER[ "SCRIPT_NAME" ], "//", "/" ), "\", "/" )
   _SERVER[ "GATEWAY_INTERFACE" ] := "CGI/1.1"
   _SERVER[ "SCRIPT_URL"        ] := _SERVER["SCRIPT_NAME"]
   _SERVER[ "SCRIPT_URI"        ] := "http://" + _HTTP_REQUEST[ "HOST" ] + _SERVER["SCRIPT_NAME"]

   //hb_ToOutDebug( "_SERVER = %s\n\r", hb_ValToExp( _SERVER ) )
   //hb_ToOutDebug( "_GET = %s\n\r", hb_ValToExp( _GET ) )
   //hb_ToOutDebug( "_POST = %s\n\r", hb_ValToExp( _POST ) )
   //hb_ToOutDebug( "_COOKIE = %s\n\r", hb_ValToExp( _COOKIE ) )
   //hb_ToOutDebug( "_HTTP_REQUEST = %s\n\r", hb_ValToExp( _HTTP_REQUEST ) )

   // After defined all SERVER vars we can define a session
   // SESSION - sessions ID is stored as a cookie value, normally as SESSIONID var name (this can be user defined)
   t_oSession := uhttpd_SessionNew( "SESSION", s_cSessionPath )
   t_oSession:Start()


   RETURN .T.


STATIC FUNCTION MakeResponse()
   LOCAL cRet, cReturnCode

   uhttpd_AddHeader( "Connection", "close" )

   IF uhttpd_GetHeader( "Location" ) != NIL
      t_nStatusCode := 301
   ENDIF
   IF uhttpd_GetHeader( "Content-Type" ) == NIL
      uhttpd_AddHeader( "Content-Type", "text/html" )
   ENDIF

   cRet := "HTTP/1.1 "
   cReturnCode := DecodeStatusCode()

   SWITCH t_nStatusCode
     CASE 200
          EXIT

     CASE 301
     CASE 400
     CASE 401
     CASE 402
     CASE 403
     CASE 404
     CASE 503
          t_cResult := "<html><body><h1>" + cReturnCode + "</h1></body></html>"
          EXIT

     // extended error messages - from Microsoft IIS Server
     CASE 500013 // error: 500-13 Server too busy
          uhttpd_AddHeader( "Retry-After", "60" )  // retry after 60 seconds
          t_cResult := "<html><body><h1>500 Server Too Busy</h1></body></html>"
          EXIT

     CASE 500100 // error: 500-100 Undeclared Variable

     OTHERWISE
          cReturnCode := "403 Forbidden"
          t_cResult := "<html><body><h1>" + cReturnCode + "</h1></body></html>"
   ENDSWITCH

   //hb_ToOutDebug( "_SESSION = %s\n\r", hb_ValToExp( _SESSION ) )

   // Close session - Autodestructor will NOT close it, because t_oSession is destroyed only at end of Thread
   t_oSession:Close()
   // t_oSession := NIL

   WriteToConsole( cReturnCode )
   cRet += cReturnCode + CR_LF
   AEVAL( t_aHeader, {|x| cRet += x[1] + ": " + x[2] + CR_LF } )
   cRet += CR_LF
   cRet += t_cResult
   RETURN cRet

STATIC FUNCTION DecodeStatusCode()
   LOCAL cReturnCode

   SWITCH t_nStatusCode
     CASE 200
          cReturnCode := "200 OK"
          EXIT
     CASE 301
          cReturnCode := "301 Moved Permanently"
          EXIT
     CASE 400
          cReturnCode := "400 Bad Request"
          EXIT
     CASE 401
          cReturnCode := "401 Unauthorized"
          EXIT
     CASE 402
          cReturnCode := "402 Payment Required"
          EXIT
     CASE 403
          cReturnCode := "403 Forbidden"
          EXIT
     CASE 404
          cReturnCode := "404 Not Found"
          EXIT
     CASE 503
          cReturnCode := "503 Service Unavailable"
          EXIT

     // extended error messages - from Microsoft IIS Server
     CASE 500013 // error: 500-13 Server too busy
          cReturnCode := "500-13 Server Too Busy"
          EXIT

     CASE 500100 // error: 500-100 Undeclared Variable

     OTHERWISE
          cReturnCode := "403 Forbidden"
   ENDSWITCH

   RETURN cReturnCode

STATIC PROCEDURE WriteToLog( cRequest )
   LOCAL cTime, cDate
   LOCAL aDays   := { "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" }
   LOCAL aMonths := {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}
   LOCAL cAccess, cError, nDoW, dDate, nDay, nMonth, nYear, nSize, cBias
   LOCAL cErrorMsg
   LOCAL cReferer

   IF hb_mutexLock( s_hmtxLog )

      //hb_ToOutDebug( "TIP_TimeStamp() = %s \n\r", TIP_TIMESTAMP() )

      cTime    := TIME()
      dDate    := Date()
      cDate    := DTOS( dDate )
      nSize    := LEN( t_cResult )
      cReferer := _SERVER[ "HTTP_REFERER" ]
      cBias    := WIN_TIMEZONEBIAS()

      cAccess := _SERVER[ "REMOTE_ADDR" ] + " - - [" + RIGHT( cDate, 2 ) + "/" + ;
                       aMonths[ VAL( SUBSTR( cDate, 5, 2 ) ) ] + ;
                       "/" + LEFT( cDate, 4 ) + ":" + cTime + ' ' + cBias + '] "' + ;
                       LEFT( cRequest, AT( CR_LF, cRequest ) - 1 ) + '" ' + ;
                       LTRIM( STR( t_nStatusCode ) ) + " " + IIF( nSize == 0, "-", LTRIM( STR( nSize ) ) ) + ;
                       ' "' + IIF( Empty( cReferer ), "-", cReferer ) + '" "' + _SERVER[ "HTTP_USER_AGENT" ] + ;
                       '"' + HB_OSNewLine()

      //hb_ToOutDebug( "AccessLog = %s \n\r", cAccess )

      FWRITE( s_hfileLogAccess, cAccess )

      IF !( t_nStatusCode == 200 ) // ok

         nDoW   := Dow( dDate )
         nDay   := Day( dDate )
         nMonth := Month( dDate )
         nYear  := Year( dDate )
         cErrorMsg := t_cErrorMsg

         cError := "[" + Left( aDays[ nDoW ], 3 ) + " " + aMonths[ nMonth ] + " " + StrZero( nDay, 2 ) + " " + ;
                   PadL( LTrim( cTime ), 8, "0" ) + " " + StrZero( nYear, 4 ) + "] [error] [client " + _SERVER[ "REMOTE_ADDR" ] + "] " + ;
                   cErrorMsg + HB_OSNewLine()

         //hb_ToOutDebug( "ErrorLog = %s \n\r", cError )

         FWRITE( s_hfileLogError, cError )
      ENDIF

      hb_mutexUnlock( s_hmtxLog )
   ENDIF

   RETURN

STATIC FUNCTION CGIExec( cProc, /*@*/ cOutPut )
   LOCAL hOut
   LOCAL cData, nLen
   LOCAL nErrorLevel
   LOCAL pThread

   IF HB_ISSTRING( cProc )

      //hb_toOutDebug( "Launching process: %s\n\r", cProc )
      // No hIn, hErr == hOut
      t_hProc := hb_processOpen( cProc, , @hOut, @hOut, .T. ) // .T. = Detached Process (Hide Window)

      IF t_hProc > -1
         //hb_toOutDebug( "Process handler: %s\n\r", t_hProc )
         //hb_toOutDebug( "Error: %s\n\r", FError() )

         pThread := hb_threadStart( @CGIKill() )

         hb_mutexNotify( s_hmtxCGIKill, .T. )

         // Nothing sent to process
         //hb_toOutDebug( "Sending: %s\n\r", cSend )
         //FWrite( hIn, cSend )

         //hb_toOutDebug( "Reading output\n\r" )

         cData := Space( 1000 )
         cOutPut := ""
         DO WHILE ( nLen := Fread( hOut, @cData, Len( cData ) ) ) > 0
            cOutPut += SubStr( cData, 1, nLen )
            cData := Space( 1000 )
         ENDDO

         //? "Reading errors"
         //hb_toOutDebug( "Reading errors\n\r" )
         //cData := Space( 1000 )
         //nLen  := Fread( hErr, @cData, 1000 )
         //hb_toOutDebug( "Received: %i bytes\n\r", nLen )
         //IF nLen >0 .and. nLen < 200
         //   hb_toOutDebug( "Dumping them: %s\n\r", SubStr( cData, 1, nLen ) )
         //ENDIF

         //? "Waiting for process termination"
         // Return value
         nErrorLevel := HB_ProcessValue( t_hProc )

         // Notify to CGIKill to terminate
         hb_mutexNotify( s_hmtxCGIKill, .F. )
         hb_threadJoin( pThread )

         FClose( t_hProc )
         FClose( hOut )

      ENDIF

   ELSE

      nErrorLevel := -1 // Error: cProc is not a valid string

   ENDIF

   RETURN nErrorLevel

STATIC PROCEDURE CGIKill()
   LOCAL lWait
   LOCAL nStartTime := hb_milliseconds()

   // Kill process after MAX_PROCESS_EXEC_TIME
   DO WHILE .T.

      hb_mutexSubscribe( s_hmtxCGIKill, 10, @lWait ) // 10 seconds

      IF HB_ISLOGICAL( lWait )
         IF lWait
            nStartTime := hb_milliseconds()
         ELSE
            EXIT
         ENDIF
      ENDIF

      IF ( hb_milliseconds() - nStartTime ) > CGI_MAX_EXEC_TIME
         // Killing process if still exists
         IF t_hProc != NIL
            HB_ProcessClose( t_hProc )
         ENDIF
         EXIT
      ENDIF
   ENDDO

   RETURN

INIT PROCEDURE SocketInit()
#ifdef USE_HB_INET
   hb_InetInit()
#else
   IF socket_init() != 0
      ? "socket_init() error"
   ENDIF
#endif
   RETURN


EXIT PROCEDURE Socketxit()
#ifdef USE_HB_INET
   hb_InetCleanup()
#else
   socket_exit()
#endif
   RETURN


/********************************************************************
  Public helper functions
********************************************************************/

FUNCTION uhttpd_OSFileName( cFileName )
   IF HB_OSPathSeparator() != "/"
      RETURN STRTRAN( cFileName, "/", HB_OSPathSeparator() )
   ENDIF
   RETURN cFileName

PROCEDURE uhttpd_SetStatusCode(nStatusCode)
   t_nStatusCode := nStatusCode
   RETURN


PROCEDURE uhttpd_AddHeader( cType, cValue, lReplace )
   LOCAL nI
   DEFAULT lReplace TO TRUE // Needed from SetCookie()

   IF lReplace .AND. ( nI := ASCAN( t_aHeader, {|x| UPPER( x[ 1 ] ) == UPPER( cType ) } ) ) > 0
      t_aHeader[ nI, 2 ] := cValue
   ELSE
      AADD( t_aHeader, { cType, cValue } )
   ENDIF
   RETURN

FUNCTION uhttpd_GetHeader( cType, /*@*/ nPos )
   DEFAULT nPos TO 1
   IF ( nPos := ASCAN( t_aHeader, {|x| UPPER( x[ 1 ] ) == UPPER( cType ) }, nPos ) ) > 0
      RETURN t_aHeader[ nPos, 2 ]
   ENDIF
   RETURN NIL

PROCEDURE uhttpd_DelHeader( cType )
   LOCAL nI

   IF ( nI := ASCAN( t_aHeader, {|x| UPPER( x[ 1 ] ) == UPPER( cType ) } ) ) > 0
      hb_aDel( t_aHeader, nI, TRUE )
   ENDIF
   RETURN

PROCEDURE uhttpd_Write( cString )
   t_cResult += cString
   RETURN

/********************************************************************
  Internal helper functions
********************************************************************/

STATIC FUNCTION readRequest( hSocket, /* @ */ cRequest )
   LOCAL nLen, cBuf
#ifdef USE_HB_INET
   LOCAL nRcvLen, nContLen
#endif

   /* receive query */
#ifdef USE_HB_INET
   cRequest := ""
   nLen     := 0
   nRcvLen  := 1
   nContLen := 0
   DO WHILE /* AT( CR_LF + CR_LF, cRequest ) == 0 .AND. */ nRcvLen > 0
      cBuf := hb_InetRecvLine( hSocket, @nRcvLen )
      //hb_ToOutDebug( " nRcvLen = %i, cBuf = %s \n\r", nRcvLen, cBuf )
      cRequest += cBuf + CR_LF
      nLen += nRcvLen
      IF nRcvLen > 0 .AND. At( "CONTENT-LENGTH:", Upper( cBuf ) ) == 1
         cBuf := Substr( cBuf, At( ":", cBuf ) + 1 )
         nContLen := Val( cBuf )
      ENDIF
   ENDDO

   //hb_ToOutDebug( " nLen = %i, nContLen = %i \n\r", nLen, nContLen )
   // if the request has a content-lenght, we must read it
   IF nLen > 0 .AND. nContLen > 0
      // cPostData is autoAllocated
      cBuf := Space( nContLen )
      IF InetRecvAll( hSocket, @cBuf, nContLen ) <= 0
         nLen := -1 // force error check
      ELSE
         cRequest += cBuf
      ENDIF
   ENDIF
#else
   cRequest := ""
   nLen     := 1
   DO WHILE AT( CR_LF + CR_LF, cRequest ) == 0 .AND. nLen > 0
      nLen := socket_recv( hSocket, @cBuf )
      cRequest += cBuf
   ENDDO
#endif

   //hb_ToOutDebug( " nLen = %i, cRequese = %s \n\r", nLen, cRequest )

   RETURN nLen

STATIC FUNCTION sendReply( hSocket, cSend )
   LOCAL nError := 0
   LOCAL nLen

#ifdef USE_HB_INET
   DO WHILE LEN( cSend ) > 0
      IF ( nLen := hb_InetSendAll( hSocket, cSend ) ) == -1
         ? "send() error:", hb_InetErrorCode( hSocket ), HB_InetErrorDesc( hSocket )
         WriteToConsole( hb_StrFormat( "ProcessConnection() - send() error: %s, cSend = %s, hSocket = %s", hb_InetErrorDesc( hSocket ), cSend, hSocket ) )
         EXIT
      ELSEIF nLen > 0
         cSend := SUBSTR( cSend, nLen + 1 )
      ENDIF
   ENDDO
#else
   DO WHILE LEN( cSend ) > 0
      IF ( nLen := socket_send( hSocket, cSend ) ) == -1
         ? "send() error:", socket_error()
         WriteToConsole( hb_StrFormat( "ServiceConnection() - send() error: %s, cSend = %s, hSocket = %s", socket_error(), cSend, hSocket ) )
         EXIT
      ELSEIF nLen > 0
         cSend := SUBSTR( cSend, nLen + 1 )
      ENDIF
   ENDDO
#endif

   RETURN nError

STATIC PROCEDURE defineServerAdresses( hSocket )
#ifndef USE_HB_INET
   LOCAL aI
#endif
#ifdef USE_HB_INET
   _SERVER[ "REMOTE_ADDR" ] := hb_InetAddress( hSocket )
   _SERVER[ "REMOTE_HOST" ] := _SERVER[ "REMOTE_ADDR" ]  // no reverse DNS
   _SERVER[ "REMOTE_PORT" ] := hb_InetPort( hSocket )

   _SERVER[ "SERVER_ADDR" ] := s_cLocalAddress
   _SERVER[ "SERVER_PORT" ] := LTrim( Str( s_nLocalPort ) )
#else
   IF socket_getpeername( hSocket, @aI ) != -1
      _SERVER[ "REMOTE_ADDR" ] := aI[2]
      _SERVER[ "REMOTE_HOST" ] := _SERVER[ "REMOTE_ADDR" ]  // no reverse DNS
      _SERVER[ "REMOTE_PORT" ] := aI[3]
   ENDIF

   IF socket_getsockname( hSocket, @aI ) != -1
      _SERVER[ "SERVER_ADDR" ] := aI[2]
      _SERVER[ "SERVER_PORT" ] := LTrim( Str( aI[3] ) )
   ENDIF
#endif

   RETURN

FUNCTION uhttpd_split( cSeparator, cString, nMax )
   LOCAL aRet := {}, nI
   LOCAL nIter := 0

   DEFAULT nMax TO 0

   DO WHILE ( nI := AT( cSeparator, cString ) ) > 0
      AADD( aRet, LEFT( cString, nI - 1 ) )
      cString := SUBSTR( cString, nI + LEN( cSeparator ) )
      IF nMax > 0 .AND. ++nIter >= nMax
         EXIT
      ENDIF
   ENDDO
   AADD( aRet, cString )
   RETURN aRet

FUNCTION uhttpd_join( cSeparator, aData )
   LOCAL cRet := "", nI

   FOR nI := 1 TO LEN( aData )
       IF nI > 1;  cRet += cSeparator
       ENDIF
       IF     VALTYPE(aData[nI]) $ "CM";  cRet += aData[nI]
       ELSEIF VALTYPE(aData[nI]) == "N";  cRet += LTRIM(STR(aData[nI]))
       ELSEIF VALTYPE(aData[nI]) == "D";  cRet += IF(!EMPTY(aData[nI]), DTOC(aData[nI]), "")
       ELSE
       ENDIF
   NEXT
   RETURN cRet

STATIC FUNCTION uproc_default()
   LOCAL cFileName, nI, cI
   LOCAL cExt, xResult, pHRB, oError, cHRBBody

   //cFileName := STRTRAN(cRoot + _SERVER["SCRIPT_NAME"], "//", "/")
   cFileName := _SERVER[ "SCRIPT_FILENAME" ]

   //hb_ToOutDebug( "cFileName = %s, uhttpd_OSFileName( cFileName ) = %s,\n\r _SERVER = %s\n\r", cFileName, uhttpd_OSFileName( cFileName ), hb_ValToExp( _SERVER ) )

   IF HB_HHasKey( s_hScriptAliases, _SERVER[ "SCRIPT_NAME" ] )
      cFileName := _SERVER[ "DOCUMENT_ROOT" ] +  hb_hGet( s_hScriptAliases, _SERVER[ "SCRIPT_NAME" ] )
   ENDIF

   // Security
   IF ".." $ cFileName
      uhttpd_SetStatusCode( 403 )
      t_cErrorMsg := "Characters not allowed"
      RETURN MakeResponse()
   ENDIF

   //hb_toOutDebug( "cFileName = %s, uhttpd_OSFileName( cFileName ) = %s,\n\r s_hScriptAliases = %s\n\r", cFileName, uhttpd_OSFileName( cFileName ), hb_ValToExp( s_hScriptAliases ) )

   IF Upper( _SERVER[ "SCRIPT_NAME" ] ) == "/SERVERSTATUS"
      ShowServerStatus()
   ELSEIF HB_FileExists( uhttpd_OSFileName( cFileName ) )
      IF ( nI := RAT( ".", cFileName ) ) > 0
         SWITCH ( cExt := LOWER( SUBSTR( cFileName, nI + 1 ) ) )
            CASE "hrb" ;                                 cI := "text/html";                EXIT
            CASE "exe" ;                                 cI := "text/html";                EXIT

            CASE "css" ;                                 cI := "text/css";                 EXIT
            CASE "htm" ;   CASE "html";                  cI := "text/html";                EXIT
            CASE "txt" ;   CASE "text";  CASE "asc"
            CASE "c"   ;   CASE "h";     CASE "cpp"
            CASE "hpp" ;   CASE "log";                   cI := "text/plain";               EXIT
            CASE "rtf" ;                                 cI := "text/rtf";                 EXIT
            CASE "xml" ;                                 cI := "text/xml";                 EXIT
            CASE "xsl" ;                                 cI := "text/xsl";                 EXIT
            CASE "bmp" ;                                 cI := "image/bmp";                EXIT
            CASE "gif" ;                                 cI := "image/gif";                EXIT
            CASE "jpg" ;   CASE "jpe";   CASE "jpeg";    cI := "image/jpeg";               EXIT
            CASE "png" ;                                 cI := "image/png";                EXIT
            CASE "tif" ;   CASE "tiff";                  cI := "image/tiff";               EXIT
            CASE "djv" ;   CASE "djvu";                  cI := "image/vnd.djvu";           EXIT
            CASE "ico" ;                                 cI := "image/x-icon";             EXIT
            CASE "xls" ;                                 cI := "application/excel";        EXIT
            CASE "doc" ;                                 cI := "application/msword";       EXIT
            CASE "pdf" ;                                 cI := "application/pdf";          EXIT
            CASE "ps"  ;   CASE "eps";                   cI := "application/postscript";   EXIT
            CASE "ppt" ;                                 cI := "application/powerpoint";   EXIT
            CASE "bz2" ;                                 cI := "application/x-bzip2";      EXIT
            CASE "gz"  ;                                 cI := "application/x-gzip";       EXIT
            CASE "tgz" ;                                 cI := "application/x-gtar";       EXIT
            CASE "js"  ;                                 cI := "application/x-javascript"; EXIT
            CASE "tar" ;                                 cI := "application/x-tar";        EXIT
            CASE "tex" ;                                 cI := "application/x-tex";        EXIT
            CASE "zip" ;                                 cI := "application/zip";          EXIT
            CASE "midi";                                 cI := "audio/midi";               EXIT
            CASE "mp3" ;                                 cI := "audio/mpeg";               EXIT
            CASE "wav" ;                                 cI := "audio/x-wav";              EXIT
            CASE "qt"  ;   CASE "mov";                   cI := "video/quicktime";          EXIT
            CASE "avi" ;                                 cI := "video/x-msvideo";          EXIT
            OTHERWISE
              cI := "application/octet-stream"
         ENDSWITCH

         IF cExt == "hrb"

            // Starting HRB module

            TRY
               // Lock HRB to avoid MT race conditions
               IF !HRB_ACTIVATE_CACHE
                  cHRBBody := HRB_LoadFromFile( uhttpd_OSFileName( cFileName ) )
               ENDIF
               IF hb_mutexLock( s_hmtxHRB )
                  IF HRB_ACTIVATE_CACHE
                     // caching modules
                     IF !hb_HHasKey( s_hHRBModules, cFileName )
                        hb_HSet( s_hHRBModules, cFileName, HRB_LoadFromFile( uhttpd_OSFileName( cFileName ) ) )
                     ENDIF
                     cHRBBody := s_hHRBModules[ cFileName ]
                  ENDIF
                  IF !EMPTY( pHRB := HB_HRBLOAD( cHRBBody ) )

                      xResult := HRBMAIN()

                      HB_HRBUNLOAD( pHRB )
                  ELSE
                      uhttpd_SetStatusCode( 404 )
                      t_cErrorMsg := "File does not exist: " + cFileName
                  ENDIF
                  hb_mutexUnlock( s_hmtxHRB )

               ENDIF

               IF HB_ISSTRING( xResult )
                  uhttpd_AddHeader( "Content-Type", cI )
                  uhttpd_Write( xResult )
               ELSE
                  // Application in HRB module is responsible to send HTML content
               ENDIF

            CATCH oError

               WriteToConsole( "Error!" )

               uhttpd_AddHeader( "Content-Type", "text/html" )
               uhttpd_Write( "Error" )
               uhttpd_Write( "<br>Description: " + hb_cStr( oError:Description ) )
               uhttpd_Write( "<br>Filename: "    + hb_cStr( oError:filename ) )
               uhttpd_Write( "<br>Operation: "   + hb_cStr( oError:operation ) )
               uhttpd_Write( "<br>OsCode: "      + hb_cStr( oError:osCode ) )
               uhttpd_Write( "<br>GenCode: "     + hb_cStr( oError:genCode ) )
               uhttpd_Write( "<br>SubCode: "     + hb_cStr( oError:subCode ) )
               uhttpd_Write( "<br>SubSystem: "   + hb_cStr( oError:subSystem ) )
               uhttpd_Write( "<br>Args: "        + hb_cStr( hb_ValToExp( oError:args ) ) )
               uhttpd_Write( "<br>ProcName: "    + hb_cStr( procname( 0 ) ) )
               uhttpd_Write( "<br>ProcLine: "    + hb_cStr( procline( 0 ) ) )
            END

         ELSEIF cExt == "exe"

            // Starting CGI application

            IF ( CGIExec( uhttpd_OSFileName(cFileName), @xResult ) ) == 0

               //uhttpd_AddHeader( "Content-Type", cI )
               //uhttpd_Write( xResult )
               RETURN "HTTP/1.1 200 OK " + CR_LF + xResult

            ELSE

               uhttpd_AddHeader( "Content-Type", cI )
               uhttpd_Write( "CGI Error" )

            ENDIF

         ELSE
            uhttpd_AddHeader( "Content-Type", cI )
            uhttpd_Write( HB_MEMOREAD( uhttpd_OSFileName( cFileName ) ) )
         ENDIF

      ELSE
         cI := "application/octet-stream"
         uhttpd_AddHeader( "Content-Type", cI )
         uhttpd_Write( HB_MEMOREAD( uhttpd_OSFileName( cFileName ) ) )
      ENDIF

   ELSEIF HB_DirExists( uhttpd_OSFileName( cFileName ) )
      IF RIGHT( cFileName, 1 ) != "/"
         uhttpd_AddHeader( "Location", "http://" + _SERVER[ "HTTP_HOST" ] + _SERVER[ "SCRIPT_NAME" ] + "/" )
         RETURN MakeResponse()
      ENDIF
      IF ASCAN( { "index.html", "index.htm" }, ;
                {|x| IIF( HB_FileExists( uhttpd_OSFileName( cFileName + X ) ), ( cFileName += X, .T. ), .F. ) } ) > 0
         uhttpd_AddHeader( "Content-Type", "text/html" )
         uhttpd_Write( HB_MEMOREAD( uhttpd_OSFileName( cFileName ) ) )
         RETURN MakeResponse()
      ENDIF

      // If I'm here it's means that I have no page, so, if it is defined, I will display content folder
      IF !s_lIndexes
         uhttpd_SetStatusCode( 403 )
         t_cErrorMsg := "Display file list not allowed"
         RETURN MakeResponse()
      ENDIF

      // ----------------------- display folder content -------------------------------------
      ShowFolder( cFileName )

   ELSE
      uhttpd_SetStatusCode( 404 )
      t_cErrorMsg := "File does not exist: " + cFileName
   ENDIF
   RETURN MakeResponse()

// Define environment SET variables - TODO: Actually only for windows, make multiplatform
STATIC PROCEDURE Define_Env( hmServer )
   LOCAL v

   FOR EACH v IN hmServer
       WIN_SETENV( v:__enumKey(), v:__enumValue() )
   NEXT

   RETURN

// ------------------------------- DEFAULT PAGES -----------------------------------

STATIC PROCEDURE ShowServerStatus()

   uhttpd_AddHeader( "Content-Type", "text/html" )
   uhttpd_Write( '<html><head>' )
   uhttpd_Write( '<META HTTP-EQUIV="Refresh" CONTENT="' + LTrim( Str( PAGE_STATUS_REFRESH ) ) + ';URL=/ServerStatus">' )
   uhttpd_Write( '<title>Server Status</title><body><h1>Server Status</h1><pre>')
   //uhttpd_Write( '<table border="0">')

   uhttpd_Write( 'SERVER: ' + _SERVER[ "SERVER_SOFTWARE" ] + " Server at " + _SERVER[ "SERVER_NAME" ] + " Port " + _SERVER[ "SERVER_PORT" ] )
   uhttpd_Write( '<br>' )
   IF hb_mutexLock( s_hmtxBusy )
      uhttpd_Write( '<br>Thread: ' + Str( s_nThreads ) )
      uhttpd_Write( '<br>Connections: ' + Str( s_nConnections ) )
      uhttpd_Write( '<br>Max Connections: ' + Str( s_nMaxConnections ) )
      uhttpd_Write( '<br>Total Connections: ' + Str( s_nTotConnections ) )
      uhttpd_Write( '<br>Running Thread: ' + hb_ValToExp( s_aRunningThreads ) )

      uhttpd_Write( '<br>Service Thread: ' + Str( s_nServiceThreads ) )
      uhttpd_Write( '<br>Service Connections: ' + Str( s_nServiceConnections ) )
      uhttpd_Write( '<br>Max Service Connections: ' + Str( s_nMaxServiceConnections ) )
      uhttpd_Write( '<br>Total Service Connections: ' + Str( s_nTotServiceConnections ) )
      uhttpd_Write( '<br>Service Thread: ' + hb_ValToExp( s_aServiceThreads ) )
      hb_mutexUnlock( s_hmtxBusy )
   ENDIF
   uhttpd_Write( '<br>Time: ' + Time() )

   //uhttpd_Write( '</table>')
   uhttpd_Write( "<hr></pre></body></html>" )

   RETURN

STATIC PROCEDURE ShowFolder( cDir )
   LOCAL aDir, aF
   LOCAL cParentDir, nPos

   uhttpd_AddHeader( "Content-Type", "text/html" )

   aDir := DIRECTORY( uhttpd_OSFileName( cDir ), "D" )
   IF HB_HHasKey( _GET, "s" )
      IF _GET[ "s" ] == "s"
         ASORT( aDir,,, {|X,Y| IIF( X[ 5 ] == "D", IIF( Y[ 5 ] == "D", X[ 1 ] < Y[ 1 ], .T. ), ;
                                    IIF( Y[ 5 ] == "D", .F., X[ 2 ] < Y[ 2 ] ) ) } )
      ELSEIF _GET[ "s" ] == "m"
         ASORT( aDir,,, {|X,Y| IIF( X[ 5 ] == "D", IIF( Y[ 5 ] == "D", X[ 1 ] < Y[ 1 ], .T.), ;
                                    IIF( Y[ 5 ] == "D", .F., DTOS( X[ 3 ] ) + X[ 4 ] < DTOS( Y[ 3 ] ) + Y[ 4 ] ) ) } )
      ELSE
         ASORT( aDir,,, {|X,Y| IIF( X[ 5 ] == "D", IIF( Y[ 5 ] == "D", X[ 1 ] < Y[ 1 ], .T. ), ;
                                    IIF( Y[ 5 ] == "D", .F., X[ 1 ] < Y[ 1 ] ) ) } )
      ENDIF
   ELSE
      ASORT( aDir,,, {|X,Y| IIF( X[ 5 ] == "D", IIF( Y[ 5 ] == "D", X[ 1 ] < Y[ 1 ], .T. ), ;
                                 IIF( Y[ 5 ] == "D", .F., X[ 1 ] < Y[ 1 ] ) ) } )
   ENDIF

   uhttpd_Write( '<html><body><h1>Index of ' + _SERVER[ "SCRIPT_NAME" ] + '</h1><pre>      ')
   uhttpd_Write( '<a href="?s=n">Name</a>                                                  ')
   uhttpd_Write( '<a href="?s=m">Modified</a>             ' )
   uhttpd_Write( '<a href="?s=s">Size</a>' + CR_LF + '<hr>' )

   // Adding Upper Directory
   nPos := RAT( "/", SUBSTR( cDir, 1, Len( cDir ) - 1 ) )
   cParentDir := SUBSTR( cDir, 1, nPos )
   cParentDir := SUBSTR( cParentDir, Len( _SERVER[ "DOCUMENT_ROOT" ] ) + 1 )

   //hb_ToOutDebug( "cDir = %s, nPos = %i, cParentDir = %s\n\r", cDir, nPos, cParentDir )

   IF !Empty( cParentDir )
      // Add parent directory
      hb_aIns( aDir, 1, { "<parent>", 0, "", "", "D" }, .T. )
   ENDIF

   FOR EACH aF IN aDir
       IF aF[ 1 ] == "<parent>"
          uhttpd_Write( '[DIR] <a href="' + cParentDir + '">..</a>' + ;
                  CR_LF )
       ELSEIF LEFT( aF[ 1 ], 1 ) == "."
       ELSEIF "D" $ aF[ 5 ]
          uhttpd_Write( '[DIR] <a href="' + aF[ 1 ] + '/">'+ aF[ 1 ] + '</a>' + SPACE( 50 - LEN( aF[ 1 ] ) ) + ;
                  DTOC( aF[ 3 ] ) + ' ' + aF[ 4 ] + CR_LF )
       ELSE
          uhttpd_Write( '      <a href="' + aF[ 1 ] + '">'+ aF[ 1 ] + '</a>' + SPACE( 50 - LEN( aF[ 1 ] ) ) + ;
                  DTOC( aF[ 3 ]) + ' ' + aF[ 4 ] + STR( aF[ 2 ], 12 ) + CR_LF )
       ENDIF
   NEXT
   uhttpd_Write( "<hr></pre></body></html>" )

   RETURN

// ------------------------------- Utility functions --------------------------------

// from Przemek's example, useful to use encrypted HRB module files
STATIC FUNCTION HRB_LoadFromFileEncrypted( cFile, cKey )
   LOCAL cHrbBody
   cHrbBody := hb_memoread( cFile )
   cHrbBody := sx_decrypt( cHrbBody, cKey )
   cHrbBody := hb_zuncompress( cHrbBody )
   RETURN cHrbBody

/*
// Reverse function to save is:
PROCEDURE HRB_SaveToFileEncrypted( cHrbBody, cKey, cEncFileName )
   LOCAL cFile
   IF !Empty( cHrbBody )
      cHrbBody := hb_zcompress( cHrbBody )
      cHrbBody := sx_encrypt( cHrbBody, cKey )
      hb_memowrit( cEncFileName, cHrbBody )
   ENDIF
   RETURN
*/

STATIC FUNCTION HRB_LoadFromFile( cFile )
   RETURN hb_memoread( cFile )

STATIC PROCEDURE Help()
   //LOCAL cPrg := hb_argv( 0 )
   //LOCAL nPos := RAt( "\", cPrg )
   //__OutDebug( hb_argv(0) )
   //IF nPos > 0
   //   cPrg := SubStr( cPrg, nPos + 1 )
   //ENDIF
   ?
   ? "(C) 2009 Francesco Saverio Giudice <info@fsgiudice.com>"
   ?
   ? APP_NAME + " - web server - v. " + APP_VERSION
   ? "Based on original work of Mindaugas Kavaliauskas <dbtopas@dbtopas.lt>"
   ?
   ? "Parameters: (all optionals)"
   ?
   ? "-p       | --port           webserver tcp port        (default: " + LTrim( Str( LISTEN_PORT ) ) + ")"
   ? "-c       | --config         Configuration file        (default: " + APP_NAME + ".ini)"
   ? "                            It is possibile to define file path"
   ? "-d       | --docroot        Document root directory   (default: <curdir>\home)"
   ? "-i       | --indexes        Allow directory view      (default: no)"
   ? "-s       | --stop           Stop webserver"
   ? "-ts      | --start-threads  Define starting threads   (default: " + LTrim( Str( START_RUNNING_THREADS ) ) + ")"
   ? "-tm      | --max-threads    Define max threads        (default: " + LTrim( Str( MAX_RUNNING_THREADS ) ) + ")"
   ? "-h | -?  | --help           This help message"
   ?
   WAIT
   RETURN

STATIC PROCEDURE SysSettings()
   SET SCOREBOARD OFF
   SET CENTURY     ON
   SET DATE   ITALIAN
   SET BELL       OFF
   SET DELETED     ON
   SET EXACT      OFF
   SET CONFIRM     ON
   SET ESCAPE      ON
   SET WRAP        ON
   SET EPOCH TO  2000
   //RDDSetDefault( "DBFCDX" )
   RETURN

STATIC FUNCTION Exe_Path()
   LOCAL cPath := hb_argv( 0 )
   LOCAL nPos  := RAt( "\", cPath )
   IF nPos == 0
      cPath := ""
   ELSE
      cPath := SubStr( cPath, 1, nPos-1 )
   ENDIF
   RETURN cPath

STATIC FUNCTION Exe_Name()
   LOCAL cPrg := hb_argv( 0 )
   LOCAL nPos := RAt( "\", cPrg )
   IF nPos > 0
      cPrg := SubStr( cPrg, nPos+1 )
   ENDIF
   RETURN cPrg

STATIC PROCEDURE Progress( /*@*/ nProgress )
   LOCAL cString := "["

   DO CASE
      CASE nProgress == 0
          cString += "-"
      CASE nProgress == 1
          cString += "\"
      CASE nProgress == 2
          cString += "|"
      CASE nProgress == 3
          cString += "/"
   ENDCASE

   cString += "]"

   nProgress++

   IF nProgress == 4
      nProgress := 0
   ENDIF

   // using hb_dispOutAt() to avoid MT screen updates problem
   hb_dispOutAt( 10,  5, cString )
   hb_dispOutAt(  0, 60, "Time: " + Time() )

   RETURN

// Show messages in console
#define CONSOLE_FIRSTROW   12
#define CONSOLE_LASTROW    MaxRow()
STATIC PROCEDURE WriteToConsole( ... )
   LOCAL cMsg

   IF hb_mutexLock( s_hmtxConsole )
      IF s_lConsole

         FOR EACH cMsg IN hb_aParams()

             hb_Scroll( CONSOLE_FIRSTROW, 0, CONSOLE_LASTROW, MaxCol(), -1 )
             hb_DispOutAt( CONSOLE_FIRSTROW, 0, PadR( "> " + hb_cStr( cMsg ), MaxCol() ) )

#ifdef DEBUG_ACTIVE
             hb_ToOutDebug( ">>> %s\n\r", cMsg )
#endif

         NEXT

      ENDIF
      hb_mutexUnlock( s_hmtxConsole )
   ENDIF

   RETURN

STATIC FUNCTION ParseIni( cConfig )
   LOCAL hIni := hb_IniRead( cConfig, TRUE ) // TRUE = load all keys in MixedCase, redundant as it is default, but to remember
   LOCAL cSection, hSect, cKey, xVal, cVal, nPos
   LOCAL hDefault

   //hb_ToOutDebug( "cConfig = %s,\n\rhIni = %s\n\r", cConfig, hb_ValToExp( hIni ) )

   // Define here what attributes we can have in ini config file and their defaults
   // Please add all keys in uppercase. hDefaults is Case Insensitive
   hDefault := hb_HSetCaseMatch( ;
   { ;
     "MAIN"     => { ;
                     "PORT"                 => LISTEN_PORT              ,;
                     "DOCUMENT_ROOT"        => EXE_Path() + "\home"     ,;
                     "SHOW_INDEXES"         => FALSE                    ,;
                     "SCRIPTALIASMIXEDCASE" => TRUE                     ,;
                     "SESSIONPATH"          => EXE_Path() + "\sessions"  ;
                   },;
     "LOGFILES" => { ;
                     "ACCESS"               => FILE_ACCESS_LOG          ,;
                     "ERROR"                => FILE_ERROR_LOG            ;
                   },;
     "THREADS"  => { ;
                     "MAX_WAIT"             => THREAD_MAX_WAIT          ,;
                     "START_NUM"            => START_RUNNING_THREADS    ,;
                     "MAX_NUM"              => MAX_RUNNING_THREADS       ;
                   },;
     "ALIASES"  => { => } ;
   }, FALSE )

   //hb_ToOutDebug( "hDefault = %s\n\r", hb_ValToExp( hDefault ) )

   // Now read changes from ini file and modify only admited keys
   IF !Empty( hIni )
      FOR EACH cSection IN hIni:Keys

          cSection := Upper( cSection )

          //hb_ToOutDebug( "cSection = %s\n\r", cSection )

          IF cSection $ hDefault

             hSect := hIni[ cSection ]

             //hb_ToOutDebug( "hSect = %s\n\r", hb_ValToExp( hSect ) )

             IF HB_IsHash( hSect )
                FOR EACH cKey IN hSect:Keys

                    // Please, below check values MUST be uppercase

                    //hb_ToOutDebug( "cKey = %s\n\r", cKey )

                    IF cSection == "ALIASES"
                       xVal := hSect[ cKey ]
                       IF xVal <> NIL
                          hDefault[ cSection ][ cKey ] := xVal
                       ENDIF

                    ELSEIF ( cKey := Upper( cKey ) ) $ hDefault[ cSection ]  // force cKey to be uppercase

                       IF ( nPos := hb_HScan( hSect, {|k| Upper( k ) == cKey } ) ) > 0
                          cVal := hb_HValueAt( hSect, nPos )

                          //hb_ToOutDebug( "cVal = %s\n\r", cVal )

                          DO CASE
                             CASE cSection == "MAIN"
                                  DO CASE
                                     CASE cKey == "PORT"
                                          xVal := Val( cVal )
                                     CASE cKey == "DOCUMENT_ROOT"
                                          IF !Empty( cVal )
                                             // Change APP_DIR macro with current exe path
                                             xVal := StrTran( cVal, "$(APP_DIR)", Exe_Path() )
                                          ENDIF
                                     CASE cKey == "SCRIPTALIASMIXEDCASE"
                                          xVal := cVal
                                     CASE cKey == "SESSIONPATH"
                                          IF !Empty( cVal )
                                             // Change APP_DIR macro with current exe path
                                             xVal := StrTran( cVal, "$(APP_DIR)", Exe_Path() )
                                          ENDIF
                                  ENDCASE
                             CASE cSection == "LOGFILES"
                                  DO CASE
                                     CASE cKey == "ACCESS"
                                          xVal := cVal
                                     CASE cKey == "ERROR"
                                          xVal := cVal
                                  ENDCASE
                             CASE cSection == "THREADS"
                                  DO CASE
                                     CASE cKey == "MAX_WAIT"
                                          xVal := Val( cVal )
                                     CASE cKey == "START_NUM"
                                          xVal := Val( cVal )
                                     CASE cKey == "MAX_NUM"
                                          xVal := Val( cVal )
                                  ENDCASE
                          ENDCASE
                          IF xVal <> NIL
                             hDefault[ cSection ][ cKey ] := xVal
                          ENDIF
                       ENDIF

                    ENDIF
                NEXT
             ENDIF
          ENDIF
      NEXT
   ENDIF

   RETURN hDefault

STATIC FUNCTION uhttpd_DefError( oError )
   LOCAL cMessage
   LOCAL cCallstack
   LOCAL cDOSError

   LOCAL aOptions
   LOCAL nChoice

   LOCAL n

   // By default, division by zero results in zero
   IF oError:genCode == EG_ZERODIV .AND. ;
      oError:canSubstitute
      RETURN 0
   ENDIF

   // By default, retry on RDD lock error failure */
   IF oError:genCode == EG_LOCK .AND. ;
      oError:canRetry
      // oError:tries++
      RETURN .T.
   ENDIF

   // Set NetErr() of there was a database open error
   IF oError:genCode == EG_OPEN .AND. ;
      oError:osCode == 32 .AND. ;
      oError:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   // Set NetErr() if there was a lock error on dbAppend()
   IF oError:genCode == EG_APPENDLOCK .AND. ;
      oError:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   cMessage := ErrorMessage( oError )
   IF ! Empty( oError:osCode )
      cDOSError := "(DOS Error " + hb_NToS( oError:osCode ) + ")"
   ENDIF

   // ;

   cCallstack := ""
   n := 1
   DO WHILE ! Empty( ProcName( ++n ) )
      cCallstack += "Called from " + ProcName( n ) + "(" + hb_NToS( ProcLine( n ) ) + ")  "
   ENDDO

   // Build buttons

   aOptions := {}

   AAdd( aOptions, "Quit" )

   IF oError:canRetry
      AAdd( aOptions, "Retry" )
   ENDIF

   IF oError:canDefault
      AAdd( aOptions, "Default" )
   ENDIF

   // Show alert box

#ifdef DEBUG_ACTIVE
   hb_ToOutDebug( "ERROR: %s\n\r", cMessage + " " + cCallstack )
#endif

   nChoice := 0
   DO WHILE nChoice == 0

      IF ISNIL( cDOSError )
         nChoice := Alert( cMessage + " " + cCallstack, aOptions )
      ELSE
         nChoice := Alert( cMessage + ";" + cDOSError + " " + cCallstack, aOptions )
      ENDIF

   ENDDO

   IF ! Empty( nChoice )
      DO CASE
      CASE aOptions[ nChoice ] == "Break"
         Break( oError )
      CASE aOptions[ nChoice ] == "Retry"
         RETURN .T.
      CASE aOptions[ nChoice ] == "Default"
         RETURN .F.
      ENDCASE
   ENDIF

   // "Quit" selected

   IF ! ISNIL( cDOSError )
      cMessage += " " + cDOSError
   ENDIF

   OutErr( hb_OSNewLine() )
   OutErr( cMessage )
   OutErr( hb_OSNewLine() )
   OutErr( cCallstack )

   ErrorLevel( 1 )
   QUIT

   RETURN .F.

STATIC FUNCTION ErrorMessage( oError )

   // start error message
   LOCAL cMessage := iif( oError:severity > ES_WARNING, "Error", "Warning" ) + " "

   // add subsystem name if available
   IF ISCHARACTER( oError:subsystem )
      cMessage += oError:subsystem()
   ELSE
      cMessage += "???"
   ENDIF

   // add subsystem's error code if available
   IF ISNUMBER( oError:subCode )
      cMessage += "/" + hb_NToS( oError:subCode )
   ELSE
      cMessage += "/???"
   ENDIF

   // add error description if available
   IF ISCHARACTER( oError:description )
      cMessage += "  " + oError:description
   ENDIF

   // add either filename or operation
   DO CASE
   CASE !Empty( oError:filename )
      cMessage += ": " + oError:filename
   CASE !Empty( oError:operation )
      cMessage += ": " + oError:operation
   ENDCASE

   RETURN cMessage
