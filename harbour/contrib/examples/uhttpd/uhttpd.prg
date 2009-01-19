/*
 * $Id: rlcdx.prg 9754 2008-10-27 22:40:04Z vszakats $
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
  - Add ini file for switches (port it from another project)
  - Add aliases from ini file

*/


#define APP_NAME      "uhttpd"
#define APP_VERSION   "0.1"

#ifndef _XHARBOUR_
  #include "hbcompat.ch"
#endif
#include "fileio.ch"
#include "common.ch"
#include "inkey.ch"

#include "hbextern.ch"   // need this to use with HRB
// adding GD support
REQUEST GDIMAGE, gdImageChar, GDCHART

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

#define PAGE_STATUS_REFRESH   1
#define THREAD_MAX_WAIT     ( 60 ) // HOW MUCH TIME THREAD HAS TO WAIT BEFORE FINISH - IN SECONDS

#define CR_LF    (CHR(13)+CHR(10))
#define HB_IHASH()   HB_HSETCASEMATCH( {=>}, FALSE )

#ifndef _XHARBOUR_

  #ifdef __PLATFORM__WINDOWS
     REQUEST HB_GT_WVT_DEFAULT
     REQUEST HB_GT_WIN
     REQUEST HB_GT_NUL
     #ifdef HB_MT_VM
       #define THREAD_GT hb_gtVersion()
     #endif
  #else
     REQUEST HB_GT_STD_DEFAULT
     REQUEST HB_GT_NUL
     #define THREAD_GT "XWC"
  #endif

#else

  REQUEST HB_GT_WVT
  REQUEST HB_GT_WIN
  REQUEST HB_GT_NUL

#endif

// dynamic call for HRB support
DYNAMIC HRBMAIN

STATIC hmtxQueue, hmtxServiceThreads, hmtxRunningThreads, hmtxLog, hmtxConsole, hmtxBusy
STATIC hmtxHRB
STATIC s_hfileLogAccess, s_hfileLogError, s_cDocumentRoot, s_lIndexes, s_lConsole, s_nPort
STATIC s_nThreads, s_nStartThreads, s_nMaxThreads
STATIC s_nServiceThreads, s_nStartServiceThreads, s_nMaxServiceThreads
STATIC s_nConnections, s_nMaxConnections, s_nTotConnections
STATIC s_nServiceConnections, s_nMaxServiceConnections, s_nTotServiceConnections
STATIC s_aRunningThreads := {}
STATIC s_aServiceThreads := {}

// TODO: add aliases from ini file
STATIC s_hFileAliases    := { "/info" => "/cgi-bin/info.hrb", "/wait" => "/cgi-bin/wait.hrb" }

THREAD STATIC s_cResult, s_nStatusCode, s_aHeader, s_cErrorMsg

MEMVAR _SERVER, _GET, _POST, _REQUEST, _HTTP_REQUEST, m_cPost

FUNCTION MAIN( ... )
LOCAL nPort, hListen, hSocket, aRemote, nI, cI
LOCAL hThread, aThreads, nStartThreads, nMaxThreads, nStartServiceThreads
LOCAL i, cPar, lStop
LOCAL cGT, cDocumentRoot, lIndexes, cConfig, cPort, nNewStartThreads, nNewMaxThreads
LOCAL lConsole
LOCAL nProgress := 0

   IF !HB_MTVM()
      ? "I need multhread support. Please, recompile me!"
      WAIT
      RETURN 2
   ENDIF

   // ----------------------- Initializations ---------------------------------

   SysSettings()

   // ----------------------- Parameters defaults -----------------------------

   // defaults
   nPort                := LISTEN_PORT
   lStop                := FALSE
   cDocumentRoot        := EXE_Path() + "\home"
   lIndexes             := FALSE
   cConfig              := EXE_Path() + "\" + APP_NAME + ".ini"
   lConsole             := TRUE
   nStartThreads        := START_RUNNING_THREADS
   nMaxThreads          := MAX_RUNNING_THREADS
   nStartServiceThreads := START_SERVICE_THREADS

   // Check GT version - if I have started app with //GT:NUL then I have to disable
   // console
   cGT := HB_GT_VERSION()
   IF ( cGT == "NUL" )
      lConsole := FALSE
   ENDIF

   // TOCHECK: per il momento non forzo
   //HB_HSETCASEMATCH( s_hFileAliases, FALSE )

   // ----------------- Line command parameters checking ----------------------

   i := 1
   while ( i <= PCount() )

      cPar := hb_PValue( i++ )

      do case
      case cPar == "--port"             .OR. cPar == "-p"
         cPort    := hb_PValue( i++ )

      case cPar == "--docroot"          .OR. cPar == "-d"
         cDocumentRoot := hb_PValue( i++ )

      case cPar == "--indexes"          .OR. cPar == "-i"
         lIndexes := TRUE

      case cPar == "--stop"             .OR. cPar == "-s"
         lStop    := TRUE

      case cPar == "--config"           .OR. cPar == "-c"
         cConfig     := hb_PValue( i++ )

      case cPar == "--start-threads"    .OR. cPar == "-ts"
         nNewStartThreads := hb_PValue( i++ )

      case cPar == "--max-threads"      .OR. cPar == "-tm"
         nNewMaxThreads := hb_PValue( i++ )

      case cPar == "--help"             .OR. Lower( cPar ) == "-h" .OR. cPar == "-?"
         help()
         RETURN 0

      otherwise
         help()
         RETURN 0
      endcase
   enddo

   // -------------------- checking STOP request -------------------------------

   IF lStop
      HB_MEMOWRIT( FILE_STOP, "" )
      RETURN 0
   ELSE
      FERASE( FILE_STOP )
   ENDIF

   // -------------------- checking starting values ----------------------------

   IF cPort != NIL
      nPort := VAL( cPort )
      IF nPort <= 0 .OR. nPort > 65535
         ? "Invalid port number:", nPort
         WAIT
         RETURN 1
      ENDIF
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

   IF HB_ISNUMERIC( nNewMaxThreads ) .AND. ;
      nNewMaxThreads > 0
      nMaxThreads := nNewMaxThreads
   ENDIF

   IF HB_ISNUMERIC( nNewStartThreads ) .AND. ;
      nNewStartThreads > 0
      IF nNewStartThreads <= nMaxThreads
         nStartThreads := nNewStartThreads
      ELSE
         nStartThreads := nMaxThreads
      ENDIF
   ENDIF

   // -------------------- assign STATIC values --------------------------------

   s_lIndexes        := lIndexes
   s_lConsole        := lConsole
   s_nPort           := nPort
   s_nThreads        := 0
   s_nStartThreads   := nStartThreads
   s_nMaxThreads     := nMaxThreads
   s_nServiceThreads := 0
   s_nStartServiceThreads := nStartServiceThreads
   s_nMaxServiceThreads := MAX_SERVICE_THREADS
   s_nConnections    := 0
   s_nMaxConnections := 0
   s_nTotConnections := 0
   s_nServiceConnections    := 0
   s_nMaxServiceConnections := 0
   s_nTotServiceConnections := 0

   // --------------------- Open log files -------------------------------------

   IF ( s_hfileLogAccess := FOPEN( FILE_ACCESS_LOG, FO_CREAT + FO_WRITE ) ) == -1
      ? "Can't open access log file"
      WAIT
      RETURN 1
   ENDIF
   FSEEK( s_hfileLogAccess, 0, FS_END )

   IF ( s_hfileLogError := FOPEN( FILE_ERROR_LOG, FO_CREAT + FO_WRITE ) ) == -1
      ? "Can't open error log file"
      WAIT
      RETURN 1
   ENDIF
   FSEEK( s_hfileLogError, 0, FS_END )

   // --------------------- MAIN PART ------------------------------------------

   SET CURSOR OFF

   // --------------------- define mutexes -------------------------------------

   hmtxQueue          := hb_mutexCreate()
   hmtxLog            := hb_mutexCreate()
   hmtxConsole        := hb_mutexCreate()
   hmtxBusy           := hb_mutexCreate()
   hmtxRunningThreads := hb_mutexCreate()
   hmtxServiceThreads := hb_mutexCreate()
   hmtxHRB            := hb_mutexCreate()

   WriteToConsole( "--- Starting " + APP_NAME + " ---" )

   // --------------------------------------------------------------------------
   // SOCKET CREATION
   // --------------------------------------------------------------------------

   hListen   := socket_create()
   IF socket_bind( hListen, { AF_INET, "0.0.0.0", nPort } ) == -1
      ? "bind() error", socket_error()
   ELSEIF socket_listen( hListen ) == -1
      ? "listen() error", socket_error()
   ELSE

      // --------------------------------------------------------------------------------- //
      // Starting Accept connection thread
      // --------------------------------------------------------------------------------- //

      WriteToConsole( "Starting AcceptConnection Thread" )
      aThreads := {}
      //FOR nI := 1 TO 1 // s_nMaxThreads
          AADD( aThreads, hb_threadStart( @AcceptConnections() ) )
      //NEXT

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
            IF hb_mutexLock( hmtxBusy )
               hb_DispOutAt( 5, 5, "Threads           : " + Transform( s_nThreads, "9999999999" ) )
               hb_DispOutAt( 6, 5, "Connections       : " + Transform( s_nConnections, "9999999999" ) )
               hb_DispOutAt( 7, 5, "Max Connections   : " + Transform( s_nMaxConnections, "9999999999" ) )
               hb_DispOutAt( 8, 5, "Total Connections : " + Transform( s_nTotConnections, "9999999999" ) )

               hb_DispOutAt( 5, 37, "ServiceThreads    : " + Transform( s_nServiceThreads, "9999999999" ) )
               hb_DispOutAt( 6, 37, "Connections       : " + Transform( s_nServiceConnections, "9999999999" ) )
               hb_DispOutAt( 7, 37, "Max Connections   : " + Transform( s_nMaxServiceConnections, "9999999999" ) )
               hb_DispOutAt( 8, 37, "Total Connections : " + Transform( s_nTotServiceConnections, "9999999999" ) )
               hb_mutexUnlock( hmtxBusy )
            ENDIF

            // Show progress
            Progress( @nProgress )
         ENDIF

         // Wait a connection
         IF ( nI := socket_select( { hListen },,, 50 ) ) > 0

            // reset remote values
            aRemote := NIL

            // Accept a remote connection
            hSocket := socket_accept( hListen, @aRemote )

            IF hSocket == NIL

               WriteToConsole( hb_sprintf( "accept() error: %s", socket_error() ) )

            ELSE

               // Send accepted connection to AcceptConnections() thread
               hb_mutexNotify( hmtxQueue, hSocket )

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
      AEVAL( aThreads, {|| hb_mutexNotify( hmtxQueue, NIL ) } )
      // Wait threads to end
      AEVAL( aThreads, {|h| hb_threadJoin( h ) } )

   ENDIF

   WriteToConsole( "--- Quitting " + APP_NAME + " ---" )

   // Close socket
   socket_close( hListen )

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
     hb_mutexSubscribe( hmtxQueue,, @hSocket )

     // I have a QUIT request
     IF hSocket == NIL

        // Requesting to Running threads to quit (using -1 value)
        AEVAL( s_aRunningThreads, {|| hb_mutexNotify( hmtxRunningThreads, -1 ) } )
        // waiting running threads to quit
        AEVAL( s_aRunningThreads, {|h| hb_threadJoin( h[1] ) } )

        // Requesting to Service threads to quit (using -1 value)
        AEVAL( s_aServiceThreads, {|| hb_mutexNotify( hmtxServiceThreads, -1 ) } )
        // waiting service threads to quit
        AEVAL( s_aServiceThreads, {|h| hb_threadJoin( h[1] ) } )

        EXIT
     ENDIF

     // Load current state
     IF hb_mutexLock( hmtxBusy )
        nConnections       := s_nConnections
        nThreads           := s_nThreads
        nMaxThreads        := s_nMaxThreads
        nServiceConnections:= s_nServiceConnections
        nServiceThreads    := s_nServiceThreads
        nMaxServiceThreads := s_nMaxServiceThreads
        hb_mutexUnlock( hmtxBusy )
     ENDIF

     // If I have no more thread to use ...
     IF nConnections > nMaxThreads

        // If I have no more of service threads to use ... (DOS attack ?)
        IF nServiceConnections > nMaxServiceThreads
            // DROP connection
           socket_shutdown( hSocket )
           socket_close( hSocket )

        // If I have no service threads in use ...
        ELSEIF nServiceConnections >= nServiceThreads
           // Add one more
           pThread := hb_threadStart( @ServiceConnection(), @nThreadID )
           AADD( s_aServiceThreads, { pThread, nThreadID } )
        ENDIF
        // Otherwise I send connection to service thread
        hb_mutexNotify( hmtxServiceThreads, hSocket )

        LOOP

     // If I have no running threads in use ...
     ELSEIF nConnections >= nThreads
        // Add one more
        pThread := hb_threadStart( @ProcessConnection(), @nThreadID )
        AADD( s_aRunningThreads, { pThread, nThreadID } )
     ENDIF
     // Otherwise I send connection to running thread
     hb_mutexNotify( hmtxRunningThreads, hSocket )

  ENDDO

  WriteToConsole( "Quitting AcceptConnections()" )

RETURN 0

// --------------------------------------------------------------------------------- //
// CONNECTIONS
// --------------------------------------------------------------------------------- //
STATIC FUNCTION ProcessConnection( nThreadIdRef )
LOCAL hSocket, cBuf, nLen, cRequest, cSend, aI
LOCAL nMsecs, nParseTime, nPos
LOCAL nThreadId

  nThreadId    := hb_threadID()
  nThreadIdRef := nThreadId

  WriteToConsole( "Starting ProcessConnections() " + hb_CStr( nThreadId ) )

  IF hb_mutexLock( hmtxBusy )
     s_nThreads++
     hb_mutexUnlock( hmtxBusy )
  ENDIF

  // ProcessConnection Loop
  DO WHILE .T.

     // Reset socket
     hSocket := NIL

     // releasing resources
     WIN_SYSREFRESH( 1 )

     // Waiting a connection from AcceptConnections() but up to defined time
     hb_mutexSubscribe( hmtxRunningThreads, THREAD_MAX_WAIT, @hSocket )

     // received a -1 value, I have to quit
     IF HB_ISNUMERIC( hSocket )
        EXIT
     // no socket received, thread can graceful quit only if over minimal number
     ELSEIF hSocket == NIL
        IF hb_mutexLock( hmtxBusy )
           IF s_nThreads <= s_nStartThreads
              hb_mutexUnlock( hmtxBusy )
              LOOP
           ENDIF
           hb_mutexUnlock( hmtxBusy )
        ENDIF
        EXIT
     ENDIF

     // Connection accepted
     IF hb_mutexLock( hmtxBusy )
        s_nConnections++
        s_nTotConnections++
        s_nMaxConnections := Max( s_nConnections, s_nMaxConnections )
        hb_mutexUnlock( hmtxBusy )
     ENDIF

     // Save initial time
     nMsecs := hb_milliseconds()

     BEGIN SEQUENCE

        /* receive query */
        cRequest := ""
        nLen     := 1
        DO WHILE AT( CR_LF + CR_LF, cRequest ) == 0 .AND. nLen > 0
           nLen := socket_recv( hSocket, @cBuf )
           cRequest += cBuf
        ENDDO

        IF nLen == -1
           ? "recv() error:", socket_error()
        ELSEIF nLen == 0 /* connection closed */
        ELSE

           //hb_ToOutDebug( "cRequest -- INIZIO --\n\r%s\n\rcRequest -- FINE --\n\r", cRequest )

           PRIVATE _SERVER := HB_IHASH(), _GET := HB_IHASH(), _POST := HB_IHASH(), _REQUEST := HB_IHASH(), _HTTP_REQUEST := HB_IHASH(), m_cPost
           s_cResult     := ""
           s_aHeader     := {}
           s_nStatusCode := 200
           s_cErrorMsg   := ""

           IF socket_getpeername( hSocket, @aI ) != -1
              _SERVER["REMOTE_ADDR"] := aI[2]
              _SERVER["REMOTE_HOST"] := _SERVER["REMOTE_ADDR"]  // no reverse DNS
              _SERVER["REMOTE_PORT"] := aI[3]
           ENDIF

           IF socket_getsockname( hSocket, @aI ) != -1
              _SERVER["SERVER_ADDR"] := aI[2]
              _SERVER["SERVER_PORT"] := aI[3]
           ENDIF

           IF ParseRequest( cRequest )
              //hb_ToOutDebug( "_SERVER = %s,\n\r _GET = %s,\n\r _POST = %s,\n\r _REQUEST = %s,\n\r _HTTP_REQUEST = %s\n\r", hb_ValToExp( _SERVER ), hb_ValToExp( _GET ), hb_ValToExp( _POST ), hb_ValToExp( _REQUEST ), hb_ValToExp( _HTTP_REQUEST ) )
              define_Env( _SERVER )
              uproc_default( s_cDocumentRoot, s_lIndexes )
           ELSE
              uSetStatusCode( 400 )
           ENDIF
           cSend := MakeResponse()

           //hb_ToOutDebug( "cSend = %s\n\r", cSend )

           DO WHILE LEN( cSend ) > 0
              IF ( nLen := socket_send( hSocket, cSend ) ) == -1
                 ? "send() error:", socket_error()
                 WriteToConsole( hb_sprintf( "ProcessConnection() - send() error: %s, cSend = %s, hSocket = %s", socket_error(), cSend, hSocket ) )
                 EXIT
              ELSEIF nLen > 0
                 cSend := SUBSTR( cSend, nLen + 1 )
              ENDIF
           ENDDO

           WriteToLog( cRequest )

        ENDIF
        socket_shutdown( hSocket )
        socket_close( hSocket )
     END SEQUENCE

     nParseTime := hb_milliseconds() - nMsecs
     WriteToConsole( "Page served in : " + Str( nParseTime/1000, 10, 7 ) + " seconds" )

     IF hb_mutexLock( hmtxBusy )
        s_nConnections--
        hb_mutexUnlock( hmtxBusy )
     ENDIF

  ENDDO

  WriteToConsole( "Quitting ProcessConnections() " + hb_CStr( nThreadId ) )

  IF hb_mutexLock( hmtxBusy )
     s_nThreads--
     IF ( nPos := aScan( s_aRunningThreads, {|h| h[2] == nThreadId } ) > 0 )
        hb_aDel( s_aRunningThreads, nPos, TRUE )
     ENDIF
     hb_mutexUnlock( hmtxBusy )
  ENDIF

RETURN 0

STATIC FUNCTION ServiceConnection( nThreadIdRef )
LOCAL hSocket, cBuf, nLen, cRequest, cSend, aI
LOCAL nMsecs, nParseTime, nPos
LOCAL nThreadId
LOCAL nError := 500013

  nThreadId := hb_threadID()
  nThreadIdRef := nThreadId

  WriteToConsole( "Starting ServiceConnections() " + hb_CStr( nThreadId ) )

  IF hb_mutexLock( hmtxBusy )
     s_nServiceThreads++
     hb_mutexUnlock( hmtxBusy )
  ENDIF

  DO WHILE .T.

     // Reset socket
     hSocket := NIL

     // releasing resources
     WIN_SYSREFRESH( 1 )

     // Waiting a connection from AcceptConnections() but up to defined time
     hb_mutexSubscribe( hmtxServiceThreads, THREAD_MAX_WAIT, @hSocket )

     // received a -1 value, I have to quit
     IF HB_ISNUMERIC( hSocket )
        EXIT
     // no socket received, thread can graceful quit only if over minimal number
     ELSEIF hSocket == NIL
        IF hb_mutexLock( hmtxBusy )
           IF s_nServiceThreads <= s_nStartServiceThreads
              hb_mutexUnlock( hmtxBusy )
              LOOP
           ENDIF
           hb_mutexUnlock( hmtxBusy )
        ENDIF
        EXIT
     ENDIF

     // Connection accepted
     IF hb_mutexLock( hmtxBusy )
        s_nServiceConnections++
        s_nTotServiceConnections++
        s_nMaxServiceConnections := Max( s_nServiceConnections, s_nMaxServiceConnections )
        hb_mutexUnlock( hmtxBusy )
     ENDIF

     // Save initial time
     nMsecs := hb_milliseconds()

     BEGIN SEQUENCE

        /* receive query */
        cRequest := ""
        nLen     := 1
        DO WHILE AT( CR_LF + CR_LF, cRequest ) == 0 .AND. nLen > 0
           nLen := socket_recv( hSocket, @cBuf )
           cRequest += cBuf
        ENDDO

        IF nLen == -1
           ? "recv() error:", socket_error()
        ELSEIF nLen == 0 /* connection closed */
        ELSE

           //hb_ToOutDebug( "cRequest -- INIZIO --\n\r%s\n\rcRequest -- FINE --\n\r", cRequest )

           PRIVATE _SERVER := HB_IHASH(), _GET := HB_IHASH(), _POST := HB_IHASH(), _REQUEST := HB_IHASH(), _HTTP_REQUEST := HB_IHASH(), m_cPost
           s_cResult     := ""
           s_aHeader     := {}
           s_nStatusCode := 200
           s_cErrorMsg   := ""

           IF socket_getpeername( hSocket, @aI ) != -1
              _SERVER["REMOTE_ADDR"] := aI[2]
              _SERVER["REMOTE_HOST"] := _SERVER["REMOTE_ADDR"]  // no reverse DNS
              _SERVER["REMOTE_PORT"] := aI[3]
           ENDIF

           IF socket_getsockname( hSocket, @aI ) != -1
              _SERVER["SERVER_ADDR"] := aI[2]
              _SERVER["SERVER_PORT"] := aI[3]
           ENDIF

           IF ParseRequest( cRequest )
              //hb_ToOutDebug( "_SERVER = %s,\n\r _GET = %s,\n\r _POST = %s,\n\r _REQUEST = %s,\n\r _HTTP_REQUEST = %s\n\r", hb_ValToExp( _SERVER ), hb_ValToExp( _GET ), hb_ValToExp( _POST ), hb_ValToExp( _REQUEST ), hb_ValToExp( _HTTP_REQUEST ) )
              define_Env( _SERVER )
           ENDIF
           // Error page served
           uSetStatusCode( nError )
           cSend := MakeResponse()

           DO WHILE LEN( cSend ) > 0
              IF ( nLen := socket_send( hSocket, cSend ) ) == -1
                 ? "send() error:", socket_error()
                 WriteToConsole( hb_sprintf( "ServiceConnection() - send() error: %s, cSend = %s, hSocket = %s", socket_error(), cSend, hSocket ) )
                 EXIT
              ELSEIF nLen > 0
                 cSend := SUBSTR( cSend, nLen + 1 )
              ENDIF
           ENDDO

           WriteToLog( cRequest )

        ENDIF
        socket_shutdown( hSocket )
        socket_close( hSocket )
     END SEQUENCE

     nParseTime := hb_milliseconds() - nMsecs
     WriteToConsole( "Page served in : " + Str( nParseTime/1000, 10, 7 ) + " seconds" )

     IF hb_mutexLock( hmtxBusy )
        s_nServiceConnections--
        hb_mutexUnlock( hmtxBusy )
     ENDIF

  ENDDO

  WriteToConsole( "Quitting ServiceConnections() " + hb_CStr( nThreadId ) )

  IF hb_mutexLock( hmtxBusy )
     s_nServiceThreads--
     IF ( nPos := aScan( s_aServiceThreads, {|h| h[2] == nThreadId } ) > 0 )
        hb_aDel( s_aServiceThreads, nPos, TRUE )
     ENDIF
     hb_mutexUnlock( hmtxBusy )
  ENDIF

RETURN 0

STATIC FUNCTION ParseRequest( cRequest )
LOCAL aRequest, aLine, nI, nJ, cI
LOCAL cReq, aVal, cPost

  // RFC2616
  aRequest := split( CR_LF, cRequest )

  //hb_ToOutDebug( "aRequest = %s\n\r", hb_ValToExp( aRequest ) )

  WriteToConsole( aRequest[1] )
  aLine := split( " ", aRequest[1] )
  IF LEN( aLine ) != 3 .OR. ;
     ( aLine[1] != "GET" .AND. aLine[1] != "POST" ) .OR. ; // Sorry, we support GET and POST only
     LEFT( aLine[3], 5 ) != "HTTP/"
     RETURN .F.
  ENDIF

  // define _SERVER var
  _SERVER["REQUEST_METHOD"]  := aLine[1]
  _SERVER["REQUEST_URI"]     := aLine[2]
  _SERVER["SERVER_PROTOCOL"] := aLine[3]

  IF ( nI := AT( "?", _SERVER["REQUEST_URI"] ) ) > 0
     _SERVER["SCRIPT_NAME"]  := LEFT( _SERVER["REQUEST_URI"], nI - 1)
     _SERVER["QUERY_STRING"] := SUBSTR( _SERVER["REQUEST_URI"], nI + 1)
  ELSE
     _SERVER["SCRIPT_NAME"]  := _SERVER["REQUEST_URI"]
     _SERVER["QUERY_STRING"] := ""
  ENDIF

  _SERVER["HTTP_ACCEPT"]          := ""
  _SERVER["HTTP_ACCEPT_CHARSET"]  := ""
  _SERVER["HTTP_ACCEPT_ENCODING"] := ""
  _SERVER["HTTP_ACCEPT_LANGUAGE"] := ""
  _SERVER["HTTP_CONNECTION"]      := ""
  _SERVER["HTTP_HOST"]            := ""
  _SERVER["HTTP_KEEP_ALIVE"]      := ""
  _SERVER["HTTP_REFERER"]         := ""
  _SERVER["HTTP_USER_AGENT"]      := ""
  _SERVER["HTTP_CACHE_CONTROL"]   := ""

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
          CASE "KEEP-ALIVE"
          CASE "REFERER"
          CASE "USER-AGENT"
            _SERVER[ "HTTP_" + STRTRAN( UPPER( LEFT( aRequest[nI], nJ - 1 ) ), "-", "_" ) ] := cI
            EXIT
          CASE "HOST"
            aVal := split( ":", aRequest[ nI ] )
            _SERVER[ "HTTP_" + STRTRAN( UPPER( aVal[ 1 ] ), "-", "_")] := AllTrim( aVal[ 2 ] )
            EXIT
          CASE "CONTENT-TYPE"
          CASE "CONTENT-LENGTH"
            _SERVER[ STRTRAN( UPPER( LEFT( aRequest[ nI ], nJ - 1 ) ), "-", "_" ) ] := cI
            EXIT
       ENDSWITCH
     ENDIF
  NEXT

  // GET vars
  FOR EACH cI IN split( "&", _SERVER["QUERY_STRING"] )
     IF ( nI := AT( "=", cI ) ) > 0
        _GET[ LEFT( cI, nI - 1 )] := SUBSTR( cI, nI + 1 )
        _REQUEST[ LEFT( cI, nI - 1 )] := SUBSTR( cI, nI + 1 )
     ELSE
        _GET[ cI ] := ""
        _REQUEST[ cI ] := ""
     ENDIF
  NEXT

  // Load _HTTP_REQUEST
  FOR EACH cReq IN aRequest
      IF cReq:__enumIndex() == 1 // GET request
         hb_HSet( _HTTP_REQUEST, "HTTP Request", cReq )
      ELSEIF Empty( cReq )
         EXIT
      ELSE
         aVal := split( ":", cReq, 1 )
         hb_HSet( _HTTP_REQUEST, aVal[ 1 ], IIF( Len( aVal ) == 2, AllTrim( aVal[ 2 ] ), NIL ) )
      ENDIF
  NEXT

  // POST vars
  IF "POST" $ Upper( _SERVER[ 'REQUEST_METHOD' ] )
     //hb_ToOutDebug( "POST: %s\n\r", aTail( aRequest ) )
     //cPost := SubStr( aTail( aRequest ), 1, _SERVER[ 'CONTENT_LENGTH' ] )
     cPost := aTail( aRequest )
     FOR EACH cI IN split( "&", cPost )
        IF ( nI := AT( "=", cI ) ) > 0
           _POST[ LEFT( cI, nI - 1 )] := SUBSTR( cI, nI + 1 )
           _REQUEST[ LEFT( cI, nI - 1 )] := SUBSTR( cI, nI + 1 )
        ELSE
           _POST[ cI ] := ""
           _REQUEST[ cI ] := ""
        ENDIF
     NEXT
     m_cPost := cPost
  ENDIF

  // Complete _SERVER
  _SERVER[ "SERVER_NAME"       ] = split( ":", _HTTP_REQUEST[ "HOST" ], 1 )[ 1 ]
  _SERVER[ "SERVER_SOFTWARE"   ] = APP_NAME + " " + APP_VERSION + " (" + OS() + ")"
  _SERVER[ "SERVER_SIGNATURE"  ] = "<address>" + _SERVER[ "SERVER_SOFTWARE" ] + " Server at " + _SERVER[ "SERVER_NAME" ] + " Port " + LTrim( Str( _SERVER[ "SERVER_PORT" ] ) ) + "</address>"
  _SERVER[ "DOCUMENT_ROOT"     ] = s_cDocumentRoot
  _SERVER[ "SERVER_ADMIN"      ] = "root"
  _SERVER[ "SCRIPT_FILENAME"   ] = STRTRAN( STRTRAN( _SERVER[ "DOCUMENT_ROOT" ] + _SERVER[ "SCRIPT_NAME" ], "//", "/" ), "\", "/" )
  _SERVER[ "GATEWAY_INTERFACE" ] = "CGI/1.1"
  _SERVER[ "SCRIPT_URL"        ] := _SERVER["SCRIPT_NAME"]

  //hb_ToOutDebug( "_SERVER = %s\n\r", hb_ValToExp( _SERVER ) )
  //hb_ToOutDebug( "_GET = %s\n\r", hb_ValToExp( _GET ) )
  //hb_ToOutDebug( "_POST = %s\n\r", hb_ValToExp( _POST ) )
  //hb_ToOutDebug( "_HTTP_REQUEST = %s\n\r", hb_ValToExp( _HTTP_REQUEST ) )

RETURN .T.


STATIC FUNCTION MakeResponse()
LOCAL cRet, cReturnCode

  uAddHeader("Connection", "close")

  IF uGetHeader("Location") != NIL
     s_nStatusCode := 301
  ENDIF
  IF uGetHeader("Content-Type") == NIL
     uAddHeader("Content-Type", "text/html")
  ENDIF

  cRet := "HTTP/1.1 "
  cReturnCode := DecodeStatusCode()

  SWITCH s_nStatusCode
    CASE 200
         EXIT

    CASE 301
    CASE 400
    CASE 401
    CASE 402
    CASE 403
    CASE 404
    CASE 503
         s_cResult := "<html><body><h1>" + cReturnCode + "</h1></body></html>"
         EXIT

    // extended error messages - from Microsoft IIS Server
    CASE 500013 // error: 500-13 Server too busy
         uAddHeader( "Retry-After", "60" )  // retry after 60 seconds
         s_cResult := "<html><body><h1>500 Server Too Busy</h1></body></html>"
         EXIT

    CASE 500100 // error: 500-100 Undeclared Variable

    OTHERWISE
         cReturnCode := "403 Forbidden"
         s_cResult := "<html><body><h1>" + cReturnCode + "</h1></body></html>"
  ENDSWITCH

  WriteToConsole( cReturnCode )
  cRet += cReturnCode + CR_LF
  AEVAL( s_aHeader, {|x| cRet += x[1] + ": " + x[2] + CR_LF } )
  cRet += CR_LF
  cRet += s_cResult
RETURN cRet

STATIC FUNCTION DecodeStatusCode()
LOCAL cReturnCode

  SWITCH s_nStatusCode
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

   IF hb_mutexLock( hmtxLog )

      //hb_ToOutDebug( "TIP_TimeStamp() = %s \n\r", TIP_TIMESTAMP() )

      cTime    := TIME()
      dDate    := Date()
      cDate    := DTOS( dDate )
      nSize    := LEN( s_cResult )
      cReferer := _SERVER["HTTP_REFERER"]
      cBias    := WIN_TIMEZONEBIAS()

      cAccess := _SERVER["REMOTE_ADDR"] + " - - [" + RIGHT( cDate, 2 ) + "/" + ;
                       aMonths[ VAL( SUBSTR( cDate, 5, 2 ) ) ] + ;
                       "/" + LEFT( cDate, 4 ) + ":" + cTime + ' ' + cBias + '] "' + ;
                       LEFT( cRequest, AT( CR_LF, cRequest ) - 1 ) + '" ' + ;
                       LTRIM( STR( s_nStatusCode ) ) + " " + IIF( nSize == 0, "-", LTRIM( STR( nSize ) ) ) + ;
                       ' "' + IIF( Empty( cReferer ), "-", cReferer ) + '" "' + _SERVER["HTTP_USER_AGENT"] + ;
                       '"' + HB_OSNewLine()

      //hb_ToOutDebug( "AccessLog = %s \n\r", cAccess )

      FWRITE( s_hfileLogAccess, cAccess )

      IF !( s_nStatusCode == 200 ) // ok

         nDoW   := Dow( dDate )
         nDay   := Day( dDate )
         nMonth := Month( dDate )
         nYear  := Year( dDate )
         cErrorMsg := s_cErrorMsg

         cError := "[" + Left( aDays[ nDoW ], 3 ) + " " + aMonths[ nMonth ] + " " + StrZero( nDay, 2 ) + " " + ;
                   PadL( LTrim( cTime ), 8, "0" ) + " " + StrZero( nYear, 4 ) + "] [error] [client " + _SERVER["REMOTE_ADDR"] + "] " + ;
                   cErrorMsg + HB_OSNewLine()

         //hb_ToOutDebug( "ErrorLog = %s \n\r", cError )

         FWRITE( s_hfileLogError, cError )
      ENDIF

      hb_mutexUnlock( hmtxLog )
   ENDIF

RETURN

INIT PROCEDURE SocketInit()
  IF socket_init() != 0
     ? "socket_init() error"
  ENDIF
RETURN


EXIT PROCEDURE Socketxit()
   socket_exit()
RETURN


/********************************************************************
  Public helper functions
********************************************************************/
STATIC FUNCTION split( cSeparator, cString, nMax )
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

STATIC FUNCTION join( cSeparator, aData )
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


FUNCTION uOSFileName( cFileName )
  IF HB_OSPathSeparator() != "/"
     RETURN STRTRAN( cFileName, "/", HB_OSPathSeparator() )
  ENDIF
RETURN cFileName

PROCEDURE uSetStatusCode(nStatusCode)
  s_nStatusCode := nStatusCode
RETURN


PROCEDURE uAddHeader( cType, cValue )
LOCAL nI

  IF ( nI := ASCAN( s_aHeader, {|x| UPPER( x[ 1 ] ) == UPPER( cType ) } ) ) > 0
     s_aHeader[ nI, 2 ] := cValue
  ELSE
     AADD( s_aHeader, { cType, cValue } )
  ENDIF
RETURN


FUNCTION uGetHeader( cType )
LOCAL nI

  IF ( nI := ASCAN( s_aHeader, {|x| UPPER( x[ 1 ] ) == UPPER( cType ) } ) ) > 0
     RETURN s_aHeader[ nI, 2 ]
  ENDIF
RETURN NIL


PROCEDURE uWrite( cString )
  s_cResult += cString
RETURN

#define XP_SUCCESS                 0

STATIC PROCEDURE uproc_default( cRoot, lIndex )
LOCAL cFileName, nI, cI
LOCAL cExt, xResult, pHRB, oError

  //cFileName := STRTRAN(cRoot + _SERVER["SCRIPT_NAME"], "//", "/")
  cFileName := _SERVER[ "SCRIPT_FILENAME" ]

  //hb_ToOutDebug( "cFileName = %s, uOSFileName( cFileName ) = %s,\n\r _SERVER = %s\n\r", cFileName, uOSFileName( cFileName ), hb_ValToExp( _SERVER ) )

  // Security
  IF ".." $ cFileName
     uSetStatusCode( 403 )
     s_cErrorMsg := "Characters not allowed"
     RETURN
  ENDIF

  IF HB_HHasKey( s_hFileAliases, _SERVER[ "SCRIPT_NAME" ] )
     cFileName := _SERVER[ "DOCUMENT_ROOT" ] +  hb_hGet( s_hFileAliases, _SERVER[ "SCRIPT_NAME" ] )
  ENDIF

  IF Upper( _SERVER[ "SCRIPT_NAME" ] ) == "/SERVERSTATUS"
     ShowServerStatus()
  ELSEIF HB_FileExists( uOSFileName( cFileName ) )
     IF ( nI := RAT( ".", cFileName ) ) > 0
        SWITCH ( cExt := LOWER( SUBSTR( cFileName, nI + 1 ) ) )
           CASE "hrb" ;                                 cI := "text/html";                EXIT
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
              IF hb_mutexLock( hmtxHRB )
                 IF !EMPTY( pHRB := __HRBLOAD( uOSFileName(cFileName) ) )

                     xResult := HRBMAIN()

                     __HRBUNLOAD( pHRB )

                 ENDIF
                 hb_mutexUnlock( hmtxHRB )
              ENDIF

              IF HB_ISSTRING( xResult )
                 uAddHeader( "Content-Type", cI )
                 uWrite( xResult )
              ELSE
                 // Application in HRB module is responsible to send HTML content
              ENDIF

           CATCH oError

              WriteToConsole( "Error!" )

              uAddHeader( "Content-Type", "text/html" )
              uWrite( "Error" )
              uWrite( "<br>Description: " + hb_cStr( oError:Description ) )
              uWrite( "<br>Filename: "    + hb_cStr( oError:filename ) )
              uWrite( "<br>Operation: "   + hb_cStr( oError:operation ) )
              uWrite( "<br>OsCode: "      + hb_cStr( oError:osCode ) )
              uWrite( "<br>GenCode: "     + hb_cStr( oError:genCode ) )
              uWrite( "<br>SubCode: "     + hb_cStr( oError:subCode ) )
              uWrite( "<br>SubSystem: "   + hb_cStr( oError:subSystem ) )
              uWrite( "<br>Args: "        + hb_cStr( hb_ValToExp( oError:args ) ) )
              uWrite( "<br>ProcName: "    + hb_cStr( procname( 0 ) ) )
              uWrite( "<br>ProcLine: "    + hb_cStr( procline( 0 ) ) )
           END


        ELSE
           uAddHeader( "Content-Type", cI )
           uWrite( HB_MEMOREAD( uOSFileName( cFileName ) ) )
        ENDIF

     ELSE
        cI := "application/octet-stream"
        uAddHeader( "Content-Type", cI )
        uWrite( HB_MEMOREAD( uOSFileName( cFileName ) ) )
     ENDIF

  ELSEIF HB_DirExists( uOSFileName( cFileName ) )
     IF RIGHT( cFileName, 1 ) != "/"
        uAddHeader( "Location", "http://" + _SERVER[ "HTTP_HOST" ] + _SERVER[ "SCRIPT_NAME" ] + "/" )
        RETURN
     ENDIF
     IF (nI := ASCAN( { "index.html", "index.htm" }, ;
                     {|x| IIF( HB_FileExists( uOSFileName( cFileName + X ) ), ( cFileName += X, .T. ), .F. ) } ) ) > 0
        uAddHeader( "Content-Type", "text/html" )
        uWrite( HB_MEMOREAD( uOSFileName( cFileName ) ) )
        RETURN
     ENDIF

     // If I'm here it's means that I have no page, so, if it is defined, I will display content folder
     IF !s_lIndexes
        uSetStatusCode( 403 )
        s_cErrorMsg := "Display file list not allowed"
        RETURN
     ENDIF

     // ----------------------- display folder content -------------------------------------
     ShowFolder( cFileName )

  ELSE
     uSetStatusCode( 404 )
     s_cErrorMsg := "File does not exist: " + cFileName
  ENDIF
RETURN

// Define environment SET variables - TODO: Actually only for windows, make multiplatform
STATIC PROCEDURE Define_Env( hmServer )
   LOCAL v

   FOR EACH v IN hmServer
       WIN_SETENV( v:__enumKey(), v:__enumValue() )
   NEXT

RETURN

// ------------------------------- DEFAULT PAGES -----------------------------------

STATIC PROCEDURE ShowServerStatus()

   uAddHeader( "Content-Type", "text/html" )
   uWrite( '<html><head>' )
   uWrite( '<META HTTP-EQUIV="Refresh" CONTENT="' + LTrim( Str( PAGE_STATUS_REFRESH ) ) + ';URL=/ServerStatus">' )
   uWrite( '<title>Server Status</title><body><h1>Server Status</h1><pre>')
   //uWrite( '<table border="0">')

   uWrite( 'SERVER: ' + _SERVER[ "SERVER_SOFTWARE" ] + " Server at " + _SERVER[ "SERVER_NAME" ] + " Port " + LTrim( Str( _SERVER[ "SERVER_PORT" ] ) ) )
   uWrite( '<br>' )
   IF hb_mutexLock( hmtxBusy )
      uWrite( '<br>Thread: ' + Str( s_nThreads ) )
      uWrite( '<br>Connections: ' + Str( s_nConnections ) )
      uWrite( '<br>Max Connections: ' + Str( s_nMaxConnections ) )
      uWrite( '<br>Total Connections: ' + Str( s_nTotConnections ) )
      uWrite( '<br>Running Thread: ' + hb_ValToExp( s_aRunningThreads ) )

      uWrite( '<br>Service Thread: ' + Str( s_nServiceThreads ) )
      uWrite( '<br>Service Connections: ' + Str( s_nServiceConnections ) )
      uWrite( '<br>Max Service Connections: ' + Str( s_nMaxServiceConnections ) )
      uWrite( '<br>Total Service Connections: ' + Str( s_nTotServiceConnections ) )
      uWrite( '<br>Service Thread: ' + hb_ValToExp( s_aServiceThreads ) )
      hb_mutexUnlock( hmtxBusy )
   ENDIF
   uWrite( '<br>Time: ' + Time() )

   //uWrite( '</table>')
   uWrite( "<hr></pre></body></html>" )

RETURN

STATIC PROCEDURE ShowFolder( cDir )
   LOCAL aDir, aF
   LOCAL cParentDir, nPos

   uAddHeader( "Content-Type", "text/html" )

   aDir := DIRECTORY( uOSFileName( cDir ), "D" )
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

   uWrite( '<html><body><h1>Index of ' + _SERVER[ "SCRIPT_NAME" ] + '</h1><pre>      ')
   uWrite( '<a href="?s=n">Name</a>                                                  ')
   uWrite( '<a href="?s=m">Modified</a>             ' )
   uWrite( '<a href="?s=s">Size</a>' + CR_LF + '<hr>' )

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
          uWrite( '[DIR] <a href="' + cParentDir + '">..</a>' + ;
                  CR_LF )
       ELSEIF LEFT( aF[ 1 ], 1 ) == "."
       ELSEIF "D" $ aF[ 5 ]
          uWrite( '[DIR] <a href="' + aF[ 1 ] + '/">'+ aF[ 1 ] + '</a>' + SPACE( 50 - LEN( aF[ 1 ] ) ) + ;
                  DTOC( aF[ 3 ] ) + ' ' + aF[ 4 ] + CR_LF )
       ELSE
          uWrite( '      <a href="' + aF[ 1 ] + '">'+ aF[ 1 ] + '</a>' + SPACE( 50 - LEN( aF[ 1 ] ) ) + ;
                  DTOC( aF[ 3 ]) + ' ' + aF[ 4 ] + STR( aF[ 2 ], 12 ) + CR_LF )
       ENDIF
   NEXT
   uWrite( "<hr></pre></body></html>" )

RETURN

// ------------------------------- Utility functions --------------------------------

STATIC PROCEDURE Help()
   LOCAL cPrg := hb_argv( 0 )
   LOCAL nPos := RAt( "\", cPrg )
   //__OutDebug( hb_argv(0) )
   IF nPos > 0
      cPrg := SubStr( cPrg, nPos + 1 )
   ENDIF
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
   LOCAL cPath := Exe_FullPath()
   LOCAL nPos  := RAt( "\", cPath )
   IF nPos == 0
      cPath := ""
   ELSE
      cPath := SubStr( cPath, 1, nPos-1 )
   ENDIF
RETURN cPath

STATIC FUNCTION Exe_Name()
   LOCAL cPrg := Exe_FullPath()
   LOCAL nPos := RAt( "\", cPrg )
   IF nPos > 0
      cPrg := SubStr( cPrg, nPos+1 )
   ENDIF
RETURN cPrg

STATIC PROCEDURE Progress( nProgress )
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

   hb_dispOutAt( 10,  5, cString )
   hb_dispOutAt(  0, 60, "Time: " + Time() )

RETURN

// Show messages in console
#define CONSOLE_FIRSTROW   12
#define CONSOLE_LASTROW    MaxRow()
STATIC PROCEDURE WriteToConsole( ... )
   LOCAL cMsg

   IF hb_mutexLock( hmtxConsole )
      IF s_lConsole

         FOR EACH cMsg IN hb_aParams()

             Scroll( CONSOLE_FIRSTROW, 0, CONSOLE_LASTROW, MaxCol(), -1 )
             DispOutAt( CONSOLE_FIRSTROW, 0, PadR( "> " + hb_cStr( cMsg ), MaxCol() ) )

             hb_ToOutDebug( ">>> %s\n\r", cMsg )

         NEXT

      ENDIF
      hb_mutexUnlock( hmtxConsole )
   ENDIF

RETURN



//------------------------------------------------------------------------------
// FUNZIONI C
//------------------------------------------------------------------------------
#PRAGMA BEGINDUMP

#ifdef __WIN32__

#include <windows.h>
#include "hbapi.h"
#include "hbvm.h"

HB_FUNC_STATIC( EXE_FULLPATH )
{
  char szPath[512];

  if ( !(GetModuleFileName( NULL, szPath, 512) == 0) )
     hb_retc( szPath );

}

BOOL win_SysRefresh( int iMsec )
{
   int iQuit = (int) FALSE;

   HANDLE hDummyEvent = CreateEvent(NULL, FALSE, FALSE, NULL);

   // Begin the operation and continue until it is complete
   // or until the user clicks the mouse or presses a key.

   while (MsgWaitForMultipleObjects(1, &hDummyEvent, FALSE, ( iMsec == 0, INFINITE, iMsec ), QS_ALLINPUT | QS_ALLPOSTMESSAGE) == WAIT_OBJECT_0 + 1)
   {
      MSG msg;

      while (PeekMessage(&msg, NULL,  0, 0, PM_REMOVE))
      {

         switch(msg.message)
         {
             case WM_QUIT:
             {
                  iQuit = (int) msg.wParam;
                  goto stopLoop;
             }
             //case WM_LBUTTONDOWN:
             //case WM_RBUTTONDOWN:
             //case WM_KEYDOWN:
             //case WM_LBUTTONUP:
             //case WM_RBUTTONUP:
             //case WM_KEYUP:
             //    //
             //    // Perform any required cleanup.
             //    //
             //    break;
             //    //exit;
             //
             default:
                TranslateMessage(&msg);
                DispatchMessage(&msg);
         }

      }
      if (!iQuit)
      {
         goto stopLoop;
      }
   }

stopLoop:

   CloseHandle( hDummyEvent );

   return iQuit;

}

HB_FUNC_STATIC( WIN_SYSREFRESH )
{
   hb_retl( win_SysRefresh( ( ISNIL( 1 ) ? 0 : hb_parni( 1 ) ) ) );
}

HB_FUNC_STATIC( WIN_SETENV )
{
   hb_retl( SetEnvironmentVariable( hb_parc( 1 ), hb_parc( 2 ) ) );
}

HB_FUNC_STATIC( WIN_TIMEZONEBIAS )
{
   TIME_ZONE_INFORMATION tzInfo;
   //LONG lBias;
   int nLen;
   char *szRet = (char *) hb_xgrab( 6 );

   if ( GetTimeZoneInformation( &tzInfo ) == TIME_ZONE_ID_INVALID )
   {
      tzInfo.Bias = 0;
   }
   else
   {
      tzInfo.Bias = -tzInfo.Bias;
   }

   hb_snprintf( szRet, 6,  "%+03d%02d",
             (int)( tzInfo.Bias / 60 ),
             (int)( tzInfo.Bias % 60 > 0 ? - tzInfo.Bias % 60 : tzInfo.Bias % 60 ) );

   nLen = strlen( szRet );

   if ( nLen < 6 )
   {
      szRet = (char *) hb_xrealloc( szRet, nLen + 1 );
   }
   hb_retclen_buffer( szRet, nLen );

}

#endif
#PRAGMA ENDDUMP
