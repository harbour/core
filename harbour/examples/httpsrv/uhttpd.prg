/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    uHTTPD (Micro HTTP server)
 *
 * Copyright 2009 Francesco Saverio Giudice <info / at / fsgiudice.com>
 * Copyright 2008 Mindaugas Kavaliauskas (dbtopas at dbtopas.lt)
 * www - http://harbour-project.org
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
      - fix dynamic threads (now locked to fixed number)
      - add full mime type handling
      - add cgi exec on linux
      - add .htaccess support
      - fix binding to address

*/

// remove comment to activate hb_toOutDebug()
//#define DEBUG_ACTIVE

#define FIXED_THREADS         // This force application to use fixed number of running threads and no service threads

#include "fileio.ch"
#include "common.ch"
#include "inkey.ch"
#include "error.ch"
#include "hbmemory.ch"
#include "hbgtinfo.ch"

REQUEST __HB_EXTERN__

#include "hbsocket.ch"

#ifdef GD_SUPPORT
   // adding GD support
   REQUEST GDIMAGE, GDIMAGECHAR, GDCHART
   #define APP_GD_SUPPORT "_GD"
   #stdout "Lib GD support enabled"
#else
   #define APP_GD_SUPPORT ""
   #stdout "Lib GD support disabled"
#endif

#ifdef FIXED_THREADS
   #define APP_DT_SUPPORT "_FIXED_THREADS"
   #stdout "Fixed # of threads"
#else
   #define APP_DT_SUPPORT ""
   #stdout "Dynamic # of threads"
#endif

#define APP_NAME      "uhttpd"
#define APP_VER_NUM   "0.4.4"
#define APP_VERSION   APP_VER_NUM + APP_GD_SUPPORT + APP_DT_SUPPORT

// default values - they can changes using line command switch or ini file

#define START_RUNNING_THREADS   6             // Start threads to serve connections
#define MAX_RUNNING_THREADS    20             // Max running threads

#define START_SERVICE_THREADS   0             // Initial number for service connections
#define MAX_SERVICE_THREADS     3             // Max running threads

#define LISTEN_PORT             8082          // differs from standard 80 port for tests in case
                                              // anyone has a apache/IIS installed
#define FILE_STOP               ".uhttpd.stop"
#define FILE_ACCESS_LOG         "logs" + hb_ps() + "access.log"
#define FILE_ERROR_LOG          "logs" + hb_ps() + "error.log"
#define DIRECTORYINDEX_ARRAY    { "index.html", "index.htm" }

#define PAGE_STATUS_REFRESH   5
#define THREAD_MAX_WAIT     ( 30 ) // How much time thread has to wait a new connection before finish - IN SECONDS
#define CGI_MAX_EXEC_TIME     30   // in seconds

// TOCHECK: Caching of HRB modules (Is this faster than loading HRBBody from file where OS will cache ?)
#define HRB_ACTIVATE_CACHE   .F.   // if .T. caching of HRB modules will be enabled. (NOTE: changes of files will not be loaded until server is active)

#define CR_LF    (CHR(13)+CHR(10))
#define HB_IHASH()   HB_HSETCASEMATCH( {=>}, .F. )

#ifdef __PLATFORM__WINDOWS
   REQUEST HB_GT_WVT_DEFAULT
   REQUEST HB_GT_WIN
   REQUEST HB_GT_NUL
#else
   REQUEST HB_GT_TRM_DEFAULT
   REQUEST HB_GT_NUL
#endif

#define THREAD_GT hb_gtVersion()

// dynamic call for HRB support
DYNAMIC HRBMAIN

STATIC s_lQuitRequest := .F.

STATIC s_hmtxQueue, s_hmtxServiceThreads, s_hmtxRunningThreads, s_hmtxLog, s_hmtxConsole, s_hmtxBusy
STATIC s_hmtxHRB

STATIC s_hfileLogAccess, s_hfileLogError, s_cApplicationRoot, s_cDocumentRoot, s_lIndexes, s_lConsole, s_nPort
STATIC s_cSessionPath
STATIC s_nThreads, s_nStartThreads, s_nMaxThreads
STATIC s_nServiceThreads, s_nStartServiceThreads, s_nMaxServiceThreads
STATIC s_nConnections, s_nMaxConnections, s_nTotConnections
STATIC s_nServiceConnections, s_nMaxServiceConnections, s_nTotServiceConnections
STATIC s_aRunningThreads := {}
STATIC s_aServiceThreads := {}
STATIC s_hHRBModules     := {=>}
STATIC s_aDirectoryIndex

STATIC s_hActions          := { ;
                                /*"default-handler" => @Handler_Default()      ,*/;    // default handler
                                /*"send-as-is"      => @Handler_SendAsIs()     ,*/;
                                "cgi-script"      => @Handler_CgiScript()    ,;
                                "hrb-script"      => @Handler_HrbScript()    ,;
                                /*"server-info"     => @Handler_ServerInfo()   ,*/;
                                "server-status"   => @Handler_ServerStatus()  ;
                              }

STATIC s_hHandlers         := { ;
                                "hrb"            => "hrb-script"             ,;
                                "exe"            => "cgi-script"             ,;
                                "/serverstatus"  => "server-status"           ;
                              }

//STATIC s_lAcceptPathInfo   := .T.

// SCRIPTALIASES: now read from ini file
//STATIC s_hScriptAliases    := { "/info" => "/cgi-bin/info.hrb" }
STATIC s_hScriptAliases    := { => }
STATIC s_hAliases          := { => }

THREAD STATIC t_cResult, t_nStatusCode, /*t_aHeader,*/ t_cErrorMsg, t_oSession

MEMVAR _SERVER, _GET, _POST, _COOKIE, _SESSION, _REQUEST, _HTTP_REQUEST, _HTTP_RESPONSE, m_cPost

ANNOUNCE ERRORSYS


// ----------------------------------------
//
//               M A I N
//
// ----------------------------------------

FUNCTION MAIN( ... )
   LOCAL nPort, hListen, hSocket, aRemote, cI, xVal
   LOCAL aThreads, nStartThreads, nMaxThreads, nStartServiceThreads
   LOCAL i, cPar, lStop
   LOCAL cGT, cApplicationRoot, cDocumentRoot, lIndexes, cConfig
   LOCAL lConsole, lScriptAliasMixedCase, aDirectoryIndex
   LOCAL nProgress := 0
   LOCAL hDefault, cLogAccess, cLogError, cSessionPath
   LOCAL cCmdPort, cCmdApplicationRoot, cCmdDocumentRoot, lCmdIndexes, nCmdStartThreads, nCmdMaxThreads
   LOCAL nConsoleRows, nConsoleCols
   LOCAL nCmdConsoleRows, nCmdConsoleCols

   IF !HB_MTVM()
      ? "I need multhread support. Please, recompile me!"
      WAIT
      RETURN 2
   ENDIF

   // ----------------------- Initializations ---------------------------------

   SysSettings()

   ErrorBlock( {| oError | uhttpd_DefError( oError ) } )

   // ----------------------- Parameters defaults -----------------------------

   // defaults not changeble via ini file
   lStop                := .F.
   cConfig              := EXE_Path() + hb_ps() + APP_NAME + ".ini"
   lConsole             := .T.
   nStartServiceThreads := START_SERVICE_THREADS

   // Check GT version - if I have started app with //GT:NUL then I have to disable
   // console and application will start in hidden way.
   cGT := HB_GTVERSION()
   IF ( cGT == "NUL" )
      lConsole := .F.
   ELSE
      hb_gtInfo( HB_GTI_NOTIFIERBLOCK, {|nEvent, ...| GT_notifier( nEvent, ... ) } )
   ENDIF

   // TOCHECK: now not force case insensitive
   //HB_HSETCASEMATCH( s_hScriptAliases, .F. )

   // ----------------- Line command parameters checking ----------------------

   i := 1
   DO WHILE ( i <= PCount() )

      cPar := hb_PValue( i++ )

      DO CASE
      CASE cPar == "--port"             .OR. cPar == "-p"
         cCmdPort    := hb_PValue( i++ )

      CASE cPar == "--approot"          .OR. cPar == "-a"
         cCmdApplicationRoot := hb_PValue( i++ )

      CASE cPar == "--docroot"          .OR. cPar == "-d"
         cCmdDocumentRoot := hb_PValue( i++ )

      CASE cPar == "--indexes"          .OR. cPar == "-i"
         lCmdIndexes := .T.

      CASE cPar == "--stop"             .OR. cPar == "-s"
         lStop    := .T.

      CASE cPar == "--config"           .OR. cPar == "-c"
         cConfig     := hb_PValue( i++ )

      CASE cPar == "--start-threads"    .OR. cPar == "-ts"
         nCmdStartThreads := Val( hb_PValue( i++ ) )

      CASE cPar == "--max-threads"      .OR. cPar == "-tm"
         nCmdMaxThreads := Val( hb_PValue( i++ ) )

      CASE cPar == "--console-rows"     .OR. cPar == "-cr"
         nCmdConsoleRows := Val( hb_PValue( i++ ) )

      CASE cPar == "--console-cols"     .OR. cPar == "-cc"
         nCmdConsoleCols := Val( hb_PValue( i++ ) )

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
   cApplicationRoot      := hDefault[ "MAIN" ][ "APPLICATION_ROOT" ]
   cDocumentRoot         := hDefault[ "MAIN" ][ "DOCUMENT_ROOT" ]
   lIndexes              := hDefault[ "MAIN" ][ "SHOW_INDEXES" ]
   lScriptAliasMixedCase := hDefault[ "MAIN" ][ "SCRIPTALIASMIXEDCASE" ]
   cSessionPath          := hDefault[ "MAIN" ][ "SESSIONPATH" ]
   aDirectoryIndex       := hDefault[ "MAIN" ][ "DIRECTORYINDEX" ]
   nConsoleRows          := hDefault[ "MAIN" ][ "CONSOLE-ROWS" ]
   nConsoleCols          := hDefault[ "MAIN" ][ "CONSOLE-COLS" ]

   cLogAccess            := hDefault[ "LOGFILES" ][ "ACCESS" ]
   cLogError             := hDefault[ "LOGFILES" ][ "ERROR" ]

   nStartThreads         := hDefault[ "THREADS" ][ "START_NUM" ]
   nMaxThreads           := hDefault[ "THREADS" ][ "MAX_NUM" ]

   // ATTENTION: script aliases can be in mixed case
   // i.e. we can have /info or /Info that will be different unless lScriptAliasMixedCase will be .F.
   FOR EACH xVal IN hDefault[ "SCRIPTALIASES" ]
       IF HB_ISSTRING( xVal )
          hb_HSet( s_hScriptAliases, IIF( lScriptAliasMixedCase, xVal:__enumKey(), Upper( xVal:__enumKey() ) ), xVal )
       ENDIF
   NEXT

   // ATTENTION: path aliases cannnot be in mixed case
   // i.e. we can have /info or /Info that will be different
   FOR EACH xVal IN hDefault[ "ALIASES" ]
       IF HB_ISSTRING( xVal )
          hb_HSet( s_hAliases, xVal:__enumKey(), xVal )
       ENDIF
   NEXT

   //hb_ToOutDebug( "cLogAccess = %s, cLogError = %s\n\r", cLogAccess, cLogError )
   //hb_ToOutDebug( "hDefault = %s\n\r", hb_ValToExp( hDefault ) )
   //hb_ToOutDebug( "s_hScriptAliases = %s\n\r", hb_ValToExp( s_hScriptAliases ) )
   //hb_ToOutDebug( "s_hAliases = %s\n\r", hb_ValToExp( s_hAliases ) )

   // ------------------- Parameters forced from command line ----------------

   IF cCmdPort != NIL
      nPort := Val( cCmdPort )
   ENDIF

   IF cCmdApplicationRoot != NIL
      cApplicationRoot := cCmdApplicationRoot
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

   IF nCmdConsoleRows != NIL
      nConsoleRows := nCmdConsoleRows
   ENDIF

   IF nCmdConsoleCols != NIL
      nConsoleCols := nCmdConsoleCols
   ENDIF

   // -------------------- adjusting MACROS values ----------------------------

   // cApplicationRoot can be only ExePath() or a correct full path
   cDocumentRoot         := StrTran( cDocumentRoot, "$(APP_DIR)", cApplicationRoot )
   cSessionPath          := StrTran( cSessionPath , "$(APP_DIR)", cApplicationRoot )
   cLogAccess            := StrTran( cLogAccess   , "$(APP_DIR)", cApplicationRoot )
   cLogError             := StrTran( cLogError    , "$(APP_DIR)", cApplicationRoot )

   // -------------------- checking starting values ----------------------------

   IF nPort <= 0 .OR. nPort > 65535
      ? "Invalid port number:", nPort
      WAIT
      RETURN 1
   ENDIF

   IF HB_ISSTRING( cApplicationRoot )
      cI := cApplicationRoot
      IF HB_DirExists( cI )
         IF RIGHT( cI, 1 ) == "/" .AND. LEN(cI) > 2 .AND. SUBSTR( cI, LEN( cI ) - 2, 1 ) != ":"
           s_cApplicationRoot := LEFT( cI, LEN( cI ) - 1 )
         ELSE
           s_cApplicationRoot := cI
         ENDIF
      ELSE
         ? "Invalid application root:", cI
         WAIT
         RETURN 3
      ENDIF
   ELSE
      ? "Invalid application root"
      WAIT
      RETURN 3
   ENDIF

#ifdef DEBUG_ACTIVE
   hb_ToOutDebug( "s_cDocumentRoot = %s, cDocumentRoot = %s\n\r", s_cDocumentRoot, cDocumentRoot )
#endif

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

#ifdef DEBUG_ACTIVE
   hb_ToOutDebug( "s_cDocumentRoot = %s, cDocumentRoot = %s\n\r", s_cDocumentRoot, cDocumentRoot )
#endif

   IF nMaxThreads <= 0
      nMaxThreads := MAX_RUNNING_THREADS
   ENDIF

   IF nStartThreads < 0
      nStartThreads := 0
   ELSEIF nStartThreads > nMaxThreads
      nStartThreads := nMaxThreads
   ENDIF

   IF nConsoleRows < 1 //.OR. nConsoleRows > MaxRow() + 1
      nConsoleRows := MaxRow()
   ENDIF

   IF nConsoleCols < 1 //.OR. nConsoleCols > MaxCol() + 1
      nConsoleCols := MaxCol()
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
   s_aDirectoryIndex        := aDirectoryIndex

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

   IF s_lConsole
      SET CURSOR OFF
      SetMode( nConsoleRows, nConsoleCols )
      //hb_toOutDebug( "nConsoleRows = %s, nConsoleCols = %s", nConsoleRows, nConsoleCols )
      //hb_toOutDebug( "nCmdConsoleRows = %s, nCmdConsoleCols = %s", nCmdConsoleRows, nCmdConsoleCols )
   ENDIF

   // --------------------- define mutexes -------------------------------------

   s_hmtxQueue          := hb_mutexCreate()
   s_hmtxLog            := hb_mutexCreate()
   s_hmtxConsole        := hb_mutexCreate()
   s_hmtxBusy           := hb_mutexCreate()
   s_hmtxRunningThreads := hb_mutexCreate()
   s_hmtxServiceThreads := hb_mutexCreate()
   s_hmtxHRB            := hb_mutexCreate()

   WriteToConsole( "--- Starting " + APP_NAME + " ---" )

   // --------------------------------------------------------------------------
   // SOCKET CREATION
   // --------------------------------------------------------------------------

   hListen := hb_socketOpen()
   IF ! hb_socketBind( hListen, { HB_SOCKET_AF_INET, "0.0.0.0", nPort } )
      ? "bind() error", hb_socketGetError()
   ELSEIF ! hb_socketListen( hListen )
      ? "listen() error", hb_socketGetError()
   ELSE
      // --------------------------------------------------------------------------------- //
      // Starting Accept connection thread
      // --------------------------------------------------------------------------------- //

      WriteToConsole( "Starting AcceptConnection Thread" )
      aThreads := {}
      AADD( aThreads, hb_threadStart( @AcceptConnections() ) )
#ifdef DEBUG_ACTIVE
      hb_ToOutDebug( "Len( aThreads ) = %i\n\r", Len( aThreads ) )
#endif

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

#ifdef __PLATFORM__WINDOWS
         // windows resource releasing - 1 millisecond wait
         IF WIN_SYSREFRESH( 1 ) != 0
            EXIT
         ENDIF
#endif
         IF hb_mutexLock( s_hmtxBusy )
            IF s_lQuitRequest
               hb_mutexUnlock( s_hmtxBusy )
               EXIT
            ENDIF
            hb_mutexUnlock( s_hmtxBusy )
         ENDIF

         IF s_lConsole

            // Show application infos
            IF hb_mutexLock( s_hmtxBusy )
               hb_DispOutAt(  5,  5, "Threads           : " + Transform( s_nThreads, "9999999999" ) )
               hb_DispOutAt(  6,  5, "Connections       : " + Transform( s_nConnections, "9999999999" ) )
               hb_DispOutAt(  7,  5, "Max Connections   : " + Transform( s_nMaxConnections, "9999999999" ) )
               hb_DispOutAt(  8,  5, "Total Connections : " + Transform( s_nTotConnections, "9999999999" ) )

#ifndef FIXED_THREADS
               hb_DispOutAt(  5, 37, "ServiceThreads    : " + Transform( s_nServiceThreads, "9999999999" ) )
               hb_DispOutAt(  6, 37, "Connections       : " + Transform( s_nServiceConnections, "9999999999" ) )
               hb_DispOutAt(  7, 37, "Max Connections   : " + Transform( s_nMaxServiceConnections, "9999999999" ) )
               hb_DispOutAt(  8, 37, "Total Connections : " + Transform( s_nTotServiceConnections, "9999999999" ) )
#endif // FIXED_THREADS
               hb_DispOutAt( 10, 40, "Memory: " + hb_ntos( memory( HB_MEM_USED ) ) )
               hb_mutexUnlock( s_hmtxBusy )
            ENDIF

            // Show progress
            Progress( @nProgress )
         ENDIF

         // Wait a connection
         IF EMPTY( hSocket := hb_socketAccept( hListen, @aRemote, 50 ) )
            IF hb_socketGetError() == HB_SOCKET_ERR_TIMEOUT
               // Checking if I have to quit
               IF HB_FileExists( FILE_STOP )
                  FERASE( FILE_STOP )
                  EXIT
               ENDIF
            ELSE
               WriteToConsole( hb_StrFormat( "accept() error: %s", hb_socketGetError() ) )
            ENDIF
         ELSE
            // Send accepted connection to AcceptConnections() thread
            hb_mutexNotify( s_hmtxQueue, hSocket )
         ENDIF

         // Memory release
         //hb_GCAll( .T. )

      ENDDO

      WriteToConsole( "Waiting threads" )
      // Send to threads that they have to stop
      AEVAL( aThreads, {|| hb_mutexNotify( s_hmtxQueue, NIL ) } )
      // Wait threads to end
      AEVAL( aThreads, {|h| hb_threadJoin( h ) } )

   ENDIF

   WriteToConsole( "--- Quitting " + APP_NAME + " ---" )

   // Close socket
   hb_socketClose( hListen )

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
   LOCAL n
#ifndef FIXED_THREADS
   LOCAL nConnections, nThreads, nMaxThreads
   LOCAL nServiceConnections, nServiceThreads, nMaxServiceThreads
   LOCAL lCanNotify
#endif
   LOCAL pThread
   LOCAL lQuitRequest := .F.

   ErrorBlock( {| oError | uhttpd_DefError( oError ) } )

   WriteToConsole( "Starting AcceptConnections()" )

   IF hb_mutexLock( s_hmtxBusy )
      // Starting initial running threads
      FOR n := 1 TO s_nStartThreads
          pThread := hb_threadStart( @ProcessConnection() )
          AADD( s_aRunningThreads, pThread )
      NEXT

      // Starting initial service threads
      FOR n := 1 TO s_nStartServiceThreads
          pThread := hb_threadStart( @ServiceConnection() )
          AADD( s_aServiceThreads, pThread )
      NEXT
      hb_mutexUnlock( s_hmtxBusy )
   ENDIF

   // Main AcceptConnections loop
   DO WHILE .T.

      // reset socket
      hSocket := NIL

#ifdef __PLATFORM__WINDOWS
      // releasing resources
      IF WIN_SYSREFRESH( 1 ) != 0
         lQuitRequest := .T.
      ENDIF
#endif

      IF hb_mutexLock( s_hmtxBusy )
         IF s_lQuitRequest
            hb_mutexUnlock( s_hmtxBusy )
            lQuitRequest := .T.
         ENDIF
         hb_mutexUnlock( s_hmtxBusy )
      ENDIF

      // Waiting a connection from main application loop
      IF !lQuitRequest
         hb_mutexSubscribe( s_hmtxQueue,, @hSocket )
      ENDIF

      // I have a QUIT request
      IF hSocket == NIL .OR. lQuitRequest

         // Requesting to Running threads to quit (using -1 value)
         AEVAL( s_aRunningThreads, {|| hb_mutexNotify( s_hmtxRunningThreads, -1 ) } )
#ifndef FIXED_THREADS
         // Requesting to Service threads to quit (using -1 value)
         AEVAL( s_aServiceThreads, {|| hb_mutexNotify( s_hmtxServiceThreads, -1 ) } )
#endif
         // waiting running threads to quit
         AEVAL( s_aRunningThreads, {|h| hb_threadJoin( h ) } )
#ifndef FIXED_THREADS
         // waiting service threads to quit
         AEVAL( s_aServiceThreads, {|h| hb_threadJoin( h ) } )
#endif
         IF hb_mutexLock( s_hmtxBusy )
            //hb_ToOutDebug( "Len( s_aRunningThreads ) = %i\n\r", Len( s_aRunningThreads ) )
            asize( s_aRunningThreads, 0 )
#ifndef FIXED_THREADS
            asize( s_aServiceThreads, 0 )
#endif
            hb_mutexUnlock( s_hmtxBusy )
         ENDIF

         EXIT
      ENDIF

#ifndef FIXED_THREADS
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

      lCanNotify := .F.

      // If I have no more running threads to use ...
      IF nConnections > nMaxThreads

         // If I have no more of service threads to use ... (DOS attack ?)
         IF nServiceConnections > nMaxServiceThreads
             // DROP connection
            hb_socketShutdown( hSocket )
            hb_socketClose( hSocket )

         // If I have no service threads in use ...
         ELSEIF nServiceConnections >= nServiceThreads
            // Add one more
            IF hb_mutexLock( s_hmtxBusy )
               pThread := hb_threadStart( @ServiceConnection() )
               AADD( s_aServiceThreads, pThread )
               lCanNotify := .T.
               hb_mutexUnlock( s_hmtxBusy )
            ENDIF
         ENDIF

         // Otherwise I send connection to current service thread queue
         IF lCanNotify
            hb_mutexNotify( s_hmtxServiceThreads, hSocket )
         ENDIF

         LOOP

      // If I have no free running threads to use ...
      ELSEIF nConnections >= nThreads
         // Add one more
         IF hb_mutexLock( s_hmtxBusy )
            pThread := hb_threadStart( @ProcessConnection() )
            AADD( s_aRunningThreads, pThread )
            lCanNotify := .T.
            hb_mutexUnlock( s_hmtxBusy )
         ENDIF
      ELSE
         lCanNotify := .T.
      ENDIF
      // Otherwise I send connection to running thread queue
      //hb_ToOutDebug( "Len( s_aRunningThreads ) = %i\n\r", Len( s_aRunningThreads ) )
      IF lCanNotify
#endif // FIXED_THREADS
         hb_mutexNotify( s_hmtxRunningThreads, hSocket )
#ifndef FIXED_THREADS
      ENDIF
#endif // FIXED_THREADS

   ENDDO

   WriteToConsole( "Quitting AcceptConnections()" )

   RETURN 0

// --------------------------------------------------------------------------------- //
// CONNECTIONS
// --------------------------------------------------------------------------------- //
STATIC FUNCTION ProcessConnection()
   LOCAL hSocket, nLen, cRequest, cSend
   LOCAL nMsecs, nParseTime, nPos, nThreadID
   LOCAL lQuitRequest := .F.

   PRIVATE _SERVER, _GET, _POST, _COOKIE, _SESSION, _REQUEST, _HTTP_REQUEST, _HTTP_RESPONSE, m_cPost

   nThreadId    := hb_threadID()

#ifdef DEBUG_ACTIVE
   hb_ToOutDebug( "nThreadId = %s\r\n", nThreadId )
#endif

   ErrorBlock( {| oError | uhttpd_DefError( oError ) } )

   WriteToConsole( "Starting ProcessConnections() " + hb_CStr( nThreadID ) )

   IF hb_mutexLock( s_hmtxBusy )
      s_nThreads++
      hb_mutexUnlock( s_hmtxBusy )
   ENDIF

   // ProcessConnection Loop
   DO WHILE .T.

      // Reset socket
      hSocket := NIL

#ifdef __PLATFORM__WINDOWS
      // releasing resources
      IF WIN_SYSREFRESH( 1 ) != 0
         lQuitRequest := .T.
         EXIT
      ENDIF
#endif

      IF hb_mutexLock( s_hmtxBusy )
         IF s_lQuitRequest
            hb_mutexUnlock( s_hmtxBusy )
            lQuitRequest := .T.
            EXIT
         ENDIF
         hb_mutexUnlock( s_hmtxBusy )
      ENDIF

      // Waiting a connection from AcceptConnections() but up to defined time
      hb_mutexSubscribe( s_hmtxRunningThreads, THREAD_MAX_WAIT, @hSocket )

      // received a -1 value, I have to quit
      IF HB_ISNUMERIC( hSocket )
         lQuitRequest := .T.
         EXIT

      ELSEIF hSocket == NIL   // no socket received, thread can graceful quit, but ...
#ifndef FIXED_THREADS
         IF hb_mutexLock( s_hmtxBusy )
            // .. not if under minimal number of starting threads
            IF s_nThreads <= s_nStartThreads
               hb_mutexUnlock( s_hmtxBusy )
               LOOP
            ENDIF
            hb_mutexUnlock( s_hmtxBusy )
         ENDIF
         EXIT
#else  // FIXED_THREADS
       LOOP
#endif // FIXED_THREADS
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

         cRequest := NIL

         /* receive query */
         nLen := readRequest( hSocket, @cRequest )

#ifdef DEBUG_ACTIVE
            hb_ToOutDebug( "cRequest -- BEGIN --\n\r%s\n\rcRequest -- END --\n\r", cRequest )
#endif

         IF nLen == -1
            ? "recv() error:", hb_socketGetError()

         ELSEIF nLen == 0 /* connection closed */
         ELSE

            //hb_ToOutDebug( "cRequest -- BEGIN --\n\r%s\n\rcRequest -- END --\n\r", cRequest )

            _SERVER := HB_IHASH(); _GET := HB_IHASH(); _POST := HB_IHASH(); _COOKIE := HB_IHASH()
            _SESSION := HB_IHASH(); _REQUEST := HB_IHASH(); _HTTP_REQUEST := HB_IHASH(); _HTTP_RESPONSE := HB_IHASH()
            m_cPost := NIL
            t_cResult     := ""
            //t_aHeader     := {}
            t_nStatusCode := 200
            t_cErrorMsg   := ""

            defineServer( hSocket )

            IF ParseRequest( cRequest )
               //hb_ToOutDebug( "_SERVER = %s,\n\r _GET = %s,\n\r _POST = %s,\n\r _REQUEST = %s,\n\r _HTTP_REQUEST = %s,\n\r _HTTP_RESPONSE = %s\n\r", hb_ValToExp( _SERVER ), hb_ValToExp( _GET ), hb_ValToExp( _POST ), hb_ValToExp( _REQUEST ), hb_ValToExp( _HTTP_REQUEST ), hb_ValToExp( _HTTP_RESPONSE ) )
               cSend := uproc_default()
            ELSE
               //uhttpd_SetStatusCode( 400 )
               cSend := MakeResponse()
            ENDIF

#ifdef DEBUG_ACTIVE
            hb_ToOutDebug( "cSend = %s\n\r", cSend )
#endif

            sendReply( hSocket, cSend )

            WriteToLog( cRequest )

            // Destroy PRIVATE VARIABLES
            _SERVER := _GET := _POST := _COOKIE := _SESSION := _REQUEST := _HTTP_REQUEST := _HTTP_RESPONSE := m_cPost := NIL

         ENDIF

      END SEQUENCE

      nParseTime := hb_milliseconds() - nMsecs
      WriteToConsole( "Page served in : " + Str( nParseTime/1000, 7, 4 ) + " seconds" )

      hb_socketShutdown( hSocket )
      hb_socketClose( hSocket )

      IF hb_mutexLock( s_hmtxBusy )
         s_nConnections--
         hb_mutexUnlock( s_hmtxBusy )
      ENDIF

      // Memory release
      hb_GCAll( .T. )

   ENDDO

   WriteToConsole( "Quitting ProcessConnections() " + hb_CStr( nThreadId ) )

   // Here I remove this thread from thread queue as it is unnecessary, but only if there is not
   // an external quit request. In this case application is quitting and I cannot resize array
   // here to avoid race condition
   IF !lQuitRequest .AND. hb_mutexLock( s_hmtxBusy )
      //hb_ToOutDebug( "Len( s_aRunningThreads ) = %i\n\r", Len( s_aRunningThreads ) )
      IF ( nPos := aScan( s_aRunningThreads, hb_threadSelf() ) > 0 )
         hb_aDel( s_aRunningThreads, nPos, .T. )
         s_nThreads := Len( s_aRunningThreads )
      ENDIF
      hb_mutexUnlock( s_hmtxBusy )
   ENDIF

   RETURN 0

STATIC FUNCTION ServiceConnection()
   LOCAL hSocket, nLen, cRequest, cSend
   LOCAL nMsecs, nParseTime, nPos, nThreadId
   LOCAL nError := 500013
   LOCAL lQuitRequest := .F.

   PRIVATE _SERVER, _GET, _POST, _COOKIE, _SESSION, _REQUEST, _HTTP_REQUEST, _HTTP_RESPONSE, m_cPost

   ErrorBlock( {| oError | uhttpd_DefError( oError ) } )

   nThreadId := hb_threadID()

   WriteToConsole( "Starting ServiceConnections() " + hb_CStr( nThreadId ) )

   IF hb_mutexLock( s_hmtxBusy )
      s_nServiceThreads++
      hb_mutexUnlock( s_hmtxBusy )
   ENDIF

   DO WHILE .T.

      // Reset socket
      hSocket := NIL

#ifdef __PLATFORM__WINDOWS
      // releasing resources
      IF WIN_SYSREFRESH( 1 ) != 0
         lQuitRequest := .T.
         EXIT
      ENDIF
#endif

      IF hb_mutexLock( s_hmtxBusy )
         IF s_lQuitRequest
            hb_mutexUnlock( s_hmtxBusy )
            lQuitRequest := .T.
            EXIT
         ENDIF
         hb_mutexUnlock( s_hmtxBusy )
      ENDIF

      // Waiting a connection from AcceptConnections() but up to defined time
      hb_mutexSubscribe( s_hmtxServiceThreads, THREAD_MAX_WAIT, @hSocket )

      // received a -1 value, I have to quit
      IF HB_ISNUMERIC( hSocket )
         lQuitRequest := .T.
         EXIT
      ELSEIF hSocket == NIL   // no socket received, thread can graceful quit, but ...
         IF hb_mutexLock( s_hmtxBusy )
            // .. not if under minimal number of starting threads
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
            ? "recv() error:", hb_socketGetError()
         ELSEIF nLen == 0 /* connection closed */
         ELSE

            //hb_ToOutDebug( "cRequest -- INIZIO --\n\r%s\n\rcRequest -- FINE --\n\r", cRequest )

            _SERVER := HB_IHASH(); _GET := HB_IHASH(); _POST := HB_IHASH(); _COOKIE := HB_IHASH()
            _SESSION := HB_IHASH(); _REQUEST := HB_IHASH(); _HTTP_REQUEST := HB_IHASH(); _HTTP_RESPONSE := HB_IHASH()
            m_cPost := NIL
            t_cResult     := ""
            //t_aHeader     := {}
            t_nStatusCode := 200
            t_cErrorMsg   := ""

            defineServer( hSocket )

            IF ParseRequest( cRequest )
               //hb_ToOutDebug( "_SERVER = %s,\n\r _GET = %s,\n\r _POST = %s,\n\r _REQUEST = %s,\n\r _HTTP_REQUEST = %s,\n\r _HTTP_RESPONSE = %s\n\r", hb_ValToExp( _SERVER ), hb_ValToExp( _GET ), hb_ValToExp( _POST ), hb_ValToExp( _REQUEST ), hb_ValToExp( _HTTP_REQUEST ), hb_ValToExp( _HTTP_RESPONSE ) )
               define_Env( _SERVER )
            ENDIF
            // Error page served
            uhttpd_SetStatusCode( nError )
            cSend := MakeResponse()

            sendReply( hSocket, cSend )

            WriteToLog( cRequest )

            // Destroy PRIVATE VARIABLES
            _SERVER := _GET := _POST := _COOKIE := _SESSION := _REQUEST := _HTTP_REQUEST := _HTTP_RESPONSE := m_cPost := NIL

         ENDIF

      END SEQUENCE

      nParseTime := hb_milliseconds() - nMsecs
      WriteToConsole( "Page served in : " + Str( nParseTime/1000, 7, 4 ) + " seconds" )

      hb_socketShutdown( hSocket )
      hb_socketClose( hSocket )

      IF hb_mutexLock( s_hmtxBusy )
         s_nServiceConnections--
         hb_mutexUnlock( s_hmtxBusy )
      ENDIF

      // Memory release
      hb_GCAll( .T. )

   ENDDO

   WriteToConsole( "Quitting ServiceConnections() " + hb_CStr( nThreadId ) )

   // Here I remove this thread from thread queue as it is unnecessary, but only if there is not
   // an external quit request. In this case application is quitting and I cannot resize array
   // here to avoid race condition
   IF !lQuitRequest .AND. hb_mutexLock( s_hmtxBusy )
      IF ( nPos := aScan( s_aServiceThreads, hb_threadSelf() ) > 0 )
         hb_aDel( s_aServiceThreads, nPos, .T. )
         s_nServiceThreads := Len( s_aServiceThreads )
      ENDIF
      hb_mutexUnlock( s_hmtxBusy )
   ENDIF

   RETURN 0

STATIC FUNCTION ParseRequest( cRequest )
   LOCAL aRequest, aLine, nI, nJ, cI
   LOCAL cReq, aVal, cFields, hVars
   LOCAL hUrl

   // RFC2616
   aRequest := uhttpd_split( CR_LF, cRequest )

#ifdef DEBUG_ACTIVE
   hb_ToOutDebug( "aRequest = %s\n\r", hb_ValToExp( aRequest ) )
#endif

   WriteToConsole( aRequest[1] )
   aLine := uhttpd_split( " ", aRequest[1] )
   IF LEN( aLine ) != 3 .OR. ;
      ( aLine[1] != "GET" .AND. aLine[1] != "POST" ) .OR. ; // Sorry, we support GET and POST only
      LEFT( aLine[3], 5 ) != "HTTP/"
      // Set status code
      t_nStatusCode := 501 // Not Implemented
      RETURN .F.
   ENDIF

   // define _SERVER var
   _SERVER[ "REQUEST_METHOD"  ] := aLine[1]
   _SERVER[ "REQUEST_URI"     ] := aLine[2]
   _SERVER[ "SERVER_PROTOCOL" ] := aLine[3]

   hUrl := uhttpd_SplitUrl( _SERVER[ "REQUEST_URI" ] )

   _SERVER[ "SCRIPT_NAME"  ] := hUrl[ "URI" ]
   _SERVER[ "QUERY_STRING" ] := hUrl[ "QUERY" ]

   /*
   IF ( nI := AT( "?", _SERVER[ "REQUEST_URI" ] ) ) > 0
      _SERVER[ "SCRIPT_NAME"  ] := LEFT( _SERVER[ "REQUEST_URI" ], nI - 1)
      _SERVER[ "QUERY_STRING" ] := SUBSTR( _SERVER[ "REQUEST_URI" ], nI + 1)
   ELSE
      _SERVER[ "SCRIPT_NAME"  ] := _SERVER[ "REQUEST_URI" ]
      _SERVER[ "QUERY_STRING" ] := ""
   ENDIF
   */

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
             //aVal := uhttpd_split( ":", aRequest[ nI ] )
             //_SERVER[ "HTTP_" + STRTRAN( UPPER( aVal[ 1 ] ), "-", "_")] := AllTrim( aVal[ 2 ] )
             _SERVER[ "HTTP_" + STRTRAN( UPPER( LEFT( aRequest[nI], nJ - 1 ) ), "-", "_" ) ] := cI
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

   // check if Host field is provided
   IF hb_HPos( _HTTP_REQUEST, "Host" ) == 0

      // Try to determine Host name
      IF !Empty( hUrl[ "HOST" ] )
         _HTTP_REQUEST[ "Host" ] := hUrl[ "HOST" ]
      ELSE
         _HTTP_REQUEST[ "Host" ] := ""
         // Set status code
         t_nStatusCode := 400 // Bad Request
         RETURN .F.
      ENDIF

   ENDIF

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


   // define _HTTP_RESPONSE
   _HTTP_RESPONSE[ "X-Powered-By"      ] := Version()
   _HTTP_RESPONSE[ "Connection"        ] := "Close"
   _HTTP_RESPONSE[ "Content-Type"      ] := "text/html; charset=UTF-8"
   _HTTP_RESPONSE[ "Server"            ] := APP_NAME + " " + APP_VERSION
   //_HTTP_RESPONSE[ "Transfer-Encoding" ] := "chunked"

   // Complete _SERVER
   _SERVER[ "SERVER_NAME"       ] := uhttpd_split( ":", _HTTP_REQUEST[ "HOST" ], 1 )[ 1 ]
   _SERVER[ "SCRIPT_FILENAME"   ] := STRTRAN( STRTRAN( _SERVER[ "DOCUMENT_ROOT" ] + _SERVER[ "SCRIPT_NAME" ], "//", "/" ), "\", "/" )
   _SERVER[ "SCRIPT_URL"        ] := _SERVER[ "SCRIPT_NAME" ]
   _SERVER[ "SCRIPT_URI"        ] := "http://" + _HTTP_REQUEST[ "HOST" ] + _SERVER[ "SCRIPT_NAME" ]

#ifdef DEBUG_ACTIVE
   hb_ToOutDebug( "_SERVER = %s\n\r", hb_ValToExp( _SERVER ) )
   hb_ToOutDebug( "_GET = %s\n\r", hb_ValToExp( _GET ) )
   hb_ToOutDebug( "_POST = %s\n\r", hb_ValToExp( _POST ) )
   hb_ToOutDebug( "_COOKIE = %s\n\r", hb_ValToExp( _COOKIE ) )
   hb_ToOutDebug( "_SESSION = %s\n\r", hb_ValToExp( _SESSION ) )
   hb_ToOutDebug( "_HTTP_REQUEST = %s\n\r", hb_ValToExp( _HTTP_REQUEST ) )
   hb_ToOutDebug( "_HTTP_RESPONSE = %s\n\r", hb_ValToExp( _HTTP_RESPONSE ) )
#endif

   // After defined all SERVER vars we can define a session
   // SESSION - sessions ID is stored as a cookie value, normally as SESSIONID var name (this can be user defined)
   t_oSession := uhttpd_SessionNew( "UHTTPD-SESSION", s_cSessionPath )
   t_oSession:Start()

   RETURN .T.


STATIC FUNCTION MakeResponse()
   LOCAL cRet, cReturnCode, v

   //uhttpd_SetHeader( "X-Powered-By", Version() )

   //uhttpd_SetHeader( "Connection", "close" )

   IF uhttpd_GetHeader( "Location" ) != NIL
      t_nStatusCode := 301
   ENDIF
   IF uhttpd_GetHeader( "Content-Type" ) == NIL
      uhttpd_SetHeader( "Content-Type", "text/html" )
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
     CASE 405
     CASE 500
     CASE 501
     CASE 502
     CASE 503
     CASE 504
     CASE 505
          t_cResult := "<html><body><h1>" + cReturnCode + "</h1></body></html>"
          EXIT

     // extended error messages - from Microsoft IIS Server
     CASE 500013 // error: 500-13 Server too busy
          uhttpd_SetHeader( "Retry-After", "60" )  // retry after 60 seconds
          t_cResult := "<html><body><h1>500 Server Too Busy</h1></body></html>"
          EXIT

     CASE 500100 // error: 500-100 Undeclared Variable

     OTHERWISE
          cReturnCode := "403 Forbidden"
          t_cResult := "<html><body><h1>" + cReturnCode + "</h1></body></html>"
   ENDSWITCH

   //hb_ToOutDebug( "_SESSION = %s\n\r", hb_ValToExp( _SESSION ) )

   // Close session - Autodestructor will NOT close it, because t_oSession is destroyed only at end of Thread

   IF HB_ISOBJECT( t_oSession )
      t_oSession:Close()
   ENDIF
   // t_oSession := NIL

   WriteToConsole( cReturnCode )
   cRet += cReturnCode + CR_LF

   FOR EACH v IN _HTTP_RESPONSE
       cRet += v:__enumKey() + ": " + v + CR_LF
   NEXT

   //AEVAL( t_aHeader, {|x| cRet += x[1] + ": " + x[2] + CR_LF } )
   cRet += CR_LF
   cRet += t_cResult

   //hb_ToOutDebug( "_HTTP_RESPONSE = %s\n\rcRet = %s\n\r", hb_ValToExp( _HTTP_RESPONSE ), cRet )

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
     CASE 405
          cReturnCode := "405 Method Not Allowed"
          EXIT
     CASE 500
          cReturnCode := "500 Internal Server Error"
          EXIT
     CASE 501
          cReturnCode := "501 Not Implemented"
          EXIT
     CASE 502
          cReturnCode := "502 Bad Gateway"
          EXIT
     CASE 503
          cReturnCode := "503 Service Unavailable"
          EXIT
     CASE 504
          cReturnCode := "504 Gateway Timeout"
          EXIT
     CASE 505
          cReturnCode := "505 HTTP Version Not Supported"
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
      cBias    := uhttpd_UTCOffset()

      cAccess := _SERVER[ "REMOTE_ADDR" ] + " - - [" + RIGHT( cDate, 2 ) + "/" + ;
                       aMonths[ VAL( SUBSTR( cDate, 5, 2 ) ) ] + ;
                       "/" + LEFT( cDate, 4 ) + ":" + cTime + ' ' + cBias + '] "' + ;
                       LEFT( cRequest, AT( CR_LF, cRequest ) - 1 ) + '" ' + ;
                       LTRIM( STR( t_nStatusCode ) ) + " " + IIF( nSize == 0, "-", LTRIM( STR( nSize ) ) ) + ;
                       ' "' + IIF( Empty( cReferer ), "-", cReferer ) + '" "' + _SERVER[ "HTTP_USER_AGENT" ] + ;
                       '"' + hb_eol()

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
                   cErrorMsg + hb_eol()

         //hb_ToOutDebug( "ErrorLog = %s \n\r", cError )

         FWRITE( s_hfileLogError, cError )
      ENDIF

      hb_mutexUnlock( s_hmtxLog )
   ENDIF

   RETURN

STATIC FUNCTION CGIExec( cProc, /*@*/ cOutPut )
   LOCAL hIn, hOut
   LOCAL cData, nLen, cSend, v
   LOCAL nErrorLevel := 0, nKillExit := 0
   LOCAL pThread
   LOCAL hProc
   LOCAL hmtxCGIKill := hb_mutexCreate()
   LOCAL cCurPath
   //LOCAL cError


   IF HB_ISSTRING( cProc )

      //hb_toOutDebug( "Launching process: %s\n\r", cProc )
      // No hIn, hErr == hOut

      // save current directory
      cCurPath := hb_CurDrive() + hb_osDriveSeparator() + hb_ps() + CurDir()

      //hb_toOutDebug( "cCurPath: %s\n\r", cCurPath )

      // Change dir to document root
      DirChange( s_cDocumentRoot )

      //hb_toOutDebug( "New Path: %s\n\r", hb_CurDrive() + hb_osDriveSeparator() + hb_ps() + CurDir() )

      hProc := hb_processOpen( cProc, @hIn, @hOut, @hOut, .T. ) // .T. = Detached Process (Hide Window)

      // return to original folder
      DirChange( cCurPath )

      //hb_toOutDebug( "New 2 Path: %s\n\r", hb_CurDrive() + hb_osDriveSeparator() + hb_ps() + CurDir() )

      IF hProc > -1
         //hb_toOutDebug( "Process handler: %s\n\r", hProc )
         //hb_toOutDebug( "Error: %s\n\r", FError() )

         pThread := hb_threadStart( @CGIKill(), hProc, hmtxCGIKill )

         // Sending POST variables to CGI via STD_IN
         cSend := ""
         FOR EACH v IN _POST
             cSend += v:__enumKey() + "=" + LTrim( hb_cStr( v ) ) + IIF( v:__enumIndex() < Len( _POST ), "&", "" )
         NEXT
         FWrite( hIn, cSend )
         //hb_toOutDebug( "Sending: %s\n\r", cSend )

         hb_mutexNotify( hmtxCGIKill, { hProc, .T. } )

         //hb_toOutDebug( "Reading output\n\r" )
         cData := Space( 1000 )
         cOutPut := ""
         DO WHILE ( nLen := Fread( hOut, @cData, Len( cData ) ) ) > 0
            cOutPut += SubStr( cData, 1, nLen )
            cData := Space( 1000 )
         ENDDO

         /*
         cData := Space( 1000 )
         cError := ""
         DO WHILE ( nLen := Fread( hErr, @cData, Len( cData ) ) ) > 0
            cError += SubStr( cData, 1, nLen )
            cData := Space( 1000 )
         ENDDO

         cOutPut += cError
         */

         //hb_toOutDebug( "Received: cOutPut = %s\n\r", cOutPut )

         //? "Waiting for process termination"
         // Return value
         nErrorLevel := HB_ProcessValue( hProc )

         //hb_toOutDebug( "CGIExec HB_ProcessValue nErrorLevel = %s\n\r", nErrorLevel )

         // Notify to CGIKill to terminate
         hb_mutexNotify( hmtxCGIKill, { hProc, .F. } )
         hb_threadJoin( pThread, @nKillExit )

         //hb_toOutDebug( "CGIExec quitting CGI, nErrorLevel = %s\n\r", nKillExit )
         IF nKillExit != 0
            // retrieving last command from
            nErrorLevel := nKillExit
         ENDIF

         FClose( hProc )
         FClose( hIn )
         FClose( hOut )
         //FClose( hErr )

         //hb_toOutDebug( "CGIExec closed handles\n\r" )

      ENDIF

   ELSE

      nErrorLevel := -1 // Error: cProc is not a valid string

   ENDIF

   hmtxCGIKill := NIL

   RETURN nErrorLevel

STATIC FUNCTION CGIKill( hProc, hmtxCGIKill )
   LOCAL lWait
   LOCAL nStartTime := hb_milliseconds()
   LOCAL nErrorLevel := 0
   LOCAL aValue, hRecProc
   LOCAL hCurProc := hProc

   //hb_toOutDebug( "CGIKill() Started. nStartTime = %s\n\r", nStartTime )

   // Kill process after MAX_PROCESS_EXEC_TIME
   DO WHILE .T.

      aValue := NIL
      lWait := NIL

      hb_mutexSubscribe( hmtxCGIKill, 1, @aValue ) // 10 seconds

      IF HB_ISARRAY( aValue )
         hRecProc := aValue[ 1 ]
         lWait    := aValue[ 2 ]
         // if Process requested is different from this, sending request again in the queue
         IF !( hRecProc == hCurProc )
            lWait := NIL
         ENDIF
      ENDIF

      //hb_toOutDebug( "CGIKill() lWait = %s, time := %s\n\r", lWait, hb_milliseconds() - nStartTime )

      IF HB_ISLOGICAL( lWait )
         IF lWait
            nStartTime := hb_milliseconds()
         ELSE
            EXIT
         ENDIF
      ENDIF

      IF ( hb_milliseconds() - nStartTime ) > CGI_MAX_EXEC_TIME * 1000

         //hb_toOutDebug( "CGIKill() Killing Process hCurProc = %s\n\r", hCurProc )

         // Killing process if still exists
         IF hCurProc != NIL
            HB_ProcessClose( hCurProc )
            nErrorLevel := 1
         ENDIF
         EXIT
      ENDIF
   ENDDO

   RETURN nErrorLevel


/********************************************************************
  Public helper functions
********************************************************************/

FUNCTION uhttpd_OSFileName( cFileName )
   IF hb_ps() != "/"
      RETURN STRTRAN( cFileName, "/", hb_ps() )
   ENDIF
   RETURN cFileName

PROCEDURE uhttpd_SetStatusCode(nStatusCode)
   t_nStatusCode := nStatusCode
   RETURN


PROCEDURE uhttpd_SetHeader( cType, cValue )
   //LOCAL nI
   //DEFAULT lReplace TO .T. // Needed from SetCookie()

   hb_HSet( _HTTP_RESPONSE, cType, cValue )

   /*
   IF lReplace .AND. ( nI := ASCAN( t_aHeader, {|x| UPPER( x[ 1 ] ) == UPPER( cType ) } ) ) > 0
      t_aHeader[ nI, 2 ] := cValue
   ELSE
      AADD( t_aHeader, { cType, cValue } )
   ENDIF
   */

   RETURN


FUNCTION uhttpd_GetHeader( cType )
   RETURN uhttpd_HGetValue( _HTTP_RESPONSE, cType )
/*
   DEFAULT nPos TO 1

   nPos := hb_HPos( hHash, cKey ))
   IF ( nPos := ASCAN( t_aHeader, {|x| UPPER( x[ 1 ] ) == UPPER( cType ) }, nPos ) ) > 0
      RETURN t_aHeader[ nPos, 2 ]
   ENDIF
   RETURN NIL
*/

PROCEDURE uhttpd_DelHeader( cType )
   LOCAL nPos := hb_HPos( _HTTP_RESPONSE, cType )
   IF nPos > 0
      hb_HDelAt( _HTTP_RESPONSE, nPos )
   ENDIF
   RETURN
/*
   LOCAL nI

   IF ( nI := ASCAN( t_aHeader, {|x| UPPER( x[ 1 ] ) == UPPER( cType ) } ) ) > 0
      hb_aDel( t_aHeader, nI, .T. )
   ENDIF
   RETURN
*/

PROCEDURE uhttpd_Write( cString )
   t_cResult += cString
   RETURN

/********************************************************************
  Internal helper functions
********************************************************************/

STATIC FUNCTION readRequest( hSocket, /* @ */ cRequest )
   LOCAL cBuf, nLen, nPos

   /* receive query */
   cRequest := ""
   DO WHILE .T.
      cBuf := Space( 4096 )
      nLen := hb_socketRecv( hSocket, @cBuf )
      IF nLen <= 0
         EXIT
      ENDIF
      cRequest += LEFT( cBuf, nLen )
      IF CR_LF + CR_LF $ cRequest
         EXIT
      ENDIF
   ENDDO

   /* receive CONTENT-LENGTH data */
   IF nLen > 0
      nPos := HB_ATI( CR_LF + "CONTENT-LENGTH:", cRequest )
      IF nPos > 0
         nPos := Val( Substr( cRequest, nPos + 17, 10 ) )
         IF nPos > 0
            /* we have to decrease number of bytes to read by already read
             * data after CR_LF + CR_LF
             */
            nPos -= Len( cRequest ) - At( CR_LF + CR_LF, cRequest ) - 3
            WHILE nPos > 0
               cBuf := Space( nPos )
               nLen := hb_socketRecv( hSocket, @cBuf, nPos )
               IF nLen <= 0
                  EXIT
               ENDIF
               cRequest += LEFT( cBuf, nPos )
               nPos -= nLen
            ENDDO
         ENDIF
      ENDIF
   ENDIF

#ifdef DEBUG_ACTIVE
   hb_ToOutDebug( "readRequest(): nLen = %i, cRequest = %s \n\r", nLen, cRequest )
#endif

   RETURN nLen

STATIC FUNCTION sendReply( hSocket, cSend )
   LOCAL nError := 0
   LOCAL nLen

   DO WHILE LEN( cSend ) > 0
      IF ( nLen := hb_socketSend( hSocket, cSend ) ) == -1
         ? "send() error:", hb_socketGetError()
         WriteToConsole( hb_StrFormat( "ServiceConnection() - send() error: %s, cSend = %s, hSocket = %s", hb_socketGetError(), cSend, hSocket ) )
         EXIT
      ELSEIF nLen > 0
         cSend := SUBSTR( cSend, nLen + 1 )
      ENDIF
   ENDDO

   RETURN nError

STATIC PROCEDURE defineServer( hSocket )
   LOCAL aI

   // define _SERVER vars (address part)
   IF ! Empty( aI := hb_socketGetPeerName( hSocket ) )
      _SERVER[ "REMOTE_ADDR" ] := aI[ HB_SOCKET_ADINFO_ADDRESS ]
      _SERVER[ "REMOTE_HOST" ] := _SERVER[ "REMOTE_ADDR" ]  // no reverse DNS
      _SERVER[ "REMOTE_PORT" ] := aI[ HB_SOCKET_ADINFO_PORT ]
   ENDIF

   IF ! Empty( aI := hb_socketGetSockName( hSocket ) )
      _SERVER[ "SERVER_ADDR" ] := aI[ HB_SOCKET_ADINFO_ADDRESS ]
      _SERVER[ "SERVER_PORT" ] := LTrim( Str( aI[ HB_SOCKET_ADINFO_PORT ] ) )
   ENDIF

   // add other _SERVER vars
   _SERVER[ "REQUEST_METHOD"       ] := NIL
   _SERVER[ "REQUEST_URI"          ] := NIL
   _SERVER[ "SERVER_PROTOCOL"      ] := NIL
   _SERVER[ "SCRIPT_NAME"          ] := NIL
   _SERVER[ "QUERY_STRING"         ] := NIL
   _SERVER[ "HTTP_ACCEPT"          ] := NIL
   _SERVER[ "HTTP_ACCEPT_CHARSET"  ] := NIL
   _SERVER[ "HTTP_ACCEPT_ENCODING" ] := NIL
   _SERVER[ "HTTP_ACCEPT_LANGUAGE" ] := NIL
   _SERVER[ "HTTP_CONNECTION"      ] := NIL
   _SERVER[ "HTTP_HOST"            ] := NIL
   _SERVER[ "HTTP_KEEP_ALIVE"      ] := NIL
   _SERVER[ "HTTP_REFERER"         ] := ""
   _SERVER[ "HTTP_USER_AGENT"      ] := ""
   _SERVER[ "HTTP_CACHE_CONTROL"   ] := NIL
   _SERVER[ "HTTP_COOKIE"          ] := NIL
   _SERVER[ "SERVER_NAME"          ] := ""
   _SERVER[ "SERVER_SOFTWARE"      ] := APP_NAME + " " + APP_VERSION + " (" + OS() + ")"
   _SERVER[ "SERVER_SIGNATURE"     ] := "<address>" + _SERVER[ "SERVER_SOFTWARE" ] + " Server at " + _SERVER[ "SERVER_NAME" ] + " Port " + _SERVER[ "SERVER_PORT" ] + "</address>"
   _SERVER[ "DOCUMENT_ROOT"        ] := s_cDocumentRoot
   _SERVER[ "SERVER_ADMIN"         ] := "root@localhost"   // TOFIX: put real user
   _SERVER[ "SCRIPT_FILENAME"      ] := NIL
   _SERVER[ "GATEWAY_INTERFACE"    ] := "CGI/1.1"
   _SERVER[ "SCRIPT_URL"           ] := NIL
   _SERVER[ "SCRIPT_URI"           ] := NIL
   _SERVER[ "PATH_INFO"            ] := NIL
   _SERVER[ "PATH_TRANSLATED"      ] := NIL

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
   LOCAL cScript
   LOCAL cFileName, nI
   LOCAL cExt, cHandler, xAction, nPos
   LOCAL cBaseFile
   LOCAL cPathInfo, lFound, cFile

   // Starting from Script Name request
   cScript := _SERVER[ "SCRIPT_NAME" ]

   //cFileName := STRTRAN(cRoot + _SERVER["SCRIPT_NAME"], "//", "/")
   cFileName := NIL
   cPathInfo := ""

   DO WHILE .T.

      #ifdef DEBUG_ACTIVE
         //hb_ToOutDebug( "cFileName = %s, cScript = %s\n\r", cFileName, cScript )
      #endif

      IF cFileName == NIL

         // Special script names
         IF Upper( cScript ) == "/SERVERSTATUS"
            cFileName := "/serverstatus"
            cExt      := "/serverstatus" // special extension
         ENDIF

      ENDIF

      IF cFileName == NIL

         cFileName := FileUnAlias( cScript )

      ENDIF

      // if filename is still NIL I set it
      IF cFileName == NIL
         cFileName := _SERVER[ "SCRIPT_FILENAME" ]
      ENDIF

      #ifdef DEBUG_ACTIVE
         //hb_ToOutDebug( "cFileName = %s, uhttpd_OSFileName( cFileName ) = %s,\n\r", cFileName, uhttpd_OSFileName( cFileName ) )
      #endif

      // Security
      IF ".." $ cFileName
         uhttpd_SetStatusCode( 403 )
         t_cErrorMsg := "Characters not allowed"
         RETURN MakeResponse()
      ENDIF

      //hb_toOutDebug( "cFileName = %s, uhttpd_OSFileName( cFileName ) = %s,\n\r s_hScriptAliases = %s\n\r", cFileName, uhttpd_OSFileName( cFileName ), hb_ValToExp( s_hScriptAliases ) )

      // checking extension
      IF cExt == NIL

         // checking if file exists
         IF HB_FileExists( uhttpd_OSFileName( cFileName ) )

            // extract extension
            IF ( nI := RAT( ".", cFileName ) ) > 0
               cExt := LOWER( SUBSTR( cFileName, nI + 1 ) )
            ENDIF

         // is it a directory ?
         ELSEIF HB_DirExists( uhttpd_OSFileName( cFileName ) )

            // if it exists as folder and it is missing trailing slash I add it and redirect to it
            IF RIGHT( cFileName, 1 ) != "/"
               uhttpd_SetHeader( "Location", "http://" + _SERVER[ "HTTP_HOST" ] + _SERVER[ "SCRIPT_NAME" ] + "/" )
               RETURN MakeResponse()
            ENDIF

            // Search for directory index file, i.e.: index.html
            IF ASCAN( s_aDirectoryIndex, ;
                      {|x| IIF( HB_FileExists( uhttpd_OSFileName( cFileName + X ) ), ( cFileName += X, .T. ), .F. ) } ) > 0

               // I have to check filename again (behaviour changes on extension file name)
               // resetting extension
               cExt := NIL
               LOOP

            ENDIF

         ELSE

            // Check for PATH_INFO: I will search if there is a physical file removing parts from right
            cBaseFile := cScript
            lFound := .F.
            DO WHILE !Empty( cBaseFile )

               //hb_toOutDebug( "cBaseFile = %s, cPathInfo = %s\n\r", cBaseFile, cPathInfo )

               IF ( nPos := RAT( "/", cBaseFile ) ) > 0
                  cPathInfo := SubStr( cBaseFile, nPos ) + cPathInfo
                  cBaseFile := Left( cBaseFile, nPos - 1 )
               ELSE
                  EXIT
               ENDIF

               IF HB_FileExists( uhttpd_OSFileName( _SERVER[ "DOCUMENT_ROOT" ] + cBaseFile ) )
                  cFileName := uhttpd_OSFileName( _SERVER[ "DOCUMENT_ROOT" ] + cBaseFile )
                  lFound := .T.
                  EXIT
               ENDIF

               cFile := FileUnAlias( cBaseFile )
               IF cFile != NIL .AND. HB_FileExists( uhttpd_OSFileName( cFile ) )
                  cFileName := uhttpd_OSFileName( cFile )
                  lFound := .T.
                  EXIT
               ENDIF
            ENDDO

            //hb_toOutDebug( "Uscita: cBaseFile = %s, cPathInfo = %s\n\r", cBaseFile, cPathInfo )

            // Found a script file name
            IF lFound .AND. !Empty( cPathInfo )
               // Store PATH_INFO
               _SERVER[ "PATH_INFO" ]       := cPathInfo
               _SERVER[ "PATH_TRANSLATED" ] := cFileName
               // Restart
               LOOP
            ENDIF

         ENDIF

      ENDIF

      // Ok, now I have to see what action I have to take

      //hb_toOutDebug( "cExt = %s\n\r", cExt )

      // Begin to search Handlers
      IF cExt != NIL
         cHandler := uhttpd_HGetValue( s_hHandlers, cExt )
      ENDIF

      //hb_toOutDebug( "cHandler = %s\n\r", cHandler )

      IF cHandler != NIL
         xAction := uhttpd_HGetValue( s_hActions, cHandler )
      ENDIF

      //hb_toOutDebug( "xAction = %s\n\r", xAction )

      IF xAction == NIL
         xAction := @Handler_Default()
      ENDIF

      //hb_toOutDebug( "xAction = %s\n\r", xAction )

      // Setting CGI vars
      define_Env( _SERVER )

      // Eval Action
      RETURN hb_ExecFromArray( xAction, { cFileName } )

   ENDDO

   RETURN MakeResponse()

// Define environment SET variables - TODO: Actually only for windows, make multiplatform
STATIC PROCEDURE Define_Env( hmServer )
   LOCAL v

   FOR EACH v IN hmServer
      hb_SetEnv( v:__enumKey(), v )
   NEXT

   RETURN

// ------------------------------- DEFAULT PAGES -----------------------------------

STATIC PROCEDURE ShowServerStatus()
   LOCAL cThreads
   uhttpd_SetHeader( "Content-Type", "text/html" )
   uhttpd_Write( '<html><head>' )
   uhttpd_Write( '<META HTTP-EQUIV="Refresh" CONTENT="' + LTrim( Str( PAGE_STATUS_REFRESH ) ) + ';URL=/ServerStatus">' )
   uhttpd_Write( '<META HTTP-EQUIV="CACHE-CONTROL" CONTENT="NO-CACHE">' )
   uhttpd_Write( '<title>Server Status</title><body><h1>Server Status</h1><pre>')
   //uhttpd_Write( '<table border="0">')

   uhttpd_Write( 'SERVER: ' + _SERVER[ "SERVER_SOFTWARE" ] + " Server at " + _SERVER[ "SERVER_NAME" ] + " Port " + _SERVER[ "SERVER_PORT" ] )
   uhttpd_Write( '<br>' )
   IF hb_mutexLock( s_hmtxBusy )
      uhttpd_Write( '<br>Thread: ' + Str( s_nThreads ) )
      uhttpd_Write( '<br>Connections: ' + Str( s_nConnections ) )
      uhttpd_Write( '<br>Max Connections: ' + Str( s_nMaxConnections ) )
      uhttpd_Write( '<br>Total Connections: ' + Str( s_nTotConnections ) )
      cThreads := ""
      aEval( s_aRunningThreads, {|e| cThreads += LTrim( Str( hb_threadId( e ) ) ) + "," } )
      cThreads := "{ " + IIF( !Empty( cThreads ), Left( cThreads, Len( cThreads ) - 1 ), "<empty>" ) + " }"
      uhttpd_Write( '<br>Running Threads: ' + cThreads )

#ifndef FIXED_THREADS
      uhttpd_Write( '<br>Service Thread: ' + Str( s_nServiceThreads ) )
      uhttpd_Write( '<br>Service Connections: ' + Str( s_nServiceConnections ) )
      uhttpd_Write( '<br>Max Service Connections: ' + Str( s_nMaxServiceConnections ) )
      uhttpd_Write( '<br>Total Service Connections: ' + Str( s_nTotServiceConnections ) )
      cThreads := ""
      aEval( s_aServiceThreads, {|e| cThreads += LTrim( Str( hb_threadId( e ) ) ) + "," } )
      cThreads := "{ " + IIF( !Empty( cThreads ), Left( cThreads, Len( cThreads ) - 1 ), "<empty>" ) + " }"
      uhttpd_Write( '<br>Service Threads: ' + cThreads )
#endif // FIXED_THREADS

      hb_mutexUnlock( s_hmtxBusy )
   ENDIF
   uhttpd_Write( '<br>Time: ' + Time() )

   //uhttpd_Write( '</table>')
   uhttpd_Write( "<hr></pre></body></html>" )

   RETURN

STATIC PROCEDURE ShowFolder( cDir )
   LOCAL aDir, aF
   LOCAL cParentDir, nPos

   uhttpd_SetHeader( "Content-Type", "text/html" )

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
   ? "-p       | --port           webserver tcp port         (default: " + LTrim( Str( LISTEN_PORT ) ) + ")"
   ? "-c       | --config         Configuration file         (default: " + APP_NAME + ".ini)"
   ? "                            It is possibile to define file path"
   ? "-a       | --approot        Application root directory (default: <curdir>)"
   ? "-d       | --docroot        Document root directory    (default: <curdir>\home)"
   ? "-i       | --indexes        Allow directory view       (default: no)"
   ? "-s       | --stop           Stop webserver"
   ? "-ts      | --start-threads  Define starting threads    (default: " + LTrim( Str( START_RUNNING_THREADS ) ) + ")"
   ? "-tm      | --max-threads    Define max threads         (default: " + LTrim( Str( MAX_RUNNING_THREADS ) ) + ")"
   ? "-cr      | --console-rows   Console rows               (default: " + LTrim( Str( MaxRow() + 1 ) ) + ")"
   ? "-cc      | --console-cols   Console cols               (default: " + LTrim( Str( MaxCol() + 1 ) ) + ")"
   ? "-h | -?  | --help           This help message"
   ?
   WAIT
   RETURN

STATIC PROCEDURE SysSettings()
   SET SCOREBOARD OFF
   SET CENTURY     ON
   SET DATE      ANSI
   SET BELL       OFF
   SET DELETED     ON
   SET EXACT      OFF
   SET CONFIRM     ON
   SET ESCAPE      ON
   SET WRAP        ON
   //RDDSetDefault( "DBFCDX" )
   RETURN

STATIC FUNCTION Exe_Path()
   LOCAL cPath := hb_argv( 0 )
   LOCAL nPos  := RAt( hb_ps(), cPath )
   IF nPos == 0
      cPath := ""
   ELSE
      cPath := SubStr( cPath, 1, nPos-1 )
   ENDIF
   RETURN cPath

STATIC FUNCTION Exe_Name()
   LOCAL cPrg := hb_argv( 0 )
   LOCAL nPos := RAt( hb_ps(), cPrg )
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
   LOCAL hIni := hb_IniRead( cConfig, .T. ) // .T. = load all keys in MixedCase, redundant as it is default, but to remember
   LOCAL cSection, hSect, cKey, xVal, cVal, nPos
   LOCAL hDefault

   //hb_ToOutDebug( "cConfig = %s,\n\rhIni = %s\n\r", cConfig, hb_ValToExp( hIni ) )

   // Define here what attributes we can have in ini config file and their defaults
   // Please add all keys in uppercase. hDefaults is Case Insensitive
   hDefault := hb_HSetCaseMatch( ;
   { ;
     "MAIN"     => { ;
                     "PORT"                 => LISTEN_PORT              ,;
                     "APPLICATION_ROOT"     => EXE_Path()               ,;
                     "DOCUMENT_ROOT"        => EXE_Path() + hb_ps() + "home"     ,;
                     "SHOW_INDEXES"         => .F.                      ,;
                     "SCRIPTALIASMIXEDCASE" => .T.                      ,;
                     "SESSIONPATH"          => EXE_Path() + hb_ps() + "sessions" ,;
                     "DIRECTORYINDEX"       => DIRECTORYINDEX_ARRAY     ,;
                     "CONSOLE-ROWS"         => MaxRow() + 1             ,;
                     "CONSOLE-COLS"         => MaxCol() + 1              ;
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
     "SCRIPTALIASES"  => { => } ,;
     "ALIASES"        => { => }  ;
   }, .F. )

   //hb_ToOutDebug( "hDefault = %s\n\r", hb_ValToExp( hDefault ) )

   // Now read changes from ini file and modify only admited keys
   IF !Empty( hIni )
      FOR EACH cSection IN hIni:Keys

          cSection := Upper( cSection )

          //hb_ToOutDebug( "cSection = %s\n\r", cSection )

          IF cSection $ hDefault

             hSect := hIni[ cSection ]

             //hb_ToOutDebug( "hSect = %s\n\r", hb_ValToExp( hSect ) )

             IF HB_ISHASH( hSect )
                FOR EACH cKey IN hSect:Keys

                    // Please, below check values MUST be uppercase

                    //hb_ToOutDebug( "cKey = %s\n\r", cKey )

                    IF cSection == "SCRIPTALIASES"
                       xVal := hSect[ cKey ]
                       IF xVal != NIL
                          hDefault[ cSection ][ cKey ] := xVal
                       ENDIF

                    ELSEIF cSection == "ALIASES"
                       xVal := hSect[ cKey ]
                       IF xVal != NIL
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
                                     CASE cKey == "CONSOLE-ROWS"
                                          xVal := Val( cVal )
                                     CASE cKey == "CONSOLE-COLS"
                                          xVal := Val( cVal )
                                     CASE cKey == "APPLICATION_ROOT"
                                          IF !Empty( cVal )
                                             // Change APP_DIR macro with current exe path
                                             xVal := cVal
                                          ENDIF
                                     CASE cKey == "DOCUMENT_ROOT"
                                          IF !Empty( cVal )
                                             // After will change APP_DIR macro with application dir
                                             //xVal := StrTran( cVal, "$(APP_DIR)", Exe_Path() )
                                             xVal := cVal
                                          ENDIF
                                     CASE cKey == "SCRIPTALIASMIXEDCASE"
                                          xVal := cVal
                                     CASE cKey == "SESSIONPATH"
                                          IF !Empty( cVal )
                                             // Change APP_DIR macro with current exe path
                                             //xVal := StrTran( cVal, "$(APP_DIR)", Exe_Path() )
                                             xVal := cVal
                                          ENDIF
                                     CASE cKey == "DIRECTORYINDEX"
                                          IF !Empty( cVal )
                                             xVal := uhttpd_split( " ", AllTrim( cVal ) )
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
                          IF xVal != NIL
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

STATIC FUNCTION FileUnAlias( cScript )
   LOCAL cFileName, x

   // Checking if the request contains a Script Alias
   IF HB_HHasKey( s_hScriptAliases, cScript )
      // in this case I have to substitute the alias with the real file name
      cFileName := hb_hGet( s_hScriptAliases, cScript )

      // substitute macros
      cFileName := StrTran( cFileName, "$(DOCROOT_DIR)", _SERVER[ "DOCUMENT_ROOT" ] )
      cFileName := StrTran( cFileName, "$(APP_DIR)"    , s_cApplicationRoot )
   ENDIF

   IF cFileName == NIL

      // Checking if the request contains an alias
      FOR EACH x IN s_hAliases
          IF x:__enumKey() == Left( cScript, Len( x:__enumKey() ) )
             cFileName := x + SubStr( cScript, Len( x:__enumKey() ) + 1 )

             // substitute macros
             cFileName := StrTran( cFileName, "$(DOCROOT_DIR)", _SERVER[ "DOCUMENT_ROOT" ] )
             cFileName := StrTran( cFileName, "$(APP_DIR)"    , s_cApplicationRoot )
             EXIT
          ENDIF
      NEXT

   ENDIF

RETURN cFileName

STATIC FUNCTION uhttpd_DefError( oError )
   LOCAL cMessage
   LOCAL cCallstack
   LOCAL cDOSError

   LOCAL aOptions
   LOCAL nChoice

   LOCAL n
   LOCAL cDateTime, cString
   LOCAL cNewLine := hb_eol()

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
      cCallstack += "Called from " + ProcName( n ) + "(" + hb_NToS( ProcLine( n ) ) + ")  ;"
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

      IF cDOSError == NIL
         nChoice := Alert( cMessage + ";" + cCallstack, aOptions )
      ELSE
         nChoice := Alert( cMessage + " " + cDOSError + ";" + cCallstack, aOptions )
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

   IF cDOSError != NIL
      cMessage += " " + cDOSError
   ENDIF

   OutErr( cNewLine )
   OutErr( cMessage )
   OutErr( cNewLine )
   OutErr( cCallstack )

   // Write to errorlog
   cDateTime := HB_TTOC( hb_DateTime() )
   cString   := Replicate( "*", 70 ) + cNewLine + ;
                cDateTime            + cNewLine + ;
                Replicate( "*", 70 ) + cNewLine + ;
                cMessage             + cNewLine + ;
                cCallstack           + cNewLine + ;
                Replicate( "*", 70 ) + cNewLine

   uhttpd_WriteToLogFile( cString, Exe_Path() + "\error.log" )

   ErrorLevel( 1 )
   QUIT

   RETURN .F.

STATIC FUNCTION ErrorMessage( oError )

   // start error message
   LOCAL cMessage := iif( oError:severity > ES_WARNING, "Error", "Warning" ) + " "

   // add subsystem name if available
   IF HB_ISSTRING( oError:subsystem )
      cMessage += oError:subsystem()
   ELSE
      cMessage += "???"
   ENDIF

   // add subsystem's error code if available
   IF HB_ISNUMERIC( oError:subCode )
      cMessage += "/" + hb_NToS( oError:subCode )
   ELSE
      cMessage += "/???"
   ENDIF

   // add error description if available
   IF HB_ISSTRING( oError:description )
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

// ----------------------------------------------------------------------------------
// HANDLERS
// ----------------------------------------------------------------------------------

// This handler handle static files
STATIC FUNCTION Handler_Default( cFileName )
   LOCAL cMime
   LOCAL cExt, nI
   LOCAL hMimeTypes := LoadMimeTypes()

   // If file exists
   IF HB_FileExists( uhttpd_OSFileName( cFileName ) )
      IF ( nI := RAT( ".", cFileName ) ) > 0
         cExt  := LOWER( SUBSTR( cFileName, nI + 1 ) )
         cMime := uhttpd_HGetValue( hMimeTypes, cExt )
      ENDIF
      IF cMime == NIL
         // Unknown file type
        cMime := "application/octet-stream"
      ENDIF

      uhttpd_SetHeader( "Content-Type", cMime )
      uhttpd_Write( HB_MEMOREAD( uhttpd_OSFileName( cFileName ) ) )

   // Directory content request
   ELSEIF HB_DirExists( uhttpd_OSFileName( cFileName ) )

      // If I'm here it's means that I have no page, so, if it is defined, I will display content folder
      IF !s_lIndexes
         uhttpd_SetStatusCode( 403 )
         t_cErrorMsg := "Display file list not allowed"
      ELSE
         // ----------------------- display folder content -------------------------------------
         ShowFolder( cFileName )
      ENDIF

   ELSE

      // We cannot handle request
      uhttpd_SetStatusCode( 404 )
      t_cErrorMsg := "File does not exist: " + cFileName

   ENDIF

RETURN MakeResponse()

// This handler handle server status
STATIC FUNCTION Handler_ServerStatus()
   LOCAL cThreads
   uhttpd_SetHeader( "Content-Type", "text/html" )
   uhttpd_Write( '<html><head>' )
   uhttpd_Write( '<META HTTP-EQUIV="Refresh" CONTENT="' + LTrim( Str( PAGE_STATUS_REFRESH ) ) + ';URL=/ServerStatus">' )
   uhttpd_Write( '<META HTTP-EQUIV="CACHE-CONTROL" CONTENT="NO-CACHE">' )
   uhttpd_Write( '<title>Server Status</title><body><h1>Server Status</h1><pre>')
   //uhttpd_Write( '<table border="0">')

   uhttpd_Write( 'SERVER: ' + _SERVER[ "SERVER_SOFTWARE" ] + " Server at " + _SERVER[ "SERVER_NAME" ] + " Port " + _SERVER[ "SERVER_PORT" ] )
   uhttpd_Write( '<br>' )
   IF hb_mutexLock( s_hmtxBusy )
      uhttpd_Write( '<br>Thread: ' + Str( s_nThreads ) )
      uhttpd_Write( '<br>Connections: ' + Str( s_nConnections ) )
      uhttpd_Write( '<br>Max Connections: ' + Str( s_nMaxConnections ) )
      uhttpd_Write( '<br>Total Connections: ' + Str( s_nTotConnections ) )
      cThreads := ""
      aEval( s_aRunningThreads, {|e| cThreads += LTrim( Str( hb_threadId( e ) ) ) + "," } )
      cThreads := "{ " + IIF( !Empty( cThreads ), Left( cThreads, Len( cThreads ) - 1 ), "<empty>" ) + " }"
      uhttpd_Write( '<br>Running Threads: ' + cThreads )

#ifndef FIXED_THREADS
      uhttpd_Write( '<br>Service Thread: ' + Str( s_nServiceThreads ) )
      uhttpd_Write( '<br>Service Connections: ' + Str( s_nServiceConnections ) )
      uhttpd_Write( '<br>Max Service Connections: ' + Str( s_nMaxServiceConnections ) )
      uhttpd_Write( '<br>Total Service Connections: ' + Str( s_nTotServiceConnections ) )
      cThreads := ""
      aEval( s_aServiceThreads, {|e| cThreads += LTrim( Str( hb_threadId( e ) ) ) + "," } )
      cThreads := "{ " + IIF( !Empty( cThreads ), Left( cThreads, Len( cThreads ) - 1 ), "<empty>" ) + " }"
      uhttpd_Write( '<br>Service Threads: ' + cThreads )
#endif // FIXED_THREADS

      hb_mutexUnlock( s_hmtxBusy )
   ENDIF
   uhttpd_Write( '<br>Time: ' + Time() )

   //uhttpd_Write( '</table>')
   uhttpd_Write( "<hr></pre></body></html>" )

RETURN MakeResponse()

STATIC FUNCTION Handler_HrbScript( cFileName )
   LOCAL xResult
   LOCAL cHRBBody, pHRB, oError
   LOCAL cCurPath

   BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
      // Lock HRB to avoid MT race conditions
      IF !HRB_ACTIVATE_CACHE
         cHRBBody := HRB_LoadFromFile( uhttpd_OSFileName( cFileName ) )
      ENDIF
      IF hb_mutexLock( s_hmtxHRB )
         BEGIN SEQUENCE
            IF HRB_ACTIVATE_CACHE
               // caching modules
               IF !hb_HHasKey( s_hHRBModules, cFileName )
                  hb_HSet( s_hHRBModules, cFileName, HRB_LoadFromFile( uhttpd_OSFileName( cFileName ) ) )
               ENDIF
               cHRBBody := s_hHRBModules[ cFileName ]
            ENDIF
            WriteToConsole( "Executing: " + cFileName )
            IF !EMPTY( pHRB := HB_HRBLOAD( cHRBBody ) )

                // save current directory
                cCurPath := hb_CurDrive() + hb_osDriveSeparator() + hb_ps() + CurDir()
                // Change dir to document root
                DirChange( s_cDocumentRoot )

                xResult := HRBMAIN()

#ifdef DEBUG_ACTIVE
                hb_ToOutDebug( "Handler_HrbScript(): cFileName = %s,\n\rcCurPath = %s,\n\rs_cDocumentRoot = %s,\n\rpHRB = %s,\n\rxResult = %s\n\r", ;
                               cFileName, cCurPath, s_cDocumentRoot, pHRB, xResult )
#endif

                // return to original folder
                DirChange( cCurPath )

                HB_HRBUNLOAD( pHRB )
            ELSE
                uhttpd_SetStatusCode( 404 )
                t_cErrorMsg := "File does not exist: " + cFileName
            ENDIF
         ALWAYS
            hb_mutexUnlock( s_hmtxHRB )
         END SEQUENCE

      ENDIF

      IF HB_ISSTRING( xResult )
         uhttpd_SetHeader( "Content-Type", "text/html" )
         uhttpd_Write( xResult )
      ELSE
         // Application in HRB module is responsible to send HTML content
      ENDIF

   RECOVER USING oError

      WriteToConsole( "Error!" )

      uhttpd_SetHeader( "Content-Type", "text/html" )
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

   END SEQUENCE

   RETURN MakeResponse()

STATIC FUNCTION Handler_CgiScript( cFileName )
   LOCAL xResult

   WriteToConsole( "Executing: " + cFileName )

   IF ( CGIExec( uhttpd_OSFileName(cFileName), @xResult ) ) == 0

      //uhttpd_SetHeader( "Content-Type", cI )
      //uhttpd_Write( xResult )
      RETURN "HTTP/1.1 200 OK " + CR_LF + xResult

   ELSE

      uhttpd_SetHeader( "Content-Type", "text/html" )
      IF !Empty( xResult )
         uhttpd_Write( xResult )
      ELSE
         uhttpd_Write( "CGI Error" )
      ENDIF

   ENDIF

   RETURN MakeResponse()

STATIC FUNCTION LoadMimeTypes()
   // TODO: load mime types from file
   RETURN { ;
      "css"  =>  "text/css"                  ,;
      "htm"  =>  "text/html"                 ,;
      "html" =>  "text/html"                 ,;
      "txt"  =>  "text/plain"                ,;
      "text" =>  "text/plain"                ,;
      "asc"  =>  "text/plain"                ,;
      "c"    =>  "text/plain"                ,;
      "h"    =>  "text/plain"                ,;
      "cpp"  =>  "text/plain"                ,;
      "hpp"  =>  "text/plain"                ,;
      "log"  =>  "text/plain"                ,;
      "rtf"  =>  "text/rtf"                  ,;
      "xml"  =>  "text/xml"                  ,;
      "xsl"  =>  "text/xsl"                  ,;
      "bmp"  =>  "image/bmp"                 ,;
      "gif"  =>  "image/gif"                 ,;
      "jpg"  =>  "image/jpeg"                ,;
      "jpe"  =>  "image/jpeg"                ,;
      "jpeg" =>  "image/jpeg"                ,;
      "png"  =>  "image/png"                 ,;
      "tif"  =>  "image/tiff"                ,;
      "tiff" =>  "image/tiff"                ,;
      "djv"  =>  "image/vnd.djvu"            ,;
      "djvu" =>  "image/vnd.djvu"            ,;
      "ico"  =>  "image/x-icon"              ,;
      "xls"  =>  "application/excel"         ,;
      "doc"  =>  "application/msword"        ,;
      "pdf"  =>  "application/pdf"           ,;
      "ps"   =>  "application/postscript"    ,;
      "eps"  =>  "application/postscript"    ,;
      "ppt"  =>  "application/powerpoint"    ,;
      "bz2"  =>  "application/x-bzip2"       ,;
      "gz"   =>  "application/x-gzip"        ,;
      "tgz"  =>  "application/x-gtar"        ,;
      "js"   =>  "application/x-javascript"  ,;
      "tar"  =>  "application/x-tar"         ,;
      "tex"  =>  "application/x-tex"         ,;
      "zip"  =>  "application/zip"           ,;
      "midi" =>  "audio/midi"                ,;
      "mp3"  =>  "audio/mpeg"                ,;
      "wav"  =>  "audio/x-wav"               ,;
      "qt"   =>  "video/quicktime"           ,;
      "mov"  =>  "video/quicktime"           ,;
      "avi"  =>  "video/x-msvideo"           }

STATIC FUNCTION GT_notifier( nEvent, xParams )
   LOCAL nReturn := 0

   DO CASE
   CASE nEvent == HB_GTE_CLOSE
      IF hb_mutexLock( s_hmtxBusy )
         s_lQuitRequest := .T.
         nReturn := 1
         hb_mutexUnlock( s_hmtxBusy )
      ENDIF
   ENDCASE

   HB_SYMBOL_UNUSED( xParams )

   RETURN nReturn

STATIC FUNCTION UHTTPD_UTCOFFSET()
   LOCAL nOffset := HB_UTCOFFSET()
   RETURN iif( nOffset < 0, "-", "+" ) +;
          StrZero( nOffset / 3600, 2, 0 ) +;
          StrZero( ( nOffset % 3600 ) / 60, 2, 0 )
