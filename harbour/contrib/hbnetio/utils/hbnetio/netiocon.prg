/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour NETIO server management cmdline tool
 *
 * Copyright 2009-2011 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#include "color.ch"
#include "inkey.ch"
#include "setcurs.ch"

#include "hbgtinfo.ch"

PROCEDURE hbnetiocon_cmdUI( cIP, nPort, cPassword )
   LOCAL GetList := {}
   LOCAL hCommands
   LOCAL nSavedRow
   LOCAL nPos
   LOCAL aCommand
   LOCAL cCommand
   LOCAL cMsg

   LOCAL bKeyDown
   LOCAL bKeyUp
   LOCAL bKeyIns
   LOCAL bKeyPaste
   LOCAL bKeyTab

   LOCAL aHistory, nHistIndex

   LOCAL lQuit

   LOCAL pConnection
   LOCAL nStreamID

   LOCAL aNotification

   SET DATE ANSI
   SET CENTURY ON
   SET CONFIRM ON
   SET SCOREBOARD OFF

   SetCancel( .F. )

   IF ! Empty( cPassword )
      pConnection := ConnectLow( cIP, nPort, cPassword, @nStreamID )
      QQOut( hb_eol() )
   ENDIF

   QQOut( "Type a command or '?' for help.", hb_eol() )

   aHistory   := { "quit" }
   nHistIndex := Len( aHistory ) + 1

   hCommands  := { ;
      "?"             => { ""               , "Synonym for 'help'."                            , {|| cmdHelp( hCommands ) } },;
      "exit"          => { ""               , "Exit console."                                  , {|| lQuit := .T. } },;
      "clear"         => { ""               , "Clear screen."                                  , {|| Scroll(), SetPos( 0, 0 ) } },;
      "connect"       => { "[<ip[:port>]]"  , "Connect."                                       , {| cCommand | cmdConnect( cCommand, @pConnection, @cIP, @nPort, @nStreamID ) } },;
      "disconnect"    => { ""               , "Disconnect."                                    , {|| cmdDisconnect( @pConnection ) } },;
      "sysinfo"       => { ""               , "Show system/build information."                 , {|| cmdSysInfo( pConnection ) } },;
      "showconf"      => { ""               , "Show server configuration."                     , {|| cmdServerConfig( pConnection ) } },;
      "show"          => { ""               , "Show list of connections."                      , {|| cmdConnInfo( pConnection, .F. ) } },;
      "showadmin"     => { ""               , "Show list of management connections."           , {|| cmdConnInfo( pConnection, .T. ) } },;
      "noconn"        => { ""               , "Disable incoming connections."                  , {|| cmdConnEnable( pConnection, .F. ) } },;
      "conn"          => { ""               , "Enable incoming connections."                   , {|| cmdConnEnable( pConnection, .T. ) } },;
      "nologconn"     => { ""               , "Disable logging incoming connections."          , {|| cmdConnLogEnable( pConnection, .F. ) } },;
      "logconn"       => { ""               , "Enable logging incoming connections."           , {|| cmdConnLogEnable( pConnection, .T. ) } },;
      "filt"          => { ""               , "Show filters."                                  , {|| cmdConnFilters( pConnection, .F. ) } },;
      "filtadmin"     => { ""               , "Show filters for management connections."       , {|| cmdConnFilters( pConnection, .T. ) } },;
      "allowadd"      => { "<ip>"           , "Add allow filter"                               , {| cCommand | cmdConnFilterMod( pConnection, cCommand, "hbnetiomgm_allowadd" ) } },;
      "allowdel"      => { "<ip>"           , "Remove allow filter"                            , {| cCommand | cmdConnFilterMod( pConnection, cCommand, "hbnetiomgm_allowdel" ) } },;
      "blockadd"      => { "<ip>"           , "Add block filter"                               , {| cCommand | cmdConnFilterMod( pConnection, cCommand, "hbnetiomgm_blockadd" ) } },;
      "blockdel"      => { "<ip>"           , "Remove block filter"                            , {| cCommand | cmdConnFilterMod( pConnection, cCommand, "hbnetiomgm_blockdel" ) } },;
      "allowaddadmin" => { "<ip>"           , "Add allow filter for management connections"    , {| cCommand | cmdConnFilterMod( pConnection, cCommand, "hbnetiomgm_allowaddadmin" ) } },;
      "allowdeladmin" => { "<ip>"           , "Remove allow filter for management connections" , {| cCommand | cmdConnFilterMod( pConnection, cCommand, "hbnetiomgm_allowdeladmin" ) } },;
      "blockaddadmin" => { "<ip>"           , "Add block filter for management connections"    , {| cCommand | cmdConnFilterMod( pConnection, cCommand, "hbnetiomgm_blockaddadmin" ) } },;
      "blockdeladmin" => { "<ip>"           , "Remove block filter for management connections" , {| cCommand | cmdConnFilterMod( pConnection, cCommand, "hbnetiomgm_blockdeladmin" ) } },;
      "filtsave"      => { ""               , "Save filters to disk."                          , {|| cmdConnFilterSave( pConnection ) } },;
      "stop"          => { "[<ip:port>|all]", "Stop specified connection(s)."                  , {| cCommand | cmdConnStop( pConnection, cCommand ) } },;
      "clientinfo"    => { "[<ip:port>"     , "Show client details."                           , {| cCommand | cmdConnClientInfo( pConnection, cCommand ) } },;
      "quit"          => { ""               , "Stop server and exit console."                  , {|| cmdShutdown( pConnection ), lQuit := .T. } },;
      "help"          => { ""               , "Display this help."                             , {|| cmdHelp( hCommands ) } } }

   lQuit := .F.

   DO WHILE ! lQuit

      IF ! Empty( pConnection )
         /* Is connection alive? */
         BEGIN SEQUENCE WITH {| oError | Break( oError ) }
            netio_funcexec( pConnection, "hbnetiomgm_ping" )
         RECOVER
            QQOut( "Connection lost.", hb_eol() )
            EXIT
         END SEQUENCE
      ENDIF

      cCommand := Space( 128 )

      IF Empty( pConnection )
         QQOut( "hbnetiosrv$ " )
      ELSE
         QQOut( "hbnetiosrv://" + cIP + ":" + hb_ntos( nPort ) + "$ " )
      ENDIF
      nSavedRow := Row()

      @ nSavedRow, Col() GET cCommand PICTURE "@S" + hb_ntos( MaxCol() - Col() + 1 ) COLOR hb_ColorIndex( SetColor(), CLR_STANDARD ) + "," + hb_ColorIndex( SetColor(), CLR_STANDARD )

      SetCursor( iif( ReadInsert(), SC_INSERT, SC_NORMAL ) )

      bKeyIns   := SetKey( K_INS,;
         {|| SetCursor( iif( ReadInsert( ! ReadInsert() ),;
                          SC_NORMAL, SC_INSERT ) ) } )
      bKeyUp    := SetKey( K_UP,;
         {|| iif( nHistIndex > 1,;
                  cCommand := PadR( aHistory[ --nHistIndex ], Len( cCommand ) ), ),;
                  ManageCursor( cCommand ) } )
      bKeyDown  := SetKey( K_DOWN,;
         {|| cCommand := PadR( iif( nHistIndex < Len( aHistory ),;
             aHistory[ ++nHistIndex ],;
             ( nHistIndex := Len( aHistory ) + 1, "" ) ), Len( cCommand ) ),;
                  ManageCursor( cCommand ) } )
      bKeyPaste := SetKey( K_ALT_V, {|| hb_gtInfo( HB_GTI_CLIPBOARDPASTE )})

      bKeyTab   := SetKey( K_TAB, {|| CompleteCmd( @cCommand, hCommands ) } )

      READ

      /* Positions the cursor on the line previously saved */
      SetPos( nSavedRow, MaxCol() - 1 )

      SetKey( K_ALT_V, bKeyPaste )
      SetKey( K_DOWN,  bKeyDown  )
      SetKey( K_UP,    bKeyUp    )
      SetKey( K_INS,   bKeyIns   )
      SetKey( K_TAB,   bKeyTab   )

      QQOut( hb_eol() )

      cCommand := AllTrim( cCommand )

      /* Dump all messages in queue */
      /* TODO: Move this to a separate thread and display it in a dedicated screen area. */
      aNotification := netio_GetData( nStreamID )
      IF hb_isArray( aNotification )
         FOR EACH cMsg IN aNotification /* TODO: Protect against flood */
            QQOut( "> message from server:", cMsg, hb_eol() )
         NEXT
      ENDIF

      IF Empty( cCommand )
         LOOP
      ENDIF

      IF Empty( aHistory ) .OR. ! ATail( aHistory ) == cCommand
         IF Len( aHistory ) < 64
            AAdd( aHistory, cCommand )
         ELSE
            ADel( aHistory, 1 )
            aHistory[ Len( aHistory ) ] := cCommand
         ENDIF
      ENDIF
      nHistIndex := Len( aHistory ) + 1

      aCommand := hb_ATokens( cCommand, " " )
      IF ! Empty( aCommand ) .AND. ( nPos := hb_HPos( hCommands, Lower( aCommand[ 1 ] ) ) ) > 0
         Eval( hb_HValueAt( hCommands, nPos )[ 3 ], cCommand )
      ELSE
         QQOut( "Error: Unknown command '" + cCommand + "'.", hb_eol() )
      ENDIF
   ENDDO

   IF ! Empty( pConnection )

      netio_OpenItemStream( pConnection, "hbnetiomgm_regnotif", .F. )
      pConnection := NIL

      IF lQuit
         QQOut( "Connection closed.", hb_eol() )
      ENDIF
   ENDIF

   RETURN

PROCEDURE hbnetiocon_IPPortSplit( cAddr, /* @ */ cIP, /* @ */ nPort )
   LOCAL tmp

   IF ! Empty( cAddr )
      cIP := cAddr
      IF ( tmp := At( ":", cIP ) ) > 0
         nPort := Val( SubStr( cIP, tmp + Len( ":" ) ) )
         cIP := Left( cIP, tmp - 1 )
      ELSE
         nPort := NIL
      ENDIF
   ENDIF

   RETURN

/* connect to server */
STATIC FUNCTION ConnectLow( cIP, nPort, cPassword, /* @ */ nStreamID )
   LOCAL pConnection

   QQOut( hb_StrFormat( "Connecting to hbnetio server management at %1$s:%2$d...", cIP, nPort ), hb_eol() )

   pConnection := netio_getconnection( cIP, nPort,, cPassword )
   cPassword := NIL

   IF ! Empty( pConnection )

      netio_funcexec( pConnection, "hbnetiomgm_setclientinfo", MyClientInfo() )
      nStreamID := netio_OpenItemStream( pConnection, "hbnetiomgm_regnotif", .T. )

      QQOut( "Connected.", hb_eol() )
   ELSE
      QQOut( "Error connecting server.", hb_eol() )
   ENDIF

   RETURN pConnection

STATIC FUNCTION MyClientInfo()
   LOCAL hInfo := { => }

   hb_hKeepOrder( hInfo, .T. )

   hInfo[ "OS()"          ] := OS()
   hInfo[ "Version()"     ] := Version()
   hInfo[ "hb_Compiler()" ] := hb_Compiler()
   hInfo[ "NetName()"     ] := NetName()
   hInfo[ "hb_UserName()" ] := hb_UserName()

   RETURN hInfo

STATIC FUNCTION GetPassword()
   LOCAL GetList := {}
   LOCAL cPassword := Space( 128 )
   LOCAL nSavedRow
   LOCAL bKeyPaste

   QQOut( "Enter password: " )

   nSavedRow := Row()

   AAdd( GetList, hb_Get():New( Row(), Col(), {| v | iif( PCount() == 0, cPassword, cPassword := v ) }, "cPassword", "@S" + hb_ntos( MaxCol() - Col() + 1 ), hb_ColorIndex( SetColor(), CLR_STANDARD ) + "," + hb_ColorIndex( SetColor(), CLR_STANDARD ) ) )
   ATail( GetList ):hideInput( .T. )
   ATail( GetList ):postBlock := {|| ! Empty( cPassword ) }
   ATail( GetList ):display()

   SetCursor( iif( ReadInsert(), SC_INSERT, SC_NORMAL ) )
   bKeyPaste := SetKey( K_ALT_V, {|| hb_gtInfo( HB_GTI_CLIPBOARDPASTE ) } )

   READ

   /* Positions the cursor on the line previously saved */
   SetPos( nSavedRow, MaxCol() - 1 )
   SetKey( K_ALT_V, bKeyPaste )

   QQOut( hb_eol() )

   RETURN AllTrim( cPassword )

/* Adjusted the positioning of cursor on navigate through history. [vailtom] */
STATIC PROCEDURE ManageCursor( cCommand )
   KEYBOARD Chr( K_HOME ) + iif( ! Empty( cCommand ), Chr( K_END ), "" )
   RETURN

/* Complete the command line, based on the first characters that the user typed. [vailtom] */
STATIC PROCEDURE CompleteCmd( cCommand, hCommands )
   LOCAL s := Lower( AllTrim( cCommand ) )
   LOCAL n

   /* We need at least one character to search */
   IF Len( s ) > 1
      FOR EACH n IN hCommands
         IF s == Lower( Left( n:__enumKey(), Len( s ) ) )
            cCommand := PadR( n:__enumKey(), Len( cCommand ) )
            ManageCursor( cCommand )
            RETURN
         ENDIF
      NEXT
   ENDIF

   RETURN

STATIC FUNCTION XToStrX( xValue )
   LOCAL cType := ValType( xValue )

   LOCAL tmp
   LOCAL cRetVal

   SWITCH cType
   CASE "C"

      xValue := StrTran( xValue, Chr(  0 ), '" + Chr(  0 ) + "' )
      xValue := StrTran( xValue, Chr(  9 ), '" + Chr(  9 ) + "' )
      xValue := StrTran( xValue, Chr( 10 ), '" + Chr( 10 ) + "' )
      xValue := StrTran( xValue, Chr( 13 ), '" + Chr( 13 ) + "' )
      xValue := StrTran( xValue, Chr( 26 ), '" + Chr( 26 ) + "' )

      RETURN xValue

   CASE "N" ; RETURN hb_ntos( xValue )
   CASE "D" ; RETURN DToC( xValue )
   CASE "T" ; RETURN hb_TToC( xValue )
   CASE "L" ; RETURN iif( xValue, ".T.", ".F." )
   CASE "O" ; RETURN xValue:className() + " Object"
   CASE "U" ; RETURN "NIL"
   CASE "B" ; RETURN '{||...} -> ' + XToStrX( Eval( xValue ) )
   CASE "A"

      cRetVal := '{ '

      FOR EACH tmp IN xValue
         cRetVal += XToStrX( tmp )
         IF tmp:__enumIndex() < Len( tmp:__enumBase() )
            cRetVal += ", "
         ENDIF
      NEXT

      RETURN cRetVal + ' }'

   CASE "H"

      cRetVal := '{ '

      FOR EACH tmp IN xValue
         cRetVal += tmp:__enumKey() + " => " + XToStrX( tmp )
         IF tmp:__enumIndex() < Len( tmp:__enumBase() )
            cRetVal += ", "
         ENDIF
      NEXT

      RETURN cRetVal + ' }'

   CASE "M" ; RETURN 'M:' + xValue
   ENDSWITCH

   RETURN ""

/* Commands */

STATIC PROCEDURE cmdHelp( hCommands )
   LOCAL aTexts := {}
   LOCAL n, c, m

   m := 0
   hb_HEval( hCommands, {| k, l | m := Max( m, Len( k + iif( Empty( l[ 1 ] ), "", " " + l[ 1 ] ) ) ) } )

   AAdd( aTexts, "Commands:" )

   /* Processing commands */
   FOR EACH n IN hCommands
      AAdd( aTexts, " " + PadR( n:__enumKey() + iif( Empty( n[ 1 ] ), "", " " + n[ 1 ] ), m ) + " - " + n[ 2 ] )
   NEXT

   ASort( aTexts, 2 )

   AAdd( aTexts, "" )
   AAdd( aTexts, "Keyboard shortcuts:" )
   AAdd( aTexts, PadR( " <Up>", m )    + "  - Move up on historic list." )
   AAdd( aTexts, PadR( " <Down>", m )  + "  - Move down on historic list." )
   AAdd( aTexts, PadR( " <Tab>", m )   + "  - Complete command." )
   AAdd( aTexts, PadR( " <Alt+V>", m ) + "  - Paste from clipboard." )

   c := 0
   m := MaxRow()

   FOR EACH n IN aTexts
      QQOut( n, hb_eol() )

      IF ++c == m
         c := 0
         QQOut( "Press any key to continue..." )
         Inkey( 0 )

         Scroll( Row(), 0, Row(), MaxCol(), 0 )
         SetPos( Row(), 0 )
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE cmdConnect( cCommand, /* @ */ pConnection, /* @ */ cIP, /* @ */ nPort, /* @ */ nStreamID )
   LOCAL aToken
   LOCAL cPassword
   LOCAL nPortOld

   IF Empty( pConnection )

      aToken := hb_ATokens( cCommand, " " )

      IF Len( aToken ) >= 2
         nPortOld := nPort
         hbnetiocon_IPPortSplit( aToken[ 2 ], @cIP, @nPort )
         IF Empty( nPort )
            nPort := nPortOld
         ENDIF
      ENDIF
      IF Len( aToken ) >= 3
         cPassword := aToken[ 3 ]
      ELSE
         cPassword := GetPassword()
      ENDIF

      pConnection := ConnectLow( cIP, nPort, cPassword, @nStreamID )
   ELSE
      QQOut( "Already connected. Disconnect first.", hb_eol() )
   ENDIF

   RETURN

STATIC PROCEDURE cmdDisconnect( /* @ */ pConnection )

   IF Empty( pConnection )
      QQOut( "Not connected.", hb_eol() )
   ELSE
      pConnection := NIL
   ENDIF

   RETURN

STATIC PROCEDURE cmdSysInfo( pConnection )
   LOCAL cLine

   IF Empty( pConnection )
      QQOut( "Not connected.", hb_eol() )
   ELSE
      FOR EACH cLine IN netio_funcexec( pConnection, "hbnetiomgm_sysinfo" )
         QQOut( cLine, hb_eol() )
      NEXT
   ENDIF

   RETURN

STATIC PROCEDURE cmdServerConfig( pConnection )
   LOCAL cLine

   IF Empty( pConnection )
      QQOut( "Not connected.", hb_eol() )
   ELSE
      FOR EACH cLine IN netio_funcexec( pConnection, "hbnetiomgm_serverconfig" )
         QQOut( cLine, hb_eol() )
      NEXT
   ENDIF

   RETURN

STATIC PROCEDURE cmdConnStop( pConnection, cCommand )
   LOCAL aToken

   IF Empty( pConnection )
      QQOut( "Not connected.", hb_eol() )
   ELSE
      aToken := hb_ATokens( cCommand, " " )
      IF Len( aToken ) > 1
         netio_funcexec( pConnection, "hbnetiomgm_stop", aToken[ 2 ] )
      ELSE
         QQOut( "Error: Invalid syntax.", hb_eol() )
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE cmdConnClientInfo( pConnection, cCommand )
   LOCAL aToken
   LOCAL xCargo

   IF Empty( pConnection )
      QQOut( "Not connected.", hb_eol() )
   ELSE
      aToken := hb_ATokens( cCommand, " " )
      IF Len( aToken ) > 1
         xCargo := netio_funcexec( pConnection, "hbnetiomgm_clientinfo", aToken[ 2 ] )
         IF xCargo == NIL
            QQOut( "No information", hb_eol() )
         ELSE
            QQOut( XToStrX( xCargo ), hb_eol() )
         ENDIF
      ELSE
         QQOut( "Error: Invalid syntax.", hb_eol() )
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE cmdConnInfo( pConnection, lManagement )
   LOCAL aArray
   LOCAL hConn

   IF Empty( pConnection )
      QQOut( "Not connected.", hb_eol() )
   ELSE
      aArray := netio_funcexec( pConnection, iif( lManagement, "hbnetiomgm_adminfo", "hbnetiomgm_conninfo" ) )

      QQOut( "Number of connections: " + hb_ntos( Len( aArray ) ), hb_eol() )

      FOR EACH hConn IN aArray
         QQOut( "#" + PadR( hb_ntos( hConn[ "nThreadID" ] ), Len( Str( hConn[ "nThreadID" ] ) ) ) + " " +;
                hb_TToC( hConn[ "tStart" ], "YYYY.MM.DD", "HH:MM:SS" ) + " " +;
                PadR( hConn[ "cStatus" ], 12 ) + " " +;
                "fcnt: " + Str( hConn[ "nFilesCount" ] ) + " " +;
                "send: " + Str( hConn[ "nBytesSent" ] ) + " " +;
                "recv: " + Str( hConn[ "nBytesReceived" ] ) + " " +;
                hConn[ "cAddressPeer" ] + " " +;
                iif( "xCargo" $ hconn, hb_ValToStr( hConn[ "xCargo" ] ), "" ), hb_eol() )
      NEXT
   ENDIF

   RETURN

STATIC PROCEDURE cmdShutdown( pConnection )

   IF Empty( pConnection )
      QQOut( "Not connected.", hb_eol() )
   ELSE
      netio_funcexec( pConnection, "hbnetiomgm_shutdown" )
   ENDIF

   RETURN

STATIC PROCEDURE cmdConnEnable( pConnection, lValue )

   IF Empty( pConnection )
      QQOut( "Not connected.", hb_eol() )
   ELSE
      netio_funcexec( pConnection, "hbnetiomgm_conn", lValue )
   ENDIF

   RETURN

STATIC PROCEDURE cmdConnLogEnable( pConnection, lValue )

   IF Empty( pConnection )
      QQOut( "Not connected.", hb_eol() )
   ELSE
      netio_funcexec( pConnection, "hbnetiomgm_logconn", lValue )
   ENDIF

   RETURN

STATIC PROCEDURE cmdConnFilterMod( pConnection, cCommand, cRPC )
   LOCAL aToken

   IF Empty( pConnection )
      QQOut( "Not connected.", hb_eol() )
   ELSE
      aToken := hb_ATokens( cCommand, " " )
      IF Len( aToken ) > 1
         IF netio_funcexec( pConnection, cRPC, aToken[ 2 ] )
            QQOut( "Done", hb_eol() )
         ELSE
            QQOut( "Failed", hb_eol() )
         ENDIF
      ELSE
         QQOut( "Error: Invalid syntax.", hb_eol() )
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE cmdConnFilters( pConnection, lManagement )
   LOCAL aArray
   LOCAL hFilter

   IF Empty( pConnection )
      QQOut( "Not connected.", hb_eol() )
   ELSE
      aArray := netio_funcexec( pConnection, iif( lManagement, "hbnetiomgm_filtersadmin", "hbnetiomgm_filters" ) )

      FOR EACH hFilter IN aArray
         QQOut( hFilter[ "cType" ],;
                hFilter[ "cAddress" ], hb_eol() )
      NEXT
   ENDIF

   RETURN

STATIC PROCEDURE cmdConnFilterSave( pConnection )

   IF Empty( pConnection )
      QQOut( "Not connected.", hb_eol() )
   ELSE
      netio_funcexec( pConnection, "hbnetiomgm_filtersave" )
   ENDIF

   RETURN
