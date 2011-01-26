/*
 * $Id$
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 *
 * See COPYING for licensing terms.
 *
 */

#include "color.ch"
#include "inkey.ch"
#include "setcurs.ch"

#include "hbgtinfo.ch"

STATIC FUNCTION hbnetiosrv_LoadCmds( pConnection )
   LOCAL hCmds := { ;
      "?"         => { ""               , "Synonym for 'help'."                   , {|| cmdHelp( hCmds ) } },;
      "clear"     => { ""               , "Clear screen."                         , {|| Scroll(), SetPos( 0, 0 ) } },;
      "sysinfo"   => { ""               , "Show system/build information."        , {|| cmdSysInfo( pConnection ) } },;
      "show"      => { ""               , "Show list of connections."             , {|| cmdConnInfo( pConnection ) } },;
      "noconn"    => { ""               , "Disable incoming connections."         , {|| cmdConnEnable( pConnection, .F. ) } },;
      "conn"      => { ""               , "Enable incoming connections."          , {|| cmdConnEnable( pConnection, .T. ) } },;
      "nologconn" => { ""               , "Disable logging incoming connections." , {|| cmdConnLogEnable( pConnection, .F. ) } },;
      "logconn"   => { ""               , "Enable logging incoming connections."  , {|| cmdConnLogEnable( pConnection, .T. ) } },;
      "stop"      => { "[<ip:port>|all]", "Stop specified connection(s)."         , {| cCommand | cmdConnStop( pConnection, cCommand ) } },;
      "quit"      => { ""               , "Stop server and exit."                 , {|| netio_funcexec( pConnection, "netio_shutdown" ) } },;
      "help"      => { ""               , "Display this help."                    , {|| cmdHelp( hCmds ) } } }

   RETURN hCmds

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

STATIC FUNCTION netiosrv_clientinfo()
   LOCAL hInfo := { => }

   hb_hKeepOrder( hInfo, .T. )

   hInfo[ "OS()"          ] := OS()
   hInfo[ "Version()"     ] := Version()
   hInfo[ "hb_Compiler()" ] := hb_Compiler()
   hInfo[ "NetName()"     ] := NetName()
   hInfo[ "hb_UserName()" ] := hb_UserName()

   RETURN hInfo

PROCEDURE netiosrv_cmdUI( cIP, nPort, cPassword )
   LOCAL GetList := {}
   LOCAL hCommands
   LOCAL nSavedRow
   LOCAL nPos
   LOCAL aCommand
   LOCAL cCommand

   LOCAL bKeyDown
   LOCAL bKeyUp
   LOCAL bKeyIns
   LOCAL bKeyPaste
   LOCAL bKeyTab

   LOCAL aHistory, nHistIndex

   LOCAL pConnection

   /* connect to the server */
   QQOut( "Connecting to server management interface...", hb_eol() )

   pConnection := netio_getconnection( cIP, nPort,, cPassword )
   cPassword := NIL

   IF ! Empty( pConnection )

      netio_funcexec( pConnection, "netio_sendclientinfo", netiosrv_clientinfo() )

      QQOut( "Connected.", hb_eol() )
      QQOut( hb_eol() )
      QQOut( "Type a command or '?' for help.", hb_eol() )

      aHistory   := { "quit" }
      nHistIndex := Len( aHistory ) + 1
      hCommands  := hbnetiosrv_LoadCmds( pConnection )

      /* Command prompt */
      DO WHILE .T.

         cCommand := Space( 128 )

         QQOut( "hbnetiosrv$ " )
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

      /* Never reached */
      netio_disconnect( cIP, nPort )
   ENDIF

   RETURN

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

/* Commands */

STATIC PROCEDURE cmdSysInfo( pConnection )
   LOCAL cLine

   FOR EACH cLine IN netio_funcexec( pConnection , "netio_sysinfo" )
      QQOut( cLine, hb_eol() )
   NEXT

   RETURN

STATIC PROCEDURE cmdConnStop( pConnection, cCommand )
   LOCAL aToken := hb_ATokens( cCommand, " " )

   IF Len( aToken ) > 1
      netio_funcexec( pConnection, "netio_stop", aToken[ 2 ] )
   ELSE
      QQOut( "Error: Invalid syntax.", hb_eol() )
   ENDIF

   RETURN

STATIC PROCEDURE cmdConnInfo( pConnection )
   LOCAL aArray := netio_funcexec( pConnection, "netio_conninfo" )
   LOCAL hConn

   QQOut( "Number of connections: " + hb_ntos( Len( aArray ) ), hb_eol() )

   FOR EACH hConn IN aArray
      QQOut( "#" + hb_ntos( hConn:__enumIndex() ) + " " +;
             hb_TToC( hConn[ "tStart" ], "YYYY.MM.DD", "HH:MM:SS" ) + " " +;
             PadR( hConn[ "cStatus" ], 12 ) + " " +;
             "fcnt: " + Str( hConn[ "nFilesCount" ] ) + " " +;
             "send: " + Str( hConn[ "nBytesSent" ] ) + " " +;
             "recv: " + Str( hConn[ "nBytesReceived" ] ) + " " +;
             hConn[ "cAddressPeer" ], hb_eol() )
   NEXT

   RETURN

PROCEDURE cmdConnEnable( pConnection, lValue )

   netio_funcexec( pConnection, "netio_conn", lValue )

   RETURN

PROCEDURE cmdConnLogEnable( pConnection, lValue )

   netio_funcexec( pConnection, "netio_logconn", lValue )

   RETURN
