/*
 * $Id$
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 *    ...
 *
 * See COPYING for licensing terms.
 *
 */

FUNCTION hbnetiosrv_LoadCmds( bQuit, bShowInfo )
   LOCAL hCmds := { ;
                     "?"       => { ""               , "Synonym for 'help'."           , {|| cmdHelp( hCmds ) } },;
                     "clear"   => { ""               , "Clear screen."                 , {|| Scroll(), SetPos( 0, 0 ) } },;
                     "config"  => { ""               , "Show server configuration."    , bShowInfo },;
                     "sysinfo" => { ""               , "Show system/build information.", {|| cmdSysInfo() } },;
                     "conn"    => { ""               , "Show connection information."  , {| cCommand, netiosrv | HB_SYMBOL_UNUSED( cCommand ), cmdConnInfo( netiosrv ) } },;
                     "stop"    => { "[<ip:port>|all]", "Stop specified connection(s)." , {| cCommand, netiosrv | cmdConnStop( cCommand, netiosrv ) } },;
                     "quit"    => { ""               , "Stop server and exit."         , bQuit },;
                     "help"    => { ""               , "Display this help."            , {|| cmdHelp( hCmds ) } };
                  }

   RETURN hCmds

/* TODO: - on the fly change of RPC filter modules
         - listing open files
         - listing active locks
         - gracefully shutting down server by waiting for connections to close and not accept new ones
         - pausing server */

STATIC PROCEDURE cmdSysInfo()
   QQOut( "OS: "         + OS(), hb_eol() )
   QQOut( "Harbour: "    + Version(), hb_eol() )
   QQOut( "C Compiler: " + hb_Compiler(), hb_eol() )
   QQOut( "Memory: "     + hb_ntos( Memory( 0 ) ) + "KB", hb_eol() )
   RETURN

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
