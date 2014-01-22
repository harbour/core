/*
 * Harbour Project source code:
 * Harbour NETIO server management cmdline tool
 *
 * Copyright 2009-2011 Viktor Szakats (vszakats.net/harbour)
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
 * their web site at https://www.gnu.org/).
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
   LOCAL aCommand
   LOCAL cCommand

   LOCAL bKeyDown
   LOCAL bKeyUp
   LOCAL bKeyIns
   LOCAL bKeyPaste
   LOCAL bKeyTab

   LOCAL aHistory, nHistIndex

   LOCAL lQuit

   LOCAL netclictrl := __hbshell_plugin()
   LOCAL netclictx
   LOCAL netcliID

   LOCAL hConIO := { ;
      "displine"  => {| c | hbnetiocon_ToConsole( c ) }, ;
      "gethidden" => {|| hbnetiocon_GetHidden() } }

   Set( _SET_CONFIRM, .F. )
   Set( _SET_SCOREBOARD, .F. )

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )
   Set( _SET_TIMEFORMAT, "HH:MM:SS" )

   SetCancel( .F. )

   netclictx := Eval( netclictrl[ "init" ], hConIO, { ;
      "--netio.addr=" + cIP + ":" + hb_ntos( nPort ), ;
      "--netio.pass=" + cPassword } )
   IF ! Empty( netclictx )
      netcliID := netclictrl[ "id" ]
   ENDIF

   hbnetiocon_ToConsole( "Type a command or '?' for help." )

   aHistory   := { "quit" }
   nHistIndex := Len( aHistory ) + 1

   hCommands  := { ;
      "?"    => { "", "Synonym for 'help'." , {|| ShowHelp( hCommands ), Eval( netclictrl[ "cmd" ], netclictx, "?" ) } }, ;
      "help" => { "", "Display this help."  , {|| ShowHelp( hCommands ), Eval( netclictrl[ "cmd" ], netclictx, "?" ) } }, ;
      "quit" => { "", "Exit console."       , {|| lQuit := .T. } } }

   lQuit := .F.

   DO WHILE ! lQuit

      cCommand := Space( 128 )

      QQOut( "$ " )
      nSavedRow := Row()

      @ nSavedRow, Col() GET cCommand PICTURE "@S" + hb_ntos( MaxCol() - Col() + 1 ) COLOR hb_ColorIndex( SetColor(), CLR_STANDARD ) + "," + hb_ColorIndex( SetColor(), CLR_STANDARD )

      SetCursor( iif( ReadInsert(), SC_INSERT, SC_NORMAL ) )

      bKeyIns   := SetKey( K_INS, ;
         {|| SetCursor( iif( ReadInsert( ! ReadInsert() ), ;
                          SC_NORMAL, SC_INSERT ) ) } )
      bKeyUp    := SetKey( K_UP, ;
         {|| iif( nHistIndex > 1, ;
                  cCommand := PadR( aHistory[ --nHistIndex ], Len( cCommand ) ), ), ;
                  ManageCursor( cCommand ) } )
      bKeyDown  := SetKey( K_DOWN, ;
         {|| cCommand := PadR( iif( nHistIndex < Len( aHistory ), ;
             aHistory[ ++nHistIndex ], ;
             ( nHistIndex := Len( aHistory ) + 1, "" ) ), Len( cCommand ) ), ;
                  ManageCursor( cCommand ) } )
      bKeyPaste := SetKey( K_ALT_V, {|| hb_gtInfo( HB_GTI_CLIPBOARDPASTE ) } )

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

      aCommand := hb_ATokens( cCommand )
      IF ! Empty( aCommand )
         IF Lower( aCommand[ 1 ] ) $ hCommands
            Eval( hCommands[ Lower( aCommand[ 1 ] ) ][ 3 ], cCommand )
         ELSE
            IF hb_LeftIs( cCommand, netcliID + "." )
               IF ! Eval( netclictrl[ "cmd" ], netclictx, SubStr( cCommand, Len( netcliID ) + 2 ) )
                  hbnetiocon_ToConsole( hb_StrFormat( "Error: Unknown command '%1$s'.", cCommand ) )
               ENDIF
            ELSE
               IF ! Eval( netclictrl[ "cmd" ], netclictx, cCommand )
                  hbnetiocon_ToConsole( hb_StrFormat( "Error: Unknown command '%1$s'.", cCommand ) )
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDDO

   Eval( netclictrl[ "exit" ], netclictx )

   RETURN

/* Adjusted the positioning of cursor on navigate through history. [vailtom] */
STATIC PROCEDURE ManageCursor( cCommand )

   hb_keyPut( K_HOME )
   IF ! Empty( cCommand )
      hb_keyPut( K_END )
   ENDIF

   RETURN

/* Complete the command line, based on the first characters that the user typed. [vailtom] */
STATIC PROCEDURE CompleteCmd( cCommand, hCommands )

   LOCAL s := Lower( AllTrim( cCommand ) )
   LOCAL n

   /* We need at least one character to search */
   IF Len( s ) > 1
      FOR EACH n IN hCommands
         IF hb_LeftIs( Lower( n:__enumKey() ), s )
            cCommand := PadR( n:__enumKey(), Len( cCommand ) )
            ManageCursor( cCommand )
            RETURN
         ENDIF
      NEXT
   ENDIF

   RETURN

STATIC PROCEDURE ShowHelp( hCommands )

   LOCAL aTexts := {}
   LOCAL n, c, m

   m := 8
   hb_HEval( hCommands, {| k, l | m := Max( m, Len( k + iif( Empty( l[ 1 ] ), "", " " + l[ 1 ] ) ) ) } )

   AAdd( aTexts, "Commands:" )

   /* Processing commands */
   FOR EACH n IN hCommands
      AAdd( aTexts, " " + PadR( n:__enumKey() + iif( Empty( n[ 1 ] ), "", " " + n[ 1 ] ), m ) + " - " + n[ 2 ] )
   NEXT
   AAdd( aTexts, "" )

   AAdd( aTexts, "Keyboard shortcuts:" )
   AAdd( aTexts, PadR( " <Up>", m )    + "  - Move up on historic list." )
   AAdd( aTexts, PadR( " <Down>", m )  + "  - Move down on historic list." )
   AAdd( aTexts, PadR( " <Tab>", m )   + "  - Complete command." )
   AAdd( aTexts, PadR( " <Alt+V>", m ) + "  - Paste from clipboard." )
   AAdd( aTexts, "" )

   c := 0
   m := MaxRow()

   FOR EACH n IN aTexts
      hbnetiocon_ToConsole( n )

      IF ++c == m
         c := 0
         QQOut( "Press any key to continue..." )
         Inkey( 0 )

         hb_Scroll( Row(), 0, Row(), MaxCol(), 0 )
         SetPos( Row(), 0 )
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE hbnetiocon_ToConsole( cText )

   QQOut( cText + hb_eol() )

   RETURN

STATIC FUNCTION hbnetiocon_GetHidden()

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
