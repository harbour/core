/*
 * The Debugger Help
 *
 * Copyright 2002 Antonio Linares <alinares@fivetech.com>
 * Copyright 2014 Viktor Szakats (vszakats.net/harbour) (GetTopics())
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
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

#pragma -b-

/* NOTE: Don't use SAY/DevOut()/DevPos() for screen output, otherwise
         the debugger output may interfere with the applications output
         redirection, and is also slower. [vszakats] */

#include "box.ch"
#include "inkey.ch"

PROCEDURE __dbgHelp( cTopic )

   LOCAL cColor := iif( __dbg():lMonoDisplay, "N/W, W/N, W+/W, W+/N", "N/W, N/BG, R/W, R/BG" )
   LOCAL oBrw
   LOCAL nTopic
   LOCAL aTopics := GetTopics()

   LOCAL oDlg := HBDbWindow():New( 2, 2, MaxRow() - 2, MaxCol() - 2, "Help", cColor )

   oBrw := HBDbBrowser():New( oDlg:nTop + 1, oDlg:nLeft + 1, oDlg:nBottom - 1, oDlg:nLeft + 12 )
   oBrw:Cargo := 1
   oBrw:AddColumn( HBDbColumnNew( "", {|| aTopics[ oBrw:Cargo ][ 1 ] }, 12 ) )
   oBrw:ColorSpec := StrTran( __dbg():ClrModal(), ", R/W" )
   oBrw:SkipBlock := {| nSkip, nOld | nOld := oBrw:Cargo, oBrw:Cargo += nSkip, ;
      oBrw:Cargo := Min( Max( oBrw:Cargo, 1 ), Len( aTopics ) ), ;
      oBrw:Cargo - nOld }
   oBrw:GoTopBlock := {|| oBrw:Cargo := 1 }
   oBrw:GoBottomBlock := {|| oBrw:Cargo := Len( aTopics ) }

   IF HB_ISSTRING( cTopic ) .AND. ;
      ( nTopic := AScan( aTopics, {| x | hb_LeftEqI( x[ 1 ], cTopic ) } ) ) > 1
      oBrw:nFirstVisible := nTopic
   ENDIF

   oDlg:bPainted := {|| PaintWindow( oDlg, oBrw, aTopics ) }
   oDlg:bKeyPressed := {| nKey | ProcessKey( nKey, oDlg, oBrw, aTopics, oDlg:cColor ) }

   oDlg:ShowModal()

   RETURN

STATIC PROCEDURE PaintWindow( oDlg, oBrw, aTopics )

   hb_DispBox( oDlg:nTop + 1, oDlg:nLeft + 13, oDlg:nBottom - 1, oDlg:nLeft + 13, HB_B_SINGLE_UNI, oDlg:cColor )
   hb_DispOutAtBox( oDlg:nTop, oDlg:nLeft + 13, hb_UTF8ToStrBox( "┬" ), oDlg:cColor )
   hb_DispOutAtBox( oDlg:nBottom, oDlg:nLeft + 13, hb_UTF8ToStrBox( "┴" ), oDlg:cColor )

   oBrw:ForceStable()
   ShowTopic( oDlg, aTopics, oBrw:Cargo, 0 )  // Start on page 1

   RETURN

STATIC PROCEDURE ProcessKey( nKey, oDlg, oBrw, aTopics )

   LOCAL n
   LOCAL nSkip

   SWITCH nKey
   CASE K_UP

      IF oBrw:Cargo > 1
         oBrw:Up()
         oBrw:ForceStable()
         ShowTopic( oDlg, aTopics, oBrw:Cargo, 0 )  // Start on page 1
      ENDIF
      EXIT

   CASE K_DOWN

      IF oBrw:Cargo < Len( aTopics )
         oBrw:Down()
         oBrw:ForceStable()
         ShowTopic( oDlg, aTopics, oBrw:Cargo, 0 )  // Start on page 1
      ENDIF
      EXIT

   CASE K_HOME

      IF oBrw:Cargo > 1
         oBrw:GoTop()
         oBrw:ForceStable()
         ShowTopic( oDlg, aTopics, oBrw:Cargo, 0 )  // Start on page 1
      ENDIF
      EXIT

   CASE K_END

      IF oBrw:Cargo < Len( aTopics )
         oBrw:GoBottom()
         oBrw:ForceStable()
         ShowTopic( oDlg, aTopics, oBrw:Cargo, 0 )  // Start on page 1
      ENDIF
      EXIT

   CASE K_PGUP
   CASE K_CTRL_B

      ShowTopic( oDlg, aTopics, oBrw:Cargo, -1 )  // Skip to prev page
      EXIT

   CASE K_PGDN
#if 0
   CASE K_CTRL_F
#endif
   CASE K_SPACE

      ShowTopic( oDlg, aTopics, oBrw:Cargo, 1 )  // Skip to next page
      EXIT

   CASE K_LBUTTONDOWN

      IF ( nSkip := MRow() - oDlg:nTop - oBrw:RowPos ) != 0
         IF nSkip > 0
            FOR n := 1 TO nSkip
               oBrw:Down()
               oBrw:Stabilize()
            NEXT
         ELSE
            FOR n := 1 TO nSkip + 2 STEP -1
               oBrw:Up()
               oBrw:Stabilize()
            NEXT
         ENDIF
         oBrw:ForceStable()
         ShowTopic( oDlg, aTopics, oBrw:Cargo, 0 )  // Start on page 1
      ENDIF
      EXIT

   ENDSWITCH

   RETURN

STATIC PROCEDURE ShowTopic( oDlg, aTopics, nTopic, nPageOp )

   LOCAL oDebug := __dbg()
   LOCAL nRows  := oDlg:nBottom - oDlg:nTop - 1
   LOCAL nPages
   LOCAL nRowsToPaint
   LOCAL n

   IF nTopic > Len( aTopics )
      nTopic := 1
   ENDIF

   nPages := Int( ( Len( aTopics[ nTopic ][ 2 ] ) + nRows - 1 ) / nRows )

   IF nPages <= 1
      IF nPageOp == -1 .OR. nPageOp == 1
         RETURN
      ENDIF
      oDebug:nHelpPage := 1
   ELSE
      SWITCH nPageOp
      CASE 0  // Show first page

         oDebug:nHelpPage := 1
         EXIT

      CASE 1  // Show next page

         IF oDebug:nHelpPage < nPages
            oDebug:nHelpPage++
         ELSE
            RETURN
         ENDIF
         EXIT

      CASE -1  // Show prev page

         IF oDebug:nHelpPage > 1
            oDebug:nHelpPage--
         ELSE
            RETURN
         ENDIF
         EXIT

      ENDSWITCH
   ENDIF

   hb_Scroll( oDlg:nTop + 1, oDlg:nLeft + 14, oDlg:nBottom - 1, oDlg:nRight - 1,,, oDlg:cColor )

   nRowsToPaint := Min( nRows, Len( aTopics[ nTopic ][ 2 ] ) - ( ( oDebug:nHelpPage - 1 ) * nRows ) )

   FOR n := 1 TO nRowsToPaint
      hb_DispOutAt( 2 + n, 16, aTopics[ nTopic ][ 2 ][ ( ( oDebug:nHelpPage - 1 ) * nRows ) + n ], oDlg:cColor )
   NEXT

   hb_DispOutAt( oDlg:nBottom, oDlg:nRight - 16, " Page " + Str( oDebug:nHelpPage, 1 ) + " of " + Str( nPages, 1 ) + " ", oDlg:cColor )

   RETURN

STATIC FUNCTION GetTopics()

   LOCAL aTopics := {}
   LOCAL cLine

   FOR EACH cLine IN hb_ATokens( help_en(), Chr( 10 ) )
      IF hb_LeftEq( cLine, "--" )
         AAdd( aTopics, { PadR( SubStr( cLine, Len( "--" ) + 1 ), 12 ), {} } )
      ELSEIF ! Empty( aTopics ) .AND. ;
         ( ! Empty( cLine ) .OR. ! cLine:__enumIsLast() )  /* skip last EOL */
         AAdd( ATail( aTopics )[ 2 ], cLine )
      ENDIF
   NEXT

   RETURN aTopics

STATIC FUNCTION help_en()
   #pragma __streaminclude "en.txt" | RETURN %s
