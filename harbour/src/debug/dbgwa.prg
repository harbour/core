/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Debugger Work Area Inspector
 *
 * Copyright 2001-2002 Ignacio Ortiz de Zuniga <ignacio@fivetech.com>
 * www - http://harbour-project.org
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

#pragma DEBUGINFO=OFF

#include "box.ch"
#include "setcurs.ch"
#include "inkey.ch"

REQUEST FieldGet

PROCEDURE __dbgShowWorkAreas()

   LOCAL oDlg
   LOCAL oCol

   LOCAL aAlias := {}
   LOCAL aBrw[ 3 ]
   LOCAL aStruc
   LOCAL aInfo

   LOCAL cColor := iif( __Dbg():lMonoDisplay, "N/W, W/N, W+/W, W+/N", "N/W, N/BG, R/W, R/BG" )

   LOCAL n1
   LOCAL n2
   LOCAL n3 := 1
   LOCAL cur_id := 1

   LOCAL nOldArea := Select()

   /* We can't determine the last used area, so use 512 here */
   FOR n1 := 1 TO 512
      IF ( n1 )->( Used() )
         AAdd( aAlias, { n1, Alias( n1 ) } )
         IF n1 == nOldArea
            cur_id := Len( aAlias )
         ENDIF
      ENDIF
   NEXT

   IF Len( aAlias ) == 0
      __dbgAlert( "No workareas in use" )
      RETURN
   ENDIF

   IF ! Used()
      dbSelectArea( aAlias[ 1 ][ 1 ] )
   ENDIF

   /* Window creation */

   oDlg := HBDbWindow():New( 2, 3, 21, 74, "", cColor )

   oDlg:bKeyPressed := {| nKey | DlgWorkAreaKey( nKey, oDlg, aBrw, aAlias, @aStruc, @aInfo ) }
   oDlg:bPainted    := {|| DlgWorkAreaPaint( oDlg, aBrw ) }

   /* Alias browse */

   aBrw[ 1 ] := HBDbBrowser():new( oDlg:nTop + 1, oDlg:nLeft + 1, oDlg:nBottom - 1, oDlg:nLeft + 11 )

   aBrw[ 1 ]:Cargo         := ( n1 := cur_id )
   aBrw[ 1 ]:ColorSpec     := oDlg:cColor
   aBrw[ 1 ]:GoTopBlock    := {|| aBrw[ 1 ]:Cargo := n1 := 1 }
   aBrw[ 1 ]:GoBottomBlock := {|| aBrw[ 1 ]:Cargo := n1 := Len( aAlias ) }
   aBrw[ 1 ]:SkipBlock     := {| nSkip, nPos | nPos := n1, ;
      aBrw[ 1 ]:Cargo := n1 := iif( nSkip > 0, Min( Len( aAlias ), n1 + nSkip ), ;
      Max( 1, n1 + nSkip ) ), ;
      n1 - nPos }

   aBrw[ 1 ]:AddColumn( oCol := HBDbColumnNew( "", {|| PadR( aAlias[ n1 ][ 2 ], 11 ) } ) )

   oCol:ColorBlock := {|| iif( aAlias[ n1 ][ 1 ] == Select(), { 3, 4 }, { 1, 2 } ) }

   /* Info Browse */

   aInfo := ( aAlias[ n1 ][ 1 ] )->( DbfInfo() )

   aBrw[ 2 ] := HBDbBrowser():new( oDlg:nTop + 7, oDlg:nLeft + 13, oDlg:nBottom - 1, oDlg:nLeft + 50 )

   aBrw[ 2 ]:Cargo         := ( n2 := 1 )
   aBrw[ 2 ]:ColorSpec     := oDlg:cColor
   aBrw[ 2 ]:GoTopBlock    := {|| aBrw[ 2 ]:Cargo := n2 := 1 }
   aBrw[ 2 ]:GoBottomBlock := {|| aBrw[ 2 ]:Cargo := n2 := Len( aInfo ) }
   aBrw[ 2 ]:SkipBlock     := {| nSkip, nPos | nPos := n2, ;
      aBrw[ 2 ]:Cargo := n2 := iif( nSkip > 0, Min( Len( aInfo ), n2 + nSkip ), ;
      Max( 1, n2 + nSkip ) ), ;
      n2 - nPos }

   aBrw[ 2 ]:AddColumn( oCol := HBDbColumnNew( "", {|| PadR( aInfo[ n2 ], 38 ) } ) )

   oCol:ColorBlock := {|| iif( aAlias[ n1 ][ 1 ] == Select() .AND. n2 == 1, { 3, 4 }, { 1, 2 } ) }

   /* Struc browse */

   aStruc := ( aAlias[ n1 ][ 1 ] )->( dbStruct() )

   aBrw[ 3 ] := HBDbBrowser():new( oDlg:nTop + 1, oDlg:nLeft + 52, oDlg:nBottom - 1, oDlg:nLeft + 70 )

   aBrw[ 3 ]:Cargo         := n3 := 1
   aBrw[ 3 ]:ColorSpec     := oDlg:cColor
   aBrw[ 3 ]:GoTopBlock    := {|| aBrw[ 3 ]:Cargo := n3 := 1 }
   aBrw[ 3 ]:GoBottomBlock := {|| aBrw[ 3 ]:Cargo := n3 := Len( aStruc ) }
   aBrw[ 3 ]:SkipBlock     := {| nSkip, nPos | nPos := n3, ;
      aBrw[ 3 ]:Cargo := n3 := iif( nSkip > 0, Min( Len( aStruc ), n3 + nSkip ), ;
      Max( 1, n3 + nSkip ) ), n3 - nPos }

   aBrw[ 3 ]:AddColumn( HBDbColumnNew( "", {|| PadR( aStruc[ n3, 1 ], 11 ) + ;
      aStruc[ n3, 2 ] + ;
      Str( aStruc[ n3, 3 ], 4 ) + ;
      Str( aStruc[ n3, 4 ], 3 ) } ) )

   /* Show dialog */

   oDlg:ShowModal()

   dbSelectArea( nOldArea )

   RETURN

STATIC PROCEDURE DlgWorkAreaPaint( oDlg, aBrw )

   /* Display captions */

   hb_DispOutAt( oDlg:nTop, oDlg:nLeft + 5, " Area ", oDlg:cColor )
   hb_DispOutAt( oDlg:nTop, oDlg:nLeft + 28, " Status ", oDlg:cColor )
   hb_DispOutAt( oDlg:nTop, oDlg:nLeft + 56, " Structure ", oDlg:cColor )

   /* Display separator lines */

   hb_DispBox( oDlg:nTop + 1, oDlg:nLeft + 12, oDlg:nBottom - 1, oDlg:nLeft + 12, HB_B_SINGLE_UNI, oDlg:cColor )
   hb_DispOutAtBox( oDlg:nTop, oDlg:nLeft + 12, hb_UTF8ToStrBox( "┬" ), oDlg:cColor )
   hb_DispOutAtBox( oDlg:nBottom, oDlg:nLeft + 12, hb_UTF8ToStrBox( "┴" ), oDlg:cColor )

   hb_DispBox( oDlg:nTop + 1, oDlg:nLeft + 51, oDlg:nBottom - 1, oDlg:nLeft + 51, HB_B_SINGLE_UNI, oDlg:cColor )
   hb_DispOutAtBox( oDlg:nTop, oDlg:nLeft + 51, hb_UTF8ToStrBox( "┬" ), oDlg:cColor )
   hb_DispOutAtBox( oDlg:nBottom, oDlg:nLeft + 51, hb_UTF8ToStrBox( "┴" ), oDlg:cColor )

   hb_DispBox( oDlg:nTop + 6, oDlg:nLeft + 13, oDlg:nTop + 6, oDlg:nLeft + 50, HB_B_SINGLE_UNI, oDlg:cColor )
   hb_DispOutAtBox( oDlg:nTop + 6, oDlg:nLeft + 12, hb_UTF8ToStrBox( "├" ), oDlg:cColor )
   hb_DispOutAtBox( oDlg:nTop + 6, oDlg:nLeft + 51, hb_UTF8ToStrBox( "┤" ), oDlg:cColor )

   /* Display labels */

   hb_DispOutAt( oDlg:nTop + 1, oDlg:nLeft + 13, "Alias:                Record:         ", oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 2, oDlg:nLeft + 13, "   BOF:         Deleted:              ", oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 3, oDlg:nLeft + 13, "   EOF:           Found:              ", oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 4, oDlg:nLeft + 13, "Filter:                               ", oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 5, oDlg:nLeft + 13, "   Key:                               ", oDlg:cColor )

   /* Stabilize browse */

   aBrw[ 1 ]:ForceStable()
   aBrw[ 2 ]:ForceStable()
   aBrw[ 3 ]:ForceStable()
   aBrw[ 2 ]:Dehilite()
   aBrw[ 3 ]:Dehilite()

   UpdateInfo( oDlg, Alias() )

   RETURN

STATIC PROCEDURE DlgWorkAreaKey( nKey, oDlg, aBrw, aAlias, aStruc, aInfo )

   LOCAL oDebug := __Dbg()
   LOCAL nAlias

   IF nKey == K_TAB .OR. nKey == K_SH_TAB
      aBrw[ oDebug:nWaFocus ]:Dehilite()
      oDebug:nWaFocus += iif( nKey == K_TAB, 1, -1 )
      IF oDebug:nWaFocus < 1
         oDebug:nWaFocus := 3
      ENDIF
      IF oDebug:nWaFocus > 3
         oDebug:nWaFocus := 1
      ENDIF
      aBrw[ oDebug:nWaFocus ]:Hilite()
      RETURN
   ENDIF

   DO CASE
   CASE oDebug:nWaFocus == 1
      nAlias := aBrw[ 1 ]:Cargo
      WorkAreasKeyPressed( nKey, aBrw[ 1 ], Len( aAlias ) )
      IF nAlias != aBrw[ 1 ]:Cargo
         aBrw[ 2 ]:GoTop()
         aBrw[ 2 ]:Invalidate()
         aBrw[ 2 ]:ForceStable()
         aInfo := ( aAlias[ aBrw[ 1 ]:Cargo ][ 1 ] )->( DbfInfo( aInfo ) )
         aBrw[ 3 ]:Configure()
         aBrw[ 2 ]:Invalidate()
         aBrw[ 2 ]:RefreshAll()
         aBrw[ 2 ]:ForceStable()
         aBrw[ 2 ]:Dehilite()
         aBrw[ 3 ]:GoTop()
         aBrw[ 3 ]:Invalidate()
         aBrw[ 3 ]:ForceStable()
         aStruc := ( aAlias[ aBrw[ 1 ]:Cargo ][ 1 ] )->( dbStruct() )
         aBrw[ 3 ]:Configure()
         aBrw[ 3 ]:Invalidate()
         aBrw[ 3 ]:RefreshAll()
         aBrw[ 3 ]:ForceStable()
         aBrw[ 3 ]:Dehilite()
         UpdateInfo( oDlg, aAlias[ aBrw[ 1 ]:Cargo ][ 2 ] )
      ENDIF
   CASE oDebug:nWaFocus == 2
      WorkAreasKeyPressed( nKey, aBrw[ 2 ], Len( aInfo ) )
   CASE oDebug:nWaFocus == 3
      WorkAreasKeyPressed( nKey, aBrw[ 3 ], Len( aStruc ) )
   ENDCASE

   RETURN

STATIC PROCEDURE WorkAreasKeyPressed( nKey, oBrw, nTotal )

   DO CASE
   CASE nKey == K_UP

      IF oBrw:Cargo > 1
         oBrw:Cargo--
         oBrw:RefreshCurrent()
         oBrw:Up()
         oBrw:ForceStable()
      ENDIF

   CASE nKey == K_DOWN

      IF oBrw:Cargo < nTotal
         oBrw:Cargo++
         oBrw:RefreshCurrent()
         oBrw:Down()
         oBrw:ForceStable()
      ENDIF

   CASE nKey == K_HOME .OR. nKey == K_CTRL_PGUP .OR. nKey == K_CTRL_HOME

      IF oBrw:Cargo > 1
         oBrw:Cargo := 1
         oBrw:GoTop()
         oBrw:ForceStable()
      ENDIF

   CASE nKey == K_END .OR. nKey == K_CTRL_PGDN .OR. nKey == K_CTRL_END

      IF oBrw:Cargo < nTotal
         oBrw:Cargo := nTotal
         oBrw:GoBottom()
         oBrw:ForceStable()
      ENDIF

   ENDCASE

   RETURN

STATIC FUNCTION DbfInfo( aInfo )

   LOCAL nFor
   LOCAL xType
   LOCAL xValue
   LOCAL cValue

   aInfo := {}

   AAdd( aInfo, "[" + hb_ntos( Select( Alias() ) ) + "] " + Alias() )
   AAdd( aInfo, Space( 4 ) + "Current Driver" )
   AAdd( aInfo, Space( 8 ) + rddName() )
   AAdd( aInfo, Space( 4 ) + "Workarea Information" )
   AAdd( aInfo, Space( 8 ) + "Select Area: " + hb_ntos( Select() ) )
   AAdd( aInfo, Space( 8 ) + "Record Size: " + hb_ntos( RecSize() ) )
   AAdd( aInfo, Space( 8 ) + "Header Size: " + hb_ntos( Header() ) )
   AAdd( aInfo, Space( 8 ) + "Field Count: " + hb_ntos( FCount() ) )
   AAdd( aInfo, Space( 8 ) + "Last Update: " + DToC( LUpdate() ) )
   AAdd( aInfo, Space( 8 ) + "Index order: " + hb_ntos( IndexOrd() ) )
   AAdd( aInfo, Space( 4 ) + "Current Record" )

   FOR nFor := 1 TO FCount()

      xValue := __Dbg():GetExprValue( "FieldGet(" + hb_ntos( nFor ) + ")" )
      xType  := ValType( xValue )

      SWITCH xType
      CASE "C"
      CASE "M"
         cValue := xValue
         EXIT
      CASE "N"
         cValue := hb_ntos( xValue )
         EXIT
      CASE "D"
         cValue := DToC( xValue )
         EXIT
      CASE "T"
         cValue := hb_TSToStr( xValue )
         EXIT
      CASE "L"
         cValue := iif( xValue, ".T.", ".F." )
         EXIT
      CASE "A"
         cValue := "Array"
         EXIT
      CASE "H"
         cValue := "Hash"
         EXIT
      CASE "U"
         cValue := "NIL"
         EXIT
      OTHERWISE
         cValue := "Error"
      ENDSWITCH

      AAdd( aInfo, Space( 8 ) + PadR( FieldName( nFor ), 10 ) + " = " + PadR( cValue, 17 ) )

   NEXT

   RETURN aInfo

STATIC PROCEDURE UpdateInfo( oDlg, cAlias )

   LOCAL nOldArea

   IF Empty( cAlias )
      RETURN
   ENDIF

   nOldArea := Select()

   dbSelectArea( cAlias )

   hb_DispOutAt( oDlg:nTop + 1, oDlg:nLeft + 20, PadR( cAlias, 11 ), oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 1, oDlg:nLeft + 42, ;
      PadR( hb_ntos( RecNo() ) + "/" + hb_ntos( LastRec() ), 9 ), ;
      oDlg:cColor )

   hb_DispOutAt( oDlg:nTop + 2, oDlg:nLeft + 21, iif( Bof(), "Yes", "No " ), oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 2, oDlg:nLeft + 38, iif( Deleted(), "Yes", "No " ), oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 3, oDlg:nLeft + 21, iif( Eof(), "Yes", "No " ), oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 3, oDlg:nLeft + 38, iif( Found(), "Yes", "No " ), oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 4, oDlg:nLeft + 21, PadR( dbFilter(), 29 ), oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 5, oDlg:nLeft + 21, PadR( ordKey(), 29 ), oDlg:cColor )

   dbSelectArea( nOldArea )

   RETURN
