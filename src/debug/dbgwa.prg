/*
 * The Debugger Work Area Inspector
 *
 * Copyright 2001-2002 Ignacio Ortiz de Zuniga <ignacio@fivetech.com>
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

#include "box.ch"
#include "dbstruct.ch"
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

   LOCAL cColor := iif( __dbg():lMonoDisplay, "N/W, W/N, W+/W, W+/N", "N/W, N/BG, R/W, R/BG" )

   LOCAL n1
   LOCAL n2
   LOCAL n3 := 1
   LOCAL cur_id

   LOCAL nOldArea := Select()

   hb_WAEval( {|| AAdd( aAlias, { Select(), Alias() } ) } )

   IF Len( aAlias ) == 0
      __dbgAlert( "No workareas in use" )
      RETURN
   ENDIF

   IF ( cur_id := AScan( aAlias, {| x | x[ 1 ] == nOldArea } ) ) == 0
      cur_id := 1
      dbSelectArea( aAlias[ 1 ][ 1 ] )
   ENDIF

   /* Window creation */

   oDlg := HBDbWindow():New( 2, 2, 21, 79, "", cColor )

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

   aBrw[ 1 ]:AddColumn( oCol := HBDbColumnNew( "", {|| hb_UPadR( aAlias[ n1 ][ 2 ], 11 ) } ) )

   oCol:ColorBlock := {|| iif( aAlias[ n1 ][ 1 ] == Select(), { 3, 4 }, { 1, 2 } ) }

   IF cur_id > 1
      aBrw[ 1 ]:Configure():MoveCursor( cur_id - 1 )
   ENDIF

   /* Info Browse */

   aInfo := ( aAlias[ n1 ][ 1 ] )->( DbfInfo() )

   aBrw[ 2 ] := HBDbBrowser():new( oDlg:nTop + 7, oDlg:nLeft + 13, oDlg:nBottom - 1, oDlg:nLeft + 52 )

   aBrw[ 2 ]:Cargo         := ( n2 := 1 )
   aBrw[ 2 ]:ColorSpec     := oDlg:cColor
   aBrw[ 2 ]:GoTopBlock    := {|| aBrw[ 2 ]:Cargo := n2 := 1 }
   aBrw[ 2 ]:GoBottomBlock := {|| aBrw[ 2 ]:Cargo := n2 := Len( aInfo ) }
   aBrw[ 2 ]:SkipBlock     := {| nSkip, nPos | nPos := n2, ;
      aBrw[ 2 ]:Cargo := n2 := iif( nSkip > 0, Min( Len( aInfo ), n2 + nSkip ), ;
      Max( 1, n2 + nSkip ) ), ;
      n2 - nPos }

   aBrw[ 2 ]:AddColumn( oCol := HBDbColumnNew( "", {|| hb_UPadR( aInfo[ n2 ], 40 ) } ) )

   oCol:ColorBlock := {|| iif( aAlias[ n1 ][ 1 ] == Select() .AND. n2 == 1, { 3, 4 }, { 1, 2 } ) }

   /* Struc browse */

   aStruc := ( aAlias[ n1 ][ 1 ] )->( dbStruct() )

   aBrw[ 3 ] := HBDbBrowser():new( oDlg:nTop + 1, oDlg:nLeft + 54, oDlg:nBottom - 1, oDlg:nLeft + 76 )

   aBrw[ 3 ]:Cargo         := n3 := 1
   aBrw[ 3 ]:ColorSpec     := oDlg:cColor
   aBrw[ 3 ]:GoTopBlock    := {|| aBrw[ 3 ]:Cargo := n3 := 1 }
   aBrw[ 3 ]:GoBottomBlock := {|| aBrw[ 3 ]:Cargo := n3 := Len( aStruc ) }
   aBrw[ 3 ]:SkipBlock     := {| nSkip, nPos | nPos := n3, ;
      aBrw[ 3 ]:Cargo := n3 := iif( nSkip > 0, Min( Len( aStruc ), n3 + nSkip ), ;
      Max( 1, n3 + nSkip ) ), n3 - nPos }

   aBrw[ 3 ]:AddColumn( HBDbColumnNew( "", {|| hb_UPadR( aStruc[ n3 ][ DBS_NAME ], 10 ) + " " + ;
      hb_UPadR( aStruc[ n3 ][ DBS_TYPE ], 4 ) + " " + ;
      Str( aStruc[ n3 ][ DBS_LEN ], 3 ) + " " + ;
      Str( aStruc[ n3 ][ DBS_DEC ], 2 ) } ) )

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

   hb_DispBox( oDlg:nTop + 1, oDlg:nLeft + 53, oDlg:nBottom - 1, oDlg:nLeft + 53, HB_B_SINGLE_UNI, oDlg:cColor )
   hb_DispOutAtBox( oDlg:nTop, oDlg:nLeft + 53, hb_UTF8ToStrBox( "┬" ), oDlg:cColor )
   hb_DispOutAtBox( oDlg:nBottom, oDlg:nLeft + 53, hb_UTF8ToStrBox( "┴" ), oDlg:cColor )

   hb_DispBox( oDlg:nTop + 6, oDlg:nLeft + 13, oDlg:nTop + 6, oDlg:nLeft + 52, HB_B_SINGLE_UNI, oDlg:cColor )
   hb_DispOutAtBox( oDlg:nTop + 6, oDlg:nLeft + 12, hb_UTF8ToStrBox( "├" ), oDlg:cColor )
   hb_DispOutAtBox( oDlg:nTop + 6, oDlg:nLeft + 53, hb_UTF8ToStrBox( "┤" ), oDlg:cColor )

   /* Display labels */

   hb_DispOutAt( oDlg:nTop + 1, oDlg:nLeft + 15, "Alias:              Record:           ", oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 2, oDlg:nLeft + 15, "   BOF:         Deleted:              ", oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 3, oDlg:nLeft + 15, "   EOF:           Found:              ", oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 4, oDlg:nLeft + 15, "Filter:                               ", oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 5, oDlg:nLeft + 15, "   Key:                               ", oDlg:cColor )

   /* Stabilize browse */

   aBrw[ 1 ]:ForceStable()
   aBrw[ 2 ]:ForceStable()
   aBrw[ 3 ]:ForceStable()
   aBrw[ 2 ]:Dehilite()
   aBrw[ 3 ]:Dehilite()

   UpdateInfo( oDlg, Alias() )

   RETURN

STATIC PROCEDURE DlgWorkAreaKey( nKey, oDlg, aBrw, aAlias, /* @ */ aStruc, /* @ */ aInfo )

   LOCAL oDebug := __dbg()
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

   SWITCH oDebug:nWaFocus
   CASE 1
      nAlias := aBrw[ 1 ]:Cargo
      WorkAreasKeyPressed( nKey, aBrw[ 1 ], Len( aAlias ) )
      IF nAlias != aBrw[ 1 ]:Cargo
         aBrw[ 2 ]:GoTop()
         aBrw[ 2 ]:Invalidate()
         aBrw[ 2 ]:ForceStable()
         aInfo := ( aAlias[ aBrw[ 1 ]:Cargo ][ 1 ] )->( DbfInfo() )
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
      EXIT
   CASE 2
      WorkAreasKeyPressed( nKey, aBrw[ 2 ], Len( aInfo ) )
      EXIT
   CASE 3
      WorkAreasKeyPressed( nKey, aBrw[ 3 ], Len( aStruc ) )
      EXIT
   ENDSWITCH

   RETURN

STATIC PROCEDURE WorkAreasKeyPressed( nKey, oBrw, nTotal )

   SWITCH nKey
   CASE K_UP

      IF oBrw:Cargo > 1
         oBrw:Cargo--
         oBrw:RefreshCurrent()
         oBrw:Up()
         oBrw:ForceStable()
      ENDIF
      EXIT

   CASE K_DOWN

      IF oBrw:Cargo < nTotal
         oBrw:Cargo++
         oBrw:RefreshCurrent()
         oBrw:Down()
         oBrw:ForceStable()
      ENDIF
      EXIT

   CASE K_HOME
   CASE K_CTRL_PGUP
   CASE K_CTRL_HOME

      IF oBrw:Cargo > 1
         oBrw:Cargo := 1
         oBrw:GoTop()
         oBrw:ForceStable()
      ENDIF
      EXIT

   CASE K_END
   CASE K_CTRL_PGDN
   CASE K_CTRL_END

      IF oBrw:Cargo < nTotal
         oBrw:Cargo := nTotal
         oBrw:GoBottom()
         oBrw:ForceStable()
      ENDIF
      EXIT

   ENDSWITCH

   RETURN

STATIC FUNCTION DbfInfo()

   LOCAL nFor
   LOCAL xValue
   LOCAL cValue

   LOCAL aInfo := { ;
      "[" + hb_ntos( Select( Alias() ) ) + "] " + Alias(), ;
      Space( 4 ) + "Current Driver", ;
      Space( 8 ) + rddName(), ;
      Space( 4 ) + "Workarea Information", ;
      Space( 8 ) + "Select Area: " + hb_ntos( Select() ), ;
      Space( 8 ) + "Record Size: " + hb_ntos( RecSize() ), ;
      Space( 8 ) + "Header Size: " + hb_ntos( Header() ), ;
      Space( 8 ) + "Field Count: " + hb_ntos( FCount() ), ;
      Space( 8 ) + "Last Update: " + DToC( LUpdate() ), ;
      Space( 8 ) + "Index order: " + hb_ntos( IndexOrd() ), ;
      Space( 4 ) + "Current Record" }

   FOR nFor := 1 TO FCount()

      xValue := __dbg():GetExprValue( "FieldGet(" + hb_ntos( nFor ) + ")" )

      SWITCH ValType( xValue )
      CASE "C"
      CASE "M"
         cValue := xValue
         EXIT
#ifdef HB_CLP_STRICT
      CASE "L"
         cValue := iif( xValue, "T", "F" )
#endif
      OTHERWISE
         cValue := __dbgValToStr( xValue )
      ENDSWITCH

      AAdd( aInfo, Space( 8 ) + hb_UPadR( FieldName( nFor ), 10 ) + " = " + hb_UPadR( cValue, 19 ) )

   NEXT

   RETURN aInfo

STATIC PROCEDURE UpdateInfo( oDlg, cAlias )

   LOCAL nOldArea

   IF Empty( cAlias )
      RETURN
   ENDIF

   nOldArea := Select()

   dbSelectArea( cAlias )

   hb_DispOutAt( oDlg:nTop + 1, oDlg:nLeft + 22, hb_UPadR( cAlias, 12 ), oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 1, oDlg:nLeft + 42, ;
      hb_UPadR( hb_ntos( RecNo() ) + "/" + hb_ntos( LastRec() ), 9 ), ;
      oDlg:cColor )

   hb_DispOutAt( oDlg:nTop + 2, oDlg:nLeft + 23, iif( Bof(), "Yes", "No " ), oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 2, oDlg:nLeft + 40, iif( Deleted(), "Yes", "No " ), oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 3, oDlg:nLeft + 23, iif( Eof(), "Yes", "No " ), oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 3, oDlg:nLeft + 40, iif( Found(), "Yes", "No " ), oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 4, oDlg:nLeft + 23, hb_UPadR( dbFilterInfo(), 30 ), oDlg:cColor )
   hb_DispOutAt( oDlg:nTop + 5, oDlg:nLeft + 23, hb_UPadR( ordKey(), 30 ), oDlg:cColor )

   dbSelectArea( nOldArea )

   RETURN

STATIC FUNCTION dbFilterInfo()
   RETURN iif( Empty( dbFilter() ), ;
               iif( Empty( hb_dbGetFilter() ), "", "{|| ... }" ), dbFilter() )
