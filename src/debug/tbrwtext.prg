/*
 * Harbour Project source code:
 * Text file browser class
 *
 * Copyright 2008 Lorenzo Fiorini <lorenzo.fiorini@gmail.com>
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

#pragma -b-

#define HB_CLS_NOTOBJECT      /* do not inherit from HBObject calss */
#include "hbclass.ch"

CREATE CLASS HBBrwText

   VAR cFileName
   VAR aRows
   VAR nRows
   VAR nLineNoLen
   VAR nActiveLine
   VAR lLineNumbers
   VAR nRow
   VAR nFirstCol
   VAR nCol

   VAR oBrw

   VAR nLineOffset   INIT 1
   VAR nMaxLineLen
   VAR nTabWidth     INIT 4

   VAR nTop
   VAR nLeft
   VAR nBottom
   VAR nRight

   VAR nWidth
   VAR nHeight

   METHOD New( nTop, nLeft, nBottom, nRight, cFileName, cColors, lLineNumbers )

   METHOD RefreshAll() INLINE ::oBrw:ForceStable():RefreshAll(), Self
   METHOD ForceStable() INLINE ::oBrw:ForceStable(), Self
   METHOD RefreshCurrent() INLINE ::oBrw:RefreshCurrent(), Self
   METHOD GotoLine( n )
   METHOD SetActiveLine( n )
   METHOD GetLine()
   METHOD GetLineText()
   METHOD GetLineColor()
   METHOD Search( cString, lCaseSensitive, nMode )

   METHOD GoFirst()
   METHOD GoLast()
   METHOD Skip( n )
   METHOD GoNext()
   METHOD GoPrev()

   METHOD Resize( nTop, nLeft, nBottom, nRight )

   METHOD Up() INLINE ::oBrw:Up():ForceStable(), Self
   METHOD Down() INLINE ::oBrw:Down():ForceStable(), Self
   METHOD PageUp() INLINE ::oBrw:PageUp():ForceStable(), Self
   METHOD PageDown() INLINE ::oBrw:PageDown():ForceStable(), Self
   METHOD GoTop() INLINE ::oBrw:GoTop():ForceStable(), Self
   METHOD GoBottom() INLINE ::oBrw:GoBottom():ForceStable(), Self

   METHOD Home() INLINE iif( ::nLineOffset > 1, ( ::nLineOffset := 1, ::oBrw:RefreshAll():ForceStable() ), ), Self
   METHOD End() INLINE ::nLineOffset := Max( 1, ::nMaxLineLen - ( ::nWidth - ::nLineNoLen ) + 1 ), ::oBrw:RefreshAll():ForceStable(), Self

   METHOD Right() INLINE iif( ::nLineOffset < ::nMaxLineLen, ( ::nLineOffset++, ::oBrw:RefreshAll():ForceStable() ), ), Self
   METHOD Left() INLINE iif( ::nLineOffset > 1, ( ::nLineOffset--, ::oBrw:RefreshAll():ForceStable() ), ), Self

   METHOD RowPos() INLINE ::nRow

   METHOD LoadFile( cFileName )

   VAR colorSpec IS colorSpec IN oBrw

ENDCLASS

METHOD New( nTop, nLeft, nBottom, nRight, cFileName, cColors, lLineNumbers ) CLASS HBBrwText

   LOCAL oCol

   ::nTop := nTop
   ::nLeft := nLeft
   ::nBottom := nBottom
   ::nRight := nRight

   ::nWidth := nRight - nLeft + 1
   ::nHeight := nBottom - nTop

   ::lLineNumbers := lLineNumbers

   ::oBrw := HBDbBrowser():New( ::nTop, ::nLeft, ::nBottom, ::nRight )

   ::oBrw:colorSpec := cColors

   oCol := HBDbColumnNew( "", {|| ::GetLineText() } )

   oCol:colorBlock := {|| ::GetLineColor() }

   ::oBrw:AddColumn( oCol )

   ::oBrw:goTopBlock := {|| ::nRow := 1 }
   ::oBrw:goBottomBlock := {|| ::nRow := ::nRows }
   ::oBrw:skipBlock := {| n | ::Skip( n ) }

   IF ! Empty( cFileName )
      ::LoadFile( cFileName )
   ENDIF

   RETURN Self

METHOD GotoLine( n ) CLASS HBBrwText

   ::oBrw:MoveCursor( n - ::nRow )
   ::RefreshAll()

   RETURN Self

METHOD SetActiveLine( n ) CLASS HBBrwText

   ::nActiveLine := n
   ::RefreshAll()

   RETURN Self

METHOD GetLine() CLASS HBBrwText

   RETURN padr( hb_ntos( ::nRow ) + ":", ::nLineNoLen ) + ;
          MemoLine( ::aRows[ ::nRow ], ::nMaxLineLen, 1, ::nTabWidth, .F. )

METHOD GetLineText() CLASS HBBrwText

   RETURN PadR( SubStr( ::GetLine(), ::nLineOffset ), ::nWidth )

METHOD GetLineColor() CLASS HBBrwText

   LOCAL aColor
   LOCAL lBreak

   lBreak := __dbgIsBreak( __Dbg():pInfo, ::cFileName, ::nRow ) >= 0

   IF lBreak
      aColor := iif( ::nRow == ::nActiveLine, { 4, 4 }, { 3, 3 } )
   ELSE
      aColor := iif( ::nRow == ::nActiveLine, { 2, 2 }, { 1, 1 } )
   ENDIF

   RETURN aColor

METHOD LoadFile( cFileName ) CLASS HBBrwText

   LOCAL nMaxLineLen := 0
   LOCAL cLine

   ::cFileName := cFileName
   ::aRows := Text2Array( MemoRead( cFileName ) )
   ::nRows := Len( ::aRows )
   ::nLineNoLen := Len( hb_ntos( ::nRows ) ) + 2

   FOR EACH cLine in ::aRows
      nMaxLineLen := Max( nMaxLineLen, ;
         Len( RTrim( MemoLine( cLine, Len( cLine ) + 256, 1, ::nTabWidth, .F. ) ) ) )
   NEXT
   ::nMaxLineLen := nMaxLineLen
   ::nLineOffset := 1

   RETURN NIL

METHOD Resize( nTop, nLeft, nBottom, nRight ) CLASS HBBrwText

   LOCAL lResize := .F.

   IF nTop != NIL .AND. nTop != ::nTop
      ::nTop := nTop
      lResize := .T.
   ENDIF
   IF nLeft != NIL .AND. nLeft != ::nLeft
      ::nLeft := nLeft
      lResize := .T.
   ENDIF
   IF nBottom != NIL .AND. nBottom != ::nBottom
      ::nBottom := nBottom
      lResize := .T.
   ENDIF
   IF nRight != NIL .AND. nRight != ::nRight
      ::nRight := nRight
      lResize := .T.
   ENDIF
   IF lResize
      ::oBrw:Resize( nTop, nLeft, nBottom, nRight )
      ::nWidth := ::nRight - ::nLeft + 1
   ENDIF

   RETURN Self

METHOD Search( cString, lCaseSensitive, nMode ) CLASS HBBrwText

   LOCAL bMove
   LOCAL lFound := .F.
   LOCAL n

   IF ! lCaseSensitive
      cString := Upper( cString )
   ENDIF

   DO CASE
   CASE nMode == 0 // From Top
      ::GoTop()
      bMove := {|| ::Skip( 1 ) }
   CASE nMode == 1 // Forward
      bMove := {|| ::Skip( 1 ) }
   CASE nMode == 2 // Backward
      bMove := {|| ::Skip( -1 ) }
   ENDCASE

   n := ::nRow

   DO WHILE Eval( bMove ) != 0
      IF cString $ iif( lCaseSensitive, ::aRows[ ::nRow ], Upper( ::aRows[ ::nRow ] ) )
         lFound := .T.
         ::oBrw:MoveCursor( ::nRow - n )
         ::RefreshAll()
         EXIT
      ENDIF
   ENDDO

   RETURN lFound

METHOD GoFirst() CLASS HBBrwText

   ::nRow := 1

   RETURN .T.

METHOD GoLast() CLASS HBBrwText

   ::nRow := ::nRows

   RETURN .T.

METHOD Skip( n ) CLASS HBBrwText

   LOCAL nSkipped := 0

   IF n > 0
      IF ::nRow < ::nRows
         nSkipped := Min( ::nRows - ::nRow, n )
         ::nRow += nSkipped
      ENDIF
   ELSEIF n < 0
      IF ::nRow > 1
         nSkipped := Max( 1 - ::nRow, n )
         ::nRow += nSkipped
      ENDIF
   ENDIF

   RETURN nSkipped

METHOD GoPrev() CLASS HBBrwText

   LOCAL lMoved := .F.

   IF ::nRow > 1
      ::nRow--
      lMoved := .T.
   ENDIF

   RETURN lMoved

METHOD GoNext() CLASS HBBrwText

   LOCAL lMoved := .F.

   IF ::nRow < ::nRows
      ::nRow++
      lMoved := .T.
   ENDIF

   RETURN lMoved

STATIC FUNCTION WhichEOL( cString )

   LOCAL nCRPos := At( Chr( 13 ), cString )
   LOCAL nLFPos := At( Chr( 10 ), cString )

   IF nCRPos > 0 .AND. nLFPos == 0
      RETURN Chr( 13 )
   ELSEIF nCRPos == 0 .AND. nLFPos >  0
      RETURN Chr( 10 )
   ELSEIF nCRPos > 0 .AND. nLFPos == nCRPos + 1
      RETURN Chr( 13 ) + Chr( 10 )
   ENDIF

   RETURN hb_eol()

STATIC FUNCTION Text2Array( cString )

   RETURN hb_ATokens( cString, WhichEOL( cString ) )
