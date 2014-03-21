/*
 * Harbour Project source code:
 * The Debugger Hash Inspector
 *
 * Copyright 2006 Francesco Saverio Giudice <info / at / fsgiudice / dot / com>
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

#define HB_CLS_NOTOBJECT      /* do not inherit from HBObject calss */
#include "hbclass.ch"

#include "inkey.ch"
#include "setcurs.ch"

CREATE CLASS HBDbHash

   VAR aWindows   INIT {}
   VAR TheHash
   VAR hashName
   VAR nCurWindow INIT 0
   VAR lEditable

   METHOD New( hHash, cVarName, lEditable )

   METHOD addWindows( hHash, nRow )
   METHOD doGet( oBrowse, pItem, nSet )
   METHOD SetsKeyPressed( nKey, oBrwSets, oWnd, cName, hHash )

ENDCLASS

METHOD New( hHash, cVarName, lEditable ) CLASS HBDbHash

   hb_default( @lEditable, .T. )

   ::hashName := cVarName
   ::TheHash := hHash
   ::lEditable := lEditable

   ::addWindows( ::TheHash )

   RETURN Self

METHOD addWindows( hHash, nRow ) CLASS HBDbHash

   LOCAL oBrwSets
   LOCAL nSize := Len( hHash )
   LOCAL oWndSets
   LOCAL nWidth
   LOCAL nColWidth
   LOCAL oCol
   LOCAL nKeyLen

   IF nSize < MaxRow() - 2
      IF nRow != NIL
         oWndSets := HBDbWindow():New( GetTopPos( nRow ), 5, getBottomPos( nRow + nSize + 1 ), MaxCol() - 5, ::hashName + "[1.." + hb_ntos( nSize ) + "]", "N/W" )
      ELSE
         oWndSets := HBDbWindow():New( 1, 5, 2 + nSize, MaxCol() - 5, ::hashName + "[1.." + hb_ntos( nSize ) + "]", "N/W" )
      ENDIF
   ELSE
      oWndSets := HBDbWindow():New( 1, 5, MaxRow() - 2, MaxCol() - 5, ::hashName + "[1.." + hb_ntos( nSize ) + "]", "N/W" )
   ENDIF
   ::nCurWindow++
   oWndSets:lFocused := .T.
   AAdd( ::aWindows, oWndSets )

   nWidth := oWndSets:nRight - oWndSets:nLeft - 1
   oBrwSets := HBDbBrowser():New( oWndSets:nTop + 1, oWndSets:nLeft + 1, oWndSets:nBottom - 1, oWndSets:nRight - 1 )
   oBrwSets:autolite := .F.
   oBrwSets:ColorSpec := __dbg():ClrModal()
   oBrwSets:Cargo := { 1, {} } // Actual highligthed row
   AAdd( oBrwSets:Cargo[ 2 ], hHash )

   oBrwSets:AddColumn( oCol := HBDbColumnNew( "", {|| ::hashName + "[" + HashKeyString( hHash, oBrwSets:cargo[ 1 ] ) + "]" } ) )

   // calculate max key length
   nKeyLen := 0
   hb_HEval( hHash, {| k, v, p | HB_SYMBOL_UNUSED( k ), HB_SYMBOL_UNUSED( v ), nKeyLen := Max( nKeyLen, Len( ::hashName + "[" + HashKeyString( hHash, p ) + "]" ) ) } )
   oCol:width := nKeyLen
   oCol:DefColor := { 1, 2 }
   nColWidth := oCol:Width

   oBrwSets:AddColumn( oCol := HBDbColumnNew( "", {|| PadR( __dbgValToStr( hb_HValueAt( hHash, oBrwSets:cargo[ 1 ] ) ), nWidth - nColWidth - 1 ) } ) )

   /* 2004-08-09 - <maurilio.longo@libero.it>
                   Setting a fixed width like it is done in the next line of code wich I've
                   commented exploits a bug of current tbrowse, that is, if every column is
                   narrower than tbrowse but the sum of them is wider tbrowse paints
                   one above the other if code like the one inside RefreshVarsS() is called.
                   (That code is used to have current row fully highlighted and not only
                   current cell). Reproducing this situation on a smaller sample with
                   clipper causes that only column two is visible after first stabilization.

                   I think tbrowse should trim columns up until the point where at leat
                   two are visible in the same moment, I leave this fix to tbrowse for
                   the reader ;)
   oCol:width := 50
   */

   oCol:DefColor := { 1, 3 }

   oBrwSets:goTopBlock := {|| oBrwSets:cargo[ 1 ] := 1 }
   oBrwSets:goBottomBlock := {|| oBrwSets:cargo[ 1 ] := Len( oBrwSets:cargo[ 2 ][ 1 ] ) }
   oBrwSets:skipBlock := {| nPos | ( nPos := HashBrowseSkip( nPos, oBrwSets ), oBrwSets:cargo[ 1 ] := ;
      oBrwSets:cargo[ 1 ] + nPos, nPos ) }

   ::aWindows[ ::nCurWindow ]:bPainted    := {|| ( oBrwSets:forcestable(), RefreshVarsS( oBrwSets ) ) }
   ::aWindows[ ::nCurWindow ]:bKeyPressed := {| nKey | ::SetsKeyPressed( nKey, oBrwSets, ;
      ::aWindows[ ::nCurWindow ], ::hashName, hHash ) }

   SetCursor( SC_NONE )

   ::aWindows[ ::nCurWindow ]:ShowModal()

   RETURN Self

METHOD doGet( oBrowse, pItem, nSet ) CLASS HBDbHash

   LOCAL oErr
   LOCAL cValue := PadR( __dbgValToStr( hb_HValueAt( pItem, nSet ) ), ;
      oBrowse:nRight - oBrowse:nLeft - oBrowse:GetColumn( 1 ):width )

   // make sure browse is stable
   oBrowse:forceStable()
   // if confirming new record, append blank

   IF __dbgInput( Row(), oBrowse:nLeft + oBrowse:GetColumn( 1 ):width + 1,, @cValue, ;
      {| cValue | iif( Type( cValue ) == "UE", ( __dbgAlert( "Expression error" ), .F. ), .T. ) } )
      BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
         hb_HValueAt( pItem, nSet, &cValue )
      RECOVER USING oErr
         __dbgAlert( oErr:description )
      END SEQUENCE
   ENDIF

   RETURN NIL

METHOD SetsKeyPressed( nKey, oBrwSets, oWnd, cName, hHash ) CLASS HBDbHash

   LOCAL nSet := oBrwSets:cargo[ 1 ]
   LOCAL cOldname := ::hashName
   LOCAL uValue

   DO CASE
   CASE nKey == K_UP
      oBrwSets:Up()

   CASE nKey == K_DOWN
      oBrwSets:Down()

   CASE nKey == K_HOME .OR. nKey == K_CTRL_PGUP .OR. nKey == K_CTRL_HOME
      oBrwSets:GoTop()

   CASE nKey == K_END .OR. nKey == K_CTRL_PGDN .OR. nKey == K_CTRL_END
      oBrwSets:GoBottom()

   CASE nKey == K_PGDN
      oBrwSets:pageDown()

   CASE nKey == K_PGUP
      oBrwSets:PageUp()

   CASE nKey == K_ENTER

      uValue := hb_HValueAt( hHash, nSet )

      IF HB_ISHASH( uValue )

         IF Len( uValue ) == 0
            __dbgAlert( "Hash is empty" )
         ELSE
            SetPos( ownd:nBottom, ownd:nLeft )
            ::aWindows[ ::nCurwindow ]:lFocused := .F.

            ::hashName := ::hashName + "[" + HashKeyString( hHash, nSet ) + "]"
            ::AddWindows( hb_HValueAt( hHash, nSet ), oBrwSets:RowPos + oBrwSets:nTop )
            ::hashName := cOldName

            hb_ADel( ::aWindows, ::nCurWindow, .T. )
            IF ::nCurwindow == 0
               ::nCurwindow := 1
            ELSE
               ::nCurwindow--
            ENDIF
         ENDIF
      ELSEIF HB_ISPOINTER( uValue ) .OR. ! ::lEditable
         __dbgAlert( "Value cannot be edited" )
      ELSE
         oBrwSets:RefreshCurrent()
         DO CASE
         CASE HB_ISOBJECT( uValue )
            __dbgObject( uValue, cName + "[" + HashKeyString( hHash, nSet ) + "]" )
         CASE HB_ISARRAY( uValue )
            __dbgArrays( uValue, cName + "[" + HashKeyString( hHash, nSet ) + "]" )
         OTHERWISE
            ::doGet( oBrwSets, hHash, nSet )
         ENDCASE
         oBrwSets:RefreshCurrent()
         oBrwSets:ForceStable()
      ENDIF

   ENDCASE

   RefreshVarsS( oBrwSets )

   ::aWindows[ ::nCurwindow ]:SetCaption( cName + "[" + hb_ntos( oBrwSets:cargo[ 1 ] ) + ".." + ;
      hb_ntos( Len( hHash ) ) + "]" )

   RETURN self

FUNCTION __dbgHashes( hHash, cVarName, lEditable )
   RETURN HBDbHash():New( hHash, cVarName, lEditable )

STATIC FUNCTION GetTopPos( nPos )
   RETURN iif( ( MaxRow() - nPos ) < 5, MaxRow() - nPos, nPos )

STATIC FUNCTION GetBottomPos( nPos )
   RETURN iif( nPos < MaxRow() - 2, nPos, MaxRow() - 2 )

STATIC PROCEDURE RefreshVarsS( oBrowse )

   LOCAL nLen := oBrowse:colCount

   IF nLen == 2
      oBrowse:deHilite():colPos := 2
   ENDIF
   oBrowse:deHilite():forceStable()

   IF nLen == 2
      oBrowse:hilite():colPos := 1
   ENDIF
   oBrowse:hilite()

   RETURN

STATIC FUNCTION HashBrowseSkip( nPos, oBrwSets )
   RETURN iif( oBrwSets:cargo[ 1 ] + nPos < 1, 0 - oBrwSets:cargo[ 1 ] + 1, ;
      iif( oBrwSets:cargo[ 1 ] + nPos > Len( oBrwSets:cargo[ 2 ][ 1 ] ), ;
      Len( oBrwSets:cargo[ 2 ][ 1 ] ) - oBrwSets:cargo[ 1 ], nPos ) )

STATIC FUNCTION HashKeyString( hHash, nAt )

   LOCAL xVal := hb_HKeyAt( hHash, nAt )

   SWITCH ValType( xVal )
   CASE "C" ; RETURN '"' + xVal + '"'
   CASE "D" ; RETURN '"' + DToC( xVal ) + '"'
   CASE "N" ; RETURN hb_ntos( xVal )
   ENDSWITCH

   RETURN AllTrim( __dbgCStr( xVal ) )
