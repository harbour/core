/*
 * The Debugger Array Inspector
 *
 * Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net>
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

CREATE CLASS HBDbArray

   VAR aWindows   INIT {}
   VAR TheArray
   VAR arrayname
   VAR nCurWindow INIT 0
   VAR lEditable

   METHOD New( aArray, cVarName, lEditable )

   METHOD addWindows( aArray, nRow )
   METHOD doGet( oBrowse, pItem, nSet )
   METHOD SetsKeyPressed( nKey, oBrwSets, oWnd, cName, aArray )

ENDCLASS

METHOD New( aArray, cVarName, lEditable ) CLASS HBDbArray

   ::arrayName := cVarName
   ::TheArray := aArray
   ::lEditable := hb_defaultValue( lEditable, .T. )

   ::addWindows( ::TheArray )

   RETURN Self

METHOD addWindows( aArray, nRow ) CLASS HBDbArray

   LOCAL oBrwSets
   LOCAL nSize := Len( aArray )
   LOCAL oWndSets
   LOCAL nColWidth
   LOCAL oCol

   IF nSize < MaxRow() - 2
      IF HB_ISNUMERIC( nRow )
         oWndSets := HBDbWindow():New( GetTopPos( nRow ), 5, getBottomPos( nRow + nSize + 1 ), MaxCol() - 5, ;
            ::arrayName + "[1.." + hb_ntos( nSize ) + "]", "N/W" )
      ELSE
         oWndSets := HBDbWindow():New( 1, 5, 2 + nSize, MaxCol() - 5, ;
            ::arrayName + "[1.." + hb_ntos( nSize ) + "]", "N/W" )
      ENDIF
   ELSE
      oWndSets := HBDbWindow():New( 1, 5, MaxRow() - 2, MaxCol() - 5, ;
         ::arrayName + "[1.." + hb_ntos( nSize ) + "]", "N/W" )
   ENDIF

   ::nCurWindow++
   oWndSets:lFocused := .T.
   AAdd( ::aWindows, oWndSets )

   oBrwSets := HBDbBrowser():New( oWndSets:nTop + 1, oWndSets:nLeft + 1, oWndSets:nBottom - 1, oWndSets:nRight - 1 )
   oBrwSets:ColorSpec := __dbg():ClrModal()
   oBrwSets:Cargo := { 1, {} }  // Actual highligthed row
   AAdd( oBrwSets:Cargo[ 2 ], aArray )

   oBrwSets:AddColumn( oCol := HBDbColumnNew( "", {|| ::arrayName + "[" + hb_ntos( oBrwSets:cargo[ 1 ] ) + "]" } ) )
   oCol:width := Len( ::arrayName + "[" + hb_ntos( Len( aArray ) ) + "]" )
   oCol:DefColor := { 1, 2 }
   nColWidth := oCol:Width

   oBrwSets:AddColumn( oCol := HBDbColumnNew( "", {|| __dbgValToExp( aArray[ oBrwSets:cargo[ 1 ] ] ) } ) )

   oCol:width := oWndSets:nRight - oWndSets:nLeft - nColWidth - 2
   oCol:defColor := { 1, 3 }

   oBrwSets:goTopBlock := {|| oBrwSets:cargo[ 1 ] := 1 }
   oBrwSets:goBottomBlock := {|| oBrwSets:cargo[ 1 ] := Len( oBrwSets:cargo[ 2 ][ 1 ] ) }
   oBrwSets:skipBlock := {| nPos | nPos := ArrayBrowseSkip( nPos, oBrwSets ), ;
                                   oBrwSets:cargo[ 1 ] := oBrwSets:cargo[ 1 ] + nPos, nPos }
   oBrwSets:colPos := 2

   ::aWindows[ ::nCurWindow ]:bPainted    := {|| oBrwSets:forcestable() }
   ::aWindows[ ::nCurWindow ]:bKeyPressed := ;
      {| nKey | ::SetsKeyPressed( nKey, oBrwSets, ::aWindows[ ::nCurWindow ], ::arrayName, aArray ) }

   ::aWindows[ ::nCurWindow ]:ShowModal()

   RETURN Self

METHOD PROCEDURE doGet( oBrowse, pItem, nSet ) CLASS HBDbArray

   LOCAL oErr
   LOCAL cValue

   // make sure browse is stable
   oBrowse:forceStable()
   // if confirming new record, append blank

   cValue := __dbgValToExp( pItem[ nSet ] )

   IF __dbgInput( Row(), oBrowse:nLeft + oBrowse:GetColumn( 1 ):width + 1, ;
                  oBrowse:getColumn( 2 ):Width, @cValue, ;
                  __dbgExprValidBlock(), __dbgColors()[ 2 ], 256 )
      BEGIN SEQUENCE WITH __BreakBlock()
         pItem[ nSet ] := &cValue
      RECOVER USING oErr
         __dbgAlert( oErr:description )
      END SEQUENCE
   ENDIF

   RETURN

METHOD SetsKeyPressed( nKey, oBrwSets, oWnd, cName, aArray ) CLASS HBDbArray

   LOCAL nSet := oBrwSets:cargo[ 1 ]
   LOCAL cOldName := ::arrayName

   SWITCH nKey
   CASE K_UP
      oBrwSets:Up()
      EXIT

   CASE K_DOWN
      oBrwSets:Down()
      EXIT

   CASE K_HOME
   CASE K_CTRL_PGUP
   CASE K_CTRL_HOME
      oBrwSets:GoTop()
      EXIT

   CASE K_END
   CASE K_CTRL_PGDN
   CASE K_CTRL_END
      oBrwSets:GoBottom()
      EXIT

   CASE K_PGDN
      oBrwSets:pageDown()
      EXIT

   CASE K_PGUP
      oBrwSets:PageUp()
      EXIT

   CASE K_ENTER
      IF HB_ISARRAY( aArray[ nSet ] )
         IF Len( aArray[ nSet ] ) == 0
            __dbgAlert( "Array is empty" )
         ELSE
            SetPos( oWnd:nBottom, oWnd:nLeft )
            ::aWindows[ ::nCurWindow ]:lFocused := .F.
            ::arrayname := ::arrayname + "[" + hb_ntos( nSet ) + "]"
            ::AddWindows( aArray[ nSet ], oBrwSets:RowPos + oBrwSets:nTop )
            ::arrayname := cOldName

            hb_ADel( ::aWindows, ::nCurWindow, .T. )
            IF ::nCurWindow == 0
               ::nCurWindow := 1
            ELSE
               ::nCurWindow--
            ENDIF
         ENDIF
      ELSEIF HB_ISPOINTER( aArray[ nSet ] ) .OR. ! ::lEditable
         __dbgAlert( "Value cannot be edited" )
      ELSE
         oBrwSets:RefreshCurrent()
         DO CASE
         CASE HB_ISOBJECT( aArray[ nSet ] )
            __dbgObject( aArray[ nSet ], cName + "[" + hb_ntos( nSet ) + "]" )
         CASE HB_ISHASH( aArray[ nSet ] )
            __dbgHashes( aArray[ nSet ], cName + "[" + hb_ntos( nSet ) + "]" )
         OTHERWISE
            ::doGet( oBrwsets, aArray, nSet )
         ENDCASE
         oBrwSets:RefreshCurrent()
         oBrwSets:ForceStable()
      ENDIF
      EXIT

   ENDSWITCH

   oBrwSets:forceStable()

   ::aWindows[ ::nCurWindow ]:SetCaption( cName + "[" + hb_ntos( oBrwSets:cargo[ 1 ] ) + ".." + ;
      hb_ntos( Len( aArray ) ) + "]" )

   RETURN self

FUNCTION __dbgArrays( aArray, cVarName, lEditable )
   RETURN HBDbArray():New( aArray, cVarName, lEditable )

STATIC FUNCTION GetTopPos( nPos )
   RETURN iif( ( MaxRow() - nPos ) < 5, MaxRow() - nPos, nPos )

STATIC FUNCTION GetBottomPos( nPos )
   RETURN iif( nPos < MaxRow() - 2, nPos, MaxRow() - 2 )

STATIC FUNCTION ArrayBrowseSkip( nPos, oBrwSets )
   RETURN ;
      iif( oBrwSets:cargo[ 1 ] + nPos < 1, -oBrwSets:cargo[ 1 ] + 1, ;
      iif( oBrwSets:cargo[ 1 ] + nPos > Len( oBrwSets:cargo[ 2 ][ 1 ] ), ;
      Len( oBrwSets:cargo[ 2 ][ 1 ] ) - oBrwSets:cargo[ 1 ], nPos ) )
