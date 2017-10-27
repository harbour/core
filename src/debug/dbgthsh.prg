/*
 * The Debugger Hash Inspector
 *
 * Copyright 2006 Francesco Saverio Giudice <info / at / fsgiudice / dot / com>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

#define HB_CLS_NOTOBJECT      /* do not inherit from HBObject class */
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

   ::hashName := cVarName
   ::TheHash := hHash
   ::lEditable := hb_defaultValue( lEditable, .T. )

   ::addWindows( ::TheHash )

   RETURN Self

METHOD addWindows( hHash, nRow ) CLASS HBDbHash

   LOCAL oBrwSets
   LOCAL nSize := Len( hHash )
   LOCAL oWndSets
   LOCAL nColWidth
   LOCAL oCol
   LOCAL nKeyLen

   IF nSize < MaxRow() - 2
      IF HB_ISNUMERIC( nRow )
         oWndSets := HBDbWindow():New( GetTopPos( nRow ), 5, getBottomPos( nRow + nSize + 1 ), MaxCol() - 5, ;
            ::hashName + "[1.." + hb_ntos( nSize ) + "]", "N/W" )
      ELSE
         oWndSets := HBDbWindow():New( 1, 5, 2 + nSize, MaxCol() - 5, ;
            ::hashName + "[1.." + hb_ntos( nSize ) + "]", "N/W" )
      ENDIF
   ELSE
      oWndSets := HBDbWindow():New( 1, 5, MaxRow() - 2, MaxCol() - 5, ;
         ::hashName + "[1.." + hb_ntos( nSize ) + "]", "N/W" )
   ENDIF
   ::nCurWindow++
   oWndSets:lFocused := .T.
   AAdd( ::aWindows, oWndSets )

   oBrwSets := HBDbBrowser():New( oWndSets:nTop + 1, oWndSets:nLeft + 1, oWndSets:nBottom - 1, oWndSets:nRight - 1 )
   oBrwSets:ColorSpec := __dbg():ClrModal()
   oBrwSets:Cargo := { 1, {} }  // Actual highlighted row
   AAdd( oBrwSets:Cargo[ 2 ], hHash )

   oBrwSets:AddColumn( oCol := HBDbColumnNew( "", {|| ::hashName + "[" + HashKeyString( hHash, oBrwSets:cargo[ 1 ] ) + "]" } ) )

   // calculate max key length
   nKeyLen := 0
   hb_HEval( hHash, {| k, v, p | HB_SYMBOL_UNUSED( k ), HB_SYMBOL_UNUSED( v ), nKeyLen := Max( nKeyLen, Len( ::hashName + HashKeyString( hHash, p ) ) + 2 ) } )
   oCol:width := nKeyLen
   oCol:DefColor := { 1, 2 }
   nColWidth := oCol:Width

   oBrwSets:AddColumn( oCol := HBDbColumnNew( "", {|| __dbgValToExp( hb_HValueAt( hHash, oBrwSets:cargo[ 1 ] ) ) } ) )

   oCol:width := oWndSets:nRight - oWndSets:nLeft - nColWidth - 2
   oCol:DefColor := { 1, 3 }

   oBrwSets:goTopBlock := {|| oBrwSets:cargo[ 1 ] := 1 }
   oBrwSets:goBottomBlock := {|| oBrwSets:cargo[ 1 ] := Len( oBrwSets:cargo[ 2 ][ 1 ] ) }
   oBrwSets:skipBlock := {| nPos | nPos := HashBrowseSkip( nPos, oBrwSets ), ;
                                   oBrwSets:cargo[ 1 ] := oBrwSets:cargo[ 1 ] + nPos, nPos }
   oBrwSets:colPos := 2

   ::aWindows[ ::nCurWindow ]:bPainted    := {|| oBrwSets:forcestable() }
   ::aWindows[ ::nCurWindow ]:bKeyPressed := ;
      {| nKey | ::SetsKeyPressed( nKey, oBrwSets, ::aWindows[ ::nCurWindow ], ::hashName, hHash ) }

   ::aWindows[ ::nCurWindow ]:ShowModal()

   RETURN Self

METHOD PROCEDURE doGet( oBrowse, pItem, nSet ) CLASS HBDbHash

   LOCAL oErr
   LOCAL cValue

   // make sure browse is stable
   oBrowse:forceStable()
   // if confirming new record, append blank

   cValue := __dbgValToExp( hb_HValueAt( pItem, nSet ) )

   IF __dbgInput( Row(), oBrowse:nLeft + oBrowse:GetColumn( 1 ):width + 1, ;
                  oBrowse:getColumn( 2 ):Width, @cValue, ;
                  __dbgExprValidBlock(), __dbgColors()[ 2 ], 256 )
      BEGIN SEQUENCE WITH __BreakBlock()
         hb_HValueAt( pItem, nSet, &cValue )
      RECOVER USING oErr
         __dbgAlert( oErr:description )
      END SEQUENCE
   ENDIF

   RETURN

METHOD SetsKeyPressed( nKey, oBrwSets, oWnd, cName, hHash ) CLASS HBDbHash

   LOCAL nSet := oBrwSets:cargo[ 1 ]
   LOCAL cOldname := ::hashName
   LOCAL uValue

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
      EXIT

   ENDSWITCH

   oBrwSets:forcestable()

   ::aWindows[ ::nCurwindow ]:SetCaption( cName + "[" + hb_ntos( oBrwSets:cargo[ 1 ] ) + ".." + ;
      hb_ntos( Len( hHash ) ) + "]" )

   RETURN Self

FUNCTION __dbgHashes( hHash, cVarName, lEditable )
   RETURN HBDbHash():New( hHash, cVarName, lEditable )

STATIC FUNCTION GetTopPos( nPos )
   RETURN iif( ( MaxRow() - nPos ) < 5, MaxRow() - nPos, nPos )

STATIC FUNCTION GetBottomPos( nPos )
   RETURN iif( nPos < MaxRow() - 2, nPos, MaxRow() - 2 )

STATIC FUNCTION HashBrowseSkip( nPos, oBrwSets )
   RETURN ;
      iif( oBrwSets:cargo[ 1 ] + nPos < 1, -oBrwSets:cargo[ 1 ] + 1, ;
      iif( oBrwSets:cargo[ 1 ] + nPos > Len( oBrwSets:cargo[ 2 ][ 1 ] ), ;
      Len( oBrwSets:cargo[ 2 ][ 1 ] ) - oBrwSets:cargo[ 1 ], nPos ) )

STATIC FUNCTION HashKeyString( hHash, nAt )
   RETURN __dbgValToExp( hb_HKeyAt( hHash, nAt ) )
