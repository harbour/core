/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Debugger Object Inspector
 *
 * Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net>
 * www - http://www.harbour-project.org
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
 * along with this software; see the file COPYING.  If not, write to
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

#include "hbclass.ch"

#include "common.ch"
#include "inkey.ch"
#include "setcurs.ch"

CREATE CLASS HBDbObject

   VAR aWindows       INIT {}
   VAR Theobj
   VAR objname
   VAR nCurWindow     INIT 0
   VAR pItems         INIT {}
   VAR ArrayReference INIT {}
   VAR ArrayIndex     INIT 1
   VAR AllNames       INIT {}
   VAR lEditable

   METHOD New( aArray, cVarName, lEditable )
   METHOD addWindows( aArray, nRow )
   METHOD doGet( oBrowse, pItem, nSet )
   METHOD SetsKeyPressed( nKey, oBrwSets, oWnd, cName, aArray )

ENDCLASS

METHOD New( aArray, cVarName, lEditable ) CLASS HBDbObject

   LOCAL aTemp

   DEFAULT lEditable TO .T.

   FOR EACH aTemp IN __objGetValueList( aArray )
      AAdd( ::pItems, { aTemp[ 1 ], aTemp[ 2 ] } )
      AAdd( ::AllNames, aTemp[ 1 ] )
   NEXT

   FOR EACH aTemp IN __objGetMethodList( aArray )
      IF !Empty( aTemp )
         AAdd( ::pItems, { aTemp, "Method" } )
         AAdd( ::AllNames, aTemp )
      ENDIF
   NEXT

   ::objname := cVarName
   ::TheObj := aArray
   ::lEditable := lEditable

   ::addWindows( ::pItems )

   RETURN Self

METHOD addWindows( aArray, nRow ) CLASS HBDbObject

   LOCAL oBrwSets
   LOCAL nSize := Len( aArray )
   LOCAL oWndSets
   LOCAL nWidth
   LOCAL oCol
   LOCAL nMaxLen

   IF nSize < MaxRow()-2
      IF nRow != NIL
         oWndSets := HBDbWindow():New( nRow, 5, iif( nRow + nSize + 1 < MaxRow() - 2, nRow + nSize + 1, MaxRow() - 2 ), MaxCol() - 5, ::objname + " is of class: " + ::TheObj:ClassName(), "N/W" )
      ELSE
         oWndSets := HBDbWindow():New( 1, 5, 2 + nSize, MaxCol() - 5, ::objname + " is of class: " + ::TheObj:ClassName(), "N/W" )
      ENDIF
   ELSE
      oWndSets := HBDbWindow():New( 1, 5, MaxRow() - 2, MaxCol() - 5, ::objname + " is of class: " + ::TheObj:ClassName(), "N/W" )
   ENDIF

   ::nCurWindow++
   oWndSets:lFocused := .T.
   AAdd( ::aWindows, oWndSets )

   nWidth := oWndSets:nRight - oWndSets:nLeft - 1

   oBrwSets := TBrowseNew( oWndSets:nTop + 1, oWndSets:nLeft + 1, oWndSets:nBottom - 1, oWndSets:nRight - 1 )
   ::ArrayReference := aarray

   oBrwSets:ColorSpec := __Dbg():ClrModal()
   oBrwSets:GoTopBlock := { || ::Arrayindex := 1 }
   oBrwSets:GoBottomBlock := { || ::arrayindex := Len( ::ArrayReference ) }
   oBrwSets:SkipBlock := { | nSkip, nPos | nPos := ::arrayindex,;
                          ::arrayindex := iif( nSkip > 0, Min( ::arrayindex + nSkip, Len( ::arrayreference ) ),;
                          Max( 1, ::arrayindex + nSkip ) ), ::arrayindex - nPos }

   nMaxLen := ArrayMaxLen( ::AllNames )
   oBrwSets:AddColumn( oCol := TBColumnNew( "",;
                    { || PadR( ::ArrayReference[ ::arrayindex, 1 ], nMaxLen ) } ) )
   oCol:width := nMaxLen
   oCol:ColorBlock := { || { iif( ::Arrayindex == oBrwSets:Cargo, 2, 1 ), 2 } }
   oBrwSets:Freeze := 1

   oBrwSets:AddColumn( oCol := TBColumnNew( "", { || iif( ISCHARACTER( ::ArrayReference[ ::ArrayIndex, 2 ] ) .AND. ::ArrayReference[ ::ArrayIndex, 2 ] == "Method",;
      "Method",;
      PadR( __dbgValToStr( __ObjSendMsg( ::TheObj, ::ArrayReference[ ::arrayindex, 1 ] ) ), nWidth  - 12 ) ) } ) )

   oBrwSets:Cargo := 1 // Actual highligthed row
   oCol:ColorBlock := { || { iif( ::Arrayindex == oBrwSets:Cargo, 3, 1 ), 3 } }
   oCol:width := MaxCol() - 14 - nMaxLen
   oBrwSets:colpos := 2
   ::aWindows[ ::nCurWindow ]:bPainted    := { || oBrwSets:ForceStable() }
   ::aWindows[ ::nCurWindow ]:bKeyPressed := { | nKey | ::SetsKeyPressed( nKey, oBrwSets, Len( aArray ),;
                                               ::aWindows[ ::nCurWindow ], ::objname, Len( ::Arrayreference ), ::pitems ) }
   ::aWindows[ ::nCurwindow ]:cCaption := ::objname + " is of class: " +::TheObj:ClassName()

   SetCursor( SC_NONE )

   ::aWindows[ ::nCurWindow ]:ShowModal()

   RETURN Self

METHOD doGet( oBrowse, pItem, nSet ) CLASS HBDbObject

#ifndef HB_NO_READDBG

   LOCAL column
   LOCAL nKey
   LOCAL GetList := {}
   LOCAL lScoreSave := Set( _SET_SCOREBOARD, .F. )
   LOCAL lExitSave  := Set( _SET_EXIT, .T. )
   LOCAL bInsSave   := SetKey( K_INS )
   LOCAL cValue

   // make sure browse is stable
   oBrowse:forceStable()
   // if confirming new record, append blank



   // set insert key to toggle insert mode and cursor
   SetKey( K_INS, { || SetCursor( iif( ReadInsert( ! ReadInsert() ),;
           SC_NORMAL, SC_INSERT ) ) } )

   // initial cursor setting
   SetCursor( iif( ReadInsert(), SC_INSERT, SC_NORMAL ) )

   // get column object from browse
   column := oBrowse:getColumn( oBrowse:colPos )

   // create a corresponding GET
   cValue := PadR( __dbgValToStr( pitem[ nSet, 2 ] ), column:Width )
   @ Row(), Col() GET cValue ;
       VALID iif( Type( cValue ) == "UE", ( Alert( "Expression error" ), .F. ), .T. )

   READ

   SetCursor( SC_NONE )
   Set( _SET_SCOREBOARD, lScoreSave )
   Set( _SET_EXIT, lExitSave )
   SetKey( K_INS, bInsSave )

   IF LastKey() == K_ENTER
      __ObjSendMsg( ::TheObj, "_" + pitem[ nSet, 1 ], &cValue )
   ENDIF

   // check exit key from get
   nKey := LastKey()
   IF nKey == K_UP .OR. nKey == K_DOWN .OR. nKey == K_PGUP .OR. nKey == K_PGDN
      KEYBOARD Chr( nKey )
   ENDIF

#else

   HB_SYMBOL_UNUSED( oBrowse )
   HB_SYMBOL_UNUSED( pItem )
   HB_SYMBOL_UNUSED( nSet )

#endif

   RETURN NIL

METHOD SetsKeyPressed( nKey, oBrwSets, nSets, oWnd, cName, aArray ) CLASS HBDbObject

   LOCAL nSet := oBrwSets:Cargo
   LOCAL cOldname := ::objname

   HB_SYMBOL_UNUSED( oWnd )
   HB_SYMBOL_UNUSED( cName )

   DO CASE
   CASE nKey == K_UP

      IF oBrwSets:Cargo > 1
         oBrwSets:Cargo--
         oBrwSets:RefreshCurrent()
         oBrwSets:Up()
         oBrwSets:ForceStable()
      ENDIF

   CASE nKey == K_DOWN

      IF oBrwSets:Cargo < nSets
         oBrwSets:Cargo++
         oBrwSets:RefreshCurrent()
         oBrwSets:Down()
         oBrwSets:ForceStable()
      ENDIF

   CASE nKey == K_HOME

      IF oBrwSets:Cargo > 1
         oBrwSets:Cargo := 1
         oBrwSets:GoTop()
         oBrwSets:ForceStable()
      ENDIF

   CASE nKey == K_END

      IF oBrwSets:Cargo < nSets
         oBrwSets:Cargo := nSets
         oBrwSets:GoBottom()
         oBrwSets:ForceStable()
      ENDIF

   CASE nKey == K_PGUP

      oBrwSets:PageUp()
      oBrwSets:Cargo := ::ArrayIndex
      oBrwSets:RefreshCurrent()
      oBrwSets:ForceStable()

   CASE nKey == K_PGDN

      oBrwSets:PageDown()
      oBrwSets:Cargo := ::ArrayIndex
      oBrwSets:RefreshCurrent()
      oBrwSets:ForceStable()

   CASE nKey == K_ENTER

      IF nSet == oBrwSets:Cargo
         IF ISARRAY( aArray[ nSet, 2 ] )
            IF Len( aArray[ nSet, 2 ] ) > 0
               HBDbArray():New( aArray[ nSet, 2 ], ::pitems[ nSet, 1 ] )
            ENDIF
         ELSEIF ValType( aArray[ nSet, 2 ] ) == "H"
            IF Len( aArray[ nSet, 2 ] ) > 0
               HBDbHash():New( aArray[ nSet, 2 ], ::pitems[ nSet, 1 ] )
            ENDIF
         ELSEIF ISOBJECT( aArray[ nSet, 2 ] )
            HBDbObject():New( aArray[ nSet, 2 ], ::pitems[ nSet, 1 ] )
         ELSEIF ( ISCHARACTER( aArray[ nSet, 2 ] ) .AND. ;
                  aArray[ nSet, 2 ] == "Method" ) .OR. ;
                ISBLOCK( aArray[ nSet, 2 ] ) .OR. ;
                ValType( aArray[ nSet, 2 ] ) == "P"
            Alert( "Value cannot be edited" )
         ELSE
            IF ::lEditable
               oBrwSets:RefreshCurrent()
               ::doGet( oBrwSets, ::arrayreference, nSet )
               oBrwSets:RefreshCurrent()
               oBrwSets:ForceStable()
            else
               Alert( "Value cannot be edited" )
            ENDIF
         ENDIF
      ENDIF

   ENDCASE

   RETURN NIL

FUNCTION __dbgObject( aArray, cVarName, lEditable )
   RETURN HBDbObject():New( aArray, cVarName, lEditable )

STATIC FUNCTION ArrayMaxLen( aArray )

   LOCAL nMaxLen := 0
   LOCAL nLen
   LOCAL cItem

   FOR EACH cItem IN aArray
      nLen := Len( cItem )
      IF nMaxLen < nLen
         nMaxLen := nLen
      ENDIF
   NEXT

   RETURN nMaxLen
