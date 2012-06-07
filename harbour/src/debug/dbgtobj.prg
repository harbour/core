/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Debugger Object Inspector
 *
 * Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net>
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

#pragma DEBUGINFO=OFF

#define HB_CLS_NOTOBJECT      /* do not inherit from HBObject calss */
#include "hbclass.ch"

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

   METHOD New( oObject, cVarName, lEditable )
   METHOD addWindows( aArray, nRow )
   METHOD doGet( oBrowse, pItem, nSet )
   METHOD SetsKeyPressed( nKey, oBrwSets, nSets, aArray )

ENDCLASS

METHOD New( oObject, cVarName, lEditable ) CLASS HBDbObject

   LOCAL cMsg, cMsgAcc
   LOCAL aMessages, aMethods
   LOCAL xValue

   hb_default( @lEditable, .T. )

   /* create list of object messages */
   aMessages := oObject:classSel()
   ASort( aMessages,,, {|x,y| PAdR( x, 64 ) <= PAdR( y, 64 ) } )
   aMethods := {}
   FOR EACH cMsg IN aMessages
      IF Left( cMsg, 1 ) == "_" .AND. ;
         HB_AScan( aMessages, cMsgAcc := Substr( cMsg, 2 ),,, .T. ) != 0
         xValue := __dbgObjGetValue( oObject, cMsgAcc )
         AAdd( ::pItems, { cMsgAcc, xValue, .T. } )
         AAdd( ::AllNames, cMsgAcc )
      ELSEIF HB_AScan( aMessages, "_" + cMsg,,, .T. ) == 0
         AAdd( aMethods, cMsg )
      ENDIF
   NEXT
   FOR EACH cMsg IN aMethods
      AAdd( ::pItems, { Lower( cMsg ), "Method", .F. } )
      AAdd( ::AllNames, cMsg )
   NEXT

   ::objname := cVarName
   ::TheObj := oObject
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

   oBrwSets := HBDbBrowser():New( oWndSets:nTop + 1, oWndSets:nLeft + 1, oWndSets:nBottom - 1, oWndSets:nRight - 1 )
   ::ArrayReference := aArray

   oBrwSets:autolite := .T.
   oBrwSets:ColorSpec := __Dbg():ClrModal()
   oBrwSets:GoTopBlock := {|| ::Arrayindex := 1 }
   oBrwSets:GoBottomBlock := {|| ::arrayindex := Len( ::ArrayReference ) }
   oBrwSets:SkipBlock := {| nSkip, nPos | nPos := ::arrayindex,;
                          ::arrayindex := iif( nSkip > 0, Min( ::arrayindex + nSkip, Len( ::arrayReference ) ),;
                          Max( 1, ::arrayindex + nSkip ) ), ::arrayindex - nPos }

   nMaxLen := ArrayMaxLen( ::AllNames )
   oBrwSets:AddColumn( oCol := HBDbColumnNew( "",;
                    {|| PadR( ::ArrayReference[ ::arrayindex, 1 ], nMaxLen ) } ) )
   oCol:width := nMaxLen
   oCol:ColorBlock := {|| { iif( ::Arrayindex == oBrwSets:Cargo, 2, 1 ), 2 } }
   oBrwSets:Freeze := 1

   oBrwSets:AddColumn( oCol := HBDbColumnNew( "", {|| iif( HB_ISSTRING( ::ArrayReference[ ::ArrayIndex, 2 ] ) .AND. !::ArrayReference[ ::ArrayIndex, 3 ],;
      ::ArrayReference[ ::ArrayIndex, 2 ],;
      PadR( __dbgValToStr( __dbgObjGetValue( ::TheObj, ::ArrayReference[ ::arrayindex, 1 ] ) ), nWidth  - 12 ) ) } ) )

   oBrwSets:Cargo := 1 // Actual highlighted row
   oCol:ColorBlock := {|| { iif( ::Arrayindex == oBrwSets:Cargo, 3, 1 ), 3 } }
   oCol:width := MaxCol() - 14 - nMaxLen
   oBrwSets:colPos := 2
   ::aWindows[ ::nCurWindow ]:bPainted    := {|| oBrwSets:ForceStable() }
   ::aWindows[ ::nCurWindow ]:bKeyPressed := {| nKey | ::SetsKeyPressed( nKey, oBrwSets, Len( aArray ), ::ArrayReference ) }
   ::aWindows[ ::nCurwindow ]:cCaption := ::objname + " is of class: " +::TheObj:ClassName()

   SetCursor( SC_NONE )

   ::aWindows[ ::nCurWindow ]:ShowModal()

   RETURN Self

METHOD doGet( oBrowse, pItem, nSet ) CLASS HBDbObject

   LOCAL column
   LOCAL cValue
   LOCAL lCanAcc
   LOCAL oErr

   // make sure browse is stable
   oBrowse:forceStable()
   // if confirming new record, append blank

   // get column object from browse
   column := oBrowse:getColumn( oBrowse:colPos )

   // create a corresponding GET
   cValue := __dbgObjGetValue( ::TheObj, pitem[ nSet, 1 ], @lCanAcc )
   IF !lCanAcc
      __dbgAlert( cValue )
      RETURN NIL
   ENDIF
   cValue := PadR( __dbgValToStr( cValue ), column:Width )

   IF __dbgInput( Row(), oBrowse:nLeft + oBrowse:GetColumn( 1 ):width + 1,, @cValue, ;
                  {| cValue | iif( Type( cValue ) == "UE", ( __dbgAlert( "Expression error" ), .F. ), .T. ) } )
      BEGIN SEQUENCE WITH {|oErr| break( oErr ) }
         __dbgObjSetValue( ::TheObj, pitem[ nSet, 1 ], &cValue )
      RECOVER USING oErr
         __dbgAlert( oErr:description )
      END SEQUENCE
   ENDIF

   RETURN NIL

METHOD SetsKeyPressed( nKey, oBrwSets, nSets, aArray ) CLASS HBDbObject

   LOCAL nSet := oBrwSets:Cargo

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

   CASE nKey == K_HOME .OR. nKey == K_CTRL_PGUP .OR. nKey == K_CTRL_HOME

      IF oBrwSets:Cargo > 1
         oBrwSets:Cargo := 1
         oBrwSets:GoTop()
         oBrwSets:ForceStable()
      ENDIF

   CASE nKey == K_END .OR. nKey == K_CTRL_PGDN .OR. nKey == K_CTRL_END

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
         IF HB_ISARRAY( aArray[ nSet, 2 ] )
            IF Len( aArray[ nSet, 2 ] ) > 0
               HBDbArray():New( aArray[ nSet, 2 ], ::pitems[ nSet, 1 ] )
            ENDIF
         ELSEIF HB_ISHASH( aArray[ nSet, 2 ] )
            IF Len( aArray[ nSet, 2 ] ) > 0
               HBDbHash():New( aArray[ nSet, 2 ], ::pitems[ nSet, 1 ] )
            ENDIF
         ELSEIF HB_ISOBJECT( aArray[ nSet, 2 ] )
            HBDbObject():New( aArray[ nSet, 2 ], ::pitems[ nSet, 1 ] )
         ELSEIF ( HB_ISSTRING( aArray[ nSet, 2 ] ) .AND. ;
                  !aArray[ nSet, 3 ] ) .OR. ;
                HB_ISBLOCK( aArray[ nSet, 2 ] ) .OR. ;
                HB_ISPOINTER( aArray[ nSet, 2 ] )
            __dbgAlert( "Value cannot be edited" )
         ELSE
            IF ::lEditable
               oBrwSets:RefreshCurrent()
               ::doGet( oBrwSets, ::arrayReference, nSet )
               oBrwSets:RefreshCurrent()
               oBrwSets:ForceStable()
            else
               __dbgAlert( "Value cannot be edited" )
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

STATIC FUNCTION __dbgObjGetValue( oObject, cVar, lCanAcc )

   LOCAL nProcLevel := __Dbg():nProcLevel
   LOCAL xResult
   LOCAL oErr

   BEGIN SEQUENCE WITH {|| break() }
      xResult := __dbgSENDMSG( nProcLevel, oObject, cVar )
      lCanAcc := .T.
   RECOVER
      BEGIN SEQUENCE WITH {|oErr| break( oErr ) }
         /* Try to access variables using class code level */
         xResult := __dbgSENDMSG( 0, oObject, cVar )
         lCanAcc := .T.
      RECOVER USING oErr
         xResult := oErr:description
         lCanAcc := .F.
      END SEQUENCE
   END SEQUENCE

   RETURN xResult

STATIC FUNCTION __dbgObjSetValue( oObject, cVar, xValue )

   LOCAL nProcLevel := __Dbg():nProcLevel
   LOCAL oErr

   BEGIN SEQUENCE WITH {|| break() }
      __dbgSENDMSG( nProcLevel, oObject, "_" + cVar, xValue )
   RECOVER
      BEGIN SEQUENCE WITH {|oErr| break( oErr ) }
         /* Try to access variables using class code level */
         __dbgSENDMSG( 0, oObject, "_" + cVar, xValue )
      RECOVER USING oErr
         __dbgAlert( oErr:description )
      END SEQUENCE
   END SEQUENCE

   RETURN xValue
