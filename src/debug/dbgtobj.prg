/*
 * The Debugger Object Inspector
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

/* object message descirption */
#define OMSG_NAME       1
#define OMSG_VALUE      2
#define OMSG_EDIT       3

CREATE CLASS HBDbObject

   VAR aWindows       INIT {}
   VAR Theobj
   VAR objname
   VAR nCurWindow     INIT 0
   VAR pItems         INIT {}
   VAR ArrayIndex     INIT 1
   VAR lEditable

   METHOD New( oObject, cVarName, lEditable )
   METHOD addWindows( nRow )
   METHOD doGet( oBrowse )
   METHOD SetsKeyPressed( nKey, oBrwSets )

ENDCLASS

METHOD New( oObject, cVarName, lEditable ) CLASS HBDbObject

   LOCAL cMsg, cMsgAcc
   LOCAL aMessages, aMethods
   LOCAL xValue

   __dbgSetGo( __dbg():pInfo )

   /* create list of object messages */
   aMessages := oObject:classSel()
   ASort( aMessages,,, {| x, y | x + Chr( 0 ) < y + Chr( 0 ) } )
   aMethods := {}
   FOR EACH cMsg IN aMessages
      IF hb_LeftEq( cMsg, "_" ) .AND. ;
         hb_AScan( aMessages, cMsgAcc := SubStr( cMsg, 2 ),,, .T. ) > 0
         xValue := __dbgObjGetValue( oObject, cMsgAcc )
         AAdd( ::pItems, { cMsgAcc, xValue, .T. } )
      ELSEIF hb_AScan( aMessages, "_" + cMsg,,, .T. ) == 0
         AAdd( aMethods, cMsg )
      ENDIF
   NEXT
   FOR EACH cMsg IN aMethods
      AAdd( ::pItems, { Lower( cMsg ), "Method", .F. } )
   NEXT

   ::objname := cVarName
   ::TheObj := oObject
   ::lEditable := hb_defaultValue( lEditable, .T. )

   ::addWindows()

   RETURN Self

METHOD addWindows( nRow ) CLASS HBDbObject

   LOCAL oBrwSets
   LOCAL nSize := Len( ::pItems )
   LOCAL oWndSets
   LOCAL oCol
   LOCAL nMaxLen

   IF nSize < MaxRow() - 2
      IF HB_ISNUMERIC( nRow )
         oWndSets := HBDbWindow():New( nRow, 5, iif( nRow + nSize + 1 < MaxRow() - 2, nRow + nSize + 1, MaxRow() - 2 ), MaxCol() - 5, ;
            ::objname + " is of class: " + ::TheObj:ClassName(), "N/W" )
      ELSE
         oWndSets := HBDbWindow():New( 1, 5, 2 + nSize, MaxCol() - 5, ;
            ::objname + " is of class: " + ::TheObj:ClassName(), "N/W" )
      ENDIF
   ELSE
      oWndSets := HBDbWindow():New( 1, 5, MaxRow() - 2, MaxCol() - 5, ;
         ::objname + " is of class: " + ::TheObj:ClassName(), "N/W" )
   ENDIF

   ::nCurWindow++
   oWndSets:lFocused := .T.
   AAdd( ::aWindows, oWndSets )

   oBrwSets := HBDbBrowser():New( oWndSets:nTop + 1, oWndSets:nLeft + 1, oWndSets:nBottom - 1, oWndSets:nRight - 1 )

   oBrwSets:ColorSpec := __dbg():ClrModal()
   oBrwSets:GoTopBlock := {|| ::Arrayindex := 1 }
   oBrwSets:GoBottomBlock := {|| ::arrayindex := Len( ::pItems ) }
   oBrwSets:SkipBlock := {| nSkip, nPos | nPos := ::arrayindex, ;
                                          ::arrayindex := Max( 1, Min( ::arrayindex + nSkip, Len( ::pItems ) ) ), ;
                                          ::arrayindex - nPos }

   nMaxLen := 0
   AEval( ::pItems, {| x | nMaxLen := Max( nMaxLen, Len( x[ OMSG_NAME ] ) ) } )
   oBrwSets:AddColumn( oCol := HBDbColumnNew( "", {|| ::pItems[ ::arrayindex, OMSG_NAME ] } ) )
   oCol:width := nMaxLen
   oCol:DefColor := { 1, 2 }
   oBrwSets:Freeze := 1

   oBrwSets:AddColumn( oCol := HBDbColumnNew( "", {|| iif( ! ::pItems[ ::ArrayIndex, OMSG_EDIT ], ;
      ::pItems[ ::ArrayIndex, OMSG_VALUE ], ;
      __dbgValToExp( __dbgObjGetValue( ::TheObj, ::pItems[ ::arrayindex, OMSG_NAME ] ) ) ) } ) )

   oCol:DefColor := { 1, 3 }
   oCol:width := oWndSets:nRight - oWndSets:nLeft - nMaxLen - 2
   oBrwSets:colPos := 2

   ::aWindows[ ::nCurWindow ]:bPainted    := {|| oBrwSets:ForceStable() }
   ::aWindows[ ::nCurWindow ]:bKeyPressed := {| nKey | ::SetsKeyPressed( nKey, oBrwSets ) }
   ::aWindows[ ::nCurwindow ]:cCaption := ::objname + " is of class: " + ::TheObj:ClassName()

   ::aWindows[ ::nCurWindow ]:ShowModal()

   RETURN Self

METHOD PROCEDURE doGet( oBrowse ) CLASS HBDbObject

   LOCAL oErr
   LOCAL cValue
   LOCAL lCanAcc
   LOCAL aItemRef

   // make sure browse is stable
   oBrowse:forceStable()
   // if confirming new record, append blank

   aItemRef := ::pItems[ ::ArrayIndex ]
   cValue := __dbgObjGetValue( ::TheObj, aItemRef[ OMSG_NAME ], @lCanAcc )
   IF ! lCanAcc
      __dbgAlert( cValue )
      RETURN
   ENDIF
   cValue := __dbgValToExp( cValue )

   IF __dbgInput( Row(), oBrowse:nLeft + oBrowse:GetColumn( 1 ):width + 1, ;
                  oBrowse:getColumn( oBrowse:colPos ):Width, @cValue, ;
                  __dbgExprValidBlock(), __dbgColors()[ 2 ], 256 )
      BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
         __dbgObjSetValue( ::TheObj, aItemRef[ OMSG_NAME ], &cValue )
      RECOVER USING oErr
         __dbgAlert( oErr:description )
      END SEQUENCE
   ENDIF

   RETURN

METHOD PROCEDURE SetsKeyPressed( nKey, oBrwSets ) CLASS HBDbObject

   LOCAL aItemRef

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

      aItemRef := ::pItems[ ::ArrayIndex ]
      DO CASE
      CASE HB_ISARRAY( aItemRef[ OMSG_VALUE ] )
         IF Len( aItemRef[ OMSG_VALUE ] ) > 0
            HBDbArray():New( aItemRef[ OMSG_VALUE ], aItemRef[ OMSG_NAME ] )
         ENDIF
      CASE HB_ISHASH( aItemRef[ OMSG_VALUE ] )
         IF Len( aItemRef[ OMSG_VALUE ] ) > 0
            HBDbHash():New( aItemRef[ OMSG_VALUE ], aItemRef[ OMSG_NAME ] )
         ENDIF
      CASE HB_ISOBJECT( aItemRef[ OMSG_VALUE ] )
         HBDbObject():New( aItemRef[ OMSG_VALUE ], aItemRef[ OMSG_NAME ] )
      CASE ! aItemRef[ OMSG_EDIT ] .OR. ;
           HB_ISBLOCK( aItemRef[ OMSG_VALUE ] ) .OR. ;
           HB_ISPOINTER( aItemRef[ OMSG_VALUE ] ) .OR. ;
           ! ::lEditable
         __dbgAlert( "Value cannot be edited" )
      OTHERWISE
         oBrwSets:RefreshCurrent()
         ::doGet( oBrwSets )
         oBrwSets:RefreshCurrent()
         oBrwSets:ForceStable()
      ENDCASE

   ENDSWITCH

   oBrwSets:ForceStable()

   RETURN

FUNCTION __dbgObject( oObject, cVarName, lEditable )
   RETURN HBDbObject():New( oObject, cVarName, lEditable )

STATIC FUNCTION __dbgObjGetValue( oObject, cVar, lCanAcc )

   LOCAL nProcLevel := __dbg():nProcLevel
   LOCAL xResult
   LOCAL oErr

   BEGIN SEQUENCE WITH {|| Break() }
      xResult := __dbgSendMsg( nProcLevel, oObject, cVar )
      lCanAcc := .T.
   RECOVER
      BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
         /* Try to access variables using class code level */
         xResult := __dbgSendMsg( 0, oObject, cVar )
         lCanAcc := .T.
      RECOVER USING oErr
         xResult := oErr:description
         lCanAcc := .F.
      END SEQUENCE
   END SEQUENCE

   RETURN xResult

STATIC FUNCTION __dbgObjSetValue( oObject, cVar, xValue )

   LOCAL nProcLevel := __dbg():nProcLevel
   LOCAL oErr

   BEGIN SEQUENCE WITH {|| Break() }
      __dbgSendMsg( nProcLevel, oObject, "_" + cVar, xValue )
   RECOVER
      BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
         /* Try to access variables using class code level */
         __dbgSendMsg( 0, oObject, "_" + cVar, xValue )
      RECOVER USING oErr
         __dbgAlert( oErr:description )
      END SEQUENCE
   END SEQUENCE

   RETURN xValue
