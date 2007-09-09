/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HBGetList Class
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2001 Luiz Rafael Culik
 *    Support for CA-Cl*pper 5.3 Getsystem
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbclass.ch"

#include "button.ch"
#include "common.ch"
#include "getexit.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "tbrowse.ch"

#define SCORE_ROW       0
#define SCORE_COL       60

#define _GET_INSERT_ON  7
#define _GET_INSERT_OFF 8
#define _GET_INVD_DATE  9

#define K_UNDO          K_CTRL_U

#define MSGFLAG         1
#define MSGROW          2
#define MSGLEFT         3
#define MSGRIGHT        4
#define MSGCOLOR        5

CREATE CLASS HBGetList

   EXPORT:

   VAR HasFocus        AS LOGICAL   INIT .F.

   METHOD Settle( nPos, lInit )
   METHOD Reader( oMenu, aMsg )
   METHOD GetApplyKey( nKey, oMenu, aMsg )
   METHOD GetPreValidate( oGet, aMsg )
   METHOD GetPostValidate( oGet, aMsg )
   METHOD GetDoSetKey( bKeyBlock )
   METHOD PostActiveGet()
   METHOD GetReadVar()
   METHOD SetFormat( bFormat )
   METHOD KillRead( lKill )
   METHOD GetActive( oGet )
   METHOD DateMsg()
   METHOD ShowScoreBoard()
   METHOD ReadUpdated( lUpdated )
   METHOD ReadVar( cNewVarName )
   METHOD ReadExit( lNew )
   METHOD SetFocus()
   METHOD Updated()

#ifdef HB_COMPAT_C53
   METHOD GUIReader( oGet, oMenu, aMsg )
   METHOD GUIApplyKey( oGet, oGUI, nKey, oMenu, aMsg )
   METHOD GUIPreValidate( oGUI, aMsg )
   METHOD GUIPostValidate( oGUI, aMsg )
   METHOD TBApplyKey( oGet, oTB, nKey, oMenu, aMsg )
   METHOD TBReader( oGet, oMenu, aMsg )
   METHOD Accelerator( nKey, aMsg )
   METHOD HitTest( nMouseRow, nMouseColumn, aMsg )
   METHOD ShowGetMsg( aMsg )
   METHOD EraseGetMsg( aMsg )
#endif

   METHOD New( GetList )

   VAR lUpdated        AS LOGICAL   INIT .F.
   VAR bFormat
   VAR lKillRead       AS LOGICAL   INIT .F.
   VAR lBumpTop        AS LOGICAL   INIT .F.
   VAR lBumpBot        AS LOGICAL   INIT .F.
   VAR nLastExitState  AS NUMERIC   INIT 0
   VAR nLastPos        AS NUMERIC   INIT 0
   VAR oActiveGet
   VAR cVarName
   VAR cReadProcName   AS CHARACTER INIT ""
   VAR nReadProcLine
#ifdef HB_COMPAT_C53
   VAR nNextGet
   VAR nHitCode        AS NUMERIC   INIT 0
   VAR cMsgSaveS
   VAR nMenuID
   VAR nSaveCursor
#endif

   VAR aGetList
   VAR oGet
   VAR nPos            AS NUMERIC   INIT 1

ENDCLASS

METHOD ReadExit( lNew ) CLASS HBGetList
   RETURN iif( ISLOGICAL( lNew ), Set( _SET_EXIT, lNew ), Set( _SET_EXIT ) )

METHOD Updated() CLASS HBGetList
   RETURN ::lUpdated

METHOD SetFocus() CLASS HBGetList

   __GetListSetActive( Self )
   __GetListLast( Self )
   ::aGetList[ ::nPos ]:SetFocus()

   RETURN Self

METHOD Reader( oMenu, aMsg ) CLASS HBGetList

   LOCAL oGet := ::oGet
   LOCAL nRow
   LOCAL nCol
#ifdef HB_COMPAT_C53
   LOCAL nOldCursor
   LOCAL nKey
#endif

#ifdef HB_COMPAT_C53
   IF ::nLastExitState == GE_SHORTCUT .OR.;
      ::nLastExitState == GE_MOUSEHIT .OR.;
      ::GetPreValidate( oGet, aMsg )
#else
   IF ::GetPreValidate( oGet, aMsg )
#endif

      ::ShowGetMsg( aMsg )

      oGet:SetFocus()

      DO WHILE oGet:ExitState == GE_NOEXIT
         IF oGet:typeOut
            oGet:ExitState := GE_ENTER
         ENDIF

         IF oGet:buffer == NIL
            oGet:ExitState := GE_ENTER
         ENDIF

         DO WHILE oGet:exitState == GE_NOEXIT
#ifdef HB_COMPAT_C53
            SetCursor( iif( ::nSaveCursor == SC_NONE, SC_NORMAL, ::nSaveCursor ) )
            nKey := Inkey( 0 )
            SetCursor( SC_NONE )
            ::GetApplyKey( nKey, oMenu, aMsg )
#else
            ::GetApplyKey( Inkey( 0 ), oMenu, aMsg )
#endif
            nRow := Row()
            nCol := Col()
            ::ShowGetMsg( aMsg )
            SetPos( nRow, nCol )
         ENDDO

#ifdef HB_COMPAT_C53
         IF !::nLastExitState == GE_SHORTCUT .AND. ;
            !::nLastExitState == GE_MOUSEHIT .AND. ;
            !::GetPostValidate( oGet, aMsg )
#else
         IF !::GetPostValidate( oGet, aMsg )
#endif
            oGet:ExitState := GE_NOEXIT
         ENDIF
      ENDDO

#ifdef HB_COMPAT_C53
      nRow := Row()
      nCol := Col()
      nOldCursor := SetCursor()
#endif
      oGet:killFocus()
#ifdef HB_COMPAT_C53
      SetCursor( nOldCursor )
      SetPos( nRow, nCol )
#endif

      ::EraseGetMsg( aMsg )
   ENDIF

   RETURN Self

METHOD GetApplyKey( nKey, oMenu, aMsg ) CLASS HBGetList

   LOCAL cKey
   LOCAL bKeyBlock
   LOCAL oGet := ::oGet
   LOCAL nMouseRow
   LOCAL nMouseColumn
   LOCAL nButton
   LOCAL nHotItem

   IF ( bKeyBlock := SetKey( nKey ) ) != NIL
      ::GetDoSetKey( bKeyBlock )
      RETURN Self
   ENDIF

#ifdef HB_COMPAT_C53
   IF ::aGetList != NIL .AND. ( nHotItem := ::Accelerator( nKey, aMsg ) ) != 0

      oGet:ExitState := GE_SHORTCUT
      ::nNextGet       := nHotItem
      ::nLastExitState := GE_SHORTCUT
   ELSEIF !ISOBJECT( oMenu )
   ELSEIF ( nHotItem := oMenu:getAccel( nKey ) ) != 0
      ::nMenuID := MenuModal( oMenu, nHotItem, aMsg[ MSGROW ], aMsg[ MSGLEFT ], aMsg[ MSGRIGHT ], aMsg[ MSGCOLOR ] )
      nKey := 0
   ELSEIF IsShortCut( oMenu, nKey )
      nKey := 0
   ENDIF
#else
   HB_SYMBOL_UNUSED( oMenu )
   HB_SYMBOL_UNUSED( aMsg )
#endif

   DO CASE
   CASE nKey == K_UP
      oGet:ExitState := GE_UP
   
   CASE nKey == K_SH_TAB
      oGet:ExitState := GE_UP
   
   CASE nKey == K_DOWN
      oGet:ExitState := GE_DOWN
   
   CASE nKey == K_TAB
      oGet:ExitState := GE_DOWN
   
   CASE nKey == K_ENTER
      oGet:ExitState := GE_ENTER

   CASE nKey == K_ESC
      IF Set( _SET_ESCAPE )
         oGet:Undo()
         oGet:ExitState := GE_ESCAPE
      ENDIF

   CASE nKey == K_PGUP
      oGet:ExitState := GE_WRITE

   CASE nKey == K_PGDN
      oGet:ExitState := GE_WRITE

   CASE nKey == K_CTRL_HOME
      oGet:ExitState := GE_TOP

#ifdef CTRL_END_SPECIAL
   CASE nKey == K_CTRL_END
      oGet:ExitState := GE_BOTTOM
#else
   CASE nKey == K_CTRL_W
      oGet:ExitState := GE_WRITE
#endif

   CASE nKey == K_INS
      Set( _SET_INSERT, ! Set( _SET_INSERT ) )
      ::ShowScoreboard()

   CASE nKey == K_LBUTTONDOWN .OR. nKey == K_LDBLCLK
      nMouseRow    := MRow()
      nMouseColumn := MCol()

      nButton := 0

      IF ( nButton := oGet:HitTest( nMouseRow, nMouseColumn ) ) == HTCLIENT

         DO WHILE oGet:Col + oGet:Pos - 1 > nMouseColumn
            oGet:Left()

            // Handle editing buffer if first character is non-editable:
            IF oGet:typeOut
               // reset typeout:
               oGet:Home()
               EXIT
            ENDIF

         ENDDO

         DO WHILE oGet:Col+oGet:Pos-1 < nMouseColumn
            oGet:Right()

            // Handle editing buffer if last character is non-editable:
            IF oGet:typeOut
               // reset typeout:
               oGet:End()
               EXIT
            ENDIF

         ENDDO

      ELSEIF nButton != HTNOWHERE
      ELSEIF ::aGetList != NIL .AND. ::HitTest( nMouseRow, nMouseColumn ) != 0
         oGet:ExitState := GE_MOUSEHIT
         ::nLastExitState := GE_MOUSEHIT
      ELSE
         oGet:ExitState := GE_NOEXIT
      ENDIF

   CASE nKey == K_UNDO
      oGet:Undo()

   CASE nKey == K_HOME
      oGet:Home()

   CASE nKey == K_END
      oGet:End()

   CASE nKey == K_RIGHT
      oGet:Right()

   CASE nKey == K_LEFT
      oGet:Left()

   CASE nKey == K_CTRL_RIGHT
      oGet:WordRight()

   CASE nKey == K_CTRL_LEFT
      oGet:WordLeft()

   CASE nKey == K_BS
      oGet:BackSpace()

   CASE nKey == K_DEL
      oGet:Delete()

   CASE nKey == K_CTRL_T
      oGet:DelWordRight()

   CASE nKey == K_CTRL_Y
      oGet:DelEnd()

   CASE nKey == K_CTRL_BS
      oGet:DelWordLeft()

   OTHERWISE

      IF nKey >= 32 .AND. nKey <= 255
         cKey := Chr( nKey )

         IF oGet:type == "N" .AND. ( cKey == "." .OR. cKey == "," )
            oGet:ToDecPos()
         ELSE
            IF Set( _SET_INSERT )
               oGet:Insert( cKey )
            ELSE
               oGet:OverStrike( cKey )
            ENDIF

            IF oGet:TypeOut
               IF Set( _SET_BELL )
                  ?? Chr( 7 )
               ENDIF
               IF ! Set( _SET_CONFIRM )
                  oGet:ExitState := GE_ENTER
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDCASE

   RETURN Self

METHOD GetPreValidate( oGet, aMsg ) CLASS HBGetList

   LOCAL lUpdated
   LOCAL lWhen := .T.
   LOCAL xValue

   DEFAULT oGet TO ::oGet

   IF oGet:PreBlock != NIL

      xValue    := oGet:VarGet()
      lUpdated  := ::lUpdated

      lWhen     := Eval( oGet:PreBlock, oGet, aMsg )

      IF !( ValType( xValue ) == ValType( oGet:VarGet() ) ) .OR.;
         !( oGet:VarGet() == xValue )
         oGet:VarPut( oGet:VarGet() )
      ELSE
         oGet:Display()
      ENDIF

      ::ShowScoreBoard()

      ::lUpdated := lUpdated

/*
      IF __GetListActive() != Self
         __GetListSetActive( Self )
      ENDIF
*/
      __GetListLast( Self )
   ENDIF

   IF ::lKillRead
      lWhen := .F.
      oGet:ExitState := GE_ESCAPE
   ELSEIF ! lWhen
      oGet:ExitState := GE_WHEN
   ELSE
      oGet:ExitState := GE_NOEXIT
   ENDIF

   RETURN lWhen

METHOD GetPostValidate( oGet, aMsg ) CLASS HBGetList

   LOCAL lUpdated
   LOCAL lValid := .T.
   LOCAL xValue

   DEFAULT oGet TO ::oGet

   IF oGet:ExitState == GE_ESCAPE
      RETURN .T.
   ENDIF

   IF oGet:BadDate
      oGet:home()
      ::DateMsg()
      ::ShowScoreboard()
      RETURN .F.
   ENDIF

   IF oGet:Changed
      oGet:Assign()
      ::lUpdated := .T.
   ENDIF

   oGet:Reset():Display()

   IF oGet:PostBlock != NIL

      xValue   := oGet:VarGet()
      lUpdated := ::lUpdated

      SetPos( oGet:Row, oGet:Col + iif( oGet:Buffer == NIL, 0, Len( oGet:Buffer ) ) )
      lValid := Eval( oGet:PostBlock, oGet, aMsg )
      SetPos( oGet:Row, oGet:Col )

      IF !( ValType( xValue ) == ValType( oGet:VarGet() ) ) .OR. ;
         !( oGet:VarGet() == xValue )
         oGet:VarPut( oGet:VarGet() )
      ENDIF
      oGet:UpdateBuffer()

      ::ShowScoreBoard()
      
      ::lUpdated := lUpdated

/*
      IF __GetListActive() != Self
         __GetListSetActive( Self )
      ENDIF
*/
      __GetListLast( Self )

      IF ::lKillRead
         oGet:ExitState := GE_ESCAPE
         lValid := .T.
      ENDIF
   ENDIF

   RETURN lValid

METHOD GetDoSetKey( bKeyBlock ) CLASS HBGetList

   LOCAL oGet := ::oGet
   LOCAL lUpdated
   LOCAL xValue

   IF oGet:Changed
      oGet:Assign()
      ::lUpdated := .T.
   ENDIF

   xValue   := oGet:VarGet()
   lUpdated := ::lUpdated

   Eval( bKeyBlock, ::cReadProcName, ::nReadProcLine, ::ReadVar() )

   IF !( ValType( xValue ) == ValType( oGet:VarGet() ) ) .OR.;
      !( oGet:VarGet() == xValue )
      oGet:VarPut( oGet:VarGet() )
   ENDIF

   ::ShowScoreboard()
   oGet:UpdateBuffer()

   ::lUpdated := lUpdated

/*
   IF __GetListActive() != Self
      __GetListSetActive( Self )
   ENDIF
*/
   __GetListLast( Self )

   IF ::lKillRead
      oGet:ExitState := GE_ESCAPE
   ENDIF

   RETURN Self

METHOD Settle( nPos, lInit ) CLASS HBGetList

   LOCAL nExitState

   DEFAULT nPos  TO ::nPos
   DEFAULT lInit TO .F.

   IF nPos == 0
      nExitState := GE_DOWN
   ELSEIF nPos > 0 .AND. lInit /* NOTE: Never .T. in C5.2 mode. */
      nExitState := GE_NOEXIT
   ELSE
      nExitState := ::aGetList[ nPos ]:ExitState
   ENDIF

   IF nExitState == GE_ESCAPE .OR. nExitState == GE_WRITE
      RETURN 0
   ENDIF

   IF nExitState != GE_WHEN
      ::nLastPos := nPos
      ::lBumpTop := .F.
      ::lBumpBot := .F.
   ELSE
      IF ::nLastExitState != 0
         nExitState := ::nLastExitState
      ELSEIF ::nNextGet < ::nLastPos 
         nExitState := GE_UP
      ELSE
         nExitState := GE_DOWN
      ENDIF

   ENDIF

   DO CASE
   CASE nExitState == GE_UP
      nPos--

   CASE nExitState == GE_DOWN
      nPos++

   CASE nExitState == GE_TOP
      nPos := 1
      ::lBumpTop := .T.
      nExitState := GE_DOWN

   CASE nExitState == GE_BOTTOM
      nPos := Len( ::aGetList )
      ::lBumpBot := .T.
      nExitState := GE_UP

   CASE nExitState == GE_ENTER
      nPos++

   CASE nExitState == GE_SHORTCUT 
      RETURN ::nNextGet

   CASE nExitState == GE_MOUSEHIT
      RETURN ::nNextGet

   ENDCASE

   IF nPos == 0
      IF ! ::ReadExit() .AND. ! ::lBumpBot
         ::lBumpTop := .T.
         nPos       := ::nLastPos
         nExitState := GE_DOWN
      ENDIF

   ELSEIF nPos == Len( ::aGetList ) + 1
      IF ! ::ReadExit() .AND. nExitState != GE_ENTER .AND. ! ::lBumpTop
         ::lBumpBot := .T.
         nPos       := ::nLastPos
         nExitState := GE_UP
      ELSE
         nPos := 0
      ENDIF
   ENDIF

   ::nLastExitState := nExitState

   IF nPos != 0
      ::aGetList[ nPos ]:ExitState := nExitState
   ENDIF

   RETURN nPos

METHOD PostActiveGet() CLASS HBGetList

   ::GetActive( ::oGet )
   ::ReadVar( ::GetReadVar() )
   ::ShowScoreBoard()

   RETURN Self

METHOD GetReadVar() CLASS HBGetList

   LOCAL oGet := ::oGet
   LOCAL cName := Upper( oGet:Name )
   LOCAL n

   IF oGet:Subscript != NIL
      FOR n := 1 TO Len( oGet:Subscript )
         cName += "[" + LTrim( Str( oGet:Subscript[ n ] ) ) + "]"
      NEXT
   ENDIF

   RETURN cName

METHOD SetFormat( bFormat ) CLASS HBGetList

   LOCAL bSavFormat := ::bFormat

   ::bFormat := bFormat

   RETURN bSavFormat

METHOD KillRead( lKill ) CLASS HBGetList

   LOCAL lSavKill := ::lKillRead

   IF PCount() > 0
      ::lKillRead := lKill
   ENDIF

   RETURN lSavKill

METHOD GetActive( oGet ) CLASS HBGetList

   LOCAL oOldGet := ::oActiveGet

   IF PCount() > 0
      ::oActiveGet := oGet
   ENDIF

   RETURN oOldGet

METHOD ShowScoreboard() CLASS HBGetList

   LOCAL nRow
   LOCAL nCol
   LOCAL nOldCursor

   IF Set( _SET_SCOREBOARD )

      nRow := Row()
      nCol := Col()

      nOldCursor := SetCursor( SC_NONE )

      DispOutAt( SCORE_ROW, SCORE_COL, iif( Set( _SET_INSERT ), NationMsg( _GET_INSERT_ON ), NationMsg( _GET_INSERT_OFF ) ) )
      SetPos( nRow, nCol )

      SetCursor( nOldCursor )

   ENDIF

   RETURN Self

METHOD DateMsg() CLASS HBGetList

   LOCAL nRow
   LOCAL nCol

   IF Set( _SET_SCOREBOARD )

      nRow := Row()
      nCol := Col()

      DispOutAt( SCORE_ROW, SCORE_COL, NationMsg( _GET_INVD_DATE ) )
      SetPos( nRow, nCol )

      DO WHILE NextKey() == 0
      ENDDO

      DispOutAt( SCORE_ROW, SCORE_COL, Space( Len( NationMsg( _GET_INVD_DATE ) ) ) )
      SetPos( nRow, nCol )

   ENDIF

   RETURN Self

METHOD ReadVar( cNewVarName ) CLASS HBGetList

   LOCAL cOldName := ::cVarName

   IF ISCHARACTER( cNewVarName )
      ::cVarName := cNewVarName
   ENDIF

   RETURN cOldName

METHOD ReadUpdated( lUpdated ) CLASS HBGetList

   LOCAL lSavUpdated := ::lUpdated

   IF PCount() > 0
      ::lUpdated := lUpdated
   ENDIF

   RETURN lSavUpdated

#ifdef HB_COMPAT_C53

METHOD GUIReader( oGet, oMenu, aMsg ) CLASS HBGetList

   LOCAL oGUI

   IF ::GUIPreValidate( oGet:Control, aMsg ) .AND. ;
      ISOBJECT( oGet:Control )

      ::ShowGetMsg( aMsg )

      // Activate the GET for reading
      oGUI := oGet:Control
      oGUI:Select( oGet:VarGet() )
      oGUI:setFocus()

      DO WHILE oGet:exitState == GE_NOEXIT .AND. !::lKillRead

         // Check for initial typeout (no editable positions)
         IF oGUI:typeOut
            oGet:exitState := GE_ENTER
         ENDIF

         // Apply keystrokes until exit
         DO WHILE oGet:exitState == GE_NOEXIT .AND. !::lKillRead
            ::GUIApplyKey( oGet, oGUI, Inkey( 0 ), oMenu, aMsg )

            ::ShowGetMsg( aMsg )
         ENDDO

         // Disallow exit if the VALID condition is not satisfied

         IF !::GUIPostValidate( oGUI, aMsg )
            oGet:exitState := GE_NOEXIT
         ENDIF
      ENDDO

         // De-activate the GET
      oGet:VarPut( oGUI:Buffer )
      oGUI:killFocus()

      ::EraseGetMsg( aMsg )

      IF oGUI:ClassName() == "LISTBOX" .AND. ;
         oGUI:dropDown .AND. ;
         oGUI:isOpen 

         oGUI:Close()
      ENDIF

   ENDIF

   RETURN Self

METHOD GUIApplyKey( oGet, oGUI, nKey, oMenu, aMsg ) CLASS HBGetList

   LOCAL bKeyBlock
   LOCAL oTheClass
   LOCAL nHotItem
   LOCAL lClose
   LOCAL nMouseRow
   LOCAL nMouseColumn
   LOCAL nButton

   // Check for SET KEY first
   IF ( bKeyBlock := SetKey( nKey ) ) != NIL
      ::GetDoSetKey( bKeyBlock, oGet )
   ENDIF

   IF ( nHotItem := ::Accelerator( nKey, aMsg ) ) != 0
      oGet:ExitState := GE_SHORTCUT
      ::nNextGet       := nHotItem
   ELSEIF !ISOBJECT( oMenu )
   ELSEIF ( nHotItem := oMenu:getAccel( nKey ) ) != 0
      ::nMenuID := MenuModal( oMenu, nHotItem, aMsg[ MSGROW ], aMsg[ MSGLEFT ], aMsg[ MSGRIGHT ], aMsg[ MSGCOLOR ] )
      nKey := 0
   ELSEIF IsShortCut( oMenu, nKey )
      nKey := 0
   ENDIF

   IF nKey == 0
   ELSEIF ( oTheClass := oGUI:ClassName() ) == "RADIOGROUP"
      IF nKey == K_UP
         oGUI:PrevItem()
         nKey := 0

      ELSEIF nKey == K_DOWN
         oGUI:NextItem()
         nKey := 0

      ELSEIF ( nHotItem := oGUI:GetAccel( nKey ) ) != 0
         oGUI:Select( nHotItem )

      ENDIF

      IF ISNUMBER( oGet:VarGet() )
         oGet:VarPut( oGUI:Value )
      ENDIF

   ELSEIF oTheClass == "CHECKBOX"
      IF nKey == K_SPACE
         oGUI:Select()
      ENDIF

   ELSEIF oTheClass == "PUSHBUTTON"
      IF nKey == K_SPACE
         oGUI:Select( K_SPACE )

      ELSEIF nKey == K_ENTER
         oGUI:Select()
         nKey := 0

      ENDIF

   ELSEIF oTheClass == "LISTBOX"
      IF nKey == K_UP
         oGUI:PrevItem()
         nKey := 0

      ELSEIF nKey == K_DOWN
         oGUI:NextItem()
         nKey := 0

      ELSEIF nKey == K_SPACE
         IF ! oGUI:DropDown
         ELSEIF ! oGUI:IsOpen
            oGUI:Open()
            nKey := 0
         ENDIF

      ELSEIF ( nButton := oGUI:FindText( chr(nKey), oGUI:Value+1, .F., .F. ) ) != 0
         oGUI:Select( nButton )

      ENDIF

      IF ISNUMBER( oGet:VarGet() )
         oGet:VarPut( oGUI:Value )

      ENDIF

   ENDIF

   DO CASE
   CASE nKey == K_UP
      oGet:ExitState := GE_UP

   CASE nKey == K_SH_TAB
      oGet:ExitState := GE_UP

   CASE nKey == K_DOWN
      oGet:ExitState := GE_DOWN

   CASE nKey == K_TAB
      oGet:ExitState := GE_DOWN

   CASE nKey == K_ENTER
      oGet:ExitState := GE_ENTER

   CASE nKey == K_ESC
      IF set( _SET_ESCAPE )
         oGet:ExitState := GE_ESCAPE
      ENDIF

   CASE nKey == K_PGUP
      oGet:ExitState := GE_WRITE

   CASE nKey == K_PGDN
      oGet:ExitState := GE_WRITE

   CASE nKey == K_CTRL_HOME
      oGet:ExitState := GE_TOP


#ifdef CTRL_END_SPECIAL

   // Both ^W and ^End go to the last GET
   CASE nKey == K_CTRL_END
      oGet:ExitState := GE_BOTTOM

#else

   // Both ^W and ^End terminate the READ (the default)
   CASE nKey == K_CTRL_W
      oGet:ExitState := GE_WRITE

#endif

   CASE nKey == K_LBUTTONDOWN .OR. nKey == K_LDBLCLK
      nMouseRow    := mROW()
      nMouseColumn := mCOL()
      lClose := .T.
      nButton:=0

      IF ( nButton := oGUI:HitTest( nMouseRow, nMouseColumn ) ) == HTNOWHERE
         // Changed test:
         IF ::HitTest( nMouseRow, nMouseColumn  ) != 0
            oGet:ExitState := GE_MOUSEHIT
            ::nLastExitState := GE_MOUSEHIT
         ELSE
            oGet:ExitState := GE_NOEXIT
         ENDIF

      ELSEIF nButton >= HTCLIENT
         oGUI:Select( nButton )

      ELSEIF nButton == HTDROPBUTTON
         IF !oGUI:IsOpen
            oGUI:Open()
            lClose := .F.
         ENDIF

      ELSEIF nButton >= HTSCROLLFIRST .AND. nButton <= HTSCROLLLAST
         oGUI:Scroll( nButton )
         lClose := .F.

      ENDIF

      IF ! lClose
      ELSEIF ! oTheClass == "LISTBOX"
      ELSEIF ! oGUI:DropDown
      ELSEIF oGUI:IsOpen
         oGUI:Close()
         oGUI:Display()
      ENDIF

   ENDCASE

   RETURN Self

METHOD GUIPreValidate( oGUI, aMsg ) CLASS HBGetList

   LOCAL oGet := ::oGet
   LOCAL lUpdated
   LOCAL lWhen := .T.
   LOCAL xValue

   IF oGet:preBlock != NIL

      xValue   := oGet:VarGet()
      lUpdated := ::lUpdated

      lWhen := Eval( oGet:preBlock, oGet, aMsg )

      IF !( ValType( xValue ) == ValType( oGet:VarGet() ) ) .OR.;
         !( oGet:VarGet() == xValue )
         oGet:VarPut( oGet:VarGet() )
      ELSEIF !( oGUI:ClassName() == "TBROWSE" )
         oGet:Display()
      ENDIF

      ::ShowScoreBoard()

      ::lUpdated := lUpdated

/*
      IF __GetListActive() != Self
         __GetListSetActive( Self )
      ENDIF
*/
      __GetListLast( Self )
   ENDIF

   IF ::lKillRead
      lWhen := .F.
      oGet:ExitState := GE_ESCAPE

   ELSEIF !lWhen
      oGet:ExitState := GE_WHEN

   ELSE
      oGet:ExitState := GE_NOEXIT

   ENDIF

   RETURN lWhen

METHOD GUIPostValidate( oGUI, aMsg ) CLASS HBGetList

   LOCAL oGet := ::oGet
   LOCAL lUpdated
   LOCAL lValid := .T.
   LOCAL xValue

   IF oGet:exitState == GE_ESCAPE
      RETURN .T.                   // NOTE
   ENDIF

   IF oGet:BadDate
      oGet:home()
      ::DateMsg()
      ::ShowScoreboard()
      RETURN .F.
   ENDIF

   IF oGet:Changed
      oGet:UpdateBuffer()
      ::lUpdated := .T.
   ENDIF

   oGet:Reset():Display()

/*
   // If editing occurred, assign the new value to the variable
   IF !( uOldData == uNewData )
      oGet:VarPut( uNewData )
      ::lUpdated := .T.
   ENDIF
*/

   // Check VALID condition if specified
   IF oGet:postBlock != NIL

      xValue   := oGet:VarGet()
      lUpdated := ::lUpdated

      lValid := Eval( oGet:postBlock, oGet, aMsg )

      // Reset S'87 compatibility cursor position
      SetPos( oGet:Row, oGet:Col )

      IF !( ValType( xValue ) == ValType( oGet:VarGet() ) ) .OR.;
         !( oGet:VarGet() == xValue )
         oGet:VarPut( oGet:VarGet() )
      ENDIF
      oGet:UpdateBuffer()

      ::ShowScoreBoard()
      IF ! ( oGUI:ClassName == "TBROWSE" )
         oGUI:Select( oGet:VarGet() )
      ENDIF

      ::lUpdated := lUpdated

/*
      IF __GetListActive() != Self
         __GetListSetActive( Self )
      ENDIF
*/
      __GetListLast( Self )

      IF ::lKillRead
         oGet:ExitState := GE_ESCAPE      // Provokes ReadModal() exit
         lValid := .T.
      ENDIF

   ENDIF

   RETURN lValid 

METHOD TBApplyKey( oGet, oTB, nKey, oMenu, aMsg ) CLASS HBGetList

   LOCAL bKeyBlock
   LOCAL nMouseRow
   LOCAL nMouseColumn
   LOCAL nButton
   LOCAL nHotItem
   LOCAL lSetKey

   // Check for SET KEY first
   IF ( bKeyBlock := SetKey( nKey ) ) != NIL
      IF lSetKey := ::GetDoSetKey( bKeyBlock, oGet )
         RETURN Self
      ENDIF
   ENDIF

   IF ( nHotItem := ::Accelerator( nKey, aMsg ) ) != 0
      oGet:ExitState := GE_SHORTCUT
      ::nNextGet       := nHotItem
   ELSEIF !ISOBJECT( oMenu )
   ELSEIF ( nHotItem := oMenu:getAccel( nKey ) ) != 0
      ::nMenuID := MenuModal( oMenu, nHotItem, aMsg[ MSGROW ], aMsg[ MSGLEFT ], aMsg[ MSGRIGHT ], aMsg[ MSGCOLOR ] )
      nKey := 0
   ELSEIF IsShortCut( oMenu, nKey )
      nKey := 0
   ENDIF

   DO CASE
   CASE nKey == K_TAB 
      oGet:ExitState := GE_DOWN

   CASE nKey == K_SH_TAB
      oGet:ExitState := GE_UP

   CASE nKey == K_ENTER
      IF !oTb:Stable()
         oTb:ForceStable()
      ENDIF
      oGet:ExitState := GE_ENTER

   CASE nKey == K_ESC 
      IF set( _SET_ESCAPE )
         oGet:ExitState := GE_ESCAPE
      ENDIF

#ifdef CTRL_END_SPECIAL

   // Both ^W and ^End go to the last GET
   CASE nKey == K_CTRL_END
      oGet:ExitState := GE_BOTTOM

#else

   // Both ^W and ^End terminate the READ (the default)
   CASE nKey == K_CTRL_W
      oGet:ExitState := GE_WRITE

#endif

   CASE nKey == K_LBUTTONDOWN .OR. nKey == K_LDBLCLK
      nMouseRow    := MRow()
      nMouseColumn := MCol()
      nButton      := 0

      IF ( nButton := oTB:HitTest( nMouseRow, nMouseColumn ) ) == HTNOWHERE
         IF ::HitTest( nMouseRow, nMouseColumn, aMsg ) != 0
            oGet:ExitState := GE_MOUSEHIT
         ELSE
            oGet:ExitState := GE_NOEXIT
         ENDIF
      ENDIF

   ENDCASE

   RETURN Self

METHOD TBReader( oGet, oMenu, aMsg ) CLASS HBGetList

   LOCAL oTB
   LOCAL nKey
   LOCAL lAutoLite
   LOCAL nSaveCursor
   LOCAL nProcessed
// LOCAL oGUI := oGet:control

   // Read the GET if the WHEN condition is satisfied
   IF ISOBJECT( oGet:control ) .AND. ;
      ::nLastExitState == GE_SHORTCUT .OR. ;
      ::nLastExitState == GE_MOUSEHIT .OR. ;
      ::GetPreValidate( oGet, aMsg )

      ::ShowGetMsg( aMsg )
      ::nLastExitState := 0

      nSaveCursor := SetCursor( SC_NONE )

      // Activate the GET for reading
      oTB := oGet:Control

      lAutoLite := oTB:Autolite
      oTB:Autolite := .T.
      oTB:Hilite()

      IF oGet:exitState == GE_NOEXIT
         IF ::nHitcode == HTCELL
            // Replaces call to TBMouse( oTB, mROW(), mCOL() ):
            oTB:RowPos := oTb:mRowPos
            oTB:ColPos := oTb:mColPos
            oTB:Invalidate()
         ENDIF
      ENDIF

      ::nHitcode := 0

      DO WHILE oGet:exitState == GE_NOEXIT .AND. !::lKillRead

         // Apply keystrokes until exit
         DO WHILE oGet:exitState == GE_NOEXIT .AND. !::lKillRead
            nKey := 0

            DO WHILE !oTB:Stabilize() .AND. nKey == 0
               nKey := Inkey()
            ENDDO

            IF nKey == 0
               nKey := Inkey(0)
            ENDIF

            nProcessed := oTB:ApplyKey( nKey )
            IF nProcessed == TBR_EXIT
               oGet:exitState := GE_ESCAPE
               EXIT

            ELSEIF nProcessed == TBR_EXCEPTION
               ::TBApplyKey( oGet, oTB, nKey, oMenu, aMsg )

               ::ShowGetMsg( aMsg )

            ENDIF

         ENDDO

         // Disallow exit if the VALID condition is not satisfied
         IF ::nLastExitState == GE_SHORTCUT
         ELSEIF ::nLastExitState == GE_MOUSEHIT
         ELSEIF !::GetPostValidate( oGet, aMsg )
            oGet:ExitState := GE_NOEXIT
         ENDIF

      ENDDO

      // De-activate the GET
      oTB:Autolite := lAutoLite
      oTB:DeHilite()

      ::EraseGetMsg( aMsg )

      SetCursor( nSaveCursor )
   ENDIF

   RETURN Self

METHOD Accelerator( nKey, aMsg ) CLASS HBGetList

   LOCAL nGet
   LOCAL oGet
   LOCAL nHotPos
   LOCAL cKey
   LOCAL cCaption
   LOCAL nStart
   LOCAL nEnd
   LOCAL nIteration
   LOCAL lGUI

   IF nKey >= K_ALT_Q .AND. nKey <= K_ALT_P
      cKey := substr( "qwertyuiop", nKey - K_ALT_Q + 1, 1 )

   ELSEIF nKey >= K_ALT_A .AND. nKey <= K_ALT_L
      cKey := substr( "asdfghjkl", nKey - K_ALT_A + 1, 1 )

   ELSEIF nKey >= K_ALT_Z .AND. nKey <= K_ALT_M
      cKey := substr( "zxcvbnm", nKey - K_ALT_Z + 1, 1 )

   ELSEIF nKey >= K_ALT_1 .AND. nKey <= K_ALT_0
      cKey := substr( "1234567890", nKey - K_ALT_1 + 1, 1 )

   ELSE
      RETURN 0

   ENDIF

   nStart := ::nPos + 1
   nEnd   := len( ::aGetList )

   FOR nIteration := 1 TO 2
      FOR nGet := nStart TO nEnd

         oGet  := ::aGetList[ nGet ]

         IF ISOBJECT( oGet:Control ) .AND. ;
            !( oGet:Control:ClassName() == "TBROWSE" )

            cCaption := oGet:Control:Caption
         ELSE
            cCaption := oGet:Caption
         ENDIF

         IF ( nHotPos := at( "&", cCaption ) ) == 0

         ELSEIF nHotPos == len( cCaption )

         ELSEIF lower( substr( cCaption, nHotPos + 1, 1 ) ) == cKey

            // Test the current GUI-GET or Get PostValidation:
            lGUI := ISOBJECT( ::aGetList[ ::nPos ]:Control )

            IF lGUI .AND. !::GUIPostValidate( ::aGetList[ ::nPos ]:Control, aMsg )
               RETURN 0

            ELSEIF !lGUI .AND. !::GetPostValidate( ::aGetList[ ::nPos ], aMsg )
               RETURN 0

            ENDIF
      
            // Test the next GUI-GET or Get PreValidation:
            lGUI := ISOBJECT( oGet:Control )

            IF lGUI .AND. !::GUIPreValidate( oGet:Control, aMsg )
               // RETURN 0  // Commented out.
               RETURN nGet  // Changed.

            ELSEIF !lGUI .AND. !::GetPreValidate( oGet, aMsg )
               // RETURN 0  // Commented out.
               RETURN nGet  // Changed.

            ENDIF

            RETURN nGet
         ENDIF
      NEXT

      nStart := 1
      nEnd   := ::nPos - 1
   NEXT

   RETURN 0

METHOD HitTest( nMouseRow, nMouseCol, aMsg ) CLASS HBGetList

   LOCAL nCount
   LOCAL nTotal
   LOCAL lGUI

   ::nNextGet := 0
   nTotal := Len( ::aGetList )

   FOR nCount := 1 TO nTotal
      IF ( ::nHitCode := ::aGetList[ nCount ]:HitTest( nMouseRow, nMouseCol ) ) != HTNOWHERE
         ::nNextGet := nCount
         EXIT
      ENDIF
   NEXT

   // DO WHILE ::nNextGet != 0  // Commented out.

   IF ::nNextGet != 0  // Changed.

      // Test the current GUI-GET or Get PostValidation:
      lGUI := ISOBJECT( ::aGetList[ ::nPos ]:Control )

      IF lGUI .AND. !::GUIPostValidate( ::aGetList[ ::nPos ]:Control, aMsg )

         ::nNextGet := 0
         // EXIT  // Commented out.
         RETURN 0  // Changed.

      ELSEIF !lGUI .AND. !::GetPostValidate( ::aGetList[ ::nPos ], aMsg )

         ::nNextGet := 0
         // EXIT  // Commented out.
         RETURN 0  // Changed.

      ENDIF
      
      // Test the next GUI-GET or Get PreValidation:
      lGUI := ISOBJECT( ::aGetList[ ::nNextGet ]:Control )

      IF lGUI .AND. !::GUIPreValidate( ::aGetList[ ::nNextGet ]:Control, aMsg )

         ::nNextGet := 0
         // EXIT  // Commented out.
         RETURN ::nNextGet  // Changed.

      ELSEIF !lGUI .AND. !::GetPreValidate( ::aGetList[ ::nNextGet ], aMsg )

         ::nNextGet := 0
         // EXIT  // Commented out.
         RETURN ::nNextGet  // Changed.

      ENDIF

      // EXIT  // Commented out.
      RETURN ::nNextGet  // Changed.
   // ENDDO  // Commented out.

   ENDIF

   // RETURN ::nNextGet != 0  // Commented out.
   RETURN 0

METHOD ShowGetMsg( aMsg ) CLASS HBGetList

#ifdef HB_COMPAT_C53
   LOCAL oGet
   LOCAL cMsg
   LOCAL lMOldState

   IF !Empty( aMsg ) .AND. aMsg[ MSGFLAG ]

      oGet := ::oGet
      cMsg := iif( ISOBJECT( oGet:control ), oGet:control:message, oGet:message )

      IF !Empty( cMsg )
         lMOldState := MSetCursor( .F. )
         DispOutAt( aMsg[ MSGROW ], aMsg[ MSGLEFT ], PadC( cMsg, aMsg[ MSGRIGHT ] - aMsg[ MSGLEFT ] + 1 ), aMsg[ MSGCOLOR ] )
         MSetCursor( lMOldState )
      ENDIF
   ENDIF
#else
   HB_SYMBOL_UNUSED( aMsg )
#endif

   RETURN NIL

METHOD EraseGetMsg( aMsg ) CLASS HBGetList

#ifdef HB_COMPAT_C53
   LOCAL nRow := Row()
   LOCAL nCol := Col()
   LOCAL lMOldState

   IF !Empty( aMsg ) .AND. aMsg[ MSGFLAG ]
      lMOldState := MSetCursor( .F. )
      RestScreen( aMsg[ MSGROW ], aMsg[ MSGLEFT ], aMsg[ MSGROW ], aMsg[ MSGRIGHT ], ::cMsgSaveS )
      MSetCursor( lMOldState )
   ENDIF

   SetPos( nRow, nCol )
#else
   HB_SYMBOL_UNUSED( aMsg )
#endif

   RETURN NIL

#endif

METHOD New( GetList ) CLASS HBGetList

   ::aGetList := GetList

   IF ISARRAY( GetList ) .AND. Len( GetList ) >= 1
      ::oGet := GetList[ 1 ]
   ENDIF

   RETURN Self
