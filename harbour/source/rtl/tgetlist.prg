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
 *    Support for Ca-Clipper 5.3 Getsystem
 *
 * See doc/license.txt for licensing terms.
 *
 */


#include "hbclass.ch"
#include "common.ch"
#include "getexit.ch"
#include "inkey.ch"
#include "setcurs.ch"

#define SCORE_ROW       0
#define SCORE_COL       60

#define _GET_INSERT_ON  7
#define _GET_INSERT_OFF 8
#define _GET_INVD_DATE  9

#define K_UNDO          K_CTRL_U

CLASS HBGetList

   DATA aGetList
   DATA oGet, nPos
   DATA bFormat
   DATA lUpdated
   DATA lKillRead
   DATA lBumpTop, lBumpBot
   DATA nLastExitState
   DATA nLastPos
   DATA oActiveGet
   DATA cReadProcName, nReadProcLine
   DATA cVarName
   DATA lHasFocus

   METHOD New( GetList )
   METHOD Settle( nPos )
   METHOD Reader()
   METHOD GetApplyKey( nKey )
   METHOD GetPreValidate()
   METHOD GetPostValidate()
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
   METHOD ReadExit( lNew ) INLINE Set( _SET_EXIT, lNew )
   METHOD SetFocus()
   METHOD Updated() INLINE ::lUpdated
   #ifdef  HB_COMPAT_C53
   METHOD GUIReader(oget,getsys,a,b)
   METHOD GUIApplyKey(  oGUI, nKey )
   METHOD GuiPreValidate(oGui)
   METHOD GuiPostValidate(oGui)
   #endif
ENDCLASS

METHOD New( GetList ) CLASS HBGetList

   ::aGetList       := GetList
   ::lKillRead      := .f.
   ::lBumpTop       := .f.
   ::lBumpBot       := .f.
   ::nLastExitState := 0
   ::nLastPos       := 0
   ::cReadProcName  := ""
   ::lUpdated       := .f.
   ::nPos           := 1
   ::oGet           := iif( ISARRAY( GetList ) .AND. Len( GetList ) >= 1, GetList[ 1 ], NIL )
   ::lHasFocus      := .F.

return Self

METHOD SetFocus() CLASS HBGetList

   __GetListSetActive( Self )
   ::aGetList[ ::nPos ]:SetFocus()

   return Self
METHOD Reader() CLASS HBGetList

   local oGet := ::oGet

   if ::GetPreValidate()

      oGet:SetFocus()

      while oGet:ExitState == GE_NOEXIT
         if oGet:typeOut
            oGet:ExitState := GE_ENTER
         endif

         while oGet:exitState == GE_NOEXIT
            ::GetApplyKey( Inkey( 0 ) )
         end

         if ! ::GetPostValidate()
            oGet:ExitState := GE_NOEXIT
         endif
      end

      oGet:killFocus()
   endif

return Self

METHOD GetApplyKey( nKey ) CLASS HBGetList

   local cKey, bKeyBlock, oGet := ::oGet

   if ! ( ( bKeyBlock := Setkey( nKey ) ) == NIL )
      ::GetDoSetKey( bKeyBlock )
      return Self
   endif

   do case
      case nKey == K_UP
         oGet:ExitState := GE_UP

      case nKey == K_SH_TAB
         oGet:ExitState := GE_UP

      case nKey == K_DOWN
         oGet:ExitState := GE_DOWN

      case nKey == K_TAB
         oGet:ExitState := GE_DOWN

      case nKey == K_ENTER
         oGet:ExitState := GE_ENTER

      case nKey == K_ESC
         if Set( _SET_ESCAPE )
            oGet:UnDo()
            oGet:ExitState := GE_ESCAPE
         endif

      case nKey == K_PGUP
         oGet:ExitState := GE_WRITE

      case nKey == K_PGDN
         oGet:ExitState := GE_WRITE

      case nKey == K_CTRL_HOME
         oGet:ExitState := GE_TOP

   #ifdef CTRL_END_SPECIAL
      case nKey == K_CTRL_END
         oGet:ExitState := GE_BOTTOM
   #else
      case nKey == K_CTRL_W
         oGet:ExitState := GE_WRITE
   #endif

      case nKey == K_INS
         Set( _SET_INSERT, ! Set( _SET_INSERT ) )
         ::ShowScoreboard()

      case nKey == K_UNDO
         oGet:UnDo()

      case nKey == K_HOME
         oGet:Home()

      case nKey == K_END
         oGet:End()

      case nKey == K_RIGHT
         oGet:Right()

      case nKey == K_LEFT
         oGet:Left()

      case nKey == K_CTRL_RIGHT
         oGet:WordRight()

      case nKey == K_CTRL_LEFT
         oGet:WordLeft()

      case nKey == K_BS
         oGet:BackSpace()

      case nKey == K_DEL
         oGet:Delete()

      case nKey == K_CTRL_T
         oGet:DelWordRight()

      case nKey == K_CTRL_Y
         oGet:DelEnd()

      case nKey == K_CTRL_BS
         oGet:DelWordLeft()

      otherwise

         if nKey >= 32 .and. nKey <= 255
            cKey := Chr( nKey )

            if oGet:type == "N" .and. ( cKey == "." .or. cKey == "," )
               oGet:ToDecPos()
            else
               if Set( _SET_INSERT )
                  oGet:Insert( cKey )
               else
                  oGet:OverStrike( cKey )
               endif

               if oGet:TypeOut
                  if Set( _SET_BELL )
                     ?? Chr( 7 )
                  endif
                  if ! Set( _SET_CONFIRM )
                     oGet:ExitState := GE_ENTER
                  endif
               endif
            endif
         endif
      endcase

return Self

METHOD GetPreValidate() CLASS HBGetList

   local oGet := ::oGet
   local lUpdated, lWhen := .t.

   if oGet:PreBlock != NIL
      lUpdated := ::lUpdated
      lWhen := Eval( oGet:PreBlock, oGet )
      oGet:Display()
      ::ShowScoreBoard()
      ::lUpdated := lUpdated
   endif

   if ::lKillRead
      lWhen := .f.
      oGet:ExitState := GE_ESCAPE
   elseif ! lWhen
      oGet:ExitState := GE_WHEN
   else
      oGet:ExitState := GE_NOEXIT
   end

return lWhen

METHOD GetPostValidate() CLASS HBGetList

   local oGet := ::oGet
   local lUpdated, lValid := .t.

   if oGet:ExitState == GE_ESCAPE
      return .t.
   endif

   if oGet:BadDate()
      oGet:SetFocus()
      ::DateMsg()
      ::ShowScoreboard()
      return .f.
   endif

   if oGet:Changed
      oGet:Assign()
      ::lUpdated := .t.
   endif

   oGet:Reset()

   if oGet:PostBlock != NIL

      lUpdated := ::lUpdated
      SetPos( oGet:Row, oGet:Col + Len( oGet:Buffer ) )
      lValid := Eval( oGet:PostBlock, oGet )
      SetPos( oGet:Row, oGet:Col )
      ::ShowScoreBoard()
      oGet:UpdateBuffer()
      ::lUpdated := lUpdated

      if ::lKillRead
         oGet:ExitState := GE_ESCAPE
         lValid := .t.
      endif
   endif

return lValid

METHOD GetDoSetKey( bKeyBlock ) CLASS HBGetList

   local oGet := ::oGet, lUpdated

   if oGet:Changed
      oGet:Assign()
      ::lUpdated := .t.
   endif

   lUpdated := ::lUpdated

   Eval( bKeyBlock, ::cReadProcName, ::nReadProcLine, ::ReadVar() )

   ::ShowScoreboard()
   oGet:UpdateBuffer()

   ::lUpdated := lUpdated

   if ::lKillRead
      oGet:ExitState := GE_ESCAPE
   endif

return Self

METHOD Settle( nPos ) CLASS HBGetList

   local nExitState

   if nPos == NIL
      nPos := ::nPos
   endif

   if nPos == 0
      nExitState := GE_DOWN
   else
      nExitState := ::aGetList[ nPos ]:ExitState
   endif

   if nExitState == GE_ESCAPE .or. nExitState == GE_WRITE
      return 0
   endif

   if nExitState != GE_WHEN
      ::nLastPos := nPos
      ::lBumpTop := .f.
      ::lBumpBot := .f.
   else
      nExitState := ::nLastExitState
   endif

   do case
      case nExitState == GE_UP
         nPos--

      case nExitState == GE_DOWN
         nPos++

      case nExitState == GE_TOP
         nPos := 1
         ::lBumpTop := .T.
         nExitState := GE_DOWN

      case nExitState == GE_BOTTOM
         nPos := Len( ::aGetList )
         ::lBumpBot := .t.
         nExitState := GE_UP

      case nExitState == GE_ENTER
         nPos++
   endcase

   if nPos == 0
      if ! ::ReadExit() .and. ! ::lBumpBot
         ::lBumpTop := .t.
         nPos       := ::nLastPos
         nExitState := GE_DOWN
      endif

   elseif nPos == Len( ::aGetList ) + 1
      if ! ::ReadExit() .and. nExitState != GE_ENTER .and. ! ::lBumpTop
         ::lBumpBot := .t.
         nPos       := ::nLastPos
         nExitState := GE_UP
      else
         nPos := 0
      endif
   endif

   ::nLastExitState := nExitState

   if nPos != 0
      ::aGetList[ nPos ]:ExitState := nExitState
   endif

return nPos

METHOD PostActiveGet() CLASS HBGetList

   ::GetActive( ::oGet )
   ::ReadVar( ::GetReadVar() )
   ::ShowScoreBoard()

return Self

METHOD GetReadVar() CLASS HBGetList

   local oGet := ::oGet
   local cName := Upper( oGet:Name )
   local n

   if oGet:Subscript != NIL
      for n := 1 TO Len( oGet:Subscript )
         cName += "[" + LTrim( Str( oGet:Subscript[ n ] ) ) + "]"
      next
   end

return cName

METHOD SetFormat( bFormat ) CLASS HBGetList

   local bSavFormat := ::bFormat

   ::bFormat := bFormat

return bSavFormat

METHOD KillRead( lKill ) CLASS HBGetList

   local lSavKill := ::lKillRead

   if PCount() > 0
      ::lKillRead := lKill
   endif

return lSavKill

METHOD GetActive( oGet ) CLASS HBGetList

   local oOldGet := ::oActiveGet

   if PCount() > 0
      ::oActiveGet := oGet
   endif

return oOldGet

METHOD ShowScoreboard() CLASS HBGetList

   local nRow, nCol, nOldCursor

   if Set( _SET_SCOREBOARD )

      nRow := Row()
      nCol := Col()

      nOldCursor := SetCursor( SC_NONE )

      DispOutAt( SCORE_ROW, SCORE_COL, iif( Set( _SET_INSERT ), NationMsg( _GET_INSERT_ON ), NationMsg( _GET_INSERT_OFF ) ) )
      SetPos( nRow, nCol )

      SetCursor( nOldCursor )

   endif

return Self

METHOD DateMsg() CLASS HBGetList

   local nRow
   local nCol

   if Set( _SET_SCOREBOARD )

      nRow := Row()
      nCol := Col()

      DispOutAt( SCORE_ROW, SCORE_COL, NationMsg( _GET_INVD_DATE ) )
      SetPos( nRow, nCol )

      do while NextKey() == 0
      enddo

      DispOutAt( SCORE_ROW, SCORE_COL, Space( Len( NationMsg( _GET_INVD_DATE ) ) ) )
      SetPos( nRow, nCol )

   endif

return Self

METHOD ReadVar( cNewVarName ) CLASS HBGetList

   local cOldName := ::cVarName

   if ISCHARACTER( cNewVarName )
      ::cVarName := cNewVarName
   endif

return cOldName

METHOD ReadUpdated( lUpdated ) CLASS HBGetList

   local lSavUpdated := ::lUpdated

   if PCount() > 0
      ::lUpdated := lUpdated
   endif

return lSavUpdated

#ifdef HB_COMPAT_C53
METHOD GuiReader(oget,getsys,a,b) CLASS HBGetList
//Local oGet := ::oGet
   Local oGui
   IF ( ! ::GUIPreValidate( oGet , oGet:Control ) )
   elseif ( ValType( oGet:Control ) == "O" )

      // Activate the GET for reading
      oGUI := oGet:Control
      oGUI:Select( oGet:VarGet() )
      oGUI:setFocus()


      WHILE ( oGet:exitState == GE_NOEXIT .AND. !::lKillRead )

         // Check for initial typeout (no editable positions)
         IF ( oGui:typeOut )
            oGet:exitState := GE_ENTER
         ENDIF

         // Apply keystrokes until exit
         WHILE ( oGet:exitState == GE_NOEXIT .AND. !::lKillRead )
            ::GUIApplyKey( oGUI,inkey(0))
         ENDDO

         // Disallow exit if the VALID condition is not satisfied

         IF ( !::GUIPostValidate( oGet, oGUI ) )
            oGet:exitState := GE_NOEXIT
         ENDIF
      ENDDO

      // De-activate the GET
      oGet:VarPut( oGUI:Buffer )
      oGUI:killFocus()

      if ( ! oGUI:ClassName() == "LISTBOX" )
      elseif ( ! oGUI:DropDown )
      elseif ( oGUI:IsOpen )
         oGUI:Close()
      endif

   ENDIF

   RETURN Self

METHOD GUIApplyKey(  oGUI, nKey ) CLASS HBGetList
   Local oGet:= ::oGet
   LOCAL cKey
   LOCAL bKeyBlock
   local TheClass
   local nHotItem
   local lClose

   // Check for SET KEY first
   IF !( ( bKeyBlock := setkey( nKey ) ) == NIL )
      ::GetDoSetKey( bKeyBlock, oGet )

   ENDIF


   if ( nKey == 0 )
   elseif ( ( TheClass := oGUI:ClassName() ) == "RADIOGROUP" )
      if  ( nKey == K_UP )
         oGUI:PrevItem()
         nKey := 0

      elseif ( nKey == K_DOWN )
         oGUI:NextItem()
         nKey := 0

      elseif ( ( nHotItem := oGUI:GetAccel( nKey ) ) != 0 )
         oGUI:Select( nHotItem )

      endif

   elseif ( TheClass == "CHECKBOX" )
      if ( nKey == K_SPACE )
         oGUI:Select()

      endif

   elseif ( TheClass == "PUSHBUTTON" )
      if ( nKey == K_SPACE )
         oGUI:Select( K_SPACE )

      elseif ( nKey == K_ENTER )
         oGUI:Select()
         nKey := 0

      endif

   elseif ( TheClass == "LISTBOX" )
      if  ( nKey == K_UP )
         oGUI:PrevItem()
         nKey := 0

      elseif ( nKey == K_DOWN )
         oGUI:NextItem()
         nKey := 0

      elseif ( nKey == K_SPACE )
         if ( ! oGUI:DropDown )
         elseif ( ! oGUI:IsOpen )
            oGUI:Open()
            nKey := 0
         endif


      endif

   endif

   DO CASE
   CASE ( nKey == K_UP )
      oGet:exitState := GE_UP

   CASE ( nKey == K_SH_TAB )
      oGet:exitState := GE_UP

   CASE ( nKey == K_DOWN )
      oGet:exitState := GE_DOWN

   CASE ( nKey == K_TAB )
      oGet:exitState := GE_DOWN

   CASE ( nKey == K_ENTER )
      oGet:exitState := GE_ENTER

   CASE ( nKey == K_ESC )
      IF ( SET( _SET_ESCAPE ) )
         oGet:exitState := GE_ESCAPE
      ENDIF

   CASE ( nKey == K_PGUP )
      oGet:exitState := GE_WRITE

   CASE ( nKey == K_PGDN )
      oGet:exitState := GE_WRITE

   CASE ( nKey == K_CTRL_HOME )
      oGet:exitState := GE_TOP


#ifdef CTRL_END_SPECIAL

   // Both ^W and ^End go to the last GET
   CASE ( nKey == K_CTRL_END )
      oGet:exitState := GE_BOTTOM

#else

   // Both ^W and ^End terminate the READ (the default)
   CASE ( nKey == K_CTRL_W )
      oGet:exitState := GE_WRITE

#endif


      if ( ! lClose )
      elseif ( ! TheClass == "LISTBOX" )
      elseif ( ! oGUI:DropDown )
      elseif ( oGUI:IsOpen )
         oGUI:Close()
         oGUI:Display()
      endif

   ENDCASE

   RETURN Self



METHOD GUIPostValidate( oGUI ) CLASS HBGetList
   Local oGet := ::oGet
   LOCAL lSavUpdated
   LOCAL lValid := .T.
   LOCAL uOldData, uNewData


   IF ( oGet:exitState == GE_ESCAPE )
      RETURN ( .T. )                   // NOTE
   ENDIF


   // If editing occurred, assign the new value to the variable
   IF ( ! ( uOldData == uNewData ) )
      oGet:VarPut( uNewData )
      ::lUpdated := .T.
   ENDIF

   // Check VALID condition if specified
   IF !( oGet:postBlock == NIL )

      lSavUpdated := ::lUpdated

      lValid := EVAL( oGet:postBlock, oGet )

      // Reset S'87 compatibility cursor position
      SETPOS( oGet:row, oGet:col )

      ::ShowScoreBoard()
      IF ( ! ( oGUI:ClassName == "TBROWSE" ) )
         oGUI:Select( oGet:VarGet() )
      ENDIF

      ::lUpdated := lSavUpdated

      IF ( ::lKillRead )
         oGet:exitState := GE_ESCAPE      // Provokes ReadModal() exit
         lValid := .T.

      ENDIF
   ENDIF

   RETURN ( lValid )

METHOD GUIPreValidate(  oGUI ) CLASS HBGetList
   Local oGet := ::oGet
   LOCAL lSavUpdated
   LOCAL lWhen := .T.

   IF !( oGet:preBlock == NIL )
      lSavUpdated := ::lUpdated

      lWhen := EVAL( oGet:preBlock, oGet )


      ::ShowScoreBoard()

      ::lUpdated := lSavUpdated
   ENDIF

   IF (::lKillRead)

      lWhen := .F.
      oGet:exitState := GE_ESCAPE

   ELSEIF ( !lWhen )

      oGet:exitState := GE_WHEN

   ELSE

      oGet:exitState := GE_NOEXIT

   ENDIF

   RETURN (lWhen)

#endif
