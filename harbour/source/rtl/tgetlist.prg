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
#include "button.ch"
#include "tbrowse.ch"

#define SCORE_ROW       0
#define SCORE_COL       60

#define _GET_INSERT_ON  7
#define _GET_INSERT_OFF 8
#define _GET_INVD_DATE  9

#define K_UNDO          K_CTRL_U

CLASS HBGetList

   DATA aGetList
   DATA oGet
   DATA nPos            INIT 1
   DATA bFormat
   DATA lUpdated        INIT .f.
   DATA lKillRead       INIT .f.
   DATA lBumpTop        INIT .f.
   DATA lBumpBot        INIT .f.
   DATA nLastExitState  INIT 0
   DATA nLastPos        INIT 0
   DATA oActiveGet
   DATA cReadProcName   INIT ""
   DATA nReadProcLine
   DATA cVarName
   DATA lHasFocus       INIT .f.
   DATA nHitCode        INIT 0
   DATA nNextGet

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
   METHOD ReadExit( lNew )
   METHOD SetFocus()
   METHOD Updated()

#ifdef HB_COMPAT_C53
   METHOD GUIReader( oGet, GetList, oMenu, aMsg )
   METHOD GUIApplyKey( oGUI, nKey )
   METHOD GUIPreValidate( oGUI )
   METHOD GUIPostValidate( oGUI )
   METHOD TBApplyKey( oGet, oTB,  nKey )
   METHOD TBReader( oGet)
   METHOD Accelerator( nKey )
   METHOD HitTest( nMouseRow, nMouseColumn, aMsg )
#endif

ENDCLASS

METHOD New( GetList ) CLASS HBGetList

   ::aGetList       := GetList
   ::oGet           := iif( ISARRAY( GetList ) .AND. Len( GetList ) >= 1, GetList[ 1 ], NIL )

return Self

METHOD ReadExit( lNew ) CLASS HBGetList
   return iif( ISLOGICAL( lNew ), Set( _SET_EXIT, lNew ), Set( _SET_EXIT ) )

METHOD Updated() CLASS HBGetList
   return ::lUpdated

METHOD SetFocus() CLASS HBGetList

   __GetListSetActive( Self )
   __GetListLast( Self )
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

         if oGet:buffer == NIL
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

   local cKey
   local bKeyBlock
   local oGet := ::oGet
   local nMouseRow
   local nMouseColumn
   local nButton
   local nHotItem

   if ( bKeyBlock := Setkey( nKey ) ) != NIL
      ::GetDoSetKey( bKeyBlock )
      return Self
   endif

   if ::aGetList != NIL .AND. ;
      ( nHotItem := ::Accelerator( nKey ) ) != 0

      oGet:ExitState := GE_SHORTCUT
      ::nNextGet       := nHotItem
      ::nLastExitState := GE_SHORTCUT  // Added.

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
            oGet:Undo()
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

      case nKey == K_LBUTTONDOWN .OR. nKey == K_LDBLCLK
         nMouseRow    := mROW()
         nMouseColumn := mCOL()

         nButton := 0

         if ( nButton := oGet:HitTest( nMouseRow, nMouseColumn ) ) == HTCLIENT

            do while oGet:Col+oGet:Pos-1 > nMouseColumn
               oGet:Left()

               // Handle editing buffer if first character is non-editable:
               if oGet:typeOut
                  // reset typeout:
                  oGet:Home()
                  exit
               endif

            enddo

            do while oGet:Col+oGet:Pos-1 < nMouseColumn
               oGet:Right()

               // Handle editing buffer if last character is non-editable:
               if oGet:typeOut
                  // reset typeout:
                  oGet:End()
                  exit
               endif

            enddo

         elseif nButton != HTNOWHERE

         elseif ::aGetList != NIL .AND. ::HitTest( nMouseRow, nMouseColumn ) != 0   // Changed.
            oGet:ExitState := GE_MOUSEHIT
            ::nLastExitState := GE_MOUSEHIT  // Added.

         else
            oGet:ExitState := GE_NOEXIT

         endif


      case nKey == K_UNDO
         oGet:Undo()

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
   local xValue

   if oGet:PreBlock != NIL

      xValue    := oGet:VarGet()
      lUpdated  := ::lUpdated

      lWhen     := Eval( oGet:PreBlock, oGet )

      if !( ValType( xValue ) == ValType( oGet:VarGet() ) ) .or.;
         !( oGet:VarGet() == xValue )
         oGet:VarPut( oGet:VarGet() )
      else
         oGet:Display()
      endif

      ::ShowScoreBoard()

      ::lUpdated := lUpdated

/*
      if __GetListActive() != Self
         __GetListSetActive( Self )
      endif
*/
      __GetListLast( Self )
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
   local lUpdated
   local lValid := .t.
   local xValue

   if oGet:ExitState == GE_ESCAPE
      return .t.
   endif

   if oGet:BadDate
      oGet:home()
      ::DateMsg()
      ::ShowScoreboard()
      return .f.
   endif

   if oGet:Changed
      oGet:Assign()
      ::lUpdated := .t.
   endif

   oGet:Reset():Display()

   if oGet:PostBlock != NIL

      xValue   := oGet:VarGet()
      lUpdated := ::lUpdated

      SetPos( oGet:Row, oGet:Col + iif( oGet:Buffer == NIL, 0, Len( oGet:Buffer ) ) )
      lValid   := Eval( oGet:PostBlock, oGet )
      SetPos( oGet:Row, oGet:Col )

      if !( ValType( xValue ) == ValType( oGet:VarGet() ) ) .or.;
         !( oGet:VarGet() == xValue )
         oGet:VarPut( oGet:VarGet() )
      endif
      oGet:UpdateBuffer()

      ::ShowScoreBoard()
      
      ::lUpdated := lUpdated

/*
      if __GetListActive() != Self
         __GetListSetActive( Self )
      endif
*/
      __GetListLast( Self )

      if ::lKillRead
         oGet:ExitState := GE_ESCAPE
         lValid := .t.
      endif
   endif

return lValid

METHOD GetDoSetKey( bKeyBlock ) CLASS HBGetList

   local oGet := ::oGet, lUpdated, xValue

   if oGet:Changed
      oGet:Assign()
      ::lUpdated := .t.
   endif

   xValue   := oGet:VarGet()
   lUpdated := ::lUpdated

   Eval( bKeyBlock, ::cReadProcName, ::nReadProcLine, ::ReadVar() )

   if !( ValType( xValue ) == ValType( oGet:VarGet() ) ) .or.;
      !( oGet:VarGet() == xValue )
      oGet:VarPut( oGet:VarGet() )
   endif

   ::ShowScoreboard()
   oGet:UpdateBuffer()

   ::lUpdated := lUpdated

/*
   if __GetListActive() != Self
      __GetListSetActive( Self )
   endif
*/
   __GetListLast( Self )

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
      if ::nLastExitState != 0
         nExitState := ::nLastExitState
      elseif ::nNextGet < ::nLastPos 
         nExitState := GE_UP
      else
         nExitState := GE_DOWN
      endif

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

      case nExitState == GE_SHORTCUT 
         return ::nNextGet

      case nExitState == GE_MOUSEHIT
         return ::nNextGet

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

METHOD GuiReader( oGet, GetList, oMenu, aMsg ) CLASS HBGetList
   Local oGUI

   HB_SYMBOL_UNUSED( GetList )
   HB_SYMBOL_UNUSED( oMenu )
   HB_SYMBOL_UNUSED( aMsg )

   if ! ::GUIPreValidate( oGet:Control )

   elseif ISOBJECT( oGet:Control )

      // Activate the GET for reading
      oGUI := oGet:Control
      oGUI:Select( oGet:VarGet() )
      oGUI:setFocus()

      do while oGet:exitState == GE_NOEXIT .AND. !::lKillRead

         // Check for initial typeout (no editable positions)
         if oGUI:typeOut
            oGet:exitState := GE_ENTER
         endif

         // Apply keystrokes until exit
         do while oGet:exitState == GE_NOEXIT .AND. !::lKillRead
            ::GUIApplyKey( oGUI, inkey(0) )
         enddo

         // Disallow exit if the VALID condition is not satisfied

         if !::GUIPostValidate( oGUI )
            oGet:exitState := GE_NOEXIT
         endif
      enddo

         // De-activate the GET
      oGet:VarPut( oGUI:Buffer )
      oGUI:killFocus()

      if !( oGUI:ClassName() == "LISTBOX" )
      elseif ! oGUI:DropDown
      elseif oGUI:IsOpen
         oGUI:Close()
      endif

   endif

   return Self

METHOD GUIApplyKey( oGUI, nKey ) CLASS HBGetList
   Local oGet := ::oGet
   Local bKeyBlock
   Local oTheClass
   Local nHotItem
   Local lClose
   Local nMouseRow, nMouseColumn, nButton

   // Check for SET KEY first
   if ( bKeyBlock := SetKey( nKey ) ) != NIL
      ::GetDoSetKey( bKeyBlock, oGet )
   endif

   if ( nHotItem := ::Accelerator( nKey ) ) != 0
      oGet:ExitState := GE_SHORTCUT
      ::nNextGet := nHotItem
   endif 

   if nKey == 0
   elseif ( oTheClass := oGUI:ClassName() ) == "RADIOGROUP"
      if nKey == K_UP
         oGUI:PrevItem()
         nKey := 0

      elseif nKey == K_DOWN
         oGUI:NextItem()
         nKey := 0

      elseif ( nHotItem := oGUI:GetAccel( nKey ) ) != 0
         oGUI:Select( nHotItem )

      endif

      if ISNUMBER( oGet:VarGet() )
         oGet:VarPut( oGUI:Value )
      endif

   elseif oTheClass == "CHECKBOX"
      if nKey == K_SPACE
         oGUI:Select()
      endif

   elseif oTheClass == "PUSHBUTTON"
      if nKey == K_SPACE
         oGUI:Select( K_SPACE )

      elseif nKey == K_ENTER
         oGUI:Select()
         nKey := 0

      endif

   elseif oTheClass == "LISTBOX"
      if nKey == K_UP
         oGUI:PrevItem()
         nKey := 0

      elseif nKey == K_DOWN
         oGUI:NextItem()
         nKey := 0

      elseif nKey == K_SPACE
         if ! oGUI:DropDown
         elseif ! oGUI:IsOpen
            oGUI:Open()
            nKey := 0
         endif

      elseif ( nButton := oGUI:FindText( chr(nKey), oGUI:Value+1, .f., .f. ) ) != 0
         oGUI:Select( nButton )

      endif

      if ISNUMBER( oGet:VarGet() )
         oGet:VarPut( oGUI:Value )

      endif

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
      if set( _SET_ESCAPE )
         oGet:ExitState := GE_ESCAPE
      endif

   case nKey == K_PGUP
      oGet:ExitState := GE_WRITE

   case nKey == K_PGDN
      oGet:ExitState := GE_WRITE

   case nKey == K_CTRL_HOME
      oGet:ExitState := GE_TOP


#ifdef CTRL_END_SPECIAL

   // Both ^W and ^End go to the last GET
   case nKey == K_CTRL_END
      oGet:ExitState := GE_BOTTOM

#else

   // Both ^W and ^End terminate the READ (the default)
   case nKey == K_CTRL_W
      oGet:ExitState := GE_WRITE

#endif

   case nKey == K_LBUTTONDOWN .or. nKey == K_LDBLCLK
      nMouseRow    := mROW()
      nMouseColumn := mCOL()
      lClose := .T.
      nButton:=0

      if ( nButton := oGUI:HitTest( nMouseRow, nMouseColumn ) ) == HTNOWHERE
         // Changed test:
         if ::HitTest( nMouseRow, nMouseColumn  ) != 0
            oGet:ExitState := GE_MOUSEHIT
            ::nLastExitState := GE_MOUSEHIT  // Added.
         else
            oGet:ExitState := GE_NOEXIT
         endif

      elseif nButton >= HTCLIENT
         oGUI:Select( nButton )

      elseif nButton == HTDROPBUTTON
         if !oGUI:IsOpen
            oGUI:Open()
            lClose := .F.
         endif

      elseif nButton >= HTSCROLLFIRST .and. nButton <= HTSCROLLLAST
         oGUI:Scroll( nButton )
         lClose := .F.

      endif

      if ! lClose
      elseif ! oTheClass == "LISTBOX"
      elseif ! oGUI:DropDown
      elseif oGUI:IsOpen
         oGUI:Close()
         oGUI:Display()
      endif

   endcase

   return Self

METHOD TBApplyKey( oGet, oTB, nKey, aMsg ) CLASS HBGetList

   Local bKeyBlock
   Local nMouseRow, nMouseColumn
   Local nButton
   Local nHotItem
   Local lSetKey

   // Check for SET KEY first
   if ( bKeyBlock := SETKEY( nKey ) ) != NIL
      if lSetKey := ::GetDoSetKey( bKeyBlock, oGet )
         return Self
      endif
   endif

   if ( nHotItem := ::Accelerator(  nKey) ) != 0
      oGet:ExitState := GE_SHORTCUT
   endif

   do case
   case nKey == K_TAB 
      oGet:ExitState := GE_DOWN

   case nKey == K_SH_TAB
      oGet:ExitState := GE_UP

   case nKey == K_ENTER
      if !oTb:Stable()
         oTb:ForceStable()
      endif  
      oGet:ExitState := GE_ENTER

   case nKey == K_ESC 
      if set( _SET_ESCAPE )
         oGet:ExitState := GE_ESCAPE
      endif

#ifdef CTRL_END_SPECIAL

   // Both ^W and ^End go to the last GET
   case nKey == K_CTRL_END
      oGet:ExitState := GE_BOTTOM

#else

   // Both ^W and ^End terminate the READ (the default)
   case nKey == K_CTRL_W
      oGet:ExitState := GE_WRITE

#endif

   case nKey == K_LBUTTONDOWN .or. nKey == K_LDBLCLK
      nMouseRow    := mROW()
      nMouseColumn := mCOL()
      nButton      := 0

      if ( nButton := oTB:HitTest( nMouseRow, nMouseColumn ) ) == HTNOWHERE // Changed test:
         if ::HitTest(  nMouseRow, nMouseColumn, aMsg ) != 0
            oGet:ExitState := GE_MOUSEHIT
         else
            oGet:ExitState := GE_NOEXIT
         endif
      endif

   endcase

   return self


METHOD GUIPostValidate( oGUI ) CLASS HBGetList
   Local oGet := ::oGet
   Local lUpdated
   Local lValid := .T.
   Local xValue

   if oGet:exitState == GE_ESCAPE
      return .t.                   // NOTE
   endif

   if oGet:BadDate
      oGet:home()
      ::DateMsg()
      ::ShowScoreboard()
      return .f.
   endif

   if oGet:Changed
      oGet:UpdateBuffer()
      ::lUpdated := .t.
   endif

   oGet:Reset():Display()

/*
   // If editing occurred, assign the new value to the variable
   if !( uOldData == uNewData )
      oGet:VarPut( uNewData )
      ::lUpdated := .T.
   endif
*/

   // Check VALID condition if specified
   if oGet:postBlock != NIL

      xValue   := oGet:VarGet()
      lUpdated := ::lUpdated

      lValid := Eval( oGet:postBlock, oGet )

      // Reset S'87 compatibility cursor position
      setpos( oGet:Row, oGet:Col )

      if !( ValType( xValue ) == ValType( oGet:VarGet() ) ) .or.;
         !( oGet:VarGet() == xValue )
         oGet:VarPut( oGet:VarGet() )
      endif
      oGet:UpdateBuffer()

      ::ShowScoreBoard()
      if ! ( oGUI:ClassName == "TBROWSE" )
         oGUI:Select( oGet:VarGet() )
      endif

      ::lUpdated := lUpdated

/*
      if __GetListActive() != Self
         __GetListSetActive( Self )
      endif
*/
      __GetListLast( Self )

      if ::lKillRead
         oGet:ExitState := GE_ESCAPE      // Provokes ReadModal() exit
         lValid := .T.
      endif

   endif

   return  lValid 

METHOD GUIPreValidate( oGUI ) CLASS HBGetList
   Local oGet := ::oGet
   Local lUpdated
   Local lWhen := .T.
   Local xValue

   HB_SYMBOL_UNUSED( oGUI )

   if oGet:preBlock != NIL

      xValue   := oGet:VarGet()
      lUpdated := ::lUpdated

      lWhen := eval( oGet:preBlock, oGet )

      if !( ValType( xValue ) == ValType( oGet:VarGet() ) ) .or.;
         !( oGet:VarGet() == xValue )
         oGet:VarPut( oGet:VarGet() )
      else
         oGet:Display()
      endif

      ::ShowScoreBoard()

      ::lUpdated := lUpdated

/*
      if __GetListActive() != Self
         __GetListSetActive( Self )
      endif
*/
      __GetListLast( Self )
   endif

   if ::lKillRead
      lWhen := .F.
      oGet:ExitState := GE_ESCAPE

   elseif !lWhen
      oGet:ExitState := GE_WHEN

   else
      oGet:ExitState := GE_NOEXIT

   endif

   return lWhen

METHOD TBReader( oGet, oGetsys, aMsg ) CLASS HBGetList
   Local oTB, nKey, lAutoLite, nSaveCursor, nProcessed
//   Local oGUI := oGet:control

   HB_SYMBOL_UNUSED( oGetsys )

   // Read the GET if the WHEN condition is satisfied
   if ISOBJECT( oGet:control ) .AND. ;    // Moved up 2 lines.
      ::nLastExitState == GE_SHORTCUT .OR. ;  // Added.
      ::nLastExitState == GE_MOUSEHIT .OR. ;  // Added.
      ::GetPreValidate( oGet, aMsg )

//      ShowGetMsg( oGet, aMsg )
      ::nLastExitState := 0  // Added.

      nSaveCursor := SetCursor( SC_NONE )

      // Activate the GET for reading
      oTB := oGet:Control

      lAutoLite := oTB:Autolite
      oTB:Autolite := .T.
      oTB:Hilite()

      if oGet:exitState == GE_NOEXIT  // Added.
         if ::nHitcode == HTCELL
            //   tracelog('hitcode ',::nHitcode )
            // Replaces call to TBMouse( oTB, mROW(), mCOL() ):
            oTB:RowPos := oTb:mRowPos
            oTB:ColPos := oTb:mColPos
            oTB:Invalidate()
         endif
      endif  // Added.

      ::nHitcode := 0

      do while oGet:exitState == GE_NOEXIT .AND. !::lKillRead

         // Apply keystrokes until exit
         do while oGet:exitState == GE_NOEXIT .AND. !::lKillRead
            nKey := 0

            do while !oTB:Stabilize() .and. nKey == 0
               nKey := Inkey()
            enddo

            if nKey == 0
               nKey := Inkey(0)
            endif

            nProcessed := oTB:ApplyKey( nKey )
            if nProcessed == TBR_EXIT
               oGet:exitState := GE_ESCAPE
               exit

            elseif nProcessed == TBR_EXCEPTION
               ::TBApplyKey( oGet, oTB,  nKey, aMsg )

               // nRow := ROW()  // Commented out.
               // nCol := COL()  // Commented out.
               // ShowGetMsg( oGet, aMsg )
               // SetPos( nRow, nCol )  // Commented out.

            endif

         enddo

         // Disallow exit if the VALID condition is not satisfied
         if ::nLastExitState == GE_SHORTCUT       // Added.
         elseif ::nLastExitState == GE_MOUSEHIT   // Added.
         elseif !::GetPostValidate( oGet, aMsg )  // Changed.
         // if !::GUIPostValidate( oGet, oGUI, aMsg ) // Old test.
            oGet:ExitState := GE_NOEXIT
         endif

      enddo

      // De-activate the GET
      oTB:Autolite := lAutoLite
      oTB:DeHilite()

      SetCursor( nSaveCursor )
   endif

   return Self

METHOD Accelerator( nKey ) CLASS HBGetList

   Local nGet, oGet, nHotPos, cKey, cCaption, nStart, nEnd
   Local nIteration, lGUI

   if nKey >= K_ALT_Q .and. nKey <= K_ALT_P
      cKey := substr( "qwertyuiop", nKey - K_ALT_Q + 1, 1 )

   elseif nKey >= K_ALT_A .and. nKey <= K_ALT_L
      cKey := substr( "asdfghjkl", nKey - K_ALT_A + 1, 1 )

   elseif nKey >= K_ALT_Z .and. nKey <= K_ALT_M
      cKey := substr( "zxcvbnm", nKey - K_ALT_Z + 1, 1 )

   elseif nKey >= K_ALT_1 .and. nKey <= K_ALT_0
      cKey := substr( "1234567890", nKey - K_ALT_1 + 1, 1 )

   else
      return 0

   endif

   nStart := ::nPos + 1
   nEnd   := len( ::aGetList )

   for nIteration := 1 to 2
       for nGet := nStart to nEnd

          oGet  := ::aGetList[ nGet ]

          if ISOBJECT( oGet:Control ) .and. ;
             !( oGet:Control:ClassName() == "TBROWSE" )

             cCaption := oGet:Control:Caption
          else
             cCaption := oGet:Caption
          endif

          if ( nHotPos := at( "&", cCaption ) ) == 0

          elseif nHotPos == len( cCaption )

          elseif lower( substr( cCaption, nHotPos + 1, 1 ) ) == cKey

             // Test the current GUI-GET or Get PostValidation:
             lGUI := ISOBJECT( ::aGetList[ ::nPos ]:Control )

             if lGUI .and. !::GUIPostValidate( ::aGetList[ ::nPos ]:Control )
                return 0

             elseif !lGUI .and. !::GetPostValidate( ::aGetList[ ::nPos ] )
                return 0

             endif
      
             // Test the next GUI-GET or Get PreValidation:
             lGUI := ISOBJECT( oGet:Control )

             if lGUI .and. !::GUIPreValidate( oGet:Control )
                // return 0  // Commented out.
                return nGet  // Changed.

             elseif !lGUI .and. !::GetPreValidate( oGet )
                // return 0  // Commented out.
                return nGet  // Changed.

             endif

             return ( nGet )
          endif
       next

       nStart := 1
       nEnd   := ::nPos - 1

   next

   return  0

METHOD HitTest( nMouseRow, nMouseCol, aMsg ) CLASS HBGetList
   Local nCount, nTotal, lGUI

   ::nNextGet := 0
   nTotal  := len( ::aGetList )

   for nCount := 1 to nTotal
      if ( ::nHitCode := ::aGetList[ nCount ]:HitTest( nMouseRow, nMouseCol ) ) != HTNOWHERE
         ::nNextGet := nCount
         exit
      endif
   next

   // do while ::nNextGet != 0  // Commented out.

   if ::nNextGet != 0  // Changed.

      // Test the current GUI-GET or Get PostValidation:
      lGUI := ISOBJECT( ::aGetList[ ::nPos ]:Control )

      if lGUI .and. !::GUIPostValidate( ::aGetList[ ::nPos ]:Control, aMsg )

         ::nNextGet := 0
         // exit  // Commented out.
         return 0  // Changed.

      elseif !lGUI .and. !::GetPostValidate( ::aGetList[ ::nPos ], aMsg )

         ::nNextGet := 0
         // exit  // Commented out.
         return 0  // Changed.

      endif
      
      // Test the next GUI-GET or Get PreValidation:
      lGUI := ISOBJECT( ::aGetList[ ::nNextGet ]:Control )

      if lGUI .and. !::GUIPreValidate( ::aGetList[ ::nNextGet ]:Control, aMsg )

         ::nNextGet := 0
         // exit  // Commented out.
         return ::nNextGet  // Changed.

      elseif !lGUI .and. !::GetPreValidate( ::aGetList[ ::nNextGet ], aMsg )

         ::nNextGet := 0
         // exit  // Commented out.
         return ::nNextGet  // Changed.

      endif

      // exit  // Commented out.
      return ::nNextGet  // Changed.
   // enddo  // Commented out.

   endif  // Changed.

   // return ::nNextGet != 0  // Commented out.
   return 0  // Changed.

#endif
