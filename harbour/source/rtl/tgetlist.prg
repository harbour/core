/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * TGetList Class
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#include "hbclass.ch"
#include "common.ch"
#include "getexit.ch"
#include "inkey.ch"
#include "setcurs.ch"

#define SCORE_ROW       0
#define SCORE_COL       60

#define K_UNDO   K_CTRL_U

CLASS TGetList

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

ENDCLASS

METHOD New( GetList ) CLASS TGetList

   ::aGetList       := GetList
   ::lKillRead      := .f.
   ::lBumpTop       := .f.
   ::lBumpBot       := .f.
   ::nLastExitState := 0
   ::nLastPos       := 0
   ::cReadProcName  := ""
   ::lUpdated       := .f.
   ::nPos           := 1
   ::oGet           := GetList[ 1 ]
   ::lHasFocus      := .F.

return Self

METHOD SetFocus() CLASS TGetList

   __GetListSetActive( Self )
   ::aGetList[ ::nPos ]:SetFocus()

   return Self

METHOD Reader() CLASS TGetList

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

METHOD GetApplyKey( nKey ) CLASS TGetList

   local cKey, bKeyBlock, oGet := ::oGet

   if ! ( ( bKeyBlock := Setkey( nKey ) ) == nil )
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

METHOD GetPreValidate() CLASS TGetList

   local oGet := ::oGet
   local lUpdated, lWhen := .t.

   if oGet:PreBlock != nil
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

METHOD GetPostValidate() CLASS TGetList

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

   if oGet:PostBlock != nil

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

METHOD GetDoSetKey( bKeyBlock ) CLASS TGetList

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

METHOD Settle( nPos ) CLASS TGetList

   local nExitState

   if nPos == nil
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

METHOD PostActiveGet() CLASS TGetList

   ::GetActive( ::oGet )
   ::ReadVar( ::GetReadVar() )
   ::ShowScoreBoard()

return Self

METHOD GetReadVar() CLASS TGetList

   local oGet := ::oGet
   local cName := Upper( oGet:Name )
   local n

   if oGet:Subscript != nil
      for n := 1 TO Len( oGet:Subscript )
         cName += "[" + LTrim( Str( oGet:Subscript[ n ] ) ) + "]"
      next
   end

return cName

METHOD SetFormat( bFormat ) CLASS TGetList

   local bSavFormat := ::bFormat

   ::bFormat := bFormat

return bSavFormat

METHOD KillRead( lKill ) CLASS TGetList

   local lSavKill := ::lKillRead

   if PCount() > 0
      ::lKillRead := lKill
   endif

return lSavKill

METHOD GetActive( oGet ) CLASS TGetList

   local oOldGet := ::oActiveGet

   if PCount() > 0
      ::oActiveGet := oGet
   endif

return oOldGet

METHOD ShowScoreboard() CLASS TGetList

   local nRow, nCol, nOldCursor

   if Set( _SET_SCOREBOARD )
      nRow := Row()
      nCol := Col()
      nOldCursor := SetCursor( SC_NONE )
      DispOutAt( SCORE_ROW, SCORE_COL, If( Set( _SET_INSERT ), "Ins", "   " ) )
      SetPos( nRow, nCol )
      SetCursor( nOldCursor )
   endif

return Self

METHOD DateMsg() CLASS TGetList

   local nRow
   local nCol

   if Set( _SET_SCOREBOARD )

      nRow := Row()
      nCol := Col()

      DispOutAt( SCORE_ROW, SCORE_COL, "Invalid date" )
      SetPos( nRow, nCol )

      do while NextKey() == 0
      enddo

      DispOutAt( SCORE_ROW, SCORE_COL, Space( Len( "Invalid date" ) ) )
      SetPos( nRow, nCol )

   endif

return Self

METHOD ReadVar( cNewVarName ) CLASS TGetList

   local cOldName := ::cVarName

   if ISCHARACTER( cNewVarName )
      ::cVarName := cNewVarName
   endif

return cOldName

METHOD ReadUpdated( lUpdated ) CLASS TGetList

   local lSavUpdated := ::lUpdated

   if PCount() > 0
      ::lUpdated := lUpdated
   endif

return lSavUpdated
