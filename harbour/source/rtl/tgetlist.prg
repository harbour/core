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
   Data nHitcode
   Data nNextGet
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
#ifdef HB_COMPAT_C53

   METHOD GUIReader(oget,getsys,a,b)
   METHOD GUIApplyKey( oGUI, nKey )
   METHOD GUIPreValidate(oGui)
   METHOD GUIPostValidate(oGui)
   METHOD TBApplyKey( oGet, oTB,  nKey)
   method TBReader( oGet)
   Method Accelerator(  nKey ) // Removed STATIC
   method HitTest( MouseRow, MouseCol, aMsg ) // Removed STATIC
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
   ::nHitcode:=0

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

   local cKey, bKeyBlock, oGet := ::oGet
   LOCAL MouseRow, MouseColumn
   LOCAL nButton
   LOCAL nHotItem

   if ! ( ( bKeyBlock := Setkey( nKey ) ) == NIL )
      ::GetDoSetKey( bKeyBlock )
      return Self
   endif
   IF ( !( ::aGetList == NIL ) .AND. ;
    ( ( nHotItem := ::Accelerator( nKey ) ) != 0 ) )
      oGet:ExitState := GE_SHORTCUT
      oGet:NnEXTgET  := nHotItem
      oGet:nLastExitState := GE_SHORTCUT  // Added.

   ENDIF

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

   CASE( ( nKey == K_LBUTTONDOWN ) .OR. ( nKey == K_LDBLCLK ) )
      MouseRow    := mROW()
      MouseColumn := mCOL()

	 nButton := 0
      IF ( nButton != 0 )

      ELSEIF ( ( nButton := ;
	 oGet:HitTest( MouseRow, MouseColumn ) ) == HTCLIENT )

	 DO WHILE ( oGet:Col + oGet:Pos - 1 > MouseColumn )
	    oGet:Left()

	    // Handle editing buffer if first character is non-editable:
	    IF oGet:typeOut
	       // reset typeout:
	       oGet:Home()
	       EXIT
	    ENDIF

	 ENDDO

	 DO WHILE ( oGet:Col + oGet:Pos - 1 < MouseColumn )
	    oGet:Right()

	    // Handle editing buffer if last character is non-editable:
	    IF oGet:typeOut
	       // reset typeout:
	       oGet:End()
	       EXIT
	    ENDIF

	 ENDDO

      ELSEIF !( nButton == HTNOWHERE )
      ELSEIF ( !( ::aGetList == NIL ) .AND. ;
     ::HitTest( MouseRow, MouseColumn, ) != 0 )  // Changed.
	 oGet:exitstate := GE_MOUSEHIT
     oget:nLastExitState := GE_MOUSEHIT  // Added.
      ELSE
	 oGet:exitstate := GE_NOEXIT
      ENDIF


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
      oGet:type := ValType( oGet:VarGet() )
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
//      oGet:SetFocus()
      oGet:TypeOut := .f.
      ::DateMsg()
      ::ShowScoreboard()
      return .f.
   endif

   if oGet:Changed
      oGet:UpdateBuffer()
      ::lUpdated := .t.
   endif

   oGet:Reset():Display()

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
      IF ::nLastExitState != 0
     nExitState := ::nLastExitState
      ELSEIF ::nNextGet < ::nLastPos 
	 nExitState := GE_UP
      ELSE
	 nExitState := GE_DOWN
      ENDIF

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
   CASE( nExitState == GE_SHORTCUT )
      RETURN ( ::nNextGet )

   CASE( nExitState == GE_MOUSEHIT )
      RETURN ( ::nNextGet )

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
   LOCAL MouseRow, MouseColumn,nButton

   // Check for SET KEY first
   IF !( ( bKeyBlock := setkey( nKey ) ) == NIL )
      ::GetDoSetKey( bKeyBlock, oGet )

   ENDIF
   IF ( ( nHotItem := ::Accelerator( nKey ) ) != 0 )
      oGet:ExitState := GE_SHORTCUT
      ::nNextGet := nHotItem
   endif 

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

      IF VALTYPE( oGet:VarGet() ) == "N"
	 oGet:VarPut( oGui:Value )
      ENDIF

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
      ELSEIF ( ( nButton := oGUI:FindText( CHR( nKey ), oGUI:Value + 1, ;
					   .F., .F. ) ) != 0 )
	 oGUI:Select( nButton )


      endif
      IF VALTYPE( oGet:VarGet() ) == "N"
	 oGet:VarPut( oGui:Value )
      ENDIF

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
   CASE( ( nKey == K_LBUTTONDOWN ) .OR. ( nKey == K_LDBLCLK ) )
      MouseRow    := mROW()
      MouseColumn := mCOL()
      nButton:=0
      lClose := .T.

      IF ( nButton != 0 )
      ELSEIF ( ( nButton := ;
	 oGUI:HitTest( MouseRow, MouseColumn ) ) == HTNOWHERE )
	 // Changed test:
     IF ( ::HitTest( MouseRow, MouseColumn  ) != 0 )
	    oGet:exitstate := GE_MOUSEHIT
        ::nLastExitState := GE_MOUSEHIT  // Added.
	 ELSE
	    oGet:exitstate := GE_NOEXIT
	 ENDIF

      ELSEIF ( nButton >= HTCLIENT )
	 oGUI:Select( nButton )

      ELSEIF ( nButton == HTDROPBUTTON )
	 IF ( !oGUI:IsOpen )
	    oGUI:Open()
	    lClose := .F.

	 ENDIF

      ELSEIF ( ( nButton >= HTSCROLLFIRST ) .AND. ;
	       ( nButton <= HTSCROLLLAST ) )
	 oGUI:Scroll( nButton )
	 lClose := .F.

      ENDIF

      if ( ! lClose )
      elseif ( ! TheClass == "LISTBOX" )
      elseif ( ! oGUI:DropDown )
      elseif ( oGUI:IsOpen )
         oGUI:Close()
         oGUI:Display()
      endif

   ENDCASE

   RETURN Self
METHOD TBApplyKey( oGet, oTB, nKey, aMsg ) CLASS HBGETLIST

   LOCAL cKey
   LOCAL bKeyBlock
   LOCAL MouseRow, MouseColumn
   LOCAL nButton
   LOCAL nHotItem
   LOCAL lSetKey

   // Check for SET KEY first
   IF !( ( bKeyBlock := SETKEY( nKey ) ) == NIL )
      IF ( lSetKey := ::GetDoSetKey( bKeyBlock, oGet ) )
     RETURN self
      ENDIF
   ENDIF

   IF ( ( nHotItem := ::Accelerator(  nKey) ) != 0 )
      oGet:ExitState := GE_SHORTCUT


   ENDIF

   DO CASE
   CASE( nKey == K_TAB )
      oGet:exitState := GE_DOWN

   CASE( nKey == K_SH_TAB )
      oGet:exitState := GE_UP

   CASE( nKey == K_ENTER )
      IF !oTb:Stable()
         oTb:ForceStable()
      endif  
      oGet:exitState := GE_ENTER

   CASE( nKey == K_ESC )
      IF ( SET( _SET_ESCAPE ) )
	 oGet:exitState := GE_ESCAPE
      ENDIF

#ifdef CTRL_END_SPECIAL

   // Both ^W and ^End go to the last GET
   CASE( nKey == K_CTRL_END )
      oGet:exitState := GE_BOTTOM

#else

   // Both ^W and ^End terminate the READ (the default)
   CASE( nKey == K_CTRL_W )
      oGet:exitState := GE_WRITE

#endif
   CASE( ( nKey == K_LBUTTONDOWN ) .OR. ( nKey == K_LDBLCLK ) )
      MouseRow    := mROW()
      MouseColumn := mCOL()
      nButton:=0

      IF ( nButton != 0 )
      ELSEIF ( ( nButton := ;
	 oTB:HitTest( MouseRow, MouseColumn ) ) == HTNOWHERE )
	 // Changed test:
     IF ( ::HitTest(  MouseRow, MouseColumn, aMsg ) != 0 )
	    oGet:exitstate := GE_MOUSEHIT

	 ELSE
	    oGet:exitstate := GE_NOEXIT
	 ENDIF
      ENDIF
   ENDCASE

   RETURN self




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
method TBReader( oGet,oGetsys,  aMsg ) Class HBGETLIST
   LOCAL oTB, nKey, lAutoLite, nCell, nSaveCursor, nProcessed
   LOCAL nRow, nCol
//   local oGui:=oGet:control
   // Read the GET if the WHEN condition is satisfied
   IF ( VALTYPE( oGet:control ) == "O" ) .AND. ;  // Moved up 2 lines.
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

      IF oGet:exitState == GE_NOEXIT  // Added.
     IF ( ::nHitcode == HTCELL )
     tracelog('hitcode ',::nHitcode )
	    // Replaces call to TBMouse( oTB, mROW(), mCOL() ):
	    oTB:RowPos := oTb:mRowPos
	    oTB:ColPos := oTb:mColPos
	    oTB:Invalidate()
	 ENDIF
      ENDIF  // Added.

      ::nHitcode := 0

     WHILE ( oGet:exitState == GE_NOEXIT .AND. !::lKillRead )

	 // Apply keystrokes until exit
     WHILE ( oGet:exitState == GE_NOEXIT .AND. !::lKillRead )
	    nKey := 0

	    WHILE ( !( oTB:Stabilize() ) .AND. ( nKey == 0 ) )
           nKey := Inkey()
	    ENDDO

	    IF ( nKey == 0 )
           nKey := Inkey(0)
	    ENDIF

	    nProcessed := oTB:ApplyKey( nKey )
	    IF ( nProcessed == TBR_EXIT )
	       oGet:exitState := GE_ESCAPE
	       EXIT

	    ELSEIF ( nProcessed == TBR_EXCEPTION )
           ::TBApplyKey( oGet, oTB,  nKey, aMsg )
	       // nRow := ROW()  // Commented out.
	       // nCol := COL()  // Commented out.
//           ShowGetMsg( oGet, aMsg )
	       // DevPos( nRow, nCol )  // Commented out.

	    ENDIF

	 ENDDO

	 // Disallow exit if the VALID condition is not satisfied
     IF     ::nLastExitState  == GE_SHORTCUT  // Added.
     ELSEIF ::nLastExitState == GE_MOUSEHIT  // Added.
     ELSEIF ( !::GetPostValidate( oGet, aMsg ) )      // Changed.
     // IF ( !::GUIPostValidate( oGet, oGUI, aMsg ) ) // Old test.
	    oGet:exitState := GE_NOEXIT
	 ENDIF

      ENDDO

      // De-activate the GET
      oTB:Autolite := lAutoLite
      oTB:DeHilite()

      SetCursor( nSaveCursor )
   ENDIF

   RETURN self

METHOD Accelerator( nKey) CLASS HBGETLIST// Removed STATIC

   LOCAL nGet, oGet, nHotPos, cKey, cCaption, nStart, nEnd
   LOCAL nIteration, lGUI

      IF ( ( nKey >= K_ALT_Q ) .AND. ( nKey <= K_ALT_P ) )
	 cKey := SUBSTR( "qwertyuiop", nKey - K_ALT_Q + 1, 1 )

      ELSEIF ( ( nKey >= K_ALT_A ) .AND. ( nKey <= K_ALT_L ) )
	 cKey := SUBSTR( "asdfghjkl", nKey - K_ALT_A + 1, 1 )

      ELSEIF ( ( nKey >= K_ALT_Z ) .AND. ( nKey <= K_ALT_M ) )
	 cKey := SUBSTR( "zxcvbnm", nKey - K_ALT_Z + 1, 1 )

      ELSEIF ( ( nKey >= K_ALT_1 ) .AND. ( nKey <= K_ALT_0 ) )
	 cKey := SUBSTR( "1234567890", nKey - K_ALT_1 + 1, 1 )

      ELSE
	 RETURN ( 0 )

      ENDIF

      nStart := ::nPos + 1
      nEnd   := LEN( ::aGetList )

      FOR nIteration := 1 TO 2
	 FOR nGet := nStart TO nEnd
        oGet  := ::aGetList[ nGet ]

	    IF ( VALTYPE( oGet:Control ) == "O" .AND. ;
			  oGet:Control:ClassName() != "TBROWSE" )
	       cCaption := oGet:Control:Caption
	    ELSE
	       cCaption := oGet:Caption
	    ENDIF

	    IF ( ( nHotPos := AT( "&", cCaption ) ) == 0 )
	    ELSEIF ( nHotPos == LEN( cCaption ) )
	    ELSEIF ( LOWER( SUBSTR( cCaption, nHotPos + 1, 1 ) ) == cKey )

      // Test the current GUI-GET or Get PostValidation:
      lGUI := ( VALTYPE( ::aGetList[ ::nPos ]:Control ) == "O" )
      IF lGUI .AND. !( ::GUIPostValidate( ::aGetList[ ::nPos ], ;
        ::aGetList[ ::nPos ]:Control,  ) )
	 RETURN 0
      ELSEIF !lGUI .AND. !( ::GetPostValidate( ::aGetList[ ::nPos ], ;
      ) )
	 RETURN 0
      ENDIF
      
      // Test the next GUI-GET or Get PreValidation:
      lGUI := ( VALTYPE( oGet:Control ) == "O" )
      IF lGUI .AND. !( ::GUIPreValidate( oGet, oGet:Control,   ) )
	 // RETURN 0  // Commented out.
	 RETURN nGet  // Changed.
      ELSEIF !lGUI .AND. !( ::GetPreValidate( oGet,  ) )
	 // RETURN 0  // Commented out.
	 RETURN nGet  // Changed.
      ENDIF

	       RETURN ( nGet )
	    ENDIF
	 NEXT

	 nStart := 1
     nEnd   := ::nPos - 1

      NEXT

   RETURN ( 0 )
method  HitTest(  MouseRow, MouseCol, aMsg ) CLASS HBGETLIST
   LOCAL nCount, nTotal, lGUI

::nNextGet := 0
   nTotal  := LEN( ::aGetList )

   FOR nCount := 1 TO nTotal
      IF ( ( ::nHitCode := ;
     ::aGetList[ nCount ]:HitTest( MouseRow, MouseCol ) ) != HTNOWHERE )
     ::nNextGet := nCount
	 EXIT
      ENDIF
   NEXT

   // DO WHILE !( ::nNextGet == 0 )  // Commented out.
   IF !( ::nNextGet == 0 )  // Changed.
      // Test the current GUI-GET or Get PostValidation:
      lGUI := ( VALTYPE( ::aGetList[ ::nPos]:Control ) == "O" )
      IF lGUI .AND. !( ::GUIPostValidate( ::aGetList[ ::nPos], ;
        ::aGetList[ ::nPos]:Control, aMsg  ) )
     ::nNextGet := 0
	 // EXIT  // Commented out.
	 RETURN 0  // Changed.
      ELSEIF !lGUI .AND. !( ::GetPostValidate( ::aGetList[ ::nPos], ;
	 aMsg ) )
     ::nNextGet := 0
	 // EXIT  // Commented out.
	 RETURN 0  // Changed.
      ENDIF
      
      // Test the next GUI-GET or Get PreValidation:
      lGUI := ( VALTYPE( ::aGetList[ ::nNextGet ]:Control ) == "O" )
      IF lGUI .AND. !( ::GUIPreValidate( ::aGetList[ ::nNextGet ], ;
        ::aGetList[ ::nNextGet ]:Control, aMsg  ) )
     ::nNextGet := 0
	 // EXIT  // Commented out.
     RETURN ::nNextGet  // Changed.
      ELSEIF !lGUI .AND. !( ::GetPreValidate( ::aGetList[ ::nNextGet ], ;
	 aMsg ) )
     ::nNextGet := 0
	 // EXIT  // Commented out.
     RETURN ::nNextGet  // Changed.
      ENDIF
      // EXIT  // Commented out.
      RETURN ::nNextGet  // Changed.
   // ENDDO  // Commented out.
   ENDIF  // Changed.

   // RETURN ( ::nNextGet != 0 )  // Commented out.
   RETURN 0  // Changed.


#endif
