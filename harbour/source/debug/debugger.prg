/*
 * $Id$
 */

/* Harbour debugger first outline
 * Copyright(C) 1999 by Antonio Linares <alinares@fivetech.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR
 * PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to:
 *
 * The Free Software Foundation, Inc.,
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "box.ch"
#include "classes.ch"
#include "inkey.ch"

#xcommand DEFAULT <uVar1> := <uVal1> ;
               [, <uVarN> := <uValN> ] => ;
                  <uVar1> := If( <uVar1> == nil, <uVal1>, <uVar1> ) ;;
                [ <uVarN> := If( <uVarN> == nil, <uValN>, <uVarN> ); ]

#xcommand MENU [<oMenu>] => [ <oMenu> := ] TDbMenu():New()
#xcommand MENUITEM <cPrompt> [ ACTION <uAction> ] => ;
   TDbMenu():AddItem( TDbMenuItem():New( <cPrompt> [,{|Self|<uAction>}] ) )
#xcommand SEPARATOR => TDbMenu():AddItem( TDbMenuItem():New( "-" ) )
#xcommand ENDMENU => ATail( TDbMenu():aMenus ):Build()

static oDebugger

function __dbgEntry( uParam )  // debugger entry point

   do case
      case ValType( uParam ) == "C"   // called from hvm.c hb_vmModuleName()
           if oDebugger == nil
              oDebugger = TDebugger():New()
              oDebugger:Activate( uParam )
           endif

      case ValType( uParam ) == "N"   // called from hvm.c hb_vmDebugShowLines()
           if oDebugger != nil
              oDebugger:cAppImage  = SaveScreen()
              oDebugger:nAppRow    = Row()
              oDebugger:nAppCol    = Col()
              oDebugger:cAppColors = SetColor()
              oDebugger:nAppCursor = SetCursor()
              RestScreen( 0, 0, MaxRow(), MaxCol(), oDebugger:cImage )
              DispEnd()
              oDebugger:GoToLine( uParam )
              oDebugger:HandleEvent()
           endif
   endcase

return nil

CLASS TDebugger

   DATA   oPullDown
   DATA   oWndCode, oWndCommand
   DATA   oBar, oBrwText
   DATA   cImage, nOldCursor
   DATA   lEnd
   DATA   cAppImage, nAppRow, nAppCol, cAppColors, nAppCursor

   METHOD New()
   METHOD Activate( cModuleName )
   METHOD Show()
   METHOD ShowCode( cModuleName )
   METHOD HandleEvent()
   METHOD Hide()

   METHOD Open()
   METHOD InputBox( cMsg, uValue )
   METHOD Exit() INLINE ::lEnd := .t.

   METHOD GoToLine( nLine )

ENDCLASS

METHOD New() CLASS TDebugger

   ::oPullDown   = BuildMenu( Self )
   ::oWndCode    = TDbWindow():New( 1, 0, MaxRow() - 6, MaxCol(),, "BG+/B" )
   ::oWndCommand = TDbWindow():New( MaxRow() - 5, 0, MaxRow() - 1, MaxCol(),;
                                    " Command ", "BG+/B" )
   ::lEnd        = .f.

return Self

METHOD Activate( cModuleName ) CLASS TDebugger

   ::cAppImage  = SaveScreen()
   ::nAppRow    = Row()
   ::nAppCol    = Col()
   ::cAppColors = SetColor()
   ::nAppCursor = SetCursor()
   ::Show()
   ::ShowCode( cModuleName )
   ::cImage := SaveScreen()
   DispBegin()
   RestScreen( 0, 0, MaxRow(), MaxCol(), ::cAppImage )

return nil

METHOD HandleEvent() CLASS TDebugger

   local nPopup

   ::lEnd = .f.

   while ! ::lEnd

      nKey = InKey( 0 )

      do case
         case ::oPullDown:IsOpen()
              ::oPullDown:ProcessKey( nKey )

         case nKey == K_ESC
              ::Exit()

         case nKey == K_UP
              ::oBrwText:Up()
              ::oBrwText:ForceStable()

         case nKey == K_DOWN
              ::oBrwText:Down()
              ::oBrwText:ForceStable()

         case nKey == K_HOME
              ::oBrwText:GoTop()
              ::oBrwText:ForceStable()

         case nKey == K_END
              ::oBrwText:GoBottom()
              ::oBrwText:ForceStable()

         case nKey == K_F4
              ::cImage = SaveScreen()
              RestScreen( 0, 0, MaxRow(), MaxCol(), ::cAppImage )
              InKey( 0 )
              RestScreen( 0, 0, MaxRow(), MaxCol(), ::cImage )

         case nKey == K_F8
              ::cImage = SaveScreen()
              DispBegin()
              RestScreen( 0, 0, MaxRow(), MaxCol(), ::cAppImage )
              SetPos( ::nAppRow, ::nAppCol )
              SetColor( ::cAppColors )
              SetCursor( ::nAppCursor )
              ::Exit()

         otherwise
              if ( nPopup := ::oPullDown:GetHotKeyPos( AltToKey( nKey ) ) ) != 0
                 ::oPullDown:ShowPopup( nPopup )
              endif
      endcase
   end

return nil

METHOD Hide() CLASS TDebugger

   RestScreen( ,,,, ::cAppImage )
   ::cAppImage = nil
   SetCursor( ::nOldCursor )
   SetColor( "N/W" )

return nil

METHOD Show() CLASS TDebugger

   ::nOldCursor = SetCursor( 0 )
   ::cAppImage  = SaveScreen()
   ::nAppRow    = Row()
   ::nAppCol    = Col()
   ::cAppColors = SetColor()
   ::oPullDown:Display()
   ::oWndCode:Show( .t. )
   ::oWndCommand:Show()

   @ MaxRow(), 0 SAY ;
   "F1-Help F2-Zoom F3-Repeat F4-User F5-Go F6-WA F7-Here F8-Step F9-BkPt F10-Trace" COLOR "N/BG"
   @ MaxRow(),  0 SAY "F1" COLOR "GR+/BG"
   @ MaxRow(),  8 SAY "F2" COLOR "GR+/BG"
   @ MaxRow(), 16 SAY "F3" COLOR "GR+/BG"
   @ MaxRow(), 26 SAY "F4" COLOR "GR+/BG"
   @ MaxRow(), 34 SAY "F5" COLOR "GR+/BG"
   @ MaxRow(), 40 SAY "F6" COLOR "GR+/BG"
   @ MaxRow(), 46 SAY "F7" COLOR "GR+/BG"
   @ MaxRow(), 54 SAY "F8" COLOR "GR+/BG"
   @ MaxRow(), 62 SAY "F9" COLOR "GR+/BG"
   @ MaxRow(), 70 SAY "F10" COLOR "GR+/BG"

return nil

METHOD ShowCode( cModuleName ) CLASS TDebugger

   local cPrgName := SubStr( cModuleName, 1, At( ":", cModuleName ) - 1 )

   ::oBrwText = TBrwText():New( ::oWndCode:nTop + 1, ::oWndCode:nLeft + 1,;
                ::oWndCode:nBottom - 1, ::oWndCode:nRight - 1, cPrgName, "BG+/B, N/BG" )

   ::oBrwText:ForceStable()

   @ ::oWndCode:nTop, ( ( ::oWndCode:nRight - ::oWndCode:nLeft ) / 2 ) - ;
     ( Len( cPrgName ) + 2 ) / 2 SAY " " + cPrgName + " "

return nil

METHOD Open() CLASS TDebugger

   local cFileName := ::InputBox( "Please enter the filename", Space( 30 ) )

return nil

METHOD InputBox( cMsg, uValue ) CLASS TDebugger

   local nTop    := ( MaxRow() / 2 ) - 5
   local nLeft   := ( MaxCol() / 2 ) - 20
   local nBottom := ( MaxRow() / 2 ) - 3
   local nRight  := ( MaxCol() / 2 ) + 20
   local uTemp   := uValue
   local GetList := {}
   local nOldCursor
   local cImage := SaveScreen( nTop, nLeft, nBottom + 1, nRight + 1 )
   local lScoreBoard := Set( _SET_SCOREBOARD, .f. )

   @ nTop, nLeft, nBottom, nRight BOX B_SINGLE COLOR ::oPullDown:cClrPopup
   @ nTop, nLeft + ( ( nRight - nLeft ) ) / 2 - Len( cMsg ) / 2 SAY ;
      cMsg COLOR ::oPullDown:cClrPopup
   Shadow( nTop, nLeft, nBottom, nRight )

   @ nTop + 1, nLeft + 1 GET uTemp
   nOldCursor = SetCursor( 1 )
   READ
   SetCursor( nOldCursor )
   RestScreen( nTop, nLeft, nBottom + 1, nRight + 1, cImage )
   Set( _SET_SCOREBOARD, lScoreBoard )

return If( LastKey() != K_ESC, uTemp, uValue )

METHOD GotoLine( nLine ) CLASS TDebugger

   ::oBrwText:GotoLine( nLine )

return nil

CLASS TDbWindow  // Debugger windows

   DATA   nTop, nLeft, nBottom, nRight
   DATA   cCaption
   DATA   cBackImage, cColor

   METHOD New( nTop, nLeft, nBottom, nRight, cCaption, cColor )
   METHOD Show( lFocused )
   METHOD Move()

ENDCLASS

METHOD New( nTop, nLeft, nBottom, nRight, cCaption, cColor ) CLASS TDbWindow

   ::nTop     = nTop
   ::nLeft    = nLeft
   ::nBottom  = nBottom
   ::nRight   = nRight
   ::cCaption = cCaption
   ::cColor   = cColor

return Self

METHOD Show( lFocused ) CLASS TDbWindow

   DEFAULT lFocused := .f.

   ::cBackImage = SaveScreen( ::nTop, ::nLeft, ::nBottom, ::nRight )

   SetColor( ::cColor )
   SCROLL( ::nTop, ::nLeft, ::nBottom, ::nRight )
   @ ::nTop, ::nLeft, ::nBottom, ::nRight BOX If( lFocused, B_DOUBLE, B_SINGLE ) ;
      COLOR ::CColor

   if ! Empty( ::cCaption )
      @ ::nTop, ::nLeft + ( ::nRight - ::nLeft ) / 2 - Len( ::cCaption ) / 2 ;
         SAY ::cCaption
   endif

return nil

/*Method move()
Move a window across the screen
Copyright Luiz Rafael Culik 1999
*/
METHOD Move() Class TDbWindow

#define pbar1 replicate(chr(176),8)+chr(32)

   local noldtop  := ::nTop
   local noldleft := ::nLeft
   local noldbottom := ::nbottom
   local noldright := ::nright
   local nkey

   while .t.
      restscreen(,,,, ::cbackimage)
      dispbox(::ntop,::nleft,::nright,::nbottom,pbar1)
      nkey=inkey(0)
      do case
         case nkey==K_UP
              if(::ntop !=0,(::ntop--,::nbottom--),nil)

         case nkey==K_DOWN
              if(::nbottom !=maxrow(),(::ntop++,::nbottom++),nil)

         case nkey==K_LEFT
              if(::nleft!=0,(::nleft--,::nright--),nil)

         case nkey==K_RIGHT
              if(::nbottom !=maxrow(),(::nleft++,::nright++),nil)

         case nkey==K_ESC
              ::ntop:=noldtop
              ::nleft:=nolfleft
              ::nbottom:=noldbottom
              ::nright:=noldright
      endcase

      if ( nkey==K_ESC .or. nkey==K_ENTER)
         exit
      end
   end

   // __keyboard(chr(0)),inkey())

return nil

CLASS TDbMenu  /* debugger menu */

   CLASSDATA aMenus

   DATA   nTop, nLeft, nBottom, nRight
   DATA   aItems
   DATA   cClrHilite, cClrHotKey, cClrHotFocus, cClrPopup
   DATA   nOpenPopup             // zero if no popup is shown
   DATA   lPopup
   DATA   cBackImage

   METHOD New( aItems )
   METHOD AddItem( oMenuItem )
   METHOD Build()
   METHOD ClosePopup()
   METHOD Close() INLINE ::ClosePopup( ::nOpenPopup ), ::nOpenPopup := 0
   METHOD DeHilite()
   METHOD Display()
   METHOD EvalAction()
   METHOD GetHotKeyPos( nKey )
   METHOD GoBottom()
   METHOD GoDown() INLINE ::aItems[ ::nOpenPopup ]:bAction:GoRight()
   METHOD GoLeft()
   METHOD GoRight()
   METHOD GoTop()
   METHOD GoUp() INLINE ::aItems[ ::nOpenPopup ]:bAction:GoLeft()
   METHOD IsOpen() INLINE ::nOpenPopup != 0
   METHOD ProcessKey( nKey )
   METHOD ShowPopup( nPopup )

ENDCLASS

METHOD New() CLASS TDbMenu

   local n, nCol := 0

   if ::aMenus == nil
      ::aMenus = {}
      ::lPopup = .f.
   else
      ::lPopup = .t.
   endif

   ::nTop         = 0
   ::nLeft        = 0
   ::nBottom      = 0
   ::nRight       = 0
   ::aItems       = {}
   ::cClrHilite   = "W+/N"
   ::cClrHotFocus = "GR+/N"
   ::cClrHotKey   = "GR+/BG"
   ::cClrPopup    = "N/BG"
   ::nOpenPopup   = 0

   AAdd( ::aMenus, Self )

return Self

METHOD AddItem( oMenuItem ) CLASS TDbMenu

   local oLastMenu := ATail( ::aMenus ), oLastMenuItem

   if oLastMenu:lPopup
      oMenuItem:nRow = Len( oLastMenu:aItems )
      oMenuItem:nCol = oLastMenu:nLeft + 1
   else
      oMenuItem:nRow = 0
      if Len( oLastMenu:aItems ) > 0
         oLastMenuItem = ATail( oLastMenu:aItems )
         oMenuItem:nCol = oLastMenuItem:nCol + ;
                          Len( StrTran( oLastMenuItem:cPrompt, "&", "" ) )
      else
         oMenuItem:nCol = 0
      endif
   endif

   AAdd( ATail( ::aMenus ):aItems, oMenuItem )

return oMenuItem

METHOD Build() CLASS TDbMenu

   local n, nPos := 0, oMenuItem

   if Len( ::aMenus ) == 1           // pulldown menu
      for n = 1 to Len( ::aItems )
         ::aItems[ n ]:nRow = 0
         ::aItems[ n ]:nCol = nPos
         nPos += Len( StrTran( ::aItems[ n ]:cPrompt, "&", "" ) )
      next
   else
      oMenuItem = ATail( ::aMenus[ Len( ::aMenus ) - 1 ]:aItems )
      ::nTop    = oMenuItem:nRow + 1
      ::nLeft   = oMenuItem:nCol
      nPos = ::nLeft
      for n = 1 to Len( ::aItems )
         ::aItems[ n ]:nRow = ::nTop + n
         ::aItems[ n ]:nCol = ::nLeft + 1
         nPos = Max( nPos, ::nLeft + Len( StrTran( ::aItems[ n ]:cPrompt, "&", "" ) ) + 1 )
      next
      ::nRight  = nPos
      ::nBottom = ::nTop + Len( ::aItems ) + 1
      for n = 1 to Len( ::aItems )
         if ::aItems[ n ]:cPrompt != "-"
            ::aItems[ n ]:cPrompt = PadR( ::aItems[ n ]:cPrompt, ::nRight - ::nLeft )
         endif
      next
      ATail( ::aMenus[ Len( ::aMenus ) - 1 ]:aItems ):bAction = ATail( ::aMenus )
      ::aMenus = ASize( ::aMenus, Len( ::aMenus ) - 1 )
   endif

return nil

METHOD ClosePopup( nPopup ) CLASS TDbMenu

   local oPopup

   // dispbegin()
   if nPopup != 0
      oPopup = ::aItems[ nPopup ]:bAction
      RestScreen( oPopup:nTop, oPopup:nLeft, oPopup:nBottom + 1, oPopup:nRight + 1,;
                  oPopup:cBackImage )
      oPopup:cBackImage = nil
      @ 0, ::aItems[ nPopup ]:nCol SAY ;
        StrTran( ::aItems[ nPopup ]:cPrompt, "&", "" ) COLOR ::cClrPopup

      @ 0, ::aItems[ nPopup ]:nCol + nAt := At( "&", ::aItems[ nPopup ]:cPrompt ) - 1 SAY ;
        SubStr( ::aItems[ nPopup ]:cPrompt, nAt + 2, 1 ) COLOR ::cClrHotKey
   endif
   // dispend()

return nil

METHOD DeHilite() CLASS TDbMenu

   local oMenuItem := ::aItems[ ::nOpenPopup ]

   oMenuItem:Display( ::cClrPopup, ::cClrHotKey )

return nil

METHOD Display() CLASS TDbMenu

   local n, nAt

   SetColor( ::cClrPopup )

   // DispBegin()
   if ! ::lPopup
      @ 0, 0 SAY Space( MaxCol() + 1 ) COLOR ::cClrPopup
      DevPos( 0, 0 )
   else
      ::cBackImage = SaveScreen( ::nTop, ::nLeft, ::nBottom + 1, ::nRight + 1 )
      @ ::nTop, ::nLeft, ::nBottom, ::nRight BOX B_SINGLE
      Shadow( ::nTop, ::nLeft, ::nBottom, ::nRight )
   endif

   for n = 1 to Len( ::aItems )
      if ::aItems[ n ]:cPrompt == "-"  // Separator
         @ ::aItems[ n ]:nRow, ::nLeft SAY ;
            Chr( 195 ) + Replicate( Chr( 196 ), ::nRight - ::nLeft - 1 ) + Chr( 180 )
      else
         @ ::aItems[ n ]:nRow, ::aItems[ n ]:nCol SAY ;
            StrTran( ::aItems[ n ]:cPrompt, "&", "" )

         @ ::aItems[ n ]:nRow, ::aItems[ n ]:nCol + nAt := ;
            At( "&", ::aItems[ n ]:cPrompt ) - 1 SAY ;
            SubStr( ::aItems[ n ]:cPrompt, nAt + 2, 1 ) COLOR ::cClrHotKey
      endif
   next
   // DispEnd()

return nil

METHOD EvalAction() CLASS TDbMenu

   local oPopup, oMenuItem

   oPopup = ::aItems[ ::nOpenPopup ]:bAction
   oMenuItem = oPopup:aItems[ oPopup:nOpenPopup ]

   if oMenuItem:bAction != nil
      ::Close()
      Eval( oMenuItem:bAction, oMenuItem )
   endif

return nil

METHOD GetHotKeyPos( cKey ) CLASS TDbMenu

   local n

   for n = 1 to Len( ::aItems )
      if Upper( SubStr( ::aItems[ n ]:cPrompt,;
         At( "&", ::aItems[ n ]:cPrompt ) + 1, 1 ) ) == cKey
         return n
      endif
   next

return 0

return .f.

METHOD GoBottom() CLASS TDbMenu

   local oPopup

   if ::IsOpen()
      oPopup = ::aItems[ ::nOpenPopup ]:bAction
      oPopup:DeHilite()
      oPopup:ShowPopup( Len( oPopup:aItems ) )
   endif

return nil

METHOD GoLeft() CLASS TDbMenu

   local oMenuItem := ::aItems[ ::nOpenPopup ]

   // DispBegin()
   if ::nOpenPopup != 0
      if ! ::lPopup
         ::ClosePopup( ::nOpenPopup )
      else
         SetColor( ::cClrPopup )
         @ oMenuItem:nRow, oMenuItem:nCol SAY ;
            StrTran( oMenuItem:cPrompt, "&", "" )

         @ oMenuItem:nRow, oMenuItem:nCol + nAt := ;
            At( "&", oMenuItem:cPrompt ) - 1 SAY ;
            SubStr( oMenuItem:cPrompt, nAt + 2, 1 ) COLOR ::cClrHotKey
      endif
      if ::nOpenPopup > 1
         --::nOpenPopup
         while ::nOpenPopup > 1 .and. ;
            SubStr( ::aItems[ ::nOpenPopup ]:cPrompt, 1, 1 ) == "-"
            --::nOpenPopup
         end
         ::ShowPopup( ::nOpenPopup )
      else
         ::ShowPopup( ::nOpenPopup := Len( ::aItems ) )
      endif
   endif
   // DispEnd()

return nil

METHOD GoRight() CLASS TDbMenu

   local oMenuItem := ::aItems[ ::nOpenPopup ]

   // DispBegin()
   if ::nOpenPopup != 0
      if ! ::lPopup
         ::ClosePopup( ::nOpenPopup )
      else
         SetColor( ::cClrPopup )
         @ oMenuItem:nRow, oMenuItem:nCol SAY ;
            StrTran( oMenuItem:cPrompt, "&", "" )

         @ oMenuItem:nRow, oMenuItem:nCol + nAt := ;
            At( "&", oMenuItem:cPrompt ) - 1 SAY ;
            SubStr( oMenuItem:cPrompt, nAt + 2, 1 ) COLOR ::cClrHotKey
      endif
      if ::nOpenPopup < Len( ::aItems )
         ++::nOpenPopup
         while ::nOpenPopup < Len( ::aItems ) .and. ;
            SubStr( ::aItems[ ::nOpenPopup ]:cPrompt, 1, 1 ) == "-"
            ++::nOpenPopup
         end
         ::ShowPopup( ::nOpenPopup )
      else
         ::ShowPopup( ::nOpenPopup := 1 )
      endif
   endif
   // DispEnd()

return nil

METHOD GoTop() CLASS TDbMenu

   local oPopup

   if ::IsOpen()
      oPopup = ::aItems[ ::nOpenPopup ]:bAction
      oPopup:DeHilite()
      oPopup:ShowPopup( 1 )
   endif

return nil

METHOD ShowPopup( nPopup ) CLASS TDbMenu

   local nAt, oPopup, oMenuItem

   if ! ::lPopup
      @ 0, ::aItems[ nPopup ]:nCol SAY ;
        StrTran( ::aItems[ nPopup ]:cPrompt, "&", "" ) COLOR ::cClrHilite

      @ 0, ::aItems[ nPopup ]:nCol + nAt := At( "&", ::aItems[ nPopup ]:cPrompt ) - 1 SAY ;
        SubStr( ::aItems[ nPopup ]:cPrompt, nAt + 2, 1 ) COLOR ::cClrHotFocus
   else
      oMenuItem = ::aItems[ nPopup ]
      @ oMenuItem:nRow, oMenuItem:nCol SAY ;
        StrTran( oMenuItem:cPrompt, "&", "" ) COLOR ::cClrHilite

      @ oMenuItem:nRow, oMenuItem:nCol + nAt := ;
        At( "&", oMenuItem:cPrompt ) - 1 SAY ;
        SubStr( oMenuItem:cPrompt, nAt + 2, 1 ) COLOR ::cClrHotFocus
   endif

   ::nOpenPopup = nPopup

   if ValType( ::aItems[ nPopup ]:bAction ) == "O"
      ::aItems[ nPopup ]:bAction:Display()
      ::aItems[ nPopup ]:bAction:ShowPopup( 1 )
   endif

return nil

METHOD ProcessKey( nKey ) CLASS TDbMenu

   local nPopuo

   do case
      case nKey == K_ESC
           ::Close()

      case nKey == K_LEFT
           ::GoLeft()

      case nKey == K_RIGHT
           ::GoRight()

      case nKey == K_DOWN
           ::GoDown()

      case nKey == K_UP
           ::GoUp()

      case nKey == K_ENTER
           ::EvalAction()

      case nKey == K_HOME
           ::GoTop()

      case nKey == K_END
           ::GoBottom()

      otherwise
         if ( nPopup := ::GetHotKeyPos( AltToKey( nKey ) ) ) != 0
            ::Close()
            ::ShowPopup( nPopup )
         endif
   endcase

return nil

CLASS TDbMenuItem

   DATA  nRow, nCol
   DATA  cPrompt
   DATA  bAction

   METHOD New( cPrompt, bAction )
   METHOD Display( cClrText, cClrHotKey )

ENDCLASS

METHOD New( cPrompt, bAction ) CLASS TDbMenuItem

   ::cPrompt = cPrompt
   ::bAction = bAction

return Self

METHOD Display( cClrText, cClrHotKey ) CLASS TDbMenuItem

   @ ::nRow, ::nCol SAY ;
      StrTran( ::cPrompt, "&", "" ) COLOR cClrText

   @ ::nRow, ::nCol + nAt := ;
      At( "&", ::cPrompt ) - 1 SAY ;
      SubStr( ::cPrompt, nAt + 2, 1 ) COLOR cClrHotKey

return nil

static function AltToKey( nKey )

   local nIndex := AScan( { K_ALT_A, K_ALT_B, K_ALT_C, K_ALT_D, K_ALT_E, K_ALT_F,;
                            K_ALT_G, K_ALT_H, K_ALT_I, K_ALT_J, K_ALT_K, K_ALT_L,;
                            K_ALT_M, K_ALT_N, K_ALT_O, K_ALT_P, K_ALT_Q, K_ALT_R,;
                            K_ALT_S, K_ALT_T, K_ALT_U, K_ALT_V, K_ALT_W, K_ALT_X,;
                            K_ALT_Y, K_ALT_Z }, nKey )

      if nIndex > 0
         cKey := SubStr( "ABCDEFGHIJKLMNOPQRSTUVWXYZ", nIndex, 1 )
      else
         cKey = ""
      endif

return cKey

function BuildMenu( oDebugger )  // Builds the debugger pulldown menu

   local oMenu

   MENU oMenu
      MENUITEM " &File "
      MENU
         MENUITEM " &Open..."        ACTION oDebugger:Open()
         MENUITEM " &Resume"         ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Shell"          ACTION Alert( "Not implemented yet!" )
         SEPARATOR
         MENUITEM " &Exit  Alt-X  "  ACTION oDebugger:Exit()
      ENDMENU

      MENUITEM " &Locate "
      MENU
         MENUITEM " &Find"       ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Next"       ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Previous"   ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Goto line..."  ACTION Alert( "Not implemented yet!" )
         SEPARATOR
         MENUITEM " &Case sensitive " ACTION Alert( "Not implemented yet!" )
      ENDMENU

      MENUITEM " &View "
      MENU
         MENUITEM " &Sets"            ACTION Alert( "Not implemented yet!" )
         MENUITEM " &WorkAreas  F6"   ACTION Alert( "Not implemented yet!" )
         MENUITEM " &App screen F4 "  ACTION Alert( "Not implemented yet!" )
         SEPARATOR
         MENUITEM " &CallStack"       ACTION Alert( "Not implemented yet!" )
      ENDMENU

      MENUITEM " &Run "
      MENU
         MENUITEM " &Restart"         ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Animate"         ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Step              F8 " ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Trace            F10"  ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Go                F5"  ACTION Alert( "Not implemented yet!" )
         MENUITEM " to &Cursor         F7"  ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Next routine Ctrl-F5"  ACTION Alert( "Not implemented yet!" )
         SEPARATOR
         MENUITEM " S&peed..."              ACTION Alert( "Not implemented yet!" )
      ENDMENU

      MENUITEM " &Point "
      MENU
         MENUITEM " &Watchpoint..."         ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Tracepoint..."         ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Breakpoint F9 "        ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Delete..."             ACTION Alert( "Not implemented yet!" )
      ENDMENU

      MENUITEM " &Monitor "
      MENU
         MENUITEM " &Public"                ACTION Alert( "Not implemented yet!" )
         MENUITEM " Pri&vate "              ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Static"                ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Local"                 ACTION Alert( "Not implemented yet!" )
         SEPARATOR
         MENUITEM " &All"                   ACTION Alert( "Not implemented yet!" )
         MENUITEM " S&ort"                  ACTION Alert( "Not implemented yet!" )
      ENDMENU

      MENUITEM " &Options "
      MENU
         MENUITEM " &Preprocessed code"     ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Line numbers"          ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Exchange screens"      ACTION Alert( "Not implemented yet!" )
         MENUITEM " swap on &Input"         ACTION Alert( "Not implemented yet!" )
         MENUITEM " code&block trace"       ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Menu Bar"              ACTION Alert( "Not implemented yet!" )
         MENUITEM " Mono &display"          ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Colors..."             ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Tab width..."          ACTION Alert( "Not implemented yet!" )
         MENUITEM " path for &files..."     ACTION Alert( "Not implemented yet!" )
         SEPARATOR
         MENUITEM " &Save settings..."      ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Restore settings... "  ACTION Alert( "Not implemented yet!" )
      ENDMENU

      MENUITEM " &Window "
      MENU
         MENUITEM " &Next     Tab "         ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Prev  Sh-Tab"          ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Move"                  ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Size"                  ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Zoom      F2"          ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Iconize"               ACTION Alert( "Not implemented yet!" )
         SEPARATOR
         MENUITEM " &Tile"                  ACTION Alert( "Not implemented yet!" )
      ENDMENU

      MENUITEM " &Help "
      MENU
         MENUITEM " &About Help "           ACTION Alert( "Not implemented yet!" )
         SEPARATOR
         MENUITEM " &Keys"                  ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Windows"               ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Menus"                 ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Commands"              ACTION Alert( "Not implemented yet!" )
      ENDMENU

   ENDMENU

return oMenu


