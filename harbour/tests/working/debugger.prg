// Harbour debugger first outline

#include "box.ch"
#include "classes.ch"
#include "inkey.ch"

#xcommand MENU [<oMenu>] => [ <oMenu> := ] TDbMenu():New()
#xcommand MENUITEM <cPrompt> => TDbMenu():AddItem( TDbMenuItem():New( <cPrompt> ) )
#xcommand SEPARATOR => TDbMenu():AddItem( TDbMenuItem():New( "-" ) )
#xcommand ENDMENU => ATail( TDbMenu():aMenus ):Build()

function Main()

   local nCursor := SetCursor( 0 )
   local lEnd := .f., nKey, nPopup
   local oMenu := BuildMenu()

   CLS
   oMenu:Display()

   SET COLOR TO BG+/B
   @ 1, 0 CLEAR TO MaxRow() - 6, MaxCol()

   @ 1, 0, MaxRow() - 6, MaxCol() BOX B_DOUBLE
   @ MaxRow() -5, 0 CLEAR TO MaxRow() - 1, MaxCol()
   @ MaxRow() - 5, 0, MaxRow() - 1, MaxCol() BOX B_SINGLE
   @ MaxRow() - 5, ( MaxCol() / 2 ) - 4 SAY " Command "

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

   while ! lEnd

      nKey = InKey( 0 )

      do case
         case nKey == K_ESC
              if oMenu:nOpenPopup != 0
                 oMenu:ClosePopup( oMenu:nOpenPopup )
                 oMenu:nOpenPopup = 0
              else
                 lEnd = .t.
              endif

         case ( nPopup := oMenu:GetHotKeyPos( AltToKey( nKey ) ) ) != 0
              if oMenu:nOpenPopup != 0
                 oMenu:ClosePopup( oMenu:nOpenPopup )
                 oMenu:nOpenPopup = 0
              endif
              oMenu:ShowPopup( nPopup )

         case nKey == K_LEFT
              if oMenu:nOpenPopup != 0
                 oMenu:GoLeft()
              endif

         case nKey == K_RIGHT
              if oMenu:nOpenPopup != 0
                 oMenu:GoRight()
              endif

         otherwise
      endcase
   end

   SetColor( "W/N" )
   SetCursor( nCursor )
   CLS

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
   METHOD Display()
   METHOD GetHotKeyPos( nKey )
   METHOD GoLeft()
   METHOD GoRight()
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

return nil

METHOD Display() CLASS TDbMenu

   local n, nAt

   SetColor( ::cClrPopup )

   if ! ::lPopup
      @ 0, 0 SAY Space( MaxCol() + 1 ) COLOR ::cClrPopup
      DevPos( 0, 0 )
   else
      ::cBackImage = SaveScreen( ::nTop, ::nLeft, ::nBottom + 1, ::nRight + 1 )
      @ Self:nTop, ::nLeft, ::nBottom, ::nRight BOX B_SINGLE
      Shadow( ::nTop, ::nLeft, ::nBottom, ::nRight )
   endif

   for n = 1 to Len( ::aItems )
      if ::aItems[ n ]:cPrompt == "-"  // Separator
         @ Self:aItems[ n ]:nRow, ::nLeft SAY ;
            Chr( 195 ) + Replicate( Chr( 196 ), ::nRight - ::nLeft - 1 ) + Chr( 180 )
      else
         @ Self:aItems[ n ]:nRow, ::aItems[ n ]:nCol SAY ;
            StrTran( ::aItems[ n ]:cPrompt, "&", "" )

         @ Self:aItems[ n ]:nRow, ::aItems[ n ]:nCol + nAt := ;
            At( "&", ::aItems[ n ]:cPrompt ) - 1 SAY ;
            SubStr( ::aItems[ n ]:cPrompt, nAt + 2, 1 ) COLOR ::cClrHotKey
      endif
   next

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

METHOD GoLeft() CLASS TDbMenu

   if ::nOpenPopup != 0
      ::ClosePopup( ::nOpenPopup )
      if ::nOpenPopup > 1
         ::ShowPopup( ::nOpenPopup -= 1 )
      else
         ::ShowPopup( ::nOpenPopup := Len( ::aItems ) )
      endif
   endif

return nil

METHOD GoRight() CLASS TDbMenu

   if ::nOpenPopup != 0
      ::ClosePopup( ::nOpenPopup )
      if ::nOpenPopup < Len( ::aItems )
         ::ShowPopup( ::nOpenPopup += 1 )
      else
         ::ShowPopup( ::nOpenPopup := 1 )
      endif
   endif

return nil

METHOD ShowPopup( nPopup ) CLASS TDbMenu

   local nAt

   @ 0, ::aItems[ nPopup ]:nCol SAY ;
      StrTran( ::aItems[ nPopup ]:cPrompt, "&", "" ) COLOR ::cClrHilite

   @ 0, ::aItems[ nPopup ]:nCol + nAt := At( "&", ::aItems[ nPopup ]:cPrompt ) - 1 SAY ;
     SubStr( ::aItems[ nPopup ]:cPrompt, nAt + 2, 1 ) COLOR ::cClrHotFocus

   // ::aItems[ nPopup ]:cBackImage := SaveScreen( 1, ::aItems[ nPopup ]:nCol,;
   //   6, ::aItems[ nPopup ]:nCol + 7 ) // including shadow areas

   // @ 1, ::aItems[ nPopup ]:nCol, 5, ::aItems[ nPopup ]:nCol + 5 BOX B_SINGLE ;
   //   COLOR ::cClrPopup

   ::nOpenPopup = nPopup

   if ::aItems[ nPopup ]:bAction != nil
      ::aItems[ nPopup ]:bAction:Display()
   endif

return nil

CLASS TDbMenuItem

   DATA  nRow, nCol
   DATA  cPrompt
   DATA  bAction

   METHOD New( cPrompt )

ENDCLASS

METHOD New( cPrompt ) CLASS TDbMenuItem

   ::cPrompt = cPrompt

return Self

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

function BuildMenu()   // Builds the debugger pulldown menu

   local oMenu

   MENU oMenu
      MENUITEM " &File "
      MENU
         MENUITEM " &Open..."
         MENUITEM " &Resume"
         MENUITEM " &Shell"
         SEPARATOR
         MENUITEM " &Exit  Alt-X  "
      ENDMENU

      MENUITEM " &Locate "
      MENU
         MENUITEM " &Find"
         MENUITEM " &Next"
         MENUITEM " &Previous"
         MENUITEM " &Goto line..."
         SEPARATOR
         MENUITEM " &Case sensitive "
      ENDMENU

      MENUITEM " &View "
      MENU
         MENUITEM " &Sets"
         MENUITEM " &WorkAreas  F6"
         MENUITEM " &App screen F4 "
         SEPARATOR
         MENUITEM " &CallStack"
      ENDMENU

      MENUITEM " &Run "
      MENU
         MENUITEM " &Restart"
         MENUITEM " &Animate"
         MENUITEM " &Step              F8 "
         MENUITEM " &Trace            F10"
         MENUITEM " &Go                F5"
         MENUITEM " to &Cursor         F7"
         MENUITEM " &Next routine Ctrl-F5"
         SEPARATOR
         MENUITEM " S&peed..."
      ENDMENU

      MENUITEM " &Point "
      MENU
         MENUITEM " &Watchpoint..."
         MENUITEM " &Tracepoint..."
         MENUITEM " &Breakpoint F9 "
         MENUITEM " &Delete..."
      ENDMENU

      MENUITEM " &Monitor "
      MENU
         MENUITEM " &Public"
         MENUITEM " Pri&vate "
         MENUITEM " &Static"
         MENUITEM " &Local"
         SEPARATOR
         MENUITEM " &All"
         MENUITEM " S&ort"
      ENDMENU

      MENUITEM " &Options "
      MENU
         MENUITEM " &Preprocessed code"
         MENUITEM " &Line numbers"
         MENUITEM " &Exchange screens"
         MENUITEM " swap on &Input"
         MENUITEM " code&block trace"
         MENUITEM " &Menu Bar"
         MENUITEM " Mono &display"
         MENUITEM " &Colors..."
         MENUITEM " &Tab width..."
         MENUITEM " path for &files..."
         SEPARATOR
         MENUITEM " &Save settings..."
         MENUITEM " &Restore settings... "
      ENDMENU

      MENUITEM " &Window "
      MENU
         MENUITEM " &Next     Tab "
         MENUITEM " &Prev  Sh-Tab"
         MENUITEM " &Move"
         MENUITEM " &Size"
         MENUITEM " &Zoom      F2"
         MENUITEM " &Iconize"
         SEPARATOR
         MENUITEM " &Tile"
      ENDMENU

      MENUITEM " &Help "
      MENU
         MENUITEM " &About Help "
         SEPARATOR
         MENUITEM " &Keys"
         MENUITEM " &Windows"
         MENUITEM " &Menus"
         MENUITEM " &Commands"
      ENDMENU

   ENDMENU

return oMenu

