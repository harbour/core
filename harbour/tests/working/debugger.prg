// Harbour debugger first outline

#include "box.ch"
#include "classes.ch"
#include "inkey.ch"

#xcommand MENU [<oMenu>] => [ <oMenu> := ] TDbMenu():New()
#xcommand MENUITEM <cPrompt> [ ACTION <uAction> ] => ;
   TDbMenu():AddItem( TDbMenuItem():New( <cPrompt> [,{|Self|<uAction>}] ) )
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
         case oMenu:IsOpen()
              oMenu:ProcessKey( nKey )

         case nKey == K_ESC
              lEnd = .t.

         otherwise
              if ( nPopup := oMenu:GetHotKeyPos( AltToKey( nKey ) ) ) != 0
                 oMenu:ShowPopup( nPopup )
              endif
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

METHOD DeHilite() CLASS TDbMenu

   local oMenuItem := ::aItems[ ::nOpenPopup ]

   oMenuItem:Display( ::cClrPopup, ::cClrHotKey )

return nil

METHOD Display() CLASS TDbMenu

   local n, nAt

   SetColor( ::cClrPopup )

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

return nil

METHOD GoRight() CLASS TDbMenu

   local oMenuItem := ::aItems[ ::nOpenPopup ]


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

function BuildMenu()   // Builds the debugger pulldown menu

   local oMenu

   MENU oMenu
      MENUITEM " &File "
      MENU
         MENUITEM " &Open..."    ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Resume"     ACTION Alert( "Not implemented yet!" )
         MENUITEM " &Shell"      ACTION Alert( "Not implemented yet!" )
         SEPARATOR
         MENUITEM " &Exit  Alt-X  "  ACTION Alert( "Not implemented yet!" )
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

