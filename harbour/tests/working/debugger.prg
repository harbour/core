// Harbour debugger first outline

#include "box.ch"
#include "classes.ch"
#include "inkey.ch"

function Main()

   local nCursor := SetCursor( 0 )
   local lEnd := .f., nKey, nPopup
   local oMenu := TDbMenu():New( { " &File ", " &Locate ", " &View ", " &Run ",;
                                   " &Point ", " &Monitor ", " &Options ",;
                                   " &Window ", " &Help " } )

   CLS
   oMenu:Display()

   SET COLOR TO W+/BG
   @ 1, 0 CLEAR TO MaxRow() - 6, MaxCol()
   @ 1, 0, MaxRow() - 6, MaxCol() BOX B_DOUBLE
   @ MaxRow() -5, 0 CLEAR TO MaxRow() - 1, MaxCol()
   @ MaxRow() - 5, 0, MaxRow() - 1, MaxCol() BOX B_SINGLE
   @ MaxRow() - 5, ( MaxCol() / 2 ) - 4 SAY " Command "

   @ MaxRow(), 0 SAY ;
   "F1-Help F2-Zoom F3-Repeat F4-User F5-Go F6-WA F7-Here F8-Step F9-BkPt F10-Trace" COLOR "N/W"
   @ MaxRow(),  0 SAY "F1" COLOR "R/W"
   @ MaxRow(),  8 SAY "F2" COLOR "R/W"
   @ MaxRow(), 16 SAY "F3" COLOR "R/W"
   @ MaxRow(), 26 SAY "F4" COLOR "R/W"
   @ MaxRow(), 34 SAY "F5" COLOR "R/W"
   @ MaxRow(), 40 SAY "F6" COLOR "R/W"
   @ MaxRow(), 46 SAY "F7" COLOR "R/W"
   @ MaxRow(), 54 SAY "F8" COLOR "R/W"
   @ MaxRow(), 62 SAY "F9" COLOR "R/W"
   @ MaxRow(), 70 SAY "F10" COLOR "R/W"

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

CLASS TDBMENU  /* debugger menu */

   DATA   aPopups
   DATA   cClrHilite, cClrHotKey, cClrHotFocus, cClrPopup
   DATA   nOpenPopup             // zero if no popup is shown

   METHOD New( aItems )
   METHOD ClosePopup()
   METHOD Display()
   METHOD GetHotKeyPos( nKey )
   METHOD GoLeft()
   METHOD GoRight()
   METHOD ShowPopup( nPopup )

ENDCLASS

METHOD New( aItems ) CLASS TDBMENU

   local n, nCol := 0

   ::aPopups      = {}
   ::cClrHilite   = "N/BG"
   ::cClrHotFocus = "R/BG"
   ::cClrHotKey   = "R/W"
   ::cClrPopup    = "N/W"
   ::nOpenPopup   = 0

   for n = 1 to Len( aItems )
      AAdd( ::aPopups, TDbPopup():New( aItems[ n ], nCol ) )
      nCol += Len( StrTran( aItems[ n ], "&", "" ) )
   next

return Self

METHOD ClosePopup( nPopup ) CLASS TDBMENU

   if nPopup != 0
      @ 0, ::aPopups[ nPopup ]:nCol SAY ;
        StrTran( ::aPopups[ nPopup ]:cPrompt, "&", "" ) COLOR ::cClrPopup

      @ 0, ::aPopups[ nPopup ]:nCol + nAt := At( "&", ::aPopups[ nPopup ]:cPrompt ) - 1 SAY ;
        SubStr( ::aPopups[ nPopup ]:cPrompt, nAt + 2, 1 ) COLOR ::cClrHotKey
   endif

return nil

METHOD Display() CLASS TDBMENU

   local n, nAt

   @ 0, 0 SAY Space( MaxCol() + 1 ) COLOR ::cClrPopup
   DevPos( 0, 0 )
   SET COLOR TO N/W

   for n = 1 to Len( ::aPopups )
      @ 0, ::aPopups[ n ]:nCol SAY StrTran( ::aPopups[ n ]:cPrompt, "&", "" )
      @ 0, ::aPopups[ n ]:nCol + nAt := At( "&", ::aPopups[ n ]:cPrompt ) - 1 SAY ;
         SubStr( ::aPopups[ n ]:cPrompt, nAt + 2, 1 ) COLOR ::cClrHotKey
   next

return nil

METHOD GetHotKeyPos( cKey ) CLASS TDBMENU

   local n

   for n = 1 to Len( ::aPopups )
      if Upper( SubStr( ::aPopups[ n ]:cPrompt,;
         At( "&", ::aPopups[ n ]:cPrompt ) + 1, 1 ) ) == cKey
         return n
      endif
   next

return 0

return .f.

METHOD GoLeft() CLASS TDBMENU

   if ::nOpenPopup != 0
      ::ClosePopup( ::nOpenPopup )
      if ::nOpenPopup > 1
         ::ShowPopup( ::nOpenPopup -= 1 )
      else
         ::ShowPopup( ::nOpenPopup := Len( ::aPopups ) )
      endif
   endif

return nil

METHOD GoRight() CLASS TDBMENU

   if ::nOpenPopup != 0
      ::ClosePopup( ::nOpenPopup )
      if ::nOpenPopup < Len( ::aPopups )
         ::ShowPopup( ::nOpenPopup += 1 )
      else
         ::ShowPopup( ::nOpenPopup := 1 )
      endif
   endif

return nil

METHOD ShowPopup( nPopup ) CLASS TDBMENU

   local nAt

   @ 0, ::aPopups[ nPopup ]:nCol SAY ;
      StrTran( ::aPopups[ nPopup ]:cPrompt, "&", "" ) COLOR ::cClrHilite

   @ 0, ::aPopups[ nPopup ]:nCol + nAt := At( "&", ::aPopups[ nPopup ]:cPrompt ) - 1 SAY ;
     SubStr( ::aPopups[ nPopup ]:cPrompt, nAt + 2, 1 ) COLOR ::cClrHotFocus

   ::aPopups[ nPopup ]:cBackImage := SaveScreen( 1, ::aPopups[ nPopup ]:nCol,;
      6, ::aPopups[ nPopup ]:nCol + 7 ) // including shadow areas

   // @ 1, ::aPopups[ nPopup ]:nCol, 5, ::aPopups[ nPopup ]:nCol + 5 BOX B_SINGLE ;
   //   COLOR ::cClrPopup

   ::nOpenPopup = nPopup

return nil

CLASS TDBPOPUP

   DATA nCol
   DATA cPrompt
   DATA cBackImage

   METHOD New( cPrompt, nCol )

ENDCLASS

METHOD New( cPrompt, nCol ) CLASS TDBPOPUP

   ::nCol    = nCol
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

