/*
 * $Id$
 */

/*
   sample on using Maximize Mode in gtwvw.
   budyanto@centrin.net.id
 */

#include "inkey.ch"

// this dimension will be used when user presses RESTORE button
STATIC s_nNormalMaxrow := 24
STATIC s_nNormalMaxcol := 79

STATIC s_lSizeReady := .F.

PROCEDURE Main()

   LOCAL ch

   // activate WVW_SIZE()
   s_lSizeReady := .T.

   // the biggest possible window
   SetColor( "N/W" )
   SetMode( wvw_maxmaxrow() + 1, wvw_maxmaxcol() + 1 )

   // enable MAXIMIZE button
   wvw_enablemaximize( 0, .T. )

   // set the window to MAXIMIZED state
   wvw_maximize( 0 )

   updatescr()
   DO WHILE ( ch := Inkey( 0 ) ) != K_ESC
      // refresh screen, probably in a new dimension
      // (You may alternatively call updatescr() from WVW_SIZE instead)
      updatescr()
   ENDDO

   RETURN

PROCEDURE diminfo()

   @ 0, 0 SAY "Window size: " + hb_ntos( MaxRow() + 1 ) + " x " + hb_ntos( MaxCol() + 1 ) + "   "

   RETURN

PROCEDURE updatescr()

   LOCAL i

   CLS
   FOR i := 0 TO MaxCol()
      @ 0, i SAY "T"
   NEXT
   FOR i := 0 TO MaxRow()
      @ i, 0 SAY "L"
   NEXT
   FOR i := 0 TO MaxCol()
      @ MaxRow(), i SAY "B"
//    @ Maxrow() - 1, i SAY Right( Transform( i, "999" ), 1 )
   NEXT
   FOR i := 0 TO MaxRow()
      @ i, MaxCol() SAY "R"
   NEXT
   @ Int( MaxRow() / 2 ) + 0, 2 SAY PadC( "Press MAXIMIZE/RESTORE button to change dimension", MaxCol() + 1 - 4 )
   @ Int( MaxRow() / 2 ) + 1, 2 SAY PadC( "Try also changing taskbar size/position", MaxCol() + 1 - 4 )
   @ Int( MaxRow() / 2 ) + 3, 2 SAY PadC( "Press any key to redraw screen", MaxCol() + 1 - 4 )
   @ Int( MaxRow() / 2 ) + 4, 2 SAY PadC( "Press ESC to quit", MaxCol() + 1 - 4 )
   diminfo()

   RETURN

// this function is called by gtwvw AFTER the size is changed
// WARNING: screen repainting is not performed completely by gtwvw at this point of call
FUNCTION WVW_SIZE( nWinNum, hWnd, message, wParam, lParam )

   LOCAL cScreen
   LOCAL lNeedReset := .F., ;
      maxsavedscrrow, maxsavedscrcol

   IF !s_lSizeReady
      // program is not ready to handle window resizing
      // (or this function is currently running)
      RETURN NIL
   ENDIF
   IF nWinNum != 0
      // only care about Main Window
      RETURN NIL
   ENDIF

   // avoid reentrance
   s_lSizeReady := .F.

   DO CASE
   CASE wParam == 2 // SIZE_MAXIMIZED
      // Alert( "MAXIMIZE" )
      // reset is required only if we are changing size
      lNeedReset := MaxCol() != wvw_maxmaxcol();
         .OR. MaxRow() != wvw_maxmaxrow()

      IF lNeedReset
         maxsavedscrrow := Min( Min( s_nNormalMaxrow, wvw_maxmaxrow() ), MaxRow() )
         maxsavedscrcol := Min( Min( s_nNormalMaxcol, wvw_maxmaxcol() ), MaxCol() )
         cScreen := SaveScreen( 0, 0, maxsavedscrrow, maxsavedscrcol )
         IF SetMode( wvw_maxmaxrow() + 1, wvw_maxmaxcol() + 1 ) // adjust maxrow() & maxcol()
            RestScreen( 0, 0, maxsavedscrrow, maxsavedscrcol, cScreen )
         ENDIF
         diminfo()  // updatescr()
      ENDIF
   CASE wParam == 0 // SIZE_RESTORED
      // Alert( "RESTORE" )
      lNeedReset := MaxCol() != s_nNormalMaxcol .OR. ;
         MaxRow() != s_nNormalMaxrow
      IF lNeedReset
         maxsavedscrrow := Min( s_nNormalMaxrow, MaxRow() )
         maxsavedscrcol := Min( s_nNormalMaxcol, MaxCol() )
         cScreen := SaveScreen( 0, 0, maxsavedscrrow, maxsavedscrcol )
         IF SetMode( s_nNormalMaxrow + 1, s_nNormalMaxcol + 1 )
            RestScreen( 0, 0, maxsavedscrrow, maxsavedscrcol, cScreen )
         ENDIF
         diminfo()  // updatescr()
      ENDIF
   OTHERWISE
      // do nothing
   ENDCASE

   // allow next call
   s_lSizeReady := .T.

   RETURN NIL
