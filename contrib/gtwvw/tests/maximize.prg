/* sample on using Maximize Mode in gtwvw. budyanto@centrin.net.id */

#require "gtwvw"

#include "inkey.ch"

// this dimension will be used when user presses RESTORE button
STATIC s_nNormalMaxrow := 24
STATIC s_nNormalMaxcol := 79

STATIC s_lSizeReady := .F.

PROCEDURE Main()

#if defined( __HBSCRIPT__HBSHELL ) .AND. defined( __PLATFORM__WINDOWS )
   hbshell_gtSelect( "GTWVW" )
#endif

   // activate WVW_SIZE()
   s_lSizeReady := .T.

   // the biggest possible window
   SetColor( "N/W" )
   SetMode( wvw_MaxMaxRow() + 1, wvw_MaxMaxCol() + 1 )

   // enable MAXIMIZE button
   wvw_EnableMaximize( 0, .T. )

   // set the window to MAXIMIZED state
   wvw_Maximize( 0 )

   updatescr()
   DO WHILE hb_keyStd( Inkey( 0 ) ) != K_ESC
      // refresh screen, probably in a new dimension
      // (You may alternatively call updatescr() from WVW_SIZE instead)
      updatescr()
   ENDDO

   RETURN

STATIC PROCEDURE diminfo()

   @ 0, 0 SAY "Window size: " + hb_ntos( MaxRow() + 1 ) + " x " + hb_ntos( MaxCol() + 1 ) + "   "

   RETURN

STATIC PROCEDURE updatescr()

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
#if 0
      @ MaxRow() - 1, i SAY Right( Str( i, 3 ), 1 )
#endif
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
PROCEDURE WVW_SIZE( nWinNum, hWnd, message, wParam, lParam )  /* must be a public function */

   LOCAL cScreen
   LOCAL lNeedReset
   LOCAL maxsavedscrrow, maxsavedscrcol

   HB_SYMBOL_UNUSED( message )
   HB_SYMBOL_UNUSED( hWnd )
   HB_SYMBOL_UNUSED( lParam )

   IF ! s_lSizeReady
      // program is not ready to handle window resizing
      // (or this function is currently running)
      RETURN
   ENDIF
   IF nWinNum != 0
      // only care about Main Window
      RETURN
   ENDIF

   // avoid reentrance
   s_lSizeReady := .F.

   DO CASE
   CASE wParam == 2 // SIZE_MAXIMIZED
      // Alert( "MAXIMIZE" )
      // reset is required only if we are changing size
      lNeedReset := MaxCol() != wvw_MaxMaxCol();
         .OR. MaxRow() != wvw_MaxMaxRow()

      IF lNeedReset
         maxsavedscrrow := Min( Min( s_nNormalMaxrow, wvw_MaxMaxRow() ), MaxRow() )
         maxsavedscrcol := Min( Min( s_nNormalMaxcol, wvw_MaxMaxCol() ), MaxCol() )
         cScreen := SaveScreen( 0, 0, maxsavedscrrow, maxsavedscrcol )
         IF SetMode( wvw_MaxMaxRow() + 1, wvw_MaxMaxCol() + 1 ) // adjust MaxRow() and MaxCol()
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

   RETURN
