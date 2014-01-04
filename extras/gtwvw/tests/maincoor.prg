/*
   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>

   Simplest demo program to show the difference of MainCoord Mode and
   Standard Mode of GTWVW.
*/

#require "gtwvw"

#include "inkey.ch"

PROCEDURE Main()

#if defined( __HBSCRIPT__HBSHELL ) .AND. defined( __PLATFORM__WINDOWS )
   hbshell_gtSelect( "GTWVW" )
#endif

   SetColor( "N/W" )

   wvw_SetMainCoord( .F. )    // Standard Mode
   fillscreen()

   wvw_SetMainCoord( .T. )    // MainCoord Mode
   fillscreen()

   RETURN

STATIC PROCEDURE fillscreen()

   LOCAL i, j

   hb_Scroll()
   wvw_nOpenWindow( "Win2", 10, 10, 19, 69 )
   wvw_nOpenWindow( "Win3", 15, 15, 22, 75 )
   DevPos( 0, 0 )
   ?? "I'm gonna fill this (" + hb_ntos( MaxRow() + 1 ) + "x" + hb_ntos( MaxCol() + 1 ) + ") screen"
   DevPos( 1, 0 )
   FOR i := 1 TO MaxRow() - 1
      FOR j := 0 TO MaxCol()
         ?? hb_ntos( j % 10 )
      NEXT
   NEXT
   ?? "Done. Press ESC to exit."
   DO WHILE Inkey( 0 ) != K_ESC
   ENDDO
   wvw_lCloseWindow()
   wvw_lCloseWindow()

   RETURN
