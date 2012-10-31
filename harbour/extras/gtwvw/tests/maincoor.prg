/*
 * $Id$
 */

/*
   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>

   Simplest demo program to show the difference of MainCoord Mode and
   Standard Mode of GTWVW.
*/

#include "inkey.ch"

PROCEDURE Main()

   SetColor( "N/W" )

   WVW_SetMainCoord( .F. )    // Standard Mode
   fillscreen()

   WVW_SetMainCoord( .T. )    // MainCoord Mode
   fillscreen()

   RETURN

PROCEDURE fillscreen()

   LOCAL i, j

   hb_Scroll()
   WVW_nOpenWindow( "Win2", 10, 10, 19, 69 )
   WVW_nOpenWindow( "Win3", 15, 15, 22, 75 )
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
   WVW_lCloseWindow()
   WVW_lCloseWindow()

   RETURN
