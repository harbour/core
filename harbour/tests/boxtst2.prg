/*
 * $Id$
 */

// Test program for box and line drawing functions.
/* Harbour Project source code
   http://harbour-project.org/
   Donated to the public domain on 2001-08-22 by David G. Holm <dholm@jsd-llc.com>
*/

#include "box.ch"

PROCEDURE Main()

   LOCAL max_row, max_col, boxColor := "W+/B,N/BG", lineColor := "W+/R, N/BG"

   SetMode( 50, 80 )
   CLEAR SCREEN
   max_row := MaxRow()
   max_col := MaxCol()

   // Draw filled boxes centered around the four screen corners.
   DispBox( -10, -10, 10, 10, B_SINGLE + "X", boxColor )
   DispBox( -10, max_col - 10, 10, max_col + 10, B_SINGLE + "X", boxColor )
   DispBox( max_row - 10, - 10, max_row + 10, 10, B_SINGLE + "X", boxColor )
   DispBox( max_row - 10, max_col - 10, max_row + 10, max_col + 10, B_SINGLE + "X", boxColor )

   // Draw non-filled boxes around the filled boxes.
   DispBox( -15, -15, 15, 15, 1, boxColor )
   DispBox( -15, max_col - 15, 15, max_col + 15, 1, boxColor )
   DispBox( max_row - 15, - 15, max_row + 15, 15, 1, boxColor )
   DispBox( max_row - 15, max_col - 15, max_row + 15, max_col + 15, 1, boxColor )

   // Draw a box in the center, then two boxes off screen.

   DispBox( 20, 20, 25, 60, 2, boxColor )
   DispBox( -10, -10, -1, -1, 2, boxColor )
   DispBox( max_row + 1, max_col + 1, max_row + 10, max_col + 10, 2, boxColor )

   // Draw horizontal lines from off-screen to on-screen,
   // off-screen to off-screen, and on-screen to off-screen.
   DispBox( 1, -10, 1, 10, B_SINGLE, lineColor )
   DispBox( 2, -10, 2, max_col + 10, B_DOUBLE, lineColor )
   DispBox( 3, max_col - 10, 3, max_col + 10, B_SINGLE, lineColor )

   // Draw vertical lines from off-screen to on-screen,
   // off-screen to off-screen, and on-screen to off-screen.
   DispBox( -10, 1, 10, 1, B_SINGLE, lineColor )
   DispBox( -10, 2, max_row + 10, 2, B_DOUBLE, lineColor )
   DispBox( max_row - 10, 3, max_row + 10, 3, B_SINGLE, lineColor )

   Inkey( 5 )

   RETURN
