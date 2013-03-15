/*
 * $Id$
 */

// Testing Harbour device management.

#include "box.ch"

PROCEDURE Main()

   DispBox( 1, 1, 5, 5, HB_B_SINGLE_UNI + "X", "color not supported" )
   DispBox( 7, 7, 13, 72, HB_B_DOUBLE_UNI + "." )
   DispBox( 14, 14, 22, 22, HB_B_SINGLE_DOUBLE_UNI )

   RETURN
