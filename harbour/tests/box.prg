/*
 * $Id$
 */

// Testing Harbour device management.

#include "box.ch"

PROCEDURE Main()

   DispBox( 1, 1, 5, 5, B_SINGLE + 'X', 'color not supported' )
   DispBox( 7, 7, 13, 72, B_DOUBLE + '.' )
   DispBox( 14, 14, 22, 22, B_SINGLE_DOUBLE )

   RETURN
