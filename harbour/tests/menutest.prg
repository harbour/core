/*
 * $Id$
 */

#include "inkey.ch"

PROCEDURE Main()

   MEMVAR ptestvar

   LOCAL testvar

   SET KEY K_F8 TO RECURSE()

   CLS

   @  1, 10 PROMPT "Menu Item 1" MESSAGE "Menu Message 1"
   @  2, 10 PROMPT "Menu Item 2" MESSAGE "Menu Message 2"
   @  3, 10 PROMPT "Menu Item 3" MESSAGE "Menu Message 3"
   @  4, 10 PROMPT "Menu Item 4" MESSAGE "Menu Message 4"

   @  6, 10 SAY "Testing with LOCAL parameter"
   @  7, 10 SAY "Press F8 to recurse into MENU TO"

   MENU TO testvar

   @  9, 10 SAY "Your Choice = " + Str( testvar, 1 )

   Inkey( 0 )

   SET KEY K_F8 TO RECURSE()

   CLS

   @  1, 10 PROMPT "Menu Item 1" MESSAGE "Menu Message 1"
   @  2, 10 PROMPT "Menu Item 2" MESSAGE "Menu Message 2"
   @  3, 10 PROMPT "Menu Item 3" MESSAGE "Menu Message 3"
   @  4, 10 PROMPT "Menu Item 4" MESSAGE "Menu Message 4"

   @  6, 10 SAY "Testing with MEMVAR parameter"
   @  7, 10 SAY "Press F8 to recurse into MENU TO"

   MENU TO ptestvar

   @  9, 10 SAY "Your Choice = " + Str( ptestvar, 1 )

   RETURN

PROCEDURE RECURSE()

   LOCAL testvar

   SET KEY K_F8 TO

   @  6, 10 SAY "                                "

   @  1, 50 PROMPT "Menu Item 1" MESSAGE "Menu Message 1"
   @  2, 50 PROMPT "Menu Item 2" MESSAGE "Menu Message 2"
   @  3, 50 PROMPT "Menu Item 3" MESSAGE "Menu Message 3"
   @  4, 50 PROMPT "Menu Item 4" MESSAGE "Menu Message 4"

   MENU TO testvar

   @  7, 10 SAY "Press F8 to recurse into MENU TO"

   @  9, 50 SAY "Your Choice = " + Str( testvar, 1 )

   SET KEY K_F8 TO RECURSE()

   RETURN
