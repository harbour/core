//
// $Id$
//

#include "inkey.ch"

procedure main()

   memvar ptestvar

   local testvar

   set key K_F8 to RECURSE()

   clear screen

   @  1, 10 prompt 'Menu Item 1' message 'Menu Message 1'
   @  2, 10 prompt 'Menu Item 2' message 'Menu Message 2'
   @  3, 10 prompt 'Menu Item 3' message 'Menu Message 3'
   @  4, 10 prompt 'Menu Item 4' message 'Menu Message 4'

   @  6, 10 say 'Testing with LOCAL parameter'
   @  7, 10 say 'Press F8 to recurse into MENU TO'

   menu to testvar

   @  9, 10 say 'Your Choice = ' + str( testvar, 1 )

   Inkey(0)

   set key K_F8 to RECURSE()

   clear screen

   @  1, 10 prompt 'Menu Item 1' message 'Menu Message 1'
   @  2, 10 prompt 'Menu Item 2' message 'Menu Message 2'
   @  3, 10 prompt 'Menu Item 3' message 'Menu Message 3'
   @  4, 10 prompt 'Menu Item 4' message 'Menu Message 4'

   @  6, 10 say 'Testing with MEMVAR parameter'
   @  7, 10 say 'Press F8 to recurse into MENU TO'

   menu to ptestvar

   @  9, 10 say 'Your Choice = ' + str( ptestvar, 1 )

   return

procedure RECURSE()

   local testvar

   set key K_F8 to

   @  6, 10 say '                                '

   @  1, 50 prompt 'Menu Item 1' message 'Menu Message 1'
   @  2, 50 prompt 'Menu Item 2' message 'Menu Message 2'
   @  3, 50 prompt 'Menu Item 3' message 'Menu Message 3'
   @  4, 50 prompt 'Menu Item 4' message 'Menu Message 4'

   menu to testvar

   @  7, 10 say 'Press F8 to recurse into MENU TO'

   @  9, 50 say 'Your Choice = ' + str( testvar, 1 )

   set key K_F8 to RECURSE()

   return
