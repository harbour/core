/*
 * $Id$
 */

function test()

   CLS

   @ 0, 0, 15, 50 BOX "         " COLOR "W+/B"
   __BOX( 1, 1, 5, 7 ) // This does nothing
   __BOX( 1, 1, 5, 7, "" )
   __BOXD( 2, 2, 6, 8 )
   __BOXS( 3, 3, 7, 9 )

   DevPos( 12, 1 )

return nil
