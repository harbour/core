/*
 * $Id$
 */

function main()

   CLS

   @ 0, 0, 15, 50 BOX "         " COLOR "W+/B"
   MESSAGE( [@ 0, 0, 15, 50 BOX "         " COLOR "W+/B"] )

   __BOX( 1, 1, 5, 7 )
   MESSAGE( [__BOX( 1, 1, 5, 7 )] )

   __BOX( 1, 1, 5, 7, "X" )
   MESSAGE( [__BOX( 1, 1, 5, 7, "X" )] )

   __BOXD( 2, 2, 6, 8 )
   MESSAGE( [__BOXD( 2, 2, 6, 8 )] )

   __BOXS( 3, 3, 7, 9 )
   MESSAGE( [__BOXS( 3, 3, 7, 9 )] )

return nil

procedure MESSAGE( cText )
   @ 16,0 CLEAR TO 16,79
   @ 16,0 SAY cText
   OUTSTD( CHR( 7 ) )
   INKEY( 0 )
return