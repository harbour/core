/*
 * $Id$
 */

#require "hbgd"

#define IMAGES_OUT "imgs_out" + hb_ps()

PROCEDURE Main()

   LOCAL im, im2, col, i

   im := gdImageCreateTrueColor( 100, 100 )
   col := gdImageColorAllocate( im, 255, 255, 255 )

   FOR i := 0 TO 99 STEP 10
      gdImageLine( im, i, 0, i, 99, col )
      gdImageLine( im, 0, i, 99, i, col )
   NEXT

   gdImagePng( im, IMAGES_OUT + "gdtest.png" )

   im2 := gdImageSquareToCircle( im, 400 )

   gdImagePng( im2, IMAGES_OUT + "gdtest_squared.png" )

   gdImageDestroy( im )
   gdImageDestroy( im2 )

   RETURN
