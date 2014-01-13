/*
 * Copyright 2004-2005 Francesco Saverio Giudice <info@fsgiudice.com>
 *
 * GD API anim gif test file - from GD official documentation, adapted to porting
 *
 * This test shows how handle either file handle than file name
 */

#require "hbgd"

#define IMAGES_IN  "imgs_in" + hb_ps()
#define IMAGES_OUT "imgs_out" + hb_ps()

PROCEDURE Main()

   LOCAL im, im2, im3
   LOCAL black, trans
   LOCAL hFile

#if 0
   // Check output directory
   IF ! hb_DirExists( IMAGES_OUT )
      DirMake( IMAGES_OUT )
   ENDIF
#endif

   /* Create the image */
   im := gdImageCreate( 100, 100 )

   /* Allocate background */
   // white := gdImageColorAllocate(im, 255, 255, 255)

   /* Allocate drawing color */
   black := gdImageColorAllocate( im, 0, 0, 0 )

   /* Allocate transparent color for animation compression */
   trans := gdImageColorAllocate( im, 1, 1, 1 )

   /* Draw rectangle */
   gdImageRectangle( im, 0, 0, 10, 10, black )

   /* Open output file in binary mode */
   hFile := FCreate( IMAGES_OUT + "anim1.gif" )
   /* Write GIF header.  Use global color map.  Loop a few times */
   gdImageGifAnimBegin( im, hFile, 1, 3 )
   gdImageGifAnimBegin( im, IMAGES_OUT + "anim2.gif", 1, 3 )
   /* Write the first frame.  No local color map.  Delay = 1s */
   gdImageGifAnimAdd( im, hFile, 0, 0, 0, 100, 1, NIL )
   gdImageGifAnimAdd( im, IMAGES_OUT + "anim2.gif", 0, 0, 0, 100, 1, NIL )
   /* construct the second frame */
   im2 := gdImageCreate( 100, 100 )
   /* Allocate background to make it white */
   gdImageColorAllocate( im2, 255, 255, 255 )
   /* Make sure the palette is identical */
   gdImagePaletteCopy ( im2, im )
   /* Draw something */
   gdImageRectangle( im2, 0, 0, 15, 15, black )
   /* Allow animation compression with transparent pixels */
   gdImageColorTransparent ( im2, trans )
   /* Add the second frame */
   gdImageGifAnimAdd( im2, hFile, 0, 0, 0, 100, 1, im )
   gdImageGifAnimAdd( im2, IMAGES_OUT + "anim2.gif", 0, 0, 0, 100, 1, im )
   /* construct the second frame */
   im3 := gdImageCreate( 100, 100 )
   /* Allocate background to make it white */
   gdImageColorAllocate( im3, 255, 255, 255 )
   /* Make sure the palette is identical */
   gdImagePaletteCopy ( im3, im )
   /* Draw something */
   gdImageRectangle( im3, 0, 0, 15, 20, black )
   /* Allow animation compression with transparent pixels */
   gdImageColorTransparent ( im3, trans )
   /* Add the third frame, compressing against the second one */
   gdImageGifAnimAdd( im3, hFile, 0, 0, 0, 100, 1, im2 )
   gdImageGifAnimAdd( im3, IMAGES_OUT + "anim2.gif", 0, 0, 0, 100, 1, im2 )
   /* Write the end marker */
   /* gdImageGifAnimEnd(out); is the same as the following: */
   // putc (";", out);
   gdImageGifAnimEnd( hFile )
   gdImageGifAnimEnd( IMAGES_OUT + "anim2.gif" )
   /* Close file */
   FClose( hFile )

   ?
   ? "Look at", IMAGES_OUT, "folder for output images"

   RETURN
