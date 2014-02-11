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

   LOCAL im1, im2, im3
   LOCAL black, trans

   LOCAL hFile
   LOCAL cFile := IMAGES_OUT + "anim2.gif"

   // Check output directory
   IF ! hb_DirExists( IMAGES_OUT )
      DirMake( IMAGES_OUT )
   ENDIF

   hFile := FCreate( IMAGES_OUT + "anim1.gif" )           /* Open output file in binary mode */

   im1 := gdImageCreate( 100, 100 )                       /* Create the image */

   black := gdImageColorAllocate( im1, 0, 0, 0 )          /* Allocate drawing color */
   trans := gdImageColorAllocate( im1, 1, 1, 1 )          /* Allocate transparent color for animation compression */

   gdImageRectangle( im1, 0, 0, 10, 10, black )           /* Draw rectangle */

   gdImageGifAnimBegin( im1, hFile, 1, 3 )                /* Write GIF header. Use global color map. Loop a few times */
   gdImageGifAnimBegin( im1, cFile, 1, 3 )

   gdImageGifAnimAdd( im1, hFile, 0, 0, 0, 100, 1, NIL )  /* Write the first frame.  No local color map.  Delay == 1s */
   gdImageGifAnimAdd( im1, cFile, 0, 0, 0, 100, 1, NIL )

   im2 := gdImageCreate( 100, 100 )                       /* construct the second frame */

   gdImageColorAllocate( im2, 255, 255, 255 )             /* Allocate background to make it white */
   gdImagePaletteCopy( im2, im1 )                         /* Make sure the palette is identical */
   gdImageRectangle( im2, 0, 0, 15, 15, black )           /* Draw something */
   gdImageColorTransparent ( im2, trans )                 /* Allow animation compression with transparent pixels */

   gdImageGifAnimAdd( im2, hFile, 0, 0, 0, 100, 1, im1 )  /* Add the second frame */
   gdImageGifAnimAdd( im2, cFile, 0, 0, 0, 100, 1, im1 )


   im3 := gdImageCreate( 100, 100 )                       /* construct the second frame */

   gdImageColorAllocate( im3, 255, 255, 255 )             /* Allocate background to make it white */
   gdImagePaletteCopy( im3, im1 )                         /* Make sure the palette is identical */
   gdImageRectangle( im3, 0, 0, 15, 20, black )           /* Draw something */
   gdImageColorTransparent ( im3, trans )                 /* Allow animation compression with transparent pixels */

   gdImageGifAnimAdd( im3, hFile, 0, 0, 0, 100, 1, im2 )  /* Add the third frame, compressing against the second one */
   gdImageGifAnimAdd( im3, cFile, 0, 0, 0, 100, 1, im2 )

   gdImageGifAnimEnd( hFile )                             /* Write the end marker. Is the same as the following: putc( ";", out ); */
   gdImageGifAnimEnd( cFile )

   FClose( hFile )

   ? "Look at", IMAGES_OUT, "folder for output images"

   RETURN
