/* Copyright 2004-2005 Francesco Saverio Giudice <info@fsgiudice.com>
 * GD API test file
 */

#require "hbgd"

#define IMAGES_IN   "imgs_in" + hb_ps()
#define IMAGES_OUT  "imgs_out" + hb_ps()

PROCEDURE Main()

   LOCAL im
   LOCAL blue

   // Check output directory
   IF ! hb_DirExists( IMAGES_OUT )
      DirMake( IMAGES_OUT )
   ENDIF

   /* This sample shows differences on use of antiliased command between a
      palette based image and a true color image.
      Normally antialias works better with a true color image.

      From GD official documentation:

      Antialiased lines can be drawn on both truecolor and palette-based images.
      However, attempts to draw antialiased lines on highly complex palette-based backgrounds
      may not give satisfactory results, due to the limited number of colors available in the
      palette. Antialiased line-drawing on simple backgrounds should work well with palette-based
      images; otherwise create or fetch a truecolor image instead. */

   /* DRAW A LINE IN A PALETTE BASED IMAGE */
   im := gdImageCreatePalette( 100, 100 )                  /* First we create a true color image */

   /* First allocate color is Background color */
   gdImageFilledRectangle( im, 0, 0, 100, 100, gdImageColorAllocate( im, 0, 0, 0 ) )

   blue := gdImageColorAllocate( im, 0, 0, 255 )           /* set foreground color */
   gdImageLine( im, 0, 0, 99, 40, blue )                   /* Now we draw an aliased line */
   gdImageSetAntiAliased( im, blue )                       /* Then we set anti-alias color */
   gdImageLine( im, 0, 40, 99, 80, gdAntiAliased )         /* and re-draw the line in antialiased mode */
   gdImageJpeg( im, IMAGES_OUT + "antialiasedpal.jpg" )    /* saving the image */

   /* DRAW A LINE IN A TRUE COLOR IMAGE */
   im := gdImageCreateTrueColor( 100, 100 )                /* First we create a true color image */

   /* Background color (true color comes with black background, we have to fill it) */
   gdImageFilledRectangle( im, 0, 0, 100, 100, gdTrueColor( 255, 255, 255 ) )

   blue := gdImageColorAllocate( im, 0, 0, 255 )           /* set foreground color */
   gdImageLine( im, 0, 0, 99, 40, blue )                   /* Now we draw an aliased line */
   gdImageSetAntiAliased( im, blue )                       /* Then we set anti-alias color */
   gdImageLine( im, 0, 40, 99, 80, gdAntiAliased )         /* and re-draw the line in antialiased mode */
   gdImageJpeg( im, IMAGES_OUT + "antialiasedtrue.jpg" )   /* saving the image */

   ? "Look at", IMAGES_OUT, "folder for output images"

   RETURN
