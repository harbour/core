/*
 * Copyright 2004-2005 Francesco Saverio Giudice <info@fsgiudice.com>
 *
 * Counter sample
 * usage:
 * counter <number to show>
 * i.e.: counter 34524
 */

#require "hbgd"

/* Some digits images from:
   http://www.digitmania.holowww.com/all.html */

#define IMAGES_IN  "digits" + hb_ps()
#define IMAGES_OUT "imgs_out" + hb_ps()

#define DISPLAY_NUM  10

PROCEDURE Main( cValue, cBaseImage )

   LOCAL oI, oIDigits, nWidth, nHeight, nDigits, nNumWidth, oTemp

#if 0
   LOCAL black, blue, red, green, cyan, gray
#endif
   LOCAL aNumberImages := {}
   LOCAL n, nValue

   // A value if not passed
   hb_default( @cValue, Str( hb_RandomInt( 1, 10 ^ DISPLAY_NUM ), DISPLAY_NUM ) )
   hb_default( @cBaseImage, "57chevy.gif" )

   IF ! hb_FileExists( IMAGES_IN + cBaseImage )
      ? "ERROR: Base Image File '" + IMAGES_IN + cBaseImage + "' not found"
      QUIT
   ENDIF

   nValue := Val( cValue )

   // Fix num length
   IF nValue > 10 ^ DISPLAY_NUM
      nValue := 10 ^ DISPLAY_NUM
   ENDIF

   cValue := StrZero( nValue, DISPLAY_NUM )

   ? "Value = ", cValue

   // Check output directory
#if 0
   IF ! hb_DirExists( IMAGES_OUT )
      DirMake( IMAGES_OUT )
   ENDIF
#endif

   /* Load a digits image in memory from file */
   oIDigits := GDImage():LoadFromGif( IMAGES_IN + cBaseImage )

   /* Get single number images */

   // Get dimensions
   nWidth  := oIDigits:Width()
   nHeight := oIDigits:Height()

   // Check base digits image
   DO CASE
   CASE nWidth % 10 == 0   // 0..9 digits
      nDigits := 10
   CASE nWidth % 11 == 0   // 0..9 :
      nDigits := 11
   CASE nWidth % 13 == 0   // 0..9 : am pm
      nDigits := 13
   OTHERWISE
      ? "Error on digits image"
   ENDCASE
   nNumWidth := nWidth / nDigits

   ? "nNumWidth, nWidth, nHeight, nDigits = ", nNumWidth, nWidth, nHeight, nDigits

   /* extracts single digits */
   FOR n := 1 TO nDigits
      oTemp := oIDigits:Copy( ( n - 1 ) * nNumWidth, 0, nNumWidth, nHeight )
      oTemp:SaveGif( IMAGES_OUT + StrZero( n - 1, 2 ) + ".gif" )
      // Here I have to clone the image, otherwise on var destruction I loose
      // the image in memory
      AAdd( aNumberImages, oTemp:Clone() )
   NEXT

   /* Create counter image in memory */
   oI := GDImage():New( nNumWidth * DISPLAY_NUM, nHeight )  // the counter
   ? "Image dimensions: ", oI:Width(), oI:Height()

   /* Allocate drawing color */
#if 0
   black := oI:SetColor( 0, 0, 0 )
   blue  := oI:SetColor( 0, 0, 255 )
   red   := oI:SetColor( 255, 0, 0 )
   green := oI:SetColor( 0, 255, 0 )
   cyan  := oI:SetColor( 0, 255, 255 )

   /* Draw rectangle */
   oI:Rectangle( 0, 0, 200, 30, , blue )
#endif

   /* Draw Digits */
   FOR n := 1 TO Len( cValue )
      // Retrieve the number from array in memory
      oTemp := aNumberImages[ Val( SubStr( cValue, n, 1 ) ) + 1 ]:Clone()
      // Save it to show the number for a position
      oTemp:SaveGif( IMAGES_OUT + "Pos_" + StrZero( n, 2 ) + ".gif" )
      // Set the digit as tile that I have to use to fill position in counter
      oI:SetTile( oTemp )
      // Fill the position with the image digit
      oI:Rectangle( ( n - 1 ) * nNumWidth, 0, ( n - 1 ) * nNumWidth + nNumWidth, nHeight, .T., gdTiled )
   NEXT

   /* Write Final Counter Image */
   oI:SaveGif( IMAGES_OUT + "counter.gif" )

   /* Destroy images in memory */
   // Class does it automatically

   ?
   ? "Look at " + IMAGES_OUT + " folder for output images"
   ?

   RETURN
