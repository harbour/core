/*
 * $Id$
 */

/*
 * Copyright 2004-2005 Francesco Saverio Giudice <info@fsgiudice.com>
 *
 * Counter sample
 * usage:
 * counter <number to show>
 * i.e.: counter 34524
 */


#include "gd.ch"
#include "common.ch"

#define IMAGES_IN  "digits/"
#define IMAGES_OUT "images_out/"

#define DISPLAY_NUM  10

PROCEDURE Main( cValue, cBaseImage )

   LOCAL oI, oIDigits, nWidth, nHeight, nDigits, nNumWidth, oTemp
   //LOCAL black, white, blue, red, green, cyan, gray
   LOCAL white
   LOCAL aNumberImages := {}
   LOCAL n, nValue

   // A value if not passed
   DEFAULT cValue     TO Str( hb_RandomInt( 1, 10^DISPLAY_NUM ), DISPLAY_NUM )
   DEFAULT cBaseImage TO "57chevy.gif"

   IF !File( IMAGES_IN + cBaseImage )
      ? "ERROR: Base Image File '" + IMAGES_IN + cBaseImage + "' not found"
      QUIT
   ENDIF

   nValue := Val( cValue )

   // Fix num lenght
   IF nValue > 10^DISPLAY_NUM
      nValue := 10^DISPLAY_NUM
   ENDIF

   cValue := StrZero( nValue, DISPLAY_NUM )

   Tracelog( "Value", cValue )

   // To set fonts run this command:
   // for windows: SET GDFONTPATH=c:\windows\fonts
   // per linux  : export GDFONTPATH=/usr/share/fonts/default/TrueType

   // SET GDFONTPATH=c:\windows\fonts
   //IF GetEnv( "GDFONTPATH" ) == ""
   //   ? "Please set GDFONTPATH"
   //   ? "On Windows: SET GDFONTPATH=c:\windows\fonts"
   //   ? "On Linux  : export GDFONTPATH=/usr/share/fonts/default/TrueType"
   //   ?
   //ENDIF

   // Check output directory
   IF !ISDirectory( IMAGES_OUT )
      DirMake( IMAGES_OUT )
   ENDIF

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

   Tracelog( "nNumWidth, nWidth, nHeight, nDigits", nNumWidth, nWidth, nHeight, nDigits )

   /* extracts single digits */
   FOR n := 1 TO nDigits
       oTemp := oIDigits:Copy( (n - 1) * nNumWidth, 0, nNumWidth, nHeight )
       oTemp:SaveGif( IMAGES_OUT + StrZero( n-1, 2 ) + ".gif" )
       // Here I have to clone the image, otherwise on var destruction I loose
       // the image in memory
       aAdd( aNumberImages, oTemp:Clone() )
   NEXT

   /* Create counter image in memory */
   oI := GDImage( nNumWidth * DISPLAY_NUM, nHeight )  // the counter
   Tracelog( "Image dimensions: ", oI:Width(), oI:Height() )

   /* Allocate background */
   white := oI:SetColor( 255, 255, 255 )

   /* Allocate drawing color */
   //black := oI:SetColor( 0, 0, 0 )
   //blue  := oI:SetColor( 0, 0, 255 )
   //red   := oI:SetColor( 255, 0, 0 )
   //green := oI:SetColor( 0, 255, 0 )
   //cyan  := oI:SetColor( 0, 255, 255 )

   /* Draw rectangle */
   //oI:Rectangle( 0, 0, 200, 30, , blue )

   /* Draw Digits */
   FOR n := 1 TO Len( cValue )
       // Retrieve the number from array in memory
       oTemp := aNumberImages[ Val( cValue[ n ] ) + 1 ]:Clone()
       // Save it to show the number for a position
       oTemp:SaveGif( IMAGES_OUT + "Pos_" + StrZero( n, 2 ) + ".gif" )
       // Set the digit as tile that I have to use to fill position in counter
       oI:SetTile( oTemp )
       // Fill the position with the image digit
       oI:Rectangle( (n - 1) * nNumWidth, 0, (n - 1) * nNumWidth + nNumWidth, nHeight, TRUE, gdTiled )
   NEXT

   /* Write Final Counter Image */
   oI:SaveGif( IMAGES_OUT + "counter.gif" )

   /* Destroy images in memory */
   // Class does it automatically

   ?
   ? "Look at " + IMAGES_OUT + " folder for output images"
   ?

RETURN

