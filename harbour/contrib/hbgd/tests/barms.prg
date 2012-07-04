/*
 * $Id$
 */

#define IMAGES_OUT "imgs_out" + hb_ps()

PROCEDURE Main()

   LOCAL bar

   // Creating some Color (arguments are R, G, B)
   LOCAL black  := { 0, 0, 0 }
   LOCAL white  := { 255, 255, 255 }

// LOCAL blue   := { 0, 0, 255 }
// LOCAL yellon := { 255, 255, 128 }

// LOCAL red    := { 255, 0, 0 }

   LOCAL ccode13    := "P48WBQ7BX3M73X8V3WRT7F9JW"
   LOCAL ccode8     := "P48WBQ7BX3M73X8V3WRT7F9JW"
   LOCAL ccode128   := "P48WBQ7BX3M73X8V3WRT7F9JW"

// LOCAL nlower := 1 , nhight := 50

   /* here is the list of arguments
   1- Barcode Type 13,8  and 128
   */

   bar := TCode():New( 13 )

   /* Here is the list of the arguments
   1 - Thickness
   2 - Color of bars
   3 - Color of spaces
   4 - Resolution
   5 - Text Font (0-5)
   */
   bar:Configure( 70, black, white, 2, 1 )

   /* Here is the list of the arguments
   1 - Width
   2 - Height
   3 - Filename (empty : display on screen)
   4 - Background color */
   bar:CreateBar( 205, 105,, white )

   // 1 - code bar
   bar:Draw13( ccode13 )

   // Build image
   bar:Finish( 2 )

   // EAN8
   bar:= TCode():New( 8 )

   bar:Configure( 70, black, white, 2, 1 )

   bar:CreateBar( 154, 100,, white )

   bar:Draw8( ccode8 )

   bar:Finish( 8 )

   bar:ResizeImage()

   // EAN128
   bar:= TCode():New( 128 )

   bar:Configure( 50, black, white, 2, 1 )

   // output image
   //bar:out_img := "image_out/"

   bar:CreateBar( 559, 88, IMAGES_OUT + "Bar128", white )

   // 1- code bar
   // 2- barcode types A/B/C
   // A- Alphanumeric characters uppercase
   // B- Alphanumeric characters upper and lowercase
   // C- Numeric pairs of integer
   bar:Draw128( cCode128, "A" )

   bar:Finish( 8 )

   // BRAZIL-FEBRABAN
   bar := TCode():New( 25 )

   bar:Configure( 25, black, white, 1, 1,, .T. )

   // output image
   //bar:out_img := "febraban/"
   bar:CreateBar( 560, 60, IMAGES_OUT + "febraban", white )
   bar:DrawI25( "P48WBQ7BX3M73X8V3WRT7F9JW" )
   bar:Finish( 8 )

   RETURN
