/*
 * $Id$
 */

/*
 * Copyright 2004-2005 Francesco Saverio Giudice <info@fsgiudice.com>
 *
 * GD Class test file
 */

#require "hbgd"

#include "gd.ch"
#include "simpleio.ch"

#define IMAGES_IN  "imgs_in" + hb_ps()
#define IMAGES_OUT "imgs_out" + hb_ps()

PROCEDURE Main()

   LOCAL black, blue, red, green, cyan, gray
   LOCAL color
   LOCAL oI, oI2, oI3, oI4, nThick, n, nSecs
   LOCAL oI5
   LOCAL oB

/*
   // Check output directory
   IF ! hb_DirExists( IMAGES_OUT )
      DirMake( IMAGES_OUT )
   ENDIF
*/

   /* Create an image in memory */
   oI := GDImage():Create( 200, 200 )

   /* Load an image in memory from file */
   oI2 := GDImage():LoadFromJpeg( IMAGES_IN + "conv_tst.jpg" )
   oI5 := GDImage():LoadFromJpeg( IMAGES_IN + "conv_tst.jpg" )

   /* Now work on first empty image */

   /* Allocate drawing color */
   black := oI:SetColor( 0, 0, 0 )
   blue  := oI:SetColor( 0, 0, 255 )
   red   := oI:SetColor( 255, 0, 0 )
   green := oI:SetColor( 0, 255, 0 )
   cyan  := oI:SetColor( 0, 255, 255 )

   /* Draw rectangle */
   oI:Rectangle( 0, 0, 199, 199, .T. , cyan )
   oI:Rectangle( 0, 0, 199, 199, , black )

   oI:SetColor( blue )

   /* Draw pixel */
   oI:SetPixel( 50, 5 )

   /* Draw lines */
   oI:Line( 0, 0, 199, 199, blue )
   oI:DashedLine( 0, 199, 199, 0, blue )
   nThick := oI:SetThickness( 5 )
   oI:Line( 50, 150, 100, 150 )
   oI:SetThickness( nThick )

   oI:AddStyle( red )
   oI:AddStyle( red )
   oI:AddStyle( red )
   oI:AddStyle( gdTransparent )
   oI:AddStyle( gdTransparent )
   oI:AddStyle( gdTransparent )
   oI:SetStyle()
   oI:Line( 50, 180, 100, 180, gdStyled )

   oI:ResetStyles()
   oI:AddStyle( black )
   oI:AddStyle( gdTransparent )
   oI:SetStyle()
   oI:Line( 50, 185, 100, 185, gdStyled )


   /* Draw polygons */
   oI:AddPoint( 10, 10 )
   oI:AddPoint( 70, 10 )
   oI:AddPoint( 80, 60 )
   oI:Polygon()

   oI:ResetPoints()
   oI:AddPoint( 160, 180 )
   oI:AddPoint( 170, 110 )
   oI:AddPoint( 150, 160 )
   oI:Polygon( , .T. , green )

   /* Draw an arc */
   oI:Arc( 50, 50, 40, 40, 30, 190, , red )
   oI:Circle( 50, 150, 45, .T., green )
   oI:Ellipse( 120, 120, 50, 20, , green )

   /* Draw a character. */
   oI:SetFontLarge()
   ? "Font Dims", oI:GetFontWidth(), oI:GetFontHeight()
   oI:SetColor( black )
   //__OutDebug( "Font", font )
   oI:Say( 0, 0, "Test" )
   oI:Say( 0, 15, "P" )
   oI:Say( 0, 30, "W" )

   oI:SayVertical( 70, 90, "Test" )
   oI:SayVertical( 70, 15, "P" )
   oI:SayVertical( 70, 30, "W" )

   oI:SayFreeType( 20, 30, "Test", "arial", 24, 15 )
   oI:SayFreeType( 40, 70, "Test2" )


   /* Set Clip Rectangle */
   oI:SetClippingArea( 25, 25, 75, 75 )

   /* Query functions */

   color := oI:GetPixel( oI:Width() / 2, oI:Height() / 2 )
   ? "Pixel Color is: ", color
   ? "RGB Values: ", oI:Red( color ), oI:Green( color ), oI:Blue( color )
   ? "Alpha Value: ",  oI:Alpha( color )

   /* Write Images on files */
   oI:SavePng( IMAGES_OUT + "rect.png" )
   oI2:SavePng( IMAGES_OUT + "test.png" )
   oI2:SaveJpeg( IMAGES_OUT + "test.jpg" )
   oI2:SaveGif( IMAGES_OUT + "test.gif" )
   //oI2:SaveWBmp( IMAGES_OUT + "vale1.bmp", black )

   /* test copy functions */

   //oI3 := GDImage():CreateTrueColor( oI2:Width * 2, oI2:Height * 2 )
   //oI2:CopyResampled( 0, 0, oI2:Width, oI2:Height, 0, 0, oI3:Width, oI3:Height, oI3 )
   //oI3:SaveJpeg("vale2.jpg")


   nSecs := Seconds()
   ? "start copy zoomed"
   oI3 := oI2:CopyZoomed( 150 )
   ? "end", Seconds() - nSecs
   nSecs := Seconds()
   ? "start save"
   oI3:SaveJpeg( IMAGES_OUT + "zoom.jpg" )
   ? "end", Seconds() - nSecs

   nSecs := Seconds()
   ? "start clone & zoom"
   oI2:Clone():Zoom( 200 )
   ? "end", Seconds() - nSecs

   nSecs := Seconds()
   ? "start clone"
   oI4 := oI5:Clone()
   ? "end", Seconds() - nSecs

   nSecs := Seconds()
   ? "start zoom"
   oI4:Zoom( 200 )
   ? "end", Seconds() - nSecs

   //__OutDebug( oI2:pImage )
   //oI4:SetFontGiant()
   gray := oI4:SetColor( 30, 30, 30 )
   blue := oI4:SetColor( 0, 0, 200 )

   //oI4:SetColor( black )
   //oI4:Say( 100, 10, "Valentina" )
   #if defined( __PLATFORM__UNIX )
      oI4:SayFreeType( oI4:CenterWidth(), oI4:CenterHeight(), "GD power", "arib____", 40, 45 )
   #else
      nSecs := Seconds()
      ? "start write"
      FOR n := 0 TO 350 STEP 10
         oI4:SayFreeType( oI4:CenterWidth(), oI4:CenterHeight(), "             GD Font Power", "arial", 20, n )
      NEXT
      ? "end", Seconds() - nSecs
      oI4:SetTransparent( blue )
      oI4:SayFreeType( oI4:CenterWidth() - 4, oI4:CenterHeight() + 4, "GD", "verdana", 70, n, gray )
      oI4:SayFreeType( oI4:CenterWidth(), oI4:CenterHeight(), "GD", "verdana", 70, n, blue )
   #endif
   oI4:SaveJpeg( IMAGES_OUT + "writing.jpg" )


   //oI4 := __ObjClone( oI2 )
   oI4 := oI2:Clone()

   nSecs := Seconds()
   ? "start rotate outside"
   oI2:Rotate( 45 )
   ? "end", Seconds() - nSecs
   oI2:SaveJpeg( IMAGES_OUT + "rotateout.jpg" )

   nSecs := Seconds()
   ? "start rotate inside"
   oI4:RotateInside( 45 )
   ? "end", Seconds() - nSecs
   //oI2:CopyRotated( , , , , , , 90, oI4 )
   oI4:SaveJpeg( IMAGES_OUT + "rotatein.jpg" )


   oI5:Zoom( 40 )
   //oI5:Rotate( 90 )
   blue := oI5:SetColor( 0, 0, 200 )
   oI5:SayFreeType( oI5:CenterWidth(), oI5:CenterHeight(), "GD", "verdana", 20, 0, blue )
   oI5:SaveJpeg( IMAGES_OUT + "gd_zoom.jpg" )


   oI5 := GDChart():New( 400, 400 )
   // Define piece colors
   blue  := oI5:SetColor( 0, 0, 200 )
   gray  := oI5:SetColor( 30, 30, 30 )
   green := oI5:SetColor( 0, 250, 0 )
   red   := oI5:SetColor( 250, 0, 0 )

   // Load an image as brush
   oB := GDImage():LoadFromGif( IMAGES_IN + "harbour.gif" )
   oB:Zoom( 15 )

   //oI5:Circle( 200, 200, oI5:Width() )
   //oI5:Line( 0, 200, 200, 200 )

   oI5:AddDef( "FONTPITCH", "GIANT" )

   oI5:SetData( { ;
      { "LABEL" => "One"  , "VALUE" => 10, "COLOR"  => blue , "FILLED" => .T. , "EXTRUDE" => 40/*, "TILE" => oB*/ }, ;
      { "LABEL" => "Two"  , "VALUE" => 35, "COLOR"  => gray , "FILLED" => .T. , "FONT" => { "NAME" => "Verdana", "PITCH" => 12, "ANGLE" => 0, "COLOR" => red }  }, ;
      { "LABEL" => "Three", "VALUE" => 55, "COLOR"  => green, "FILLED" => .T. }, ;
      { "LABEL" => "Four" , "VALUE" => 55, "FILLED" => .T.  , "TILE"   => oB }, ;
      { "LABEL" => "Five" , "VALUE" => 55, "COLOR"  => red  , "FILLED" => .T. , "EXTRUDE" => 20 }, ;
      { "LABEL" => "Six"  , "VALUE" => 55, "FILLED" => .T.  , "TILE"   => oB }, ;
      { "LABEL" => "Seven", "VALUE" => 55, "FILLED" => .T.  , "COLOR"  => green } ;
      } )

   //oI5:VerticalBarChart()
   oI5:PieChart()


   oI5:SaveJpeg( IMAGES_OUT + "pie.jpg" )

   oI5 := GDChart():New( 640, 480 )
   // Define piece colors
   blue  := oI5:SetColor( 0, 0, 200 )
   gray  := oI5:SetColor( 30, 30, 30 )
   green := oI5:SetColor( 0, 250, 0 )
   red   := oI5:SetColor( 250, 0, 0 )

   // Load an image as brush
   oB := GDImage():LoadFromJpeg( IMAGES_IN + "conv_tst.jpg" )
   oB:Zoom( 15 )

   //   oI5:AddDef( "MAXVALUE", 150 )
   oI5:AddDef( "AXISPICT", "@E 999999" )
   oI5:AddDef( "FONTPITCH", "GIANT" )
   oI5:AddDef( "COLOR", blue )

   //oI5:AddSeries( "LABEL"  => "Primo",;
   //               "VALUES" => { 10, 23, 54, 11, 32, 25 }, ;
   //               "COLOR"  => blue )

/*
   oI5:SetData( { ;
                  { "LABEL" => "One", "VALUE" => 1000, "COLOR" => blue, "FILLED" => .T., "EXTRUDE" => 40 },;
                  { "LABEL" => "Two", "VALUE" => 3500, "COLOR" => gray, "FILLED" => .T., "FONT" => { "NAME" => "Verdana", "PITCH" => 12, "ANGLE" => 0, "COLOR" => red }  },;
                  { "LABEL" => "Three", "VALUE" => 5500, "COLOR" => green, "FILLED" => .T. }, ;
                  { "LABEL" => "Four", "VALUE" => 6500, "FILLED" => .T., "TILE" => oB }, ;
                  { "LABEL" => "Five", "VALUE" => 3400, "FILLED" => .T., "COLOR" => green }, ;
                  { "LABEL" => "Six", "VALUE" => 10000 }, ;
                  { "LABEL" => "Seven", "VALUE" => 0, "FILLED" => .T., "COLOR" => red }, ;
                  { "LABEL" => "Eight", "VALUE" => -2200 }, ;
                  { "LABEL" => "Nine", "VALUE" => -3600, "COLOR" => blue, "FILLED" => .T. } ;
                } )
*/


   oI5:SetData( { ;
      { "LABEL" => "One", "VALUE" => 10, "COLOR" => blue, "FILLED" => .T. , "EXTRUDE" => 40/*, "TILE" => oB*/ }, ;
      { "LABEL" => "Two", "VALUE" => 35, "COLOR" => gray, "FILLED" => .T. , "FONT" => { "NAME" => "Verdana", "PITCH" => 12, "ANGLE" => 0, "COLOR" => red }  }, ;
      { "LABEL" => "Three", "VALUE" => 55, "COLOR" => green, "FILLED" => .T. }, ;
      { "LABEL" => "Four", "VALUE" => 65, "FILLED" => .T. , "TILE" => oB }, ;
      { "LABEL" => "Five", "VALUE" => 34, "FILLED" => .T. , "COLOR" => green }, ;
      { "LABEL" => "Six", "VALUE" => 100 }, ;
      { "LABEL" => "Seven", "VALUE" => 0, "FILLED" => .T. , "COLOR" => red }, ;
      { "LABEL" => "Eight", "VALUE" => - 0 }, ;
      { "LABEL" => "Nine", "VALUE" => - 0, "COLOR" => blue, "FILLED" => .T. } ;
      } )


   oI5:Clone():VerticalBarChart():SaveJpeg( IMAGES_OUT + "vertbars.jpg" )

   oI5:Clone():HorizontalBarChart():SaveJpeg( IMAGES_OUT + "horzbars.jpg" )

   oI5:LineChart()
   oI5:SaveJpeg( IMAGES_OUT + "hystogram1.jpg" )

   //oI4 := GDImage():CreateTrueColor( oI2:Width * 2, oI2:Height * 2 )
   //oI2:CopyResampled( 0, 0, oI2:Width, oI2:Height, 0, 0, oI2:Width, oI2:Height, oI4 )
   //oI2:CopyResampled( 0, 0, oI2:Width, oI2:Height, oI4:CenterWidth(), oI4:CenterHeight(), oI2:Width, oI2:Height, oI4 )
   //oI4:SaveJpeg("vale3.jpg")

   /* Destroy images in memory */
   // Class does it auto

   ?
   ? "Look at " + IMAGES_OUT + " folder for output images"
   ?

   RETURN
