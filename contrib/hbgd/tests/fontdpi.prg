/* Copyright 2005 Francesco Saverio Giudice <info@fsgiudice.com>
 * Graphic font DPI demo
 */

#require "hbgd"

#define IMAGES_IN   "imgs_in" + hb_ps()
#define IMAGES_OUT  "imgs_out" + hb_ps()

PROCEDURE Main()

   LOCAL oI := GDImage():Create( 600, 300 )

   hb_DirCreate( IMAGES_OUT )

   oI:Rectangle( 0, 0, 600, 300, .T., oI:SetColor( 255, 255, 255 ) )

   oI:SetColor( 0, 0, 90 )
   oI:SetFontName( "Arial" )
   oI:SetFontPitch( 10 )

   // Resolution: 96 dpi, default
   oI:SayFreeType( 10, 100, "GD_RESOLUTION:  96 dpi" )

   // Resolution: 150 dpi, using parameter 12
   oI:SayFreeType( 10, 150, "GD_RESOLUTION: 150 dpi",,,,,,,, 150 )

   // Resolution: 300 dpi, using parameter 12
   oI:SayFreeType( 10, 200, "GD_RESOLUTION: 300 dpi",,,,,,,, 300 )

   oI:SavePng(  IMAGES_OUT + "testdpi.png" )
   oI:SaveJpeg( IMAGES_OUT + "testdpi.jpg" )
   oI:SaveGif(  IMAGES_OUT + "testdpi.gif" )

   RETURN
