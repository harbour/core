/*
 * $Id$
 */

/*
 * Copyright 2004-2005 Francesco Saverio Giudice <info@fsgiudice.com>
 *
 * GD Class test file: tostring() demo
 */


#include "gd.ch"
#include "common.ch"

#define IMAGES_IN  "images_in/"
#define IMAGES_OUT "images_out/"

PROCEDURE Main()

   LOCAL im, im2
   LOCAL black, white, blue, red, green, cyan, gray
   LOCAL aClip, color, font, aRect
   LOCAL oI, oI2, oI3, oI4, nThick, n, nSecs
   LOCAL oI5
   LOCAL oB

   // Check output directory
   IF !ISDirectory( IMAGES_OUT )
      DirMake( IMAGES_OUT )
   ENDIF

   /* Load an image from file */
   oI := GDImage():LoadFromFile( IMAGES_IN + "xharbour.jpg" )

   oI:SaveJpeg( IMAGES_OUT + "testfile.jpg" )

   //Tracelog( oI:ToString() )

   MemoWrit( IMAGES_OUT + "teststring.jpg", oI:ToString() )

   oI:SaveToFile( IMAGES_OUT + "testtofile" )

   ?
   ? "Look at " + IMAGES_OUT + " folder for output images"
   ?

RETURN

