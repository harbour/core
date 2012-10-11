/*
 * $Id$
 */

/*
 * Copyright 2004-2005 Francesco Saverio Giudice <info@fsgiudice.com>
 *
 * GD Class test file: tostring() demo
 */

#require "hbgd"

#include "gd.ch"
#include "simpleio.ch"

#define IMAGES_IN  "imgs_in" + hb_ps()
#define IMAGES_OUT "imgs_out" + hb_ps()

PROCEDURE Main()

   LOCAL oI

   /*
   // Check output directory
   IF ! hb_DirExists( IMAGES_OUT )
      DirMake( IMAGES_OUT )
   ENDIF
   */

   /* Load an image from file */
   oI := GDImage():LoadFromFile( IMAGES_IN + "conv_tst.jpg" )

   oI:SaveJpeg( IMAGES_OUT + "testfile.jpg" )

   //Tracelog( oI:ToString() )

   hb_MemoWrit( IMAGES_OUT + "teststring.jpg", oI:ToString() )

   oI:SaveToFile( IMAGES_OUT + "testtofile" )

   ?
   ? "Look at " + IMAGES_OUT + " folder for output images"
   ?

   RETURN
