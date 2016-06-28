/* Copyright 2004-2005 Francesco Saverio Giudice <info@fsgiudice.com>
 * GD Class test file: tostring() demo
 */

#require "hbgd"

#define IMAGES_IN   "imgs_in" + hb_ps()
#define IMAGES_OUT  "imgs_out" + hb_ps()

PROCEDURE Main()

   LOCAL oI

   hb_vfDirMake( IMAGES_OUT )

   /* Load an image from file */
   oI := GDImage():LoadFromFile( IMAGES_IN + "conv_tst.jpg" )

   oI:SaveJpeg( IMAGES_OUT + "testfile.jpg" )

   // TraceLog( oI:ToString() )

   hb_MemoWrit( IMAGES_OUT + "teststring.jpg", oI:ToString() )

   oI:SaveToFile( IMAGES_OUT + "testtofile" )

   ? "Look at", IMAGES_OUT, "directory for output images"

   RETURN
