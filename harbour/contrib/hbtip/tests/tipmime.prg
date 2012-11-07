/*
 * $Id$
 */

/******************************************
 * TIP test
 * MIME type test
 *
 * This test tries to detect the mime type of a give file.
 *
 * Usage:
 * tipmime filename
 ******************************************/

#require "hbtip"

PROCEDURE Main( cFileName )

   IF Empty( cFileName )
      ?
      ? "Usage: mimetest <file to test>"
      ?
      QUIT
   ENDIF

   IF ! hb_FileExists( cFileName )
      ?
      ? "File", cFileName, "is not valid"
      ?
      QUIT
   ENDIF

   ? cFileName + ":", tip_FileMimeType( cFileName )
   ?

   RETURN
