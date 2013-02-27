/*
 * $Id$
 */

/******************************************
 * TIP test
 * MIME type test
 *
 * This test tries to detect the mime type of a give file.
 ******************************************/

#require "hbtip"

PROCEDURE Main( cFileName )

   IF Empty( cFileName )
      ?
      ? hb_StrFormat( "Usage: %1$s <file to test>", hb_ProgName() )
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
