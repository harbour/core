/*
 * TIP MIME type test
 * This test tries to detect the MIME type of a given file
 */

#require "hbtip"

PROCEDURE Main( cFileName )

   IF ! HB_ISSTRING( cFileName )
      ? hb_StrFormat( "Usage: %1$s <file to test>", hb_ProgName() )
   ELSEIF hb_FileExists( cFileName )
      ? cFileName + ":", tip_FileMimeType( cFileName )
   ELSE
      ? "File", cFileName, "doesn't exist."
   ENDIF

   RETURN
