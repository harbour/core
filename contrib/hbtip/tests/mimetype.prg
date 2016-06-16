/* This test tries to detect the MIME type of a given file */

#require "hbtip"

PROCEDURE Main( cFileName )

   IF ! HB_ISSTRING( cFileName )
      ? hb_StrFormat( "Usage: %1$s <file to test>", hb_ProgName() )
   ELSEIF hb_vfExists( cFileName )
      ? cFileName
      ?
      ? "tip_MimeType()", tip_MimeType( hb_MemoRead( cFileName ), "application/octet-stream" )
      ? "tip_FileMimeType()", tip_FileMimeType( cFileName, "application/octet-stream" )
      ? "tip_FileNameMimeType()", tip_FileNameMimeType( cFileName, "application/octet-stream" )
   ELSE
      ? "File", cFileName, "doesn't exist."
   ENDIF

   RETURN
