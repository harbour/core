/*
 * HBDOC extractor from source
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
 * www - http://harbour-project.org
 *
 */

#pragma -w3
#pragma -km+
#pragma -ko+

#include "directry.ch"
#include "simpleio.ch"

PROCEDURE Main()

   LOCAL aFile
   LOCAL cFile
   LOCAL cDst

   LOCAL cHdr := ""

   FOR EACH aFile IN Directory( hb_osFileMask() )
      cFile := __hbdoc_ToSource( __hbdoc_FromSource( MemoRead( aFile[ F_NAME ] ) ) )
      IF ! Empty( cFile )
         cDst := hb_FNameExtSet( aFile[ F_NAME ], ".txt" )
         IF ! hb_FileExists( cDst )
            ? "Saving", cDst
            hb_MemoWrit( cDst, cHdr + cFile )
         ENDIF
      ENDIF
   NEXT

   RETURN
