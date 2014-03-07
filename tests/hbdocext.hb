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

PROCEDURE Main()

   LOCAL aFile
   LOCAL cFile
   LOCAL cDst

   FOR EACH aFile IN Directory( hb_osFileMask() )
      IF ! Empty( cFile := __hbdoc_ToSource( __hbdoc_FromSource( MemoRead( aFile[ F_NAME ] ) ) ) )
         IF ! hb_FileExists( cDst := hb_FNameExtSet( aFile[ F_NAME ], ".txt" ) )
            ? "Saving", cDst
            hb_MemoWrit( cDst, cFile )
         ENDIF
      ENDIF
   NEXT

   RETURN
