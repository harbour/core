/*
 * Strips HBDOC docs from source files.
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

   FOR EACH aFile IN Directory( hb_osFileMask() )
      IF hb_FNameExt( aFile[ F_NAME ] ) == ".c" .OR. ;
         hb_FNameExt( aFile[ F_NAME ] ) == ".prg"
         hb_MemoWrit( aFile[ F_NAME ], __hbdoc_FilterOut( MemoRead( aFile[ F_NAME ] ) ) )
      ENDIF
   NEXT

   RETURN
