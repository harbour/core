/*
 * Strips HBDOC docs from source files.
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
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
      IF Right( aFile[ F_NAME ], 2 ) == ".c" .OR. ;
         Right( aFile[ F_NAME ], 4 ) == ".prg"

         hb_MemoWrit( aFile[ F_NAME ], __hbdoc_FilterOut( MemoRead( aFile[ F_NAME ] ) ) )
      ENDIF
   NEXT

   RETURN
