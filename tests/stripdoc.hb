#!/usr/bin/env hbmk2

/* Copyright 2010 Viktor Szakats (vszakats.net/harbour) */

/* Strips HBDOC docs from source files */

#pragma -w3
#pragma -km+
#pragma -ko+

#include "directry.ch"

PROCEDURE Main()

   LOCAL aFile

   FOR EACH aFile IN hb_vfDirectory( hb_osFileMask() )
      IF hb_FNameExt( aFile[ F_NAME ] ) == ".c" .OR. ;
         hb_FNameExt( aFile[ F_NAME ] ) == ".prg"
         hb_MemoWrit( aFile[ F_NAME ], __hbdoc_FilterOut( MemoRead( aFile[ F_NAME ] ) ) )
      ENDIF
   NEXT

   RETURN
