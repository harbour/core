/*
 * $Id$
 */

/*
 * HBDOC reader test
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 */

#include "directry.ch"

PROCEDURE Main( cRoot )
   LOCAL aEntry
   LOCAL cName

   LOCAL aDir
   LOCAL cDir

   LOCAL aFile

   LOCAL aErrMsg
   LOCAL tmp

   IF ! HB_ISSTRING( cRoot )
      /* Detect Harbour root */
      cRoot := "." + hb_ps()
      DO WHILE hb_DirExists( cRoot + ".." )
         IF hb_FileExists( cRoot + "INSTALL" ) .AND. ;
            hb_FileExists( cRoot + "COPYING" ) .AND. ;
            hb_DirExists( cRoot + "config" )
            EXIT
         ENDIF
         cRoot += ".." + hb_ps()
      ENDDO
   ELSE
      cDir := hb_DirSepAdd( cDir )
   ENDIF

   OutStd( "Root: " + cRoot + hb_eol() )

   aDir := { cRoot }

   FOR EACH aFile IN Directory( cRoot + "contrib" + hb_ps() + hb_osFileMask(), "D" )
      IF "D" $ aFile[ F_ATTR ] .AND. ;
         !( aFile[ F_NAME ] == "." ) .AND. ;
         !( aFile[ F_NAME ] == ".." )
         AAdd( aDir, cRoot + "contrib" + hb_ps() + aFile[ F_NAME ] )
      ENDIF
   NEXT

   FOR EACH cDir IN aDir

      cName := DirGetName( cDir )
      IF Empty( cName )
         cName := "harbour"
      ENDIF

      aErrMsg := {}

      aEntry := __hbdoc_LoadDir( cDir, cName, aErrMsg )

      /* TODO: apply code formatting, HBDOC section and content validation here.
               It's also possible to create output generators at this point.
               These generators should simply parse the list of entry hashes
               and spit out output in selected end-user format, like .html
               or .pdf */

      FOR EACH tmp IN aErrMsg
         OutStd( tmp + hb_eol() )
      NEXT

      IF ! Empty( aEntry )
         OutStd( __hbdoc_savehbd( cName, aEntry ), cName, Len( aEntry ), hb_eol() )
      ENDIF
   NEXT

   RETURN

STATIC FUNCTION DirGetName( cDir )
   LOCAL cName

   cDir := hb_DirSepDel( cDir )

   hb_FNameSplit( cDir,, @cName )

   IF Empty( cName ) .OR. cName == "." .OR. cName == ".."
      RETURN ""
   ENDIF

   RETURN cName
