/*
 * HBDOC reader test
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
 */

#include "directry.ch"
#include "simpleio.ch"

PROCEDURE Main( cRoot )

   LOCAL aEntry
   LOCAL cName

   LOCAL aDir
   LOCAL cDir

   LOCAL aFile

   LOCAL tModified
   LOCAL aErrMsg
   LOCAL tmp

   IF HB_ISSTRING( cRoot )
      cDir := hb_DirSepAdd( cDir )
   ELSE
      /* Detect Harbour root */
      cRoot := "." + hb_ps()
      DO WHILE hb_vfDirExists( cRoot + ".." )
         IF hb_vfExists( cRoot + "README.md" ) .AND. ;
            hb_vfExists( cRoot + "LICENSE.txt" ) .AND. ;
            hb_vfExists( cRoot + "config" )
            EXIT
         ENDIF
         cRoot += ".." + hb_ps()
      ENDDO
   ENDIF

   ? "Root:", cRoot

   aDir := { cRoot }

   FOR EACH aFile IN hb_vfDirectory( cRoot + "contrib" + hb_ps() + hb_osFileMask(), "D" )
      IF "D" $ aFile[ F_ATTR ] .AND. ;
         !( aFile[ F_NAME ] == "." ) .AND. ;
         !( aFile[ F_NAME ] == ".." )
         AAdd( aDir, cRoot + "contrib" + hb_ps() + aFile[ F_NAME ] )
      ENDIF
   NEXT

   FOR EACH cDir IN aDir

      IF ! Empty( tModified := __hbdoc_DirLastModified( cDir ) )

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
            ? tmp
         NEXT

         IF ! Empty( aEntry )
            ? __hbdoc_SaveHBD( cName, aEntry ), cName, Len( aEntry ), tModified
         ENDIF
      ENDIF
   NEXT

   RETURN

STATIC FUNCTION DirGetName( cDir )

   LOCAL cName := hb_FNameName( hb_DirSepDel( cDir ) )

   IF Empty( cName ) .OR. cName == "." .OR. cName == ".."
      RETURN ""
   ENDIF

   RETURN cName
