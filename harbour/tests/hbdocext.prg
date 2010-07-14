/*
 * $Id$
 */

/*
 * HBDOC extractor from source
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://harbour-project.org
 *
 */

#include "directry.ch"
#include "simpleio.ch"

PROCEDURE Main()
   LOCAL aFile
   LOCAL cFile
   LOCAL cDst

   LOCAL cHdr := ;
      "/*" + hb_eol() +;
      " * $" + "Id" + "$" + hb_eol() +;
      " */" + hb_eol()

   FOR EACH aFile IN Directory( "*.*" )
      cFile := __hbdoc_ToSource( __hbdoc_FromSource( MemoRead( aFile[ F_NAME ] ) ) )
      IF ! Empty( cFile )
         cDst := FNameExtSet( aFile[ F_NAME ], ".txt" )
         IF ! hb_FileExists( cDst )
            ? "Saving", cDst
            hb_MemoWrit( cDst, cHdr + cFile )
         ENDIF
      ENDIF
   NEXT

   RETURN

STATIC FUNCTION FNameExtSet( cFileName, cExt )
   LOCAL cDir, cName

   hb_FNameSplit( cFileName, @cDir, @cName )

   RETURN hb_FNameMerge( cDir, cName, cExt )
