/*
 * Strips VCS id headers from source files
 *
 * Copyright 2013 Viktor Szakats (vszakats.net/harbour)
 * www - http://harbour-project.org
 *
 */

#pragma -w3
#pragma -km+
#pragma -ko+

#include "directry.ch"

PROCEDURE Main()

   LOCAL aFile
   LOCAL cExt
   LOCAL tmp

   LOCAL cHdr1 := ;
      "/*" + hb_eol() + ;
      " * $" + "Id" + "$" + hb_eol() + ;
      " */" + hb_eol()

   LOCAL cHdr2 := ;
      "/*" + hb_eol() + ;
      " *  $" + "Id" + "$" + hb_eol() + ;
      " */" + hb_eol()

   LOCAL cHdr3 := ;
      "#" + hb_eol() + ;
      "# $" + "Id" + "$" + hb_eol() + ;
      "#" + hb_eol()

   LOCAL cHdr4 := ;
      "@rem" + hb_eol() + ;
      "@rem $" + "Id" + "$" + hb_eol() + ;
      "@rem" + hb_eol()

   LOCAL cHdr5 := ;
      "rem" + hb_eol() + ;
      "rem $" + "Id" + "$" + hb_eol() + ;
      "rem" + hb_eol()

   LOCAL cHdr6 := ;
      ";" + hb_eol() + ;
      "; $" + "Id" + "$" + hb_eol() + ;
      ";" + hb_eol()

   LOCAL cHdr7 := ;
      "'" + hb_eol() + ;
      "' $" + "Id" + "$" + hb_eol() + ;
      "'" + hb_eol()

   LOCAL hReplace := { ;
      cHdr1 =>, ;
      cHdr2 =>, ;
      cHdr3 =>, ;
      cHdr4 =>, ;
      cHdr5 =>, ;
      cHdr6 =>, ;
      cHdr7 => }

   FOR EACH aFile IN Directory( hb_osFileMask() )

      cExt := hb_FNameExt( aFile[ F_NAME ] )

      IF Empty( cExt ) .OR. ;
         "|" + cExt + "|" $ "|.c|.h|.hb|.prg|.hbm|.hbp|.hbc|.ini|.bat|.sh|.vbs|.def|.api|.ch|.txt|.mk|"

         tmp := hb_StrReplace( MemoRead( aFile[ F_NAME ] ), hReplace )

         IF ! "|" + cExt + "|" $ "|.hbm|.hbp|.hbc|.txt|"
            IF hb_LeftEq( tmp, hb_eol() + "//" ) .OR. ;
               hb_LeftEq( tmp, hb_eol() + "/*" ) .OR. ;
               hb_LeftEq( tmp, hb_eol() + ";" ) .OR. ;
               hb_LeftEq( tmp, hb_eol() + "#" )
               tmp := SubStr( tmp, Len( hb_eol() ) + 1 )
            ENDIF
         ENDIF

         hb_MemoWrit( aFile[ F_NAME ], tmp )
      ENDIF
   NEXT

   RETURN
