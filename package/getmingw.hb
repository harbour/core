#!/usr/bin/env hbmk2

/* Copyright 2015 Viktor Szakats (vszakats.net/harbour) */

/* Requires: hbrun */

#include "hbver.ch"

PROCEDURE Main()

   LOCAL cURL, cSUM, cFileName, tmp, cDst, cTarget

   IF ! hb_vfExists( "harbour.exe" )
      OutStd( "Error: This script has to be run from a Harbour binary installation." + hb_eol() )
      OutStd( "       Download it from:" + hb_eol() )
      OutStd( "          https://github.com/vszakats/harbour-core/releases/tag/v3.4.0dev" + hb_eol() )
      Inkey( 0 )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   SWITCH hb_Version( HB_VERSION_BITWIDTH )
   CASE 32
      cURL := "https://www.mirrorservice.org/sites/dl.sourceforge.net/pub/sourceforge/m/mi/mingw-w64/Toolchains targetting Win32/Personal Builds/mingw-builds/5.2.0/threads-posix/sjlj/i686-5.2.0-release-posix-sjlj-rt_v4-rev0.7z"
      cSUM := "76faee6e1644e3ba49a38b803413c75e5c9e0bf1716dcf862824439ad1a32773"
   CASE 64
      cURL := "https://www.mirrorservice.org/sites/dl.sourceforge.net/pub/sourceforge/m/mi/mingw-w64/Toolchains targetting Win64/Personal Builds/mingw-builds/5.2.0/threads-posix/sjlj/x86_64-5.2.0-release-posix-sjlj-rt_v4-rev0.7z"
      cSUM := "c0536c55a1d12882987afd0a9be377413eaf6cee105e921c949899fa9b308b35"
   ENDSWITCH

   IF HB_ISSTRING( cURL )

      OutStd( hb_StrFormat( "Downloading %1$d-bit hosted dual-target mingw...", ;
         hb_Version( HB_VERSION_BITWIDTH ) ) + hb_eol() )

      hb_vfClose( hb_vfTempFile( @cFileName,,, ".7z" ) )

      IF dl_file( cURL, cFileName )

         IF hb_SHA256( hb_MemoRead( cFileName ) ) == cSUM

            OutStd( "Checksum OK." + hb_eol() )

            cDst := hb_PathNormalize( cTarget := hb_DirBase() + ".." + hb_ps() + "comp" )

            OutStd( hb_StrFormat( "Unpacking to '%1$s'...", cDst ) + hb_eol() )

            IF hb_processRun( FNameEscape( hb_DirBase() + "7za.exe" ) + ;
                  " x -y" + ;
                  " " + FNameEscape( "-o" + cDst ) + ;
                  " " + FNameEscape( cFileName ),, @tmp, @tmp ) != 0

               hb_vfCopyFile( cFileName, tmp := "mingw.7z" )
               OutStd( hb_StrFormat( "Error: Unpacking. Please unpack '%1$s' manually to '%2$s'.", tmp, cTarget ) + hb_eol() )
            ENDIF
         ELSE
            OutStd( "Error: Checksum mismatch - corrupted download. Please retry." + hb_eol() )
         ENDIF
      ELSE
         OutStd( "Error: Downloading MinGW." + hb_eol() )
      ENDIF

      IF cFileName != NIL
         hb_vfErase( cFileName )
      ENDIF
   ENDIF

   RETURN

STATIC FUNCTION FNameEscape( cFileName )
   RETURN '"' + cFileName + '"'

STATIC FUNCTION dl_file( cURL, cFileName )

   LOCAL lSuccess
#if defined( __PLATFORM__WINDOWS )
   LOCAL tmp
   LOCAL cScript
#endif

   IF hb_processRun( ;
      "curl " + ;
      "-fsS " + ;
      "-o " + FNameEscape( cFileName ) + " " + ;
      "-L --proto-redir =https " + ;
      Chr( 34 ) + StrTran( cURL, " ", "%20" ) + Chr( 34 ) ) >= 0

      RETURN .T.
   ENDIF

   lSuccess := .F.

#if defined( __PLATFORM__WINDOWS )

#pragma __cstream | LOCAL cJS := %s
var http = new ActiveXObject("WinHttp.WinHttpRequest.5.1");
http.Open("GET", "%1$s", false);
http.Send();
if(http.Status() == 200) {
   var f = new ActiveXObject("ADODB.Stream");
   f.type = 1; f.open(); f.write(http.responseBody);
   f.savetofile("%2$s", 2);
}
#pragma __endtext

   hb_vfClose( hb_vfTempFile( @cScript,,, ".tmp" ) )

   IF hb_MemoWrit( cScript, ;
         hb_StrFormat( cJS, ;
            cURL, ;
            StrTran( cFileName, "\", "\\" ) ) )

      IF hb_processRun( "cscript" + ;
            " //nologo" + ;
            " /e:jscript" + ;
            " " + FNameEscape( cScript ),, @tmp, @tmp ) == 0
         lSuccess := .T.
      ENDIF
   ENDIF

   hb_vfErase( cScript )
#endif

   RETURN lSuccess
