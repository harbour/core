#!/usr/bin/env hbmk2

/* Copyright 2015-2016 Viktor Szakats (vszakats.net/harbour) */

/* Requires: hbmk2 or hbrun */

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
      cURL := "https://www.mirrorservice.org/sites/dl.sourceforge.net/pub/sourceforge/m/mi/mingw-w64/Toolchains targetting Win32/Personal Builds/mingw-builds/5.3.0/threads-posix/sjlj/i686-5.3.0-release-posix-sjlj-rt_v4-rev0.7z"
      cSUM := "870d50cfab3c8df9da1b890691b60bda2ccf92f122121dc75f11fe1612c6aff7"
      EXIT
   CASE 64
      cURL := "https://www.mirrorservice.org/sites/dl.sourceforge.net/pub/sourceforge/m/mi/mingw-w64/Toolchains targetting Win64/Personal Builds/mingw-builds/5.3.0/threads-posix/sjlj/x86_64-5.3.0-release-posix-sjlj-rt_v4-rev0.7z"
      cSUM := "ec28b6640ad4f183be7afcd6e9c5eabb24b89729ca3fec7618755555b5d70c19"
      EXIT
   ENDSWITCH

   IF HB_ISSTRING( cURL )

      OutStd( hb_StrFormat( "Downloading %1$d-bit hosted dual-target mingw...", ;
         hb_Version( HB_VERSION_BITWIDTH ) ) + hb_eol() )

      hb_vfClose( hb_vfTempFile( @cFileName,,, ".7z" ) )

      IF hb_processRun( ;
         "curl " + ;
         "-fsS " + ;
         "-o " + FNameEscape( cFileName ) + " " + ;
         "-L --proto-redir =https " + ;
         Chr( 34 ) + StrTran( cURL, " ", "%20" ) + Chr( 34 ) ) >= 0

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

      hb_vfErase( cFileName )
   ENDIF

   RETURN

STATIC FUNCTION FNameEscape( cFileName )
   RETURN '"' + cFileName + '"'
