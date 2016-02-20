#!/usr/bin/env hbmk2

/* Copyright 2015-2016 Viktor Szakats (vszakats.net/harbour) */

#include "hbver.ch"

PROCEDURE Main()

   LOCAL pkg, cFileName, tmp, cDst, cTarget, cCBase, cCOpt
   LOCAL cDirBase := hb_FNameDir( hbshell_ScriptName() )
   LOCAL cOldDir := hb_cwd( cDirBase )

   LOCAL aPkg := {}

   IF hb_vfExists( cDirBase + "harbour.exe" )

      cCBase := "https://downloads.sourceforge.net"; cCOpt := "-L"

      IF hb_vfExists( cDirBase + ".." + hb_ps() + "BUILD-mingw.txt" )
         SWITCH hb_Version( HB_VERSION_BITWIDTH )
         CASE 32
            AAdd( aPkg, { ;
               "dsc" => "! Downloading 32-bit hosted dual-target (multilib) mingw...", ;
               "url" => cCBase + "/mingw-w64/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/5.3.0/threads-posix/sjlj/i686-5.3.0-release-posix-sjlj-rt_v4-rev0.7z", ;
               "sum" => "870d50cfab3c8df9da1b890691b60bda2ccf92f122121dc75f11fe1612c6aff7", ;
               "fil" => "mingw" } )
            EXIT
         CASE 64
            AAdd( aPkg, { ;
               "dsc" => "! Downloading 64-bit hosted dual-target (multilib) mingw...", ;
               "url" => cCBase + "/mingw-w64/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/5.3.0/threads-posix/sjlj/x86_64-5.3.0-release-posix-sjlj-rt_v4-rev0.7z", ;
               "sum" => "ec28b6640ad4f183be7afcd6e9c5eabb24b89729ca3fec7618755555b5d70c19", ;
               "fil" => "mingw" } )
            EXIT
         ENDSWITCH
      ELSE
         IF hb_vfExists( cDirBase + ".." + hb_ps() + "BUILD-mingw32.txt" )
            AAdd( aPkg, { ;
               "dsc" => "! Downloading 32-bit hosted 32-bit-target mingw...", ;
               "url" => cCBase + "/mingw-w64/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/5.3.0/threads-posix/dwarf/i686-5.3.0-release-posix-dwarf-rt_v4-rev0.7z", ;
               "sum" => "6e067b2917583e9c654b611263d5d5e8c3215b67d76d55fa3f5f484f16f0f0b6", ;
               "fil" => "mingw32" } )
         ENDIF
         IF hb_vfExists( cDirBase + ".." + hb_ps() + "BUILD-mingw64.txt" )
            AAdd( aPkg, { ;
               "dsc" => "! Downloading 64-bit hosted 64-bit-target mingw...", ;
               "url" => cCBase + "/mingw-w64/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/5.3.0/threads-posix/seh/x86_64-5.3.0-release-posix-seh-rt_v4-rev0.7z", ;
               "sum" => "7f0e1f081d173b4a98bde3f9d1a90daf391219e6738f1f40120336b40545f090", ;
               "fil" => "mingw64" } )
         ENDIF
      ENDIF

      FOR EACH pkg IN aPkg

         IF HB_ISSTRING( pkg[ "url" ] )

            OutStd( pkg[ "dsc" ] + hb_eol() )

            hb_vfClose( hb_vfTempFile( @cFileName,,, ".7z" ) )

            IF hb_processRun( "curl" + ;
                  " -fsS" + ;
                  " -o " + FNameEscape( cFileName ) + ;
                  " " + cCOpt + ;
                  " " + pkg[ "url" ] ) == 0

               IF Empty( pkg[ "sum" ] ) .OR. hb_SHA256( hb_MemoRead( cFileName ) ) == pkg[ "sum" ]

                  IF Empty( pkg[ "sum" ] )
                     OutStd( "! Warning: Checksum not verified." + hb_eol() )
                  ELSE
                     OutStd( "! Checksum OK." + hb_eol() )
                  ENDIF

                  cDst := hb_PathNormalize( cTarget := cDirBase + ".." + hb_ps() + "comp" )

                  OutStd( hb_StrFormat( "! Unpacking to '%1$s'...", cDst ) + hb_eol() )

                  IF hb_processRun( "7za" + ;
                        " x -y" + ;
                        " " + FNameEscape( "-o" + cDst ) + ;
                        " " + FNameEscape( cFileName ),, @tmp, @tmp ) != 0

                     hb_vfCopyFile( cFileName, tmp := pkg[ "fil" ] + ".7z" )
                     OutStd( hb_StrFormat( "! Error: Unpacking. Please unpack '%1$s' manually to '%2$s'.", tmp, cTarget ) + hb_eol() )
                  ENDIF
               ELSE
                  OutStd( "! Error: Checksum mismatch - corrupted download. Please retry." + hb_eol() )
               ENDIF
            ELSE
               OutStd( "! Error: Downloading MinGW." + hb_eol() )
            ENDIF

            hb_vfErase( cFileName )
         ENDIF
      NEXT
   ENDIF

   IF Empty( aPkg )
      OutStd( "! Error: This script has to be run from a Harbour binary installation." + hb_eol() )
      OutStd( "         Download from:" + hb_eol() )
      OutStd( "            https://github.com/vszakats/harbour-core/releases/tag/v_HB_VF_DEF_" + hb_eol() )
      Inkey( 0 )
      ErrorLevel( 1 )
   ENDIF

   hb_cwd( cOldDir )

   RETURN

STATIC FUNCTION FNameEscape( cFileName )
   RETURN '"' + cFileName + '"'
