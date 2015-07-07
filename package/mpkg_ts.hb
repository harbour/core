/* Copyright 2015 Viktor Szakats (vszakats.net/harbour) */

/* Set timestamps for non-generated (repository) files
   before including them in a distributable package.
   For reproducible builds. */

#include "directry.ch"
#include "fileio.ch"

// #define DEBUG

#ifdef DEBUG
   #translate _DEBUG( [<x,...>] ) => OutStd( <x> )
#else
   #translate _DEBUG( [<x,...>] ) =>
#endif

PROCEDURE Main( cMode, cGitRoot, cBinMask )

   LOCAL tmp, aFiles, file, cStdOut, tDate, tDateHEAD
   LOCAL lShallow

   _DEBUG( "mpkg_ts: BEGIN" + hb_eol() )

   cGitRoot := hb_DirSepAdd( hb_defaultValue( cGitRoot, "." ) ) + ".git"
   IF hb_DirExists( cGitRoot )

      _DEBUG( "mpkg_ts: cwd:", hb_cwd() + hb_eol() )
      _DEBUG( "mpkg_ts: git:", cGitRoot + hb_eol() )

      hb_processRun( "git" + ;
         " " + FNameEscape( "--git-dir=" + cGitRoot ) + ;
         " rev-parse --abbrev-ref HEAD",, @cStdOut )

      hb_processRun( "git" + ;
         " " + FNameEscape( "--git-dir=" + cGitRoot ) + ;
         " rev-list " + hb_StrReplace( cStdOut, Chr( 13 ) + Chr( 10 ) ) + ;
         " --count",, @cStdOut )

      lShallow := Val( hb_StrReplace( cStdOut, Chr( 13 ) + Chr( 10 ) ) ) < 2000

      hb_processRun( "git log -1 --format=format:%ci",, @cStdOut )

      tDateHEAD := hb_CToT( cStdOut, "yyyy-mm-dd", "hh:mm:ss" )

      IF ! Empty( tDateHEAD )
         tDateHEAD -= ( ( ( iif( SubStr( cStdOut, 21, 1 ) == "-", -1, 1 ) * 60 * ;
                          ( Val( SubStr( cStdOut, 22, 2 ) ) * 60 + ;
                            Val( SubStr( cStdOut, 24, 2 ) ) ) ) - hb_UTCOffset() ) / 86400 )
      ENDIF

      _DEBUG( "mpkg_ts: date HEAD:", tDateHEAD, hb_eol() )

      SWITCH Lower( cMode := hb_defaultValue( cMode, "" ) )
      CASE "eh"

         tmp := hb_DirSepToOS( hb_defaultValue( cBinMask, "" ) )

         OutStd( "! mpkg_ts: Resetting build times in executable headers in", tmp + hb_eol() )

         FOR EACH file IN Directory( tmp )
            win_ExeResetTimestamp( hb_FNameDir( tmp ) + file[ F_NAME ], tDateHEAD )
         NEXT

         EXIT

      CASE "ts"

         IF ! Empty( tDateHEAD ) .OR. ! lShallow

            OutStd( "! mpkg_ts: Timestamping repository files..." + hb_eol() )
            IF lShallow
               OutStd( "! mpkg_ts: Warning: Shallow repository, resorting to last commit timestamp." + hb_eol() )
            ENDIF

            FOR EACH tmp IN { ;
               "bin/*.bat", ;
               "bin/*.hb", ;
               "doc/*.txt", ;
               "addons/*.txt", ;
               "contrib/", ;
               "extras/", ;
               "include/", ;
               "src/3rd/", ;
               "tests/" }

               tmp := hb_DirSepToOS( tmp )
               FOR EACH file IN iif( Empty( hb_FNameName( tmp ) ), hb_DirScan( tmp ), Directory( tmp ) )
                  file := hb_FNameDir( tmp ) + file[ F_NAME ]

                  /* NOTE: To extract proper timestamps we need full commit history */
                  IF lShallow
                     hb_FSetDateTime( file, tDateHEAD )
                  ELSE
                     hb_processRun( "git" + ;
                        " " + FNameEscape( "--git-dir=" + cGitRoot ) + ;
                        " log -1 --format=format:%ci" + ;
                        " " + FNameEscape( file ),, @cStdOut )

                     tDate := hb_CToT( cStdOut, "yyyy-mm-dd", "hh:mm:ss" )

                     IF ! Empty( tDate )
                        tDate -= ( ( ( iif( SubStr( cStdOut, 21, 1 ) == "-", -1, 1 ) * 60 * ;
                                     ( Val( SubStr( cStdOut, 22, 2 ) ) * 60 + ;
                                       Val( SubStr( cStdOut, 24, 2 ) ) ) ) - hb_UTCOffset() ) / 86400 )
                        hb_FSetDateTime( file, tDate )
                     ENDIF
                  ENDIF
               NEXT
            NEXT
         ENDIF

         /* Reset directory timestamps to last commit */
         IF ! Empty( tDateHEAD )
            OutStd( "! mpkg_ts: Timestamping directories..." + hb_eol() )
            FOR EACH file IN hb_DirScan( "." + hb_ps(),, "D" ) DESCEND
               IF "D" $ file[ F_ATTR ] .AND. ;
                  !( hb_FNameNameExt( file[ F_NAME ] ) == "." .OR. ;
                     hb_FNameNameExt( file[ F_NAME ] ) == ".." )
                  hb_FSetDateTime( file[ F_NAME ], tDateHEAD )
               ENDIF
            NEXT
         ENDIF

         EXIT

      OTHERWISE
         OutStd( "mpkg_ts: Error: Wrong mode:", "'" + cMode + "'" + hb_eol() )
      ENDSWITCH
   ELSE
      OutStd( "mpkg_ts: Error: Repository not found:", cGitRoot + hb_eol() )
   ENDIF

   _DEBUG( "mpkg_ts: FINISH" + hb_eol() )

   RETURN

STATIC FUNCTION FNameEscape( cFileName )
   RETURN '"' + cFileName + '"'

STATIC FUNCTION win_ExeResetTimestamp( cFileName, tDateHdr )

   LOCAL lSuccess := .F.

   LOCAL fhnd, nPEPos, cSignature, tDate, nDateHdr, cDateHdr

   hb_FGetDateTime( cFileName, @tDate )

   IF ( fhnd := FOpen( cFileName, FO_READWRITE + FO_EXCLUSIVE ) ) != F_ERROR
      IF ( cSignature := hb_FReadLen( fhnd, 2 ) ) == "MZ"
         FSeek( fhnd, 0x003C, FS_SET )
         nPEPos := Bin2W( hb_FReadLen( fhnd, 2 ) ) + ;
                   Bin2W( hb_FReadLen( fhnd, 2 ) ) * 0x10000
         FSeek( fhnd, nPEPos, FS_SET )
         IF !( hb_FReadLen( fhnd, 2 ) == "PE" )
            nPEPos := NIL
         ENDIF
      ELSEIF cSignature == "PE"
         nPEPos := 0
      ENDIF
      IF nPEPos != NIL

         nDateHdr := Int( ( Max( hb_defaultValue( tDateHdr, hb_SToT() ), hb_SToT( "19700101000000" ) ) - hb_SToT( "19700101000000" ) ) * 86400 )

         FSeek( fhnd, nPEPos + 0x0008, FS_SET )
         IF ( Bin2W( hb_FReadLen( fhnd, 2 ) ) + Bin2W( hb_FReadLen( fhnd, 2 ) ) * 0x10000 ) != nDateHdr

            cDateHdr := hb_BChar( nDateHdr % 256 ) + ;
                        hb_BChar( nDateHdr / 256 )
            nDateHdr /= 0x10000
            cDateHdr += hb_BChar( nDateHdr % 256 ) + ;
                        hb_BChar( nDateHdr / 256 )

            IF FSeek( fhnd, nPEPos + 0x0008, FS_SET ) == nPEPos + 0x0008 .AND. ;
               FWrite( fhnd, cDateHdr ) == hb_BLen( cDateHdr )
               lSuccess := .T.
            ENDIF
         ELSE
            lSuccess := .T.
         ENDIF
      ENDIF
      FClose( fhnd )
   ENDIF

   IF lSuccess
      hb_FSetDateTime( cFileName, tDate )
   ENDIF

   RETURN lSuccess
