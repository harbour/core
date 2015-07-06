/* Copyright 2015 Viktor Szakats (vszakats.net/harbour) */

/* Set timestamps for non-generated (repository) files
   before including them in a distributable package.
   For reproducible builds. */

#include "directry.ch"

// #define DEBUG

#ifdef DEBUG
   #translate _DEBUG( [<x,...>] ) => OutStd( <x> )
#else
   #translate _DEBUG( [<x,...>] ) =>
#endif

PROCEDURE Main( cGitRoot )

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

      _DEBUG( "mpkg_ts: date head:", tDateHEAD, hb_eol() )

      IF ! Empty( tDateHEAD ) .OR. ! lShallow

         OutStd( "! mpkg_ts: Timestamping repository files..." + hb_eol() )
         IF lShallow
            OutStd( "! mpkg_ts: Warning: Shallow repository, resorting to last commit timestamp." + hb_eol() )
         ENDIF

         FOR EACH tmp IN { ;
            "bin/*.bat", ;
            "bin/*.hb", ;
            "doc/*.txt", ;
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
   ELSE
      OutStd( "mpkg_ts: Error: Repository not found:", cGitRoot + hb_eol() )
   ENDIF

   _DEBUG( "mpkg_ts: FINISH" + hb_eol() )

   RETURN

STATIC FUNCTION FNameEscape( cFileName )
   RETURN '"' + cFileName + '"'
