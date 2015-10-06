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

PROCEDURE Main( cMode )

   LOCAL tmp, aFiles, file, cStdOut, tDate, tDateHEAD
   LOCAL cGitRoot, lShallow, cBinMask

   _DEBUG( __FILE__ + ": BEGIN" + hb_eol() )

   SWITCH Lower( cMode := hb_defaultValue( cMode, "" ) )
   CASE "nl"

      tmp := hb_DirSepToOS( hb_defaultValue( hb_PValue( 2 ), "" ) )

      OutStd( "! mpkg.hb: Converting newlines in", tmp + hb_eol() )

      FOR EACH file IN hb_vfDirectory( tmp )
         FileConvEOL( hb_FNameDir( tmp ) + file[ F_NAME ] )
      NEXT

      EXIT

   CASE "pe"

      tmp := hb_DirSepToOS( hb_defaultValue( hb_PValue( 3 ), "" ) )

      OutStd( "! mpkg.hb: Setting build times in executable headers of", tmp + hb_eol() )

      FOR EACH file IN hb_vfDirectory( tmp )
         /* Use a fixed date to change binaries only if their ingredients have changed */
         win_PESetTimestamp( hb_FNameDir( tmp ) + file[ F_NAME ] )
      NEXT

      EXIT

   CASE "ts"

      cGitRoot := hb_DirSepAdd( hb_DirSepToOS( hb_defaultValue( hb_PValue( 2 ), "." ) ) ) + ".git"
      IF hb_vfDirExists( cGitRoot )

         _DEBUG( __FILE__ + ": cwd:", hb_cwd() + hb_eol() )
         _DEBUG( __FILE__ + ": git:", cGitRoot + hb_eol() )

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

         IF Empty( tDateHEAD )
            OutStd( "! mpkg.hb: Error: Failed to obtain last commit timestamp." + hb_eol() )
         ELSE
            tDateHEAD -= ( ( ( iif( SubStr( cStdOut, 21, 1 ) == "-", -1, 1 ) * 60 * ;
                             ( Val( SubStr( cStdOut, 22, 2 ) ) * 60 + ;
                               Val( SubStr( cStdOut, 24, 2 ) ) ) ) - hb_UTCOffset() ) / 86400 )
         ENDIF

         _DEBUG( __FILE__ + ": date HEAD:", tDateHEAD, hb_eol() )

         IF ! Empty( tDateHEAD ) .OR. ! lShallow

            IF lShallow
               OutStd( "! mpkg.hb: Warning: Shallow repository, resorting to last commit timestamp." + hb_eol() )
            ENDIF

            OutStd( "! mpkg.hb: Timestamping repository files..." + hb_eol() )

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
               FOR EACH file IN iif( Empty( hb_FNameName( tmp ) ), hb_DirScan( tmp ), hb_vfDirectory( tmp ) )
                  file := hb_FNameDir( tmp ) + file[ F_NAME ]

                  /* NOTE: To extract proper timestamps we need full commit history */
                  IF lShallow
                     hb_vfTimeSet( file, tDateHEAD )
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
                        hb_vfTimeSet( file, tDate )
                     ENDIF
                  ENDIF
               NEXT
            NEXT
         ENDIF

         /* Reset directory timestamps to last commit */
         IF ! Empty( tDateHEAD )
            OutStd( "! mpkg.hb: Timestamping directories..." + hb_eol() )
            FOR EACH file IN hb_DirScan( "." + hb_ps(),, "D" ) DESCEND
               IF "D" $ file[ F_ATTR ] .AND. ;
                  !( hb_FNameNameExt( file[ F_NAME ] ) == "." .OR. ;
                     hb_FNameNameExt( file[ F_NAME ] ) == ".." )
                  hb_vfTimeSet( file[ F_NAME ], tDateHEAD )
               ENDIF
            NEXT
         ENDIF
      ELSE
         OutStd( __FILE__ + ": Error: Repository not found:", cGitRoot + hb_eol() )
      ENDIF

      EXIT

   CASE "ch"

      tmp := hb_DirSepToOS( hb_defaultValue( hb_PValue( 2 ), "" ) )

      OutStd( "! mpkg.hb: Calculating SHA-256 hash for", tmp + hb_eol() )

      FOR EACH file IN hb_vfDirectory( tmp )
         OutStd( hb_SHA256( hb_MemoRead( hb_FNameDir( tmp ) + file[ F_NAME ] ) ), hb_FNameDir( tmp ) + file[ F_NAME ] + hb_eol() )
      NEXT

      EXIT

   OTHERWISE
      OutStd( __FILE__ + ": Error: Wrong mode:", "'" + cMode + "'" + hb_eol() )
   ENDSWITCH

   _DEBUG( __FILE__ + ": FINISH" + hb_eol() )

   RETURN

STATIC FUNCTION FNameEscape( cFileName )
   RETURN '"' + cFileName + '"'

STATIC FUNCTION win_PESetTimestamp( cFileName, tDateHdr )

   LOCAL lModified := .F.

   LOCAL hFile, nPEPos, cSignature, tDate, nSections
   LOCAL nPEChecksumPos, nDWORD, cDWORD
   LOCAL tmp, tmp1

   IF Empty( tDateHdr )
      tDateHdr := hb_SToT( "20150101000000" )
   ENDIF

   hb_vfTimeGet( cFileName, @tDate )

   IF ( hFile := hb_vfOpen( cFileName, FO_READWRITE + FO_EXCLUSIVE ) ) != NIL
      IF ( cSignature := hb_vfReadLen( hFile, 2 ) ) == "MZ"
         hb_vfSeek( hFile, 0x003C, FS_SET )
         nPEPos := Bin2W( hb_vfReadLen( hFile, 2 ) ) + ;
                   Bin2W( hb_vfReadLen( hFile, 2 ) ) * 0x10000
         hb_vfSeek( hFile, nPEPos, FS_SET )
         IF !( hb_vfReadLen( hFile, 4 ) == "PE" + hb_BChar( 0 ) + hb_BChar( 0 ) )
            nPEPos := NIL
         ENDIF
      ELSEIF cSignature + hb_vfReadLen( hFile, 2 ) == "PE" + hb_BChar( 0 ) + hb_BChar( 0 )
         nPEPos := 0
      ENDIF
      IF nPEPos != NIL

         hb_vfSeek( hFile, 0x0002, FS_RELATIVE )

         nSections := Bin2W( hb_vfReadLen( hFile, 2 ) )

         nDWORD := Int( ( Max( hb_defaultValue( tDateHdr, hb_SToT() ), hb_SToT( "19700101000000" ) ) - hb_SToT( "19700101000000" ) ) * 86400 )

         IF hb_vfSeek( hFile, nPEPos + 0x0008, FS_SET ) == nPEPos + 0x0008

            cDWORD := hb_BChar( nDWORD % 0x100 ) + ;
                      hb_BChar( nDWORD / 0x100 )
            nDWORD /= 0x10000
            cDWORD += hb_BChar( nDWORD % 0x100 ) + ;
                      hb_BChar( nDWORD / 0x100 )

            IF !( hb_vfReadLen( hFile, 4 ) == cDWORD ) .AND. ;
               hb_vfSeek( hFile, nPEPos + 0x0008, FS_SET ) == nPEPos + 0x0008 .AND. ;
               hb_vfWrite( hFile, cDWORD ) == hb_BLen( cDWORD )
               lModified := .T.
            ENDIF

            IF hb_vfSeek( hFile, nPEPos + 0x0014, FS_SET ) == nPEPos + 0x0014

               nPEPos += 0x0018
               nPEChecksumPos := nPEPos + 0x0040

               IF Bin2W( hb_vfReadLen( hFile, 2 ) ) > 0x0058 .AND. ;
                  hb_vfSeek( hFile, nPEPos + 0x005C, FS_SET ) == nPEPos + 0x005C

                  nPEPos += 0x005C + ;
                            ( ( Bin2W( hb_vfReadLen( hFile, 2 ) ) + ;
                                Bin2W( hb_vfReadLen( hFile, 2 ) ) * 0x10000 ) * 8 ) + 4
                  IF hb_vfSeek( hFile, nPEPos, FS_SET ) == nPEPos
                     tmp1 := nPEPos
                     nPEPos := NIL
                     /* IMAGE_SECTION_HEADERs */
                     FOR tmp := 1 TO nSections
                        hb_vfSeek( hFile, tmp1 + ( tmp - 1 ) * 0x28, FS_SET )
                        /* IMAGE_EXPORT_DIRECTORY */
                        IF hb_vfReadLen( hFile, 8 ) == ".edata" + hb_BChar( 0 ) + hb_BChar( 0 )
                           hb_vfSeek( hFile, 0x000C, FS_RELATIVE )
                           nPEPos := Bin2W( hb_vfReadLen( hFile, 2 ) ) + ;
                                     Bin2W( hb_vfReadLen( hFile, 2 ) ) * 0x10000
                           EXIT
                        ENDIF
                     NEXT
                     IF nPEPos != NIL .AND. ;
                        hb_vfSeek( hFile, nPEPos + 0x0004, FS_SET ) == nPEPos + 0x0004
                        IF !( hb_vfReadLen( hFile, 4 ) == cDWORD ) .AND. ;
                           hb_vfSeek( hFile, nPEPos + 0x0004, FS_SET ) == nPEPos + 0x0004 .AND. ;
                           hb_vfWrite( hFile, cDWORD ) == hb_BLen( cDWORD )
                           lModified := .T.
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF

               /* Recalculate PE checksum */
               IF lModified
                  tmp := hb_vfSize( hFile )
                  hb_vfSeek( hFile, FS_SET, 0 )
                  nDWORD := win_PEChecksumCalc( hb_vfReadLen( hFile, tmp ), nPECheckSumPos )
                  IF hb_vfSeek( hFile, nPEChecksumPos ) == nPEChecksumPos
                     cDWORD := hb_BChar( nDWORD % 0x100 ) + ;
                               hb_BChar( nDWORD / 0x100 )
                     nDWORD /= 0x10000
                     cDWORD += hb_BChar( nDWORD % 0x100 ) + ;
                               hb_BChar( nDWORD / 0x100 )
                     hb_vfWrite( hFile, cDWORD )
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      hb_vfClose( hFile )
   ENDIF

   IF lModified
      hb_vfTimeSet( cFileName, tDate )
   ENDIF

   RETURN lModified

/* Based on:
      https://stackoverflow.com/questions/6429779/can-anyone-define-the-windows-pe-checksum-algorithm */
STATIC FUNCTION win_PEChecksumCalc( cData, nPECheckSumPos )

   LOCAL nChecksum := 0, nPos

   ++nPECheckSumPos

   FOR nPos := 1 TO hb_BLen( cData ) STEP 4
      IF nPos != nPECheckSumPos
         nChecksum := hb_bitAnd( nChecksum, 0xFFFFFFFF ) + ;
            ( Bin2W( hb_BSubStr( cData, nPos + 0, 2 ) ) + ;
              Bin2W( hb_BSubStr( cData, nPos + 2, 2 ) ) * 0x10000 ) + ;
            hb_bitShift( nChecksum, -32 )
         IF nChecksum > 0x100000000
            nChecksum := hb_bitAnd( nChecksum, 0xFFFFFFFF ) + hb_bitShift( nChecksum, -32 )
         ENDIF
      ENDIF
   NEXT

   nChecksum := hb_bitAnd( nChecksum, 0xFFFF ) + hb_bitShift( nChecksum, -16 )
   nChecksum := hb_bitAnd( nChecksum + hb_bitShift( nChecksum, -16 ), 0xFFFF )

   RETURN nChecksum + hb_BLen( cData )

STATIC FUNCTION StringEOLConv( cFile )

   cFile := StrTran( cFile, Chr( 13 ) + Chr( 10 ), Chr( 10 ) )

   RETURN iif( hb_eol() == Chr( 10 ), cFile, ;
      StrTran( cFile, Chr( 10 ), Chr( 13 ) + Chr( 10 ) ) )

STATIC FUNCTION FileConvEOL( cFileName )

   LOCAL cFile, tDate

   hb_vfTimeGet( cFileName, @tDate )

   RETURN ;
      ! HB_ISNULL( cFile := hb_MemoRead( cFileName ) ) .AND. ;
      hb_MemoWrit( cFileName, StringEOLConv( cFile ) ) .AND. ;
      hb_vfTimeSet( cFileName, tDate )
