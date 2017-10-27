#!/usr/bin/env hbmk2
/*
 * 3rdpatch - a tool to help update 3rd party components while keeping local fixes
 *
 * Copyright 2010, 2011 Tamas TEVESZ
 * See LICENSE.txt for licensing terms.
 *
 * 1. CONFIGURATION
 * ----------------
 *
 * For proper operation, several of the following external tools are required to
 * be present somewhere in your $PATH:
 *
 * - The GNU version of `patch', `diff' and `tar' (3rdpatch will figure it out
 *   if you have them by the names of `gpatch', `gdiff' or `gtar')
 *
 * - curl, gzip, bzip2, xz and unzip (only the Info-ZIP version of unzip has
 *   been tested)
 *
 * `curl' is unconditionally required for fetching source archives; the rest of the
 * tools are checked for on an on-demand basis.
 *
 * 3rdpatch requires several metadata (in the form of specially formatted lines)
 * in the component's Makefile (preferred) or .hbp file (if no Makefile is
 * present). Formatting rules are as follows:
 *
 * - The first character on the line is a hash mark (`#'), followed by any number
 *   of white spaces.
 * - Next comes a keyword, followed by any number of white spaces.
 * - The keyword is followed by one or more arguments, separated by white spaces.
 *   The number of arguments taken depends on the keyword itself.
 *
 * The keywords themselves are case sensitive (only upper case keywords are
 * recognized). The arguments are case sensitive as well.
 *
 * Currently recognized keywords and their arguments are as follows:
 *
 * ORIGIN
 *   Takes one argument, the URL of component's home page. Not currently used,
 *   but greatly helps locating resources regarding the component.
 *   Example: for ZLIB, it is `https://zlib.net/'.
 *
 * VER
 *   Takes one argument, the version number of the component currently in the
 *   Harbour tree. Not currently used, but greatly helps checking whether the
 *   component needs an update.
 *   Example: for PCRE2, at the time of this writing, it is `10.00'.
 *
 * URL
 *   Takes one argument, the URL to the archive to the currently installed
 *   version of the component. Used by 3rdpatch.
 *   Example: for PCRE2, at the time of this writing, it is
 *   `https://ftp.pcre.org/pub/pcre/pcre2-10.22.tar.bz2'.
 *   3rdpatch can currently unpack only `.tar.gz', `.tar.bz2', `.tgz', `.tbz',
 *   `.tbz2', `.tar.xz', `.txz', `.tar.lz', `.tlz' and `.zip' archives -- one
 *   of these must be chosen.
 *
 *   3rdpatch will also use the URL parameter to figure out what type of
 *   file it is working with, so a URL containing this sort if information must
 *   be picked. As an example, download URLs like
 *   `https://github.com/glennrp/libpng/archive/v1.6.16.tar.gz'
 *   are OK, but `https://example.org/download/latest' is not, even if latter
 *   would ultimately result (perhaps by the server using Content-Disposition
 *   or similar headers) in a file named `example-pkg-54.tar.gz'.
 *
 * DIFF
 *   Takes one argument, the file name of the diff file containing local changes
 *   needed by Harbour. In `rediff' mode, this parameter is optional; if not
 *   specified, defaults to `$(component).diff'.
 *   Example: for PCRE2, it is `pcre2.diff'.
 *
 * MAP
 *   Takes one or two arguments, specifying the correspondence of the file names
 *   between the original sources and the Harbour sources.
 *   If a particular file name is the same both in the upstream and the Harbour
 *   trees, it is sufficient to specify it only once, but every file that needs
 *   to be brought over to the Harbour tree must be specified.
 *   The very first `MAP' occurrence is treated specially: it's argument is used
 *   by 3rdpatch to locate the root of the extracted upstream source tree.
 *   Examples:
 *
 *   # MAP LICENCE
 *
 *      The file named `LICENCE' needs to be brought over from the upstream tree
 *      to the Harbour tree unchanged. In case of PCRE2, `MAP LICENCE' being the
 *      first `MAP' line also means that 3rdpatch will use the directory
 *      containing this file as a base for all other files occurring later.
 *      Accordingly, the first `MAP' entry must be flat even on the source side.
 *
 *   # MAP config.h.generic config.h
 *
 *      The file named `config.h.generic' in the upstream source tree needs to
 *      be brought over to the Harbour tree by the name `config.h'.
 *
 *   # MAP foo/bar.h
 *
 *      If the upstream source tree is not flat, relative paths may also be
 *      specified. The above means "bring `foo/bar.h' from upstream as `bar.h'
 *      into the Harbour tree". Note that the Harbour component tree is always
 *      flat, it is illegal to specify - for example - `MAP foo/bar.h zoink/baz.h'.
 *
 *   # MAP foo/bar.h baz.h
 *
 *      The upstream source file `foo/bar.h' needs to be brought over to the
 *      Harbour tree as `baz.h'. All notes above about hierarchical and flat
 *      trees strictly apply.
 *
 *   For hierarchical source trees, the path separator must always be the Unix
 *   forward slash (`/'). DOS-style backslash separators are not recognized and
 *   will produce undefined results.
 *
 *   The `-validate' command-line argument causes 3rdpatch to validate the
 *   metadata without executing any actions that might otherwise be necessary.
 *   It is recommended to use this after a component's metadata changes.
 *
 * 2. MODES OF OPERATION
 * ---------------------
 *
 * By default, 3rdpatch operates in `component version updating' mode - that is,
 * refreshing the component version to a newer upstream version. Let it be noted
 * that if the new version is very different from the currently in-tree version
 * (lots of new files, removed files, radically re-organized upstream source
 * tree, for example), 3rdpatch's utility will decrease steeply. In such cases
 * considering the full manual update of the component is advised.
 *
 * If 3rdpatch is called with the `-rediff' command-line argument, it switches
 * to a `local diff refresh' mode. This mode is used to refresh the local diff
 * after Harbour-specific modifications have been made to the component's
 * source. In order to help with the initial diff creation, 3rdpatch will proceed
 * even if no `DIFF' is specified amongst the metadata, and defaults to
 * creating a diff named `$(component).diff').
 *
 * If no differences between the original and the Harbour trees were found,
 * a possibly pre-existing diff file is removed. Following this change up
 * in the component's Makefile (or .hbp file) is left for the operator -- 3rdpatch
 * will communicate if there is a likely need to perform this action.
 *
 * It is strongly advised not to try to mix the two modes. If there are any
 * pending local modifications, a rediff should be done before a component
 * version update is performed.
 *
 * 3. TYPICAL WORKFLOW
 * -------------------
 *
 * Once it has been determined that a particular component needs an update, the URL
 * argument has to be modified to point to the new source tree archive. VER should
 * also be updated if there. While residing in the component's directory, 3rdpatch
 * needs to be run. The rest is mostly automatic - 3rdpatch retrieves, unpacks and
 * otherwise prepares the updated source tree, applies any local modifications,
 * and copies any changes back to the Harbour tree (the current working directory).
 * After some inspection and a test, it is ready to be committed.
 *
 * In rediff mode, care must be taken for the URL keyword to contain a reference
 * to the version that is in the current Harbour tree (that basically means `do not
 * touch anything', assuming correct information in the first place). After 3rdpatch
 * is finished rediffing, the new `local changes' file (see `DIFF') may be inspected,
 * and is ready to be committed.
 *
 * If errors are encountered during rediff, the contents of the temporary directory
 * may be used as a starting basis for manual re-diffing.
 *
 * 4. TROUBLESHOOTING
 * ------------------
 *
 * Several things can go wrong, and 3rdpatch tries hard handle them as gracefully as
 * possible. First and foremost, in case of even the slightest sign of something
 * not happening as intended, 3rdpatch will not modify the Harbour tree at all.
 * Everything is happening inside a temporary directory, which is not erased when
 * 3rdpatch exits (not even when it exits normally), and where certain log files are
 * created. These log files may contain information to help debugging in case of
 * an (unhandled) error.
 *
 * The organization of the temporary directory is as follows:
 *
 * $(component)         These directories are the extracted and modified versions
 * $(component).orig    of the upstream source tree. The `.orig' tree consists of
 *                      only renamed (and renames followed up in the files),
 *                      whereas the $(component) tree will have the local
 *                      modifications (see the `DIFF' keyword) applied. These two
 *                      directories are used for re-creating the local changes.
 *
 * root                 The new upstream source is unpacked in this directory.
 *
 * fetch.log            The output `curl' produced while fetching the source
 *                      archive.
 *
 * extract.log          The output the extractor (gzip, bzip2, xz) produced
 *                      while uncompressing the archive.
 *
 * archive.log          The output the archiver (tar, unzip) produced while
 *                      unpacking the archive.
 *
 * patch.log            The output `patch' produced while applying the local
 *                      changes to $(component).
 *
 * diff.log             The standard error of `diff' produced while re-creating
 *                      the local changes (the standard output is the diff itself
 *                      and is placed in the local changes file, see `DIFF').
 *
 * some archive file    The new source tree archive.
 *
 * In all error cases 3rdpatch will provide a meaningful error message. Armed with
 * that and the information here, troubleshooting should not be much of a problem.
 *
 * 5. NOTES
 * --------
 *
 * It seems that the Unix versions of GNU patch can not handle diff files with
 * DOS-style path separators, whereas the Windows (MinGW/Cygwin) versions have no
 * problem working with Unix-style path separators. They however cannot be coerced
 * into generating diffs with Unix-style path separators, which results in diffs
 * generated on Windows hosts can not be applied on non-Windows hosts.
 *
 * To remedy this situation, 3rdpatch will change diffs to use Unix-style path
 * separators. Since this is a grave problem (the diff is unappliable on
 * non-Windows hosts), this change takes place unconditionally. The user is
 * notified of the change by an informational message stating the fact. These
 * changed diffs should be committed back to the repository.
 *
 * 6. BUGS
 * -------
 *
 * None known. More testing on non-Unix systems is desired.
 *
 */

#pragma -w3
#pragma -km+
#pragma -ko+

#include "directry.ch"
#include "fileio.ch"
#include "hbver.ch"

#if defined( _TRACE )
   #define TRACE( str )   OutStd( "T: " + str + hb_eol() )
#else
   #define TRACE( str )
#endif

#define ONEARG_KW   2        /* one-arg line keyword */
#define ONEARG_ARG  3        /* one-arg line argument */
#define TWOARG_KW   2        /* two-arg line keyword */
#define TWOARG_ARG1 3        /* two-arg line first argument */
#define TWOARG_ARG2 4        /* two-arg line second argument */

#define FN_ORIG     1        /* original file name in maps */
#define FN_HB       2        /* hb file name in maps */

STATIC s_aChangeMap := {}    /* from-to file name map */
STATIC s_cTempDir
STATIC s_nErrors := 0        /* error indicator */
STATIC s_cSourceRoot         /* top directory of the newly-unpacked source tree */

STATIC s_aTools := { ;
   "patch"  =>, ;
   "diff"   =>, ;
   "curl"   =>, ;
   "tar"    =>, ;
   "gzip"   =>, ;
   "bzip2"  =>, ;
   "xz"     =>, ;
   "lzip"   =>, ;
   "unzip"  => }

PROCEDURE Main( ... )

   LOCAL cFileName
   LOCAL cFile                /* memorized input make file */
   LOCAL aDir
   LOCAL aRegexMatch          /* regex match results */
   LOCAL cMemoLine            /* MemoLine */
   LOCAL nMemoLine            /* Line number */
   LOCAL cDiffFile            /* Local modifications */
   LOCAL cCWD
   LOCAL cThisComponent       /* component being processed */
   LOCAL aOneMap              /* one pair from s_aChangeMap */
   LOCAL cCommand             /* patch/diff commands */
   LOCAL nRunResult           /* patch/diff exit statuses */
   LOCAL cDiffText            /* diff will return the new diff in this */
   LOCAL cArchiveURL          /* URL for the component */
   LOCAL cTopIndicator        /* file signifying the top of the component's source tree */
   LOCAL cStdOut              /* stdout and stderr for various externally-run apps */
   LOCAL cStdErr
   LOCAL lRediff := .F.       /* whether or not operating as rediff */
   LOCAL lValidateOnly := .F. /* syntactic metadata validation only */
   LOCAL cArg
   LOCAL cRoot := NIL
   LOCAL hFile
   LOCAL nStatus
   LOCAL nAttr, tmp

   LOCAL hRegexTake1Line := hb_regexComp( "^#[[:blank:]]*(ORIGIN|VER|URL|DIFF)[[:blank:]]+(.+?)[[:blank:]]*$" )
   LOCAL hRegexTake2Line := hb_regexComp( "^#[[:blank:]]*(MAP)[[:blank:]]+(.+?)[[:blank:]]+(.+?)[[:blank:]]*$" )

   FOR EACH cArg IN hb_AParams()
      SWITCH cArg
      CASE "-rediff"
         lRediff := .T.
         EXIT
      CASE "-validate"
         lValidateOnly := .T.
         EXIT
      CASE "-h"
      CASE "-help"
      CASE "--help"
      CASE "/?"
         Usage( 0 )
      OTHERWISE
         Usage( 1 )
      ENDSWITCH
   NEXT

   IF ! hb_vfExists( cFileName := "Makefile" )
      IF Empty( aDir := hb_vfDirectory( "*.hbp" ) )
         OutStd( "No `Makefile' or '*.hbp' file in the current directory." + hb_eol() )
         ErrorLevel( 1 )
         RETURN
      ELSE
         ASort( aDir,,, {| tmp, tmp1 | tmp[ F_NAME ] < tmp1[ F_NAME ] } )
         cFileName := aDir[ 1 ][ F_NAME ]
      ENDIF
   ENDIF

   SetupTools()

   cFile := hb_MemoRead( cFileName )
   cDiffFile := NIL        /* default to `no local diff' */

   nMemoLine := 0

   FOR EACH cMemoLine IN hb_ATokens( cFile, .T. )

      cMemoLine := AllTrim( cMemoLine )
      nMemoLine++

      IF ! Empty( aRegexMatch := hb_regex( hRegexTake1Line, cMemoLine ) )
         /* Process one-arg keywords */
         IF aRegexMatch[ ONEARG_KW ] == "DIFF"
            cDiffFile := iif( Empty( AllTrim( aRegexMatch[ ONEARG_ARG ] ) ), NIL, ;
               AllTrim( aRegexMatch[ ONEARG_ARG ] ) )
         ELSEIF aRegexMatch[ ONEARG_KW ] == "URL"
            cArchiveURL := AllTrim( aRegexMatch[ ONEARG_ARG ] )
         ENDIF

      ELSEIF ! Empty( aRegexMatch := hb_regex( hRegexTake2Line, cMemoLine ) )
         /* Process two-arg keywords */
         IF aRegexMatch[ TWOARG_KW ] == "MAP"
            /* Do not allow implicit destination with non-flat source spec */
            IF Empty( aRegexMatch[ TWOARG_ARG1 ] ) .AND. "/" $ aRegexMatch[ TWOARG_ARG2 ]
               OutStd( hb_StrFormat( "E: Non-flat source spec with implicit destination, " + ;
                  "offending line %1$d:", nMemoLine ) + hb_eol() )
               OutStd( aRegexMatch[ 1 ] + hb_eol() )
               ErrorLevel( 2 )
               RETURN
            ENDIF
            /* Do not allow tree spec in the destination ever */
            IF "/" $ aRegexMatch[ TWOARG_ARG2 ]
               OutStd( hb_StrFormat( "E: Non-flat destination, " + ;
                  "offending line %1$d:", nMemoLine ) + hb_eol() )
               OutStd( aRegexMatch[ 1 ] + hb_eol() )
               ErrorLevel( 2 )
               RETURN
            ENDIF
            /* If the source argument indicates the source tree is not flat, convert
             * path separator to native. The HB tree is always flattened. */
            IF "/" $ aRegexMatch[ TWOARG_ARG1 ]
               aRegexMatch[ TWOARG_ARG1 ] := StrTran( aRegexMatch[ TWOARG_ARG1 ], "/", hb_ps() )
            ENDIF
            /* In case the original and the HB file names are identical, the
             * second argument to `MAP' is optional. Due to the way the regex is
             * constructed, in this case the last backref will contain the only
             * file name, so shuffle arguments around accordingly
             */
            AAdd( s_aChangeMap, { ;
               iif( Empty( aRegexMatch[ TWOARG_ARG1 ] ), ;
               aRegexMatch[ TWOARG_ARG2 ], ;
               aRegexMatch[ TWOARG_ARG1 ] ), aRegexMatch[ TWOARG_ARG2 ] ;
               } )
            /* If this is the first MAP entry, treat the original part as the
             * source tree root indicator */
            IF Len( s_aChangeMap ) == 1
               cTopIndicator := s_aChangeMap[ 1 ][ FN_ORIG ]
               IF "/" $ cTopIndicator
                  OutStd( hb_StrFormat( "E: First `MAP' entry is not flat, " + ;
                     "offending line %1$d:", nMemoLine ) + hb_eol() )
                  OutStd( aRegexMatch[ 1 ] + hb_eol() )
                  ErrorLevel( 2 )
                  RETURN
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   NEXT

   DOSToUnixPathSep( cDiffFile )

   IF lValidateOnly
      OutStd( "Metadata syntax is OK." + hb_eol() )
      RETURN
   ENDIF

   IF Empty( s_aChangeMap ) .AND. cDiffFile == NIL
      OutStd( "No file name changes and no local diff, nothing to do." + hb_eol() )
      RETURN
   ENDIF

   IF ! lRediff .AND. cDiffFile != NIL .AND. ! hb_vfExists( cDiffFile )
      OutStd( hb_StrFormat( "E: `%1$s' does not exist", cDiffFile ) + hb_eol() )
      ErrorLevel( 2 )
      RETURN
   ENDIF

   cCWD := hb_cwd()
#if defined( _CURDIR )
   cRoot := cCWD
#endif

   IF ( hFile := hb_vfTempFile( @s_cTempDir, cRoot, hb_FNameName( hb_ProgName() ) + "_" ) ) != NIL
      hb_vfClose( hFile )
      hb_vfErase( s_cTempDir )
      hb_vfDirMake( s_cTempDir )
   ELSE
      OutStd( "E: Failed to create temporary directory." + hb_eol() )
      ErrorLevel( 2 )
      RETURN
   ENDIF

   cThisComponent := hb_FNameName( hb_DirSepDel( cCWD ) )

   hb_vfDirMake( CombinePath( s_cTempDir, cThisComponent ) )
   hb_vfDirMake( CombinePath( s_cTempDir, cThisComponent + ".orig" ) )
   hb_vfDirMake( CombinePath( s_cTempDir, "root" ) )

   IF lRediff .AND. cDiffFile == NIL
      OutStd( "Requested rediff mode with no existing local diff, attempting to create one." + hb_eol() )
      cDiffFile := cThisComponent + ".diff"
   ENDIF

   IF ! FetchAndExtract( cArchiveURL )
      OutStd( "E: Fetching or extracting the source archive failed." + hb_eol() )
      OutStd( hb_StrFormat( "   Inspect `%1$s' for further clues.", s_cTempDir ) + hb_eol() )
      ErrorLevel( 2 )
      RETURN
   ENDIF

   s_cSourceRoot := WalkAndFind( CombinePath( s_cTempDir, "root" ), cTopIndicator )
   IF s_cSourceRoot == NIL
      OutStd( "E: Could not find the new tree's root" + hb_eol() )
      OutStd( hb_StrFormat( "   Inspect `%1$s'", s_cTempDir ) + hb_eol() )
      ErrorLevel( 2 )
      RETURN
   ENDIF

   /*
    * Create two copies of the relevant portions of the source archive.
    * The pristine tree is for reference, used as the left component of the diff
    * Our tree will have the local diff applied, and used as the right component of the diff
    */

   s_nErrors := 0

   FOR EACH aOneMap IN s_aChangeMap
      IF ! hb_vfExists( CombinePath( s_cSourceRoot, aOneMap[ FN_ORIG ] ) )
         OutStd( hb_StrFormat( "W: `%1$s' does not exist in the source tree", aOneMap[ FN_ORIG ] ) + hb_eol() )
         OutStd( "   I will do what i can, but you'd better check the results manually." + hb_eol() )
         s_nErrors++
      ELSE
         /* Create the `pristine tree' */
         ts_hb_vfCopyFile( ;
            CombinePath( s_cSourceRoot, aOneMap[ FN_ORIG ] ), ;
            CombinePath( s_cTempDir, cThisComponent + ".orig", aOneMap[ FN_HB ] ) )

         /* Munch the file, applying the appropriate transforms */
         hb_FileTran( CombinePath( s_cTempDir, cThisComponent + ".orig", aOneMap[ FN_HB ] ) )

         /* If operating in `rediff' mode, copy the current Harbour component tree;
          * otherwise, duplicate the pristine tree */

         IF lRediff
            ts_hb_vfCopyFile( ;
               aOneMap[ FN_HB ], ;
               CombinePath( s_cTempDir, cThisComponent, aOneMap[ FN_HB ] ) )

         ELSE
            /* Copy it to `our tree' */
            ts_hb_vfCopyFile( ;
               CombinePath( s_cTempDir, cThisComponent + ".orig", aOneMap[ FN_HB ] ), ;
               tmp := CombinePath( s_cTempDir, cThisComponent, aOneMap[ FN_HB ] ) )

            /* Remove exec attribute */
            hb_vfAttrGet( tmp, @nAttr )
            hb_vfAttrSet( tmp, hb_bitAnd( nAttr, hb_bitNot( hb_bitOr( HB_FA_XUSR, HB_FA_XGRP, HB_FA_XOTH ) ) ) )
         ENDIF
      ENDIF
   NEXT

   IF cDiffFile != NIL

      IF ! lRediff /* If we have a local diff, and are not to re-create it, apply */
         cCommand := hb_StrFormat( "%1$s -l --no-backup-if-mismatch -d %2$s -p 1 -i %3$s", ;
            s_aTools[ "patch" ], ;
            CombinePath( s_cTempDir, cThisComponent ), ;
            CombinePath( cCWD, cDiffFile ) )
         TRACE( "Running " + cCommand )
         nRunResult := hb_processRun( cCommand, , @cStdOut, @cStdErr, .F. )
         SaveLog( "patch", cStdOut, cStdErr )
         IF nRunResult != 0
            OutStd( hb_StrFormat( "W: Unexpected events happened during patching, inspect %1$s", s_cTempDir ) + hb_eol() )
            s_nErrors++
         ENDIF
      ENDIF

      /* Re-create the diff */
      cCommand := hb_StrFormat( "%1$s --strip-trailing-cr -urN %2$s %3$s", ;
         s_aTools[ "diff" ], cThisComponent + ".orig", cThisComponent )

      DirChange( s_cTempDir )
      TRACE( "Running " + cCommand )
      nStatus := utc_hb_processRun( cCommand, , @cDiffText, @cStdErr, .F. )
      hb_cwd( cCWD )

      IF nStatus != 0 .AND. nStatus != 1
         OutStd( hb_StrFormat( "E: `diff' command failed with exit status %1$d.", nStatus ) + hb_eol() )
         OutStd( hb_StrFormat( "   Inspect `%1$s' for further clues.", s_cTempDir ) + hb_eol() )
         ErrorLevel( 2 )
         RETURN
      ENDIF

      SaveLog( "diff", NIL, cStdErr )

      IF cDiffText == ""
         OutStd( "No local changes; you may need to adjust `DIFF'." + hb_eol() )
         IF hb_vfExists( cDiffFile )
            hb_vfErase( cDiffFile )
            OutStd( hb_StrFormat( "Removed existing `%1$s'.", cDiffFile ) + hb_eol() )
         ENDIF
      ELSE
         hb_MemoWrit( cDiffFile, cDiffText )
         OutStd( hb_StrFormat( "Local changes saved to `%1$s'; you may need to adjust `DIFF'.", cDiffFile ) + hb_eol() )
      ENDIF
   ENDIF

   /* Only copy files back to the live tree if no errors were encountered */
   IF s_nErrors == 0
      IF ! lRediff
         /* Only copy the complete new tree back if not in Rediff mode */
         FOR EACH aOneMap IN s_aChangeMap
            ts_hb_vfCopyFile( CombinePath( s_cTempDir, cThisComponent, aOneMap[ FN_HB ] ), aOneMap[ FN_HB ] )
         NEXT
      ENDIF

      IF cDiffFile != NIL
         /* Copy the diff back to the live tree */
         ts_hb_vfCopyFile( CombinePath( s_cTempDir, cDiffFile ), cDiffFile )
         /* Convert path separators */
         DOSToUnixPathSep( cDiffFile )
      ENDIF

   ELSE
      OutStd( "Errors were encountered, no changes are made to your Harbour tree." + hb_eol() )
      OutStd( hb_StrFormat( "Inspect `%1$s' for further clues.", s_cTempDir ) + hb_eol() )
   ENDIF

   IF ! lRediff
      OutStd( hb_StrFormat( "Don't forget to update `%1$s' with the new version and URL information.", cFileName ) + hb_eol() )
   ENDIF
   OutStd( hb_StrFormat( "The temporary directory `%1$s' has not been removed.", s_cTempDir ) + hb_eol() )

   RETURN

STATIC FUNCTION ts_hb_vfCopyFile( cSrc, cDst )

   LOCAL tDate

   RETURN ;
      hb_vfCopyFile( cSrc, cDst ) != F_ERROR .AND. ;
      hb_vfTimeGet( cSrc, @tDate ) .AND. ;
      hb_vfTimeSet( cDst, tDate )

STATIC FUNCTION utc_hb_processRun( ... )

   LOCAL cTZ := GetEnv( "TZ" )
   LOCAL retval

   hb_SetEnv( "TZ", "UTC" )
   retval := hb_processRun( ... )
   hb_SetEnv( "TZ", cTZ )

   RETURN retval

/* Utility functions */

STATIC PROCEDURE SetupTools()

#if defined( __PLATFORM__UNIX )
   LOCAL cExeExt := ""
#else
   LOCAL cExeExt := ".exe"
#endif
   LOCAL cPathComp
   LOCAL cTool

   /* Look for g$tool first, only attempt raw name if it isn't found
    * Helps non-GNU user space systems with GNU tools installed.
    * Only several of the tools are known to have GNU variants. */

   FOR EACH cPathComp IN hb_ATokens( GetEnv( "PATH" ), hb_osPathListSeparator() )
      FOR EACH cTool IN hb_HKeys( s_aTools )
         IF cTool $ "patch|diff|tar" .AND. hb_vfExists( CombinePath( cPathComp, "g" + cTool ) + cExeExt )
            s_aTools[ cTool ] := CombinePath( cPathComp, "g" + cTool )
         ENDIF
      NEXT
   NEXT

   FOR EACH cPathComp IN hb_ATokens( GetEnv( "PATH" ), hb_osPathListSeparator() )
      FOR EACH cTool IN hb_HKeys( s_aTools )
         IF s_aTools[ cTool ] == NIL .AND. hb_vfExists( CombinePath( cPathComp, cTool ) + cExeExt )
            s_aTools[ cTool ] := CombinePath( cPathComp, cTool )
         ENDIF
      NEXT
   NEXT

   RETURN

STATIC FUNCTION CombinePath( ... )

   LOCAL aArguments := hb_AParams()
   LOCAL cRetVal
   LOCAL nI

   IF Len( aArguments ) == 2
      cRetVal := hb_DirSepAdd( aArguments[ 1 ] ) + aArguments[ 2 ]
   ELSE
      cRetVal := hb_DirSepAdd( aArguments[ 1 ] )
      FOR nI := 2 TO Len( aArguments ) - 1
         cRetVal += hb_DirSepAdd( aArguments[ nI ] )
      NEXT
      cRetVal += ATail( aArguments )
   ENDIF

   RETURN cRetVal

STATIC FUNCTION WalkAndFind( cTop, cLookFor )

   LOCAL aDirEntry
   LOCAL cRetVal := NIL

   cTop := hb_DirSepAdd( cTop )

   FOR EACH aDirEntry IN ASort( hb_vfDirectory( cTop + hb_osFileMask(), "D" ),,, {| aLeft | ! "D" $ aLeft[ F_ATTR ] } )  /* Files first */
      IF ! "D" $ aDirEntry[ F_ATTR ]
         IF aDirEntry[ F_NAME ] == cLookFor
            cRetVal := cTop
            EXIT
         ENDIF
      ELSEIF !( aDirEntry[ F_NAME ] == "." .OR. aDirEntry[ F_NAME ] == ".." )
         cRetVal := WalkAndFind( cTop + aDirEntry[ F_NAME ], cLookFor )
         IF ! Empty( cRetVal )
            EXIT
         ENDIF
      ENDIF
   NEXT

   RETURN cRetVal

STATIC FUNCTION FetchAndExtract( cArchiveURL )

   LOCAL cCommand
   LOCAL cExtractor := NIL
   LOCAL cExtractorArgs := NIL
   LOCAL cExtractedFileName := NIL
   LOCAL cArchiver := NIL
   LOCAL cArchiverArgs := NIL
   LOCAL nResult
   LOCAL cStdOut
   LOCAL cStdErr
   LOCAL cPattern
   LOCAL cMatchedPattern
   LOCAL cFileName
   LOCAL cFrag
   LOCAL cCWD

   /* Any given package is surely available in at least one of these formats,
    * pick one of these, refrain from the more exotic ones. */

   LOCAL aActionMap := { ;
      ".tar.gz|.tgz" => { ;
         "Extractor"          => "gzip", ;
         "ExtractorArgs"      => "-d", ;
         "ExtractedFile"      => ".tar", ;
         "Archiver"           => "tar", ;
         "ArchiverArgs"       => "--force-local -xvf" ;
      }, ;
      ".tar.bz2|.tbz|.tbz2" => { ;
         "Extractor"          => "bzip2", ;
         "ExtractorArgs"      => "-d", ;
         "ExtractedFile"      => ".tar", ;
         "Archiver"           => "tar", ;
         "ArchiverArgs"       => "--force-local -xvf" ;
      }, ;
      ".tar.xz|.txz" => { ;
         "Extractor"          => "xz", ;
         "ExtractorArgs"      => "-d", ;
         "ExtractedFile"      => ".tar", ;
         "Archiver"           => "tar", ;
         "ArchiverArgs"       => "--force-local -xvf" ;
      }, ;
      ".tar.lz|.tlz" => { ;
         "Extractor"          => "lzip", ;
         "ExtractorArgs"      => "-d", ;
         "ExtractedFile"      => ".tar", ;
         "Archiver"           => "tar", ;
         "ArchiverArgs"       => "--force-local -xvf" ;
      }, ;
      ".zip" => { ;
         "Extractor"          => NIL, ;
         "ExtractorArgs"      => NIL, ;
         "ExtractedFile"      => NIL, ;
         "Archiver"           => "unzip", ;
         "ArchiverArgs"       => "" ;
      } ;
   }

   IF Empty( cArchiveURL )
      OutStd( "E: URL missing" + hb_eol() )
      RETURN .F.
   ENDIF

   cFileName := URL_GetFileName( cArchiveURL )

   FOR EACH cPattern IN hb_HKeys( aActionMap )
      FOR EACH cFrag IN hb_ATokens( cPattern, "|" )
         IF cFrag $ cFileName
            cMatchedPattern := cFrag
            cExtractor := aActionMap[ cPattern ][ "Extractor" ]
            cExtractorArgs := aActionMap[ cPattern ][ "ExtractorArgs" ]
            cExtractedFileName := iif( aActionMap[ cPattern ][ "ExtractedFile" ] == NIL, ;
                                    NIL, ;
                                    hb_StrShrink( cFileName, Len( cMatchedPattern ) ) + ;
                                       aActionMap[ cPattern ][ "ExtractedFile" ] )
            cArchiver := aActionMap[ cPattern ][ "Archiver" ]
            cArchiverArgs := aActionMap[ cPattern ][ "ArchiverArgs" ]
            IF hb_Version( HB_VERSION_PLATFORM ) $ "|DARWIN|BSD|" .AND. cArchiver == "tar"
               /* Not supported by BSD tar */
               cArchiverArgs := StrTran( cArchiverArgs, "--force-local" )
            ENDIF
            EXIT
         ENDIF
      NEXT
   NEXT

   IF cArchiver == NIL
      OutStd( hb_StrFormat( "E: Can not find archiver for `%1$s'", ;
         hb_FNameNameExt( cArchiveURL ) ) + hb_eol() )
      RETURN .F.
   ENDIF

   /* Fetch */
   IF s_aTools[ "curl" ] == NIL
      OutStd( "E: Required `curl' was not found" + hb_eol() )
      RETURN .F.
   ENDIF
   cCommand := hb_StrFormat( "%1$s -L %2$s -# -o %3$s %4$s", s_aTools[ "curl" ], ;
      iif( hb_LeftEqI( cArchiveURL, "https:" ), "--proto-redir =https", "--proto-redir =https,http" ), ;
      CombinePath( s_cTempDir, cFileName ), FNameEscape( cArchiveURL ) )
   TRACE( "Running " + cCommand )
   nResult := hb_processRun( cCommand, , , @cStdErr, .F. )
   SaveLog( "fetch", cStdOut, cStdErr )
   IF nResult != 0
      OutStd( hb_StrFormat( "E: Error fetching %1$s", cArchiveURL ) + hb_eol() )
      RETURN .F.
   ENDIF

   /* Extract */
   IF cExtractor != NIL /* May not need extraction */
      IF s_aTools[ cExtractor ] == NIL
         OutStd( hb_StrFormat( "E: Required `%1$s' was not found", cExtractor ) + hb_eol() )
         RETURN .F.
      ENDIF
      cCommand := hb_StrFormat( "%1$s %2$s %3$s", ;
         cExtractor, cExtractorArgs, CombinePath( s_cTempDir, cFileName ) )
      TRACE( "Running " + cCommand )
      nResult := hb_processRun( cCommand, , @cStdOut, @cStdErr, .F. )
      SaveLog( "extract", cStdOut, cStdErr )
      IF nResult != 0
         OutStd( hb_StrFormat( "E: Error extracting %1$s", cFileName ) + hb_eol() )
         RETURN .F.
      ENDIF
   ELSE
      cExtractedFileName := cFileName
   ENDIF

   /* Unarchive */
   IF s_aTools[ cArchiver ] == NIL
      OutStd( hb_StrFormat( "E: Required `%1$s' was not found", cArchiver ) + hb_eol() )
      RETURN .F.
   ENDIF
   cCommand := hb_StrFormat( "%1$s %2$s %3$s", ;
      cArchiver, cArchiverArgs, CombinePath( s_cTempDir, cExtractedFileName ) )
   TRACE( "Running " + cCommand )
   cCWD := hb_cwd()
   DirChange( CombinePath( s_cTempDir, "root" ) )
   nResult := hb_processRun( cCommand, , @cStdOut, @cStdErr, .F. )
   hb_cwd( cCWD )
   SaveLog( "archive", cStdOut, cStdErr )
   IF nResult != 0
      OutStd( hb_StrFormat( "E: Error unarchiving %1$s", cFileName ) + hb_eol() )
      RETURN .F.
   ENDIF

   RETURN .T.

STATIC PROCEDURE SaveLog( cFNTemplate, cStdOut, cStdErr )

   LOCAL hFile

   IF ( hFile := hb_vfOpen( CombinePath( s_cTempDir, cFNTemplate + ".log" ), FO_CREAT + FO_TRUNC + FO_WRITE ) ) != NIL
      IF cStdOut != NIL
         hb_vfWrite( hFile, "stdout:" + hb_eol() )
         hb_vfWrite( hFile, cStdOut )
      ENDIF
      IF cStdErr != NIL
         hb_vfWrite( hFile, "stderr:" + hb_eol() )
         hb_vfWrite( hFile, cStdErr )
      ENDIF
      hb_vfClose( hFile )
   ENDIF

   RETURN

STATIC PROCEDURE Usage( nExitVal )

   OutStd( hb_StrFormat( "Usage: %1$s [-h|-help|-rediff]", hb_FNameNameExt( hb_ProgName() ) ) + hb_eol() )
   OutStd( "       Documentation is provided in the source code." + hb_eol() )
   ErrorLevel( nExitVal )
   QUIT

   RETURN

STATIC FUNCTION URL_GetFileName( cURL )

   LOCAL aComponents
   LOCAL cName
   LOCAL nIdx

   aComponents := hb_ATokens( cURL, "/" )
   nIdx := Len( aComponents )

   IF nIdx < 4 /* now what */
      RETURN ""
   ENDIF

   cName := aComponents[ nIdx ]
   DO WHILE ! "." $ cName
      cName := aComponents[ --nIdx ]
      IF nIdx < 4  /* don't drain all components */
         RETURN ""
      ENDIF
   ENDDO

   RETURN cName

STATIC FUNCTION hb_FileTran( cFileName )

   LOCAL cFileContent
   LOCAL cTransformedContent
   LOCAL aChange
   LOCAL cChangeFrom
   LOCAL cChangeTo
   LOCAL tDate

   cFileContent := hb_MemoRead( cFileName )

   /* CRLF -> LF */
   cTransformedContent := StrTran( cFileContent, Chr( 13 ) + Chr( 10 ), Chr( 10 ) )

   /* LF -> native */
   cTransformedContent := StrTran( cTransformedContent, Chr( 10 ), hb_eol() )

   FOR EACH aChange IN s_aChangeMap

      /* This is a shot in the dark. Haru works with this transform,
       * but other components may very well need different handling. */
      cChangeFrom := hb_FNameNameExt( aChange[ 1 ] )
      cChangeTo := aChange[ 2 ]

      /* Local-style includes */
      cTransformedContent := StrTran( cTransformedContent, ;
         '"' + cChangeFrom + '"', ;
         '"' + cChangeTo + '"' )

      /* System-style include */
      cTransformedContent := StrTran( cTransformedContent, ;
         "<" + cChangeFrom + ">", ;
         "<" + cChangeTo + ">" )
   NEXT

   hb_vfTimeGet( cFileName, @tDate )

   RETURN ;
      hb_MemoWrit( cFileName, cTransformedContent ) .AND. ;
      hb_vfTimeSet( cFileName, tDate )

STATIC FUNCTION FNameEscape( cFileName )

#if defined( __PLATFORM__UNIX )
   cFileName := '"' + cFileName + '"'
#endif

   RETURN cFileName

/* Check diff file for DOS-style path separators; convert them to Unix-style if needed.
 * Assumes that diffs use host-native line endings (which should be the case anyway). */
STATIC PROCEDURE DOSToUnixPathSep( cFileName )

   LOCAL cFile
   LOCAL cMemoLine
   LOCAL cNewFile
   LOCAL cLookFor
   LOCAL nStart
   LOCAL nEnd

   IF cFileName == NIL .OR. ! hb_vfExists( cFileName )
      RETURN
   ENDIF

   cFile := hb_MemoRead( cFileName )
   cNewFile := ""
   cLookFor := hb_eol()
   nStart := 1
   s_nErrors := 0

   DO WHILE .T.

      IF ( nEnd := hb_BAt( cLookFor, cFile, nStart ) ) == 0
         /* If anything is left in the input string, stick it to the end
          * of the output string. No path searching as that would be
          * an invalid diff anyway */
         cNewFile += hb_BSubStr( cFile, nStart )
         EXIT
      ENDIF

      cMemoLine := hb_BSubStr( cFile, nStart, nEnd - nStart + hb_BLen( cLookFor ) )

      IF ( hb_LeftEq( cMemoLine, "diff " ) .OR. ;
           hb_LeftEq( cMemoLine, "+++ " ) .OR. ;
           hb_LeftEq( cMemoLine, "--- " ) ) .AND. "\" $ cMemoLine

         cMemoLine := StrTran( cMemoLine, "\", "/" )
         s_nErrors++
      ENDIF

      cNewFile += cMemoLine
      nStart := hb_BLen( cNewFile ) + 1

   ENDDO

   IF s_nErrors > 0
      IF hb_MemoWrit( cFileName, cNewFile )
         OutStd( hb_StrFormat( "I: DOS-style path name separators in `%1$s' " + ;
                 "have been converted to Unix-style", cFileName ) + hb_eol() )
         OutStd( "W: Do not forget to push this file!" + hb_eol() )
      ELSE
         OutStd( hb_StrFormat( "E: Oops, something bad happened while trying to " + ;
                 "overwrite `%1$s'", cFileName ) + hb_eol() )
         OutStd( "E: You will probably have to clean up manually" + hb_eol() )
         /* XXX: Error details? */
         ErrorLevel( 2 )
         QUIT
      ENDIF
   ENDIF

   RETURN
