#!/usr/bin/hbmk2
/*
 * Harbour Project source code:
 * Commit preparer and source checker/fixer
 *
 * Copyright 2012-2013 Viktor Szakats (vszakats.net/harbour)
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at https://www.gnu.org/).
 *
 */

#define _CONFIGFIL_ ".hbcommit"
#define _CONFIGENV_ "HBCOMMIT_USER"

#pragma -w3
#pragma -km+
#pragma -ko+

#include "directry.ch"
#include "hbgtinfo.ch"

PROCEDURE Main()

   LOCAL cVCS
   LOCAL cVCSDir
   LOCAL cLocalRoot
   LOCAL aFiles
   LOCAL aChanges
   LOCAL cLog
   LOCAL nStart, nEnd
   LOCAL cMyName
   LOCAL cLogName
   LOCAL lWasChangeLog

   IF "-c" $ cli_Options()
      CheckFileList( iif( Empty( cli_Values() ), NIL, cli_Values() ) )
      ErrorLevel( 0 )
      RETURN
   ENDIF

   cVCS := VCSDetect( @cVCSDir, @cLocalRoot )

   IF cVCS == "git"
      InstallHook( cVCSDir, "pre-commit"        , hb_StrFormat( "exec hbrun %1$s --check-only", CommitScript() ) )
//    InstallHook( cVCSDir, "prepare-commit-msg", hb_StrFormat( "exec hbrun %1$s $1 --prepare-commit", CommitScript() )
   ENDIF

   aFiles := {}
   aChanges := DoctorChanges( cVCS, Changes( cVCS ), aFiles )

   IF Empty( aChanges )
      OutStd( hb_ProgName() + ": " + "no changes" + hb_eol() )
      ErrorLevel( 0 )
      RETURN
   ENDIF

   IF CheckFileList( aFiles, cLocalRoot, .F. )

      cLogName := FindChangeLog( cVCS )
      IF Empty( cLogName )
         OutStd( hb_ProgName() + ": " + "cannot find ChangeLog file" + hb_eol() )
         ErrorLevel( 2 )
      ENDIF

      IF "--check-only" $ cli_Options() .OR. ;
         "--prepare-commit" $ cli_Options()

         IF AScan( aFiles, {| tmp | tmp == hb_FNameNameExt( cLogName ) } ) == 0
            OutStd( hb_ProgName() + ": " + hb_StrFormat( "%1$s not updated. Run 'hbrun %2$s' and retry.", cLogName, CommitScript() ) + hb_eol() )
            ErrorLevel( 3 )
            RETURN
         ELSE
            IF ! GitIsMerge( cVCSDir )
               cLog := GetLastEntry( MemoRead( cLogName ), @nStart, @nEnd )
               IF ! Empty( cLog )
                  IF "--prepare-commit" $ cli_Options() .AND. ! Empty( cli_Values() )
                     hb_MemoWrit( cli_Values()[ 1 ], EntryToCommitMsg( cLog ) + hb_MemoRead( cli_Values()[ 1 ] ) )
                  ELSE
                     hbshell_gtSelect()
                     /* if clipboard already contains part of the entry, do not overwrite it */
                     IF ! hb_StrReplace( hb_gtInfo( HB_GTI_CLIPBOARDDATA ), Chr( 13 ) + Chr( 10 ) ) $ hb_StrReplace( cLog, Chr( 13 ) + Chr( 10 ) )
                        hb_gtInfo( HB_GTI_CLIPBOARDDATA, EntryToCommitMsg( cLog ) )
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ELSE
         IF cVCS == "git"
            cMyName := GitUser()
         ELSE
            IF ! Empty( GetEnv( _CONFIGENV_ ) )
               cMyName := GetEnv( _CONFIGENV_ )
            ELSEIF hb_FileExists( cLocalRoot + _CONFIGFIL_ )
               cMyName := AllTrim( hb_MemoRead( cLocalRoot + _CONFIGFIL_ ) )
            ELSE
               cMyName := "Firstname Lastname (me example.org)"
            ENDIF
         ENDIF

         // ;

         cLog := MemoRead( cLogName )

         GetLastEntry( cLog, @nStart, @nEnd )

         IF nStart > 0

            /* Strip last entry if it's empty to avoid adding double entries */
            IF IsLastEntryEmpty( SubStr( cLog, nStart, nEnd - nStart ), cLogName, @lWasChangeLog )
               OutStd( hb_ProgName() + ": " + hb_StrFormat( "Updating last empty %1$s entry", cLogName ) + hb_eol() )
               cLog := Left( cLog, nStart - 1 ) + SubStr( cLog, nEnd )
            ELSE
               lWasChangelog := .T.
            ENDIF

            cLog := ;
               Left( cLog, nStart - 1 ) + ;
               MakeEntry( aChanges, cMyName, cLogName, lWasChangeLog ) + hb_eol() + ;
               SubStr( cLog, nStart )
         ELSE
            cLog += hb_eol() + MakeEntry( aChanges, cMyName, cLogName, .T. )
         ENDIF

         hb_MemoWrit( cLogName, cLog )

         IF ! Empty( cLogName )
            OutStd( hb_ProgName() + ": " + hb_StrFormat( "Edit %1$s and commit", cLogName ) + hb_eol() )
//          LaunchCommand( GitEditor(), cLogName )
         ENDIF
      ENDIF

      ErrorLevel( 0 )
   ELSE
      OutStd( hb_ProgName() + ": " + "Please correct errors listed above and re-run" + hb_eol() )
      ErrorLevel( 1 )
   ENDIF

   RETURN

STATIC FUNCTION cli_Options()

   THREAD STATIC t_hOptions

   LOCAL tmp
   LOCAL nArg

   IF t_hOptions == NIL
      t_hOptions := { => }
      nArg := 1
      FOR tmp := 1 TO hb_argc()
         IF Left( hb_argv( tmp ), 1 ) == "-"
            t_hOptions[ hb_argv( tmp ) ] := nArg
         ELSE
            ++nArg
         ENDIF
      NEXT
   ENDIF

   RETURN t_hOptions

STATIC FUNCTION cli_Values()

   THREAD STATIC t_aValues

   LOCAL tmp

   IF t_aValues == NIL
      t_aValues := {}
      FOR tmp := 1 TO hb_argc()
         IF !( Left( hb_argv( tmp ), 1 ) == "-" )
            AAdd( t_aValues, hb_argv( tmp ) )
         ENDIF
      NEXT
   ENDIF

   RETURN t_aValues

STATIC FUNCTION CommitScript()

   LOCAL cBaseName := hb_FNameName( hb_ProgName() ) + ".hb"
   LOCAL cName

   IF hb_FileExists( cName := hb_DirSepToOS( "bin/" ) + cBaseName )
      RETURN cName
   ENDIF

   RETURN cBaseName

STATIC FUNCTION InstallHook( cDir, cHookName, cCommand )

   LOCAL cName := hb_DirSepAdd( cDir ) + hb_DirSepToOS( "hooks/" ) + cHookName
   LOCAL cFile := hb_MemoRead( cName )

   cCommand := StrTran( cCommand, "\", "/" )

   IF cCommand $ cFile
      RETURN .T.
   ENDIF

   IF Empty( cFile )
      cFile += "#!/bin/sh" + hb_eol()
   ENDIF

   RETURN hb_MemoWrit( cName, cFile + hb_eol() + cCommand + hb_eol() )

STATIC FUNCTION FindChangeLog( cVCS )

   LOCAL cDir
   LOCAL cLogName

   IF hb_FileExists( cLogName := "ChangeLog.txt" ) .OR. ;
      hb_FileExists( cLogName := "ChangeLog" )
      RETURN cLogName
   ENDIF

   cDir := iif( cVCS == "git", GitLocalRoot(), "" )

   IF hb_FileExists( cLogName := cDir + "ChangeLog.txt" ) .OR. ;
      hb_FileExists( cLogName := cDir + "ChangeLog" )
      RETURN cLogName
   ENDIF

   RETURN ""

STATIC FUNCTION GetLastEntry( cLog, /* @ */ nStart, /* @ */ nEnd )

   LOCAL cLogHeaderExp := "\n[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9] UTC[\-+][0-1][0-9][0-5][0-9] [\S ]*" + hb_eol()

   LOCAL cOldLang := hb_cdpSelect( "EN" )
   LOCAL cHit

   nEnd := 0

   cHit := hb_AtX( cLogHeaderExp, cLog )
   IF Empty( cHit )
      cHit := ""
   ENDIF

   nStart := At( AllTrim( cHit ), cLog )
   IF nStart > 0

      cHit := hb_AtX( cLogHeaderExp, cLog,, nStart + Len( cHit ) )
      IF Empty( cHit )
         cHit := ""
      ENDIF

      nEnd := At( AllTrim( cHit ), cLog )
      IF nEnd == 0
         nEnd := Len( cLog )
      ENDIF

      cLog := hb_StrShrink( SubStr( cLog, nStart, nEnd - nStart ), Len( hb_eol() ) )
   ELSE
      cLog := ""
   ENDIF

   hb_cdpSelect( cOldLang )

   RETURN cLog

STATIC FUNCTION MakeEntry( aChanges, cMyName, cLogName, lAllowChangeLog )

   LOCAL nOffset := hb_UTCOffset()

   LOCAL cLog := hb_StrFormat( "%1$s UTC%2$s%3$02d%4$02d %5$s", ;
      hb_TToC( hb_DateTime(), "YYYY-MM-DD", "HH:MM" ), ;
      iif( nOffset < 0, "-", "+" ), ;
      Int( Abs( nOffset ) / 3600 ), ;
      Int( ( ( Abs( nOffset ) / 3600 ) - Int( Abs( nOffset ) / 3600 ) ) * 60 ), ;
      cMyName ) + hb_eol()

   LOCAL cLine

   FOR EACH cLine IN aChanges
      IF lAllowChangeLog .OR. !( SubStr( cLine, 5 ) == hb_FNameNameExt( cLogName ) )
         cLog += cLine + hb_eol()
      ENDIF
   NEXT

   RETURN cLog

STATIC FUNCTION IsLastEntryEmpty( cLog, cLogName, /* @ */ lChangeLog )

   LOCAL cLine

   lChangeLog := .F.

   FOR EACH cLine IN hb_ATokens( StrTran( cLog, Chr( 13 ) ), Chr( 10 ) )
      IF cLine:__enumIndex() != 1
         IF Empty( Left( cLine, 2 ) ) .AND. ! Empty( SubStr( cLine, 3, 1 ) )
            IF SubStr( cLine, 5 ) == hb_FNameNameExt( cLogName )
               lChangeLog := .T.
            ENDIF
         ELSE
            IF ! Empty( cLine )
               RETURN .F.
            ENDIF
         ENDIF
      ENDIF
   NEXT

   RETURN .T.

/* If it's a single mod, include only the change text,
   otherwise include the whole entry. */
STATIC FUNCTION EntryToCommitMsg( cLog )

   LOCAL cLine
   LOCAL cMsg
   LOCAL nCount := 0

   FOR EACH cLine IN hb_ATokens( StrTran( cLog, Chr( 13 ) ), Chr( 10 ) )
      IF cLine:__enumIndex() != 1
         IF !( Empty( Left( cLine, 2 ) ) .AND. ! Empty( SubStr( cLine, 3, 1 ) ) )
            IF ! Empty( cLine )
               cMsg := SubStr( cLine, 7 )
               ++nCount
            ENDIF
         ENDIF
      ENDIF
   NEXT

   RETURN iif( nCount == 1, cMsg + hb_eol(), cLog )

STATIC FUNCTION VCSDetect( /* @ */ cVCSDir, /* @ */ cLocalRoot )

   DO CASE
   CASE hb_DirExists( ".svn" )
      cVCSDir := hb_DirSepToOS( "./.svn/" )
      cLocalRoot := hb_DirSepToOS( "./" )
      RETURN "svn"
   CASE hb_DirExists( ".git" )
      cVCSDir := hb_DirSepToOS( "./.git/" )
      cLocalRoot := hb_DirSepToOS( "./" )
      RETURN "git"
   CASE GitDetect( @cVCSDir )
      cVCSDir := cVCSDir
      cLocalRoot := GitLocalRoot()
      RETURN "git"
   ENDCASE

   cVCSDir := ""

   RETURN ""

STATIC FUNCTION GitDetect( /* @ */ cGitDir )

   LOCAL cStdOut, cStdErr
   LOCAL nResult := hb_processRun( "git rev-parse --is-inside-work-tree",, @cStdOut, @cStdErr )

   IF nResult == 0 .AND. hb_StrReplace( cStdOut, Chr( 13 ) + Chr( 10 ) ) == "true"
      hb_processRun( "git rev-parse --git-dir",, @cGitDir )
      cGitDir := hb_DirSepAdd( hb_DirSepToOS( hb_StrReplace( cGitDir, Chr( 13 ) + Chr( 10 ) ) ) )
      RETURN .T.
   ENDIF

   RETURN .F.

STATIC FUNCTION GitLocalRoot()

   LOCAL cStdOut, cStdErr
   LOCAL nResult := hb_processRun( "git rev-parse --show-toplevel",, @cStdOut, @cStdErr )

   RETURN iif( nResult == 0, hb_DirSepAdd( hb_DirSepToOS( hb_StrReplace( cStdOut, Chr( 13 ) + Chr( 10 ) ) ) ), "" )

STATIC FUNCTION GitIsMerge( cGitDir )
   RETURN hb_FileExists( cGitDir + "MERGE_MSG" )

STATIC FUNCTION GitFileList()

   LOCAL cStdOut
   LOCAL nResult := hb_processRun( "git ls-files",, @cStdOut )
   LOCAL aList := iif( nResult == 0, hb_ATokens( StrTran( cStdOut, Chr( 13 ) ), Chr( 10 ) ), {} )
   LOCAL cItem

   FOR EACH cItem IN aList DESCEND
      IF Empty( cItem )
         hb_ADel( aList, cItem:__enumIndex(), .T. )
      ELSE
         cItem := hb_DirSepToOS( cItem )
      ENDIF
   NEXT

   RETURN aList

STATIC FUNCTION GitUser()

   LOCAL cName := ""
   LOCAL cEMail := ""

   hb_processRun( Shell() + " " + CmdEscape( "git config user.name" ),, @cName )
   hb_processRun( Shell() + " " + CmdEscape( "git config user.email" ),, @cEMail )

   RETURN hb_StrFormat( "%s (%s)", ;
      AllTrim( hb_StrReplace( cName, Chr( 10 ) + Chr( 13 ), "" ) ), ;
      StrTran( AllTrim( hb_StrReplace( cEMail, Chr( 10 ) + Chr( 13 ), "" ) ), "@", " " ) )

STATIC FUNCTION GitEditor()

   LOCAL cValue := ""

   hb_processRun( Shell() + " " + CmdEscape( "git config --global core.editor" ),, @cValue )

   cValue := hb_StrReplace( cValue, Chr( 10 ) + Chr( 13 ), "" )

   IF Left( cValue, 1 ) == "'" .AND. Right( cValue, 1 ) == "'"
      cValue := hb_StrShrink( SubStr( cValue, 2 ), 1 )
   ENDIF

   IF Lower( cValue ) == "notepad.exe" /* banned, use notepad2.exe or else */
      cValue := ""
   ENDIF

   RETURN cValue

STATIC FUNCTION DoctorChanges( cVCS, aChanges, aFiles )

   LOCAL cLine
   LOCAL cStart
   LOCAL aNew := {}

   LOCAL cFile
   LOCAL tmp

   ASort( aChanges,,, {| x, y | x < y } )

   DO CASE
   CASE cVCS == "svn"

      FOR EACH cLine IN aChanges
         IF ! Empty( cLine ) .AND. SubStr( cLine, 8, 1 ) == " "
            cStart := Left( cLine, 1 )
            SWITCH cStart
            CASE "M"
            CASE " " /* modified props */
               cStart := "*"
               EXIT
            CASE "A"
               cStart := "+"
               EXIT
            CASE "D"
               cStart := "-"
               EXIT
            CASE "X"
               cStart := ""
               EXIT
            OTHERWISE
               cStart := "?"
            ENDSWITCH
            IF ! Empty( cStart )
               AAdd( aNew, "  " + cStart + " " + StrTran( SubStr( cLine, 8 + 1 ), "\", "/" ) )
               IF !( cStart == "-" )
                  AAdd( aFiles, SubStr( cLine, 8 + 1 ) )
               ENDIF
            ENDIF
         ENDIF
      NEXT

   CASE cVCS == "git"

      FOR EACH cLine IN aChanges
         IF ! Empty( cLine ) .AND. SubStr( cLine, 3, 1 ) == " "
            cStart := Left( cLine, 1 )
            IF Empty( Left( cLine, 1 ) )
               cStart := SubStr( cLine, 2, 1 )
            ENDIF
            SWITCH cStart
            CASE " "
            CASE "?"
               cStart := ""
               EXIT
            CASE "M"
            CASE "R"
            CASE "T"
            CASE "U"
               cStart := "*"
               EXIT
            CASE "A"
            CASE "C"
               cStart := "+"
               EXIT
            CASE "D"
               cStart := "-"
               EXIT
            OTHERWISE
               cStart := "?"
            ENDSWITCH
            IF ! Empty( cStart )
               AAdd( aNew, "  " + cStart + " " + StrTran( SubStr( cLine, 3 + 1 ), "\", "/" ) )
               IF !( cStart == "-" )
                  cFile := SubStr( cLine, 3 + 1 )
                  IF ( tmp := At( " -> ", cFile ) ) > 0
                     cFile := SubStr( cFile, tmp + Len( " -> " ) )
                  ENDIF
                  AAdd( aFiles, cFile )
               ENDIF
            ENDIF
         ENDIF
      NEXT

   ENDCASE

   RETURN aNew

STATIC FUNCTION Shell()

   LOCAL cShell

#if defined( __PLATFORM__UNIX )
   cShell := GetEnv( "SHELL" )
#else
   cShell := GetEnv( "COMSPEC" )
#endif

   IF ! Empty( cShell )
#if defined( __PLATFORM__UNIX )
      cShell += " -c"
#else
      cShell += " /c"
#endif
   ENDIF

   RETURN cShell

STATIC FUNCTION CmdEscape( cCmd )

#if defined( __PLATFORM__UNIX )
   cCmd := '"' + cCmd + '"'
#endif

   RETURN cCmd

STATIC FUNCTION Changes( cVCS )

   LOCAL cStdOut := ""

   DO CASE
   CASE cVCS == "svn" ; hb_processRun( Shell() + " " + CmdEscape( "svn status -q" ),, @cStdOut )
   CASE cVCS == "git" ; hb_processRun( Shell() + " " + CmdEscape( "git status -s" ),, @cStdOut )
   ENDCASE

   RETURN hb_ATokens( StrTran( cStdOut, Chr( 13 ) ), Chr( 10 ) )

STATIC FUNCTION LaunchCommand( cCommand, cArg )

   IF Empty( cCommand )
      RETURN -1
   ENDIF

#if defined( __PLATFORM__WINDOWS )
   IF hb_osIsWinNT()
      cCommand := 'start "" "' + cCommand + '"'
   ELSE
      cCommand := "start " + cCommand
   ENDIF
#elif defined( __PLATFORM__OS2 )
   cCommand := 'start "" "' + cCommand + '"'
#endif

   RETURN hb_run( cCommand + " " + cArg )

/* ---- */

#define _HBROOT_  hb_PathNormalize( hb_DirSepToOS( hb_DirBase() + "../" ) )  /* must end with dirsep */

STATIC FUNCTION CheckFileList( xName, cLocalRoot, lRebase )

   LOCAL lPassed := .T.

   LOCAL aErr, s
   LOCAL file

   LOCAL lApplyFixes := "--fixup" $ cli_Options()

   hb_default( @cLocalRoot, "" )
   hb_default( @lRebase, .T. )

   IF HB_ISSTRING( xName )
      xName := { xName }
      IF "--fixup-case" $ cli_Options()
         lRebase := .F.
      ENDIF
   ENDIF

   IF Empty( xName ) .OR. HB_ISARRAY( xName )
      IF ! HB_ISARRAY( xName )
         IF GitDetect()
            xName := GitFileList()
            lRebase := .F.
         ELSE
            s := hb_cwd( _HBROOT_ )
            xName := my_DirScan( hb_osFileMask() )
            hb_cwd( s )
         ENDIF
         lApplyFixes := .F.  /* do not allow to mass fix all files */
      ENDIF
      IF "--fixup-case" $ cli_Options()
         FOR EACH file IN xName
            IF "|" + hb_FNameExt( file ) + "|" $ "|.c|.cpp|.h|.api|.ch|.hb|.po|.prg|.md|.txt|"
               FixFuncCase( file, .T., lRebase )
            ENDIF
         NEXT
      ELSE
         FOR EACH file IN xName
            IF ! CheckFile( file, @aErr, lApplyFixes, cLocalRoot, lRebase )
               lPassed := .F.
               FOR EACH s IN aErr
                  OutStd( file + ": " + s + hb_eol() )
               NEXT
            ENDIF
         NEXT
      ENDIF
   ENDIF

   RETURN lPassed

STATIC FUNCTION CheckFile( cName, /* @ */ aErr, lApplyFixes, cLocalRoot, lRebase )

   LOCAL cFile
   LOCAL tmp
   LOCAL cEOL

   LOCAL lProcess
   LOCAL lReBuild
   LOCAL lRemoveEndingWhitespace

   /* TOFIX: Harbour repo specific */
   LOCAL aCanBeUpper := { ;
      "Makefile", ;
      "README.md", ;
      "COPYING.txt", ;
      "*/RELNOTES.txt", ;
      "*/HARBOUR_README_*.txt", ;
      "ChangeLog.txt", ;
      "*.po", ;
      "*.md" }

   LOCAL aCanBeDot := { ;
      ".travis.yml", ;
      ".git*", ;
      ".ackrc" }

   /* TOFIX: Harbour repo specific */
   LOCAL aCanBeLong := { ;
      "ChangeLog.txt", ;
      ".git*", ;
      "*.po", ;
      "*.md", ;
      "*.html", ;
      "*/hb-charmap.def", ;  /* TOFIX: Use 8.3 name */
      "debian/*", ;
      "package/*", ;
      "lib/3rd/*", ;
      "contrib/gtqtc/*", ;
      "contrib/hbwin/*", ;
      "contrib/rddads/unixutils.h", ;
      "extras/httpsrv/*" }

   /* TOFIX: Harbour repo specific */
   LOCAL aCanHaveNoExtension := { ;
      ".*", ;
      "Makefile", ;
      "debian/*" }

   /* TOFIX: Harbour repo specific */
   LOCAL aCanHaveTab := { ;
      "Makefile", ;
      "debian/rules", ;
      "*.mk", ;
      "*.yyc", ;
      "*.dif", ;
      "*.htm", ;
      "*.html", ;
      "*.plist", ;
      "*.xml", ;
      "*.xsd", ;
      "*.xsl", ;
      "*.java", ;
      "*.js", ;
      "*.vbs", ;
      "*.css" }

   LOCAL aAnyIdent := { ;
      "*.dif" }

   LOCAL aCanHaveSpaceAtEol := { ;
      "*.dif", ;
      "*.md" }

   LOCAL aCanHaveAnyEncoding := { ;
      "*.dif" }

   LOCAL aForcedCRLF := { ;
      "*.bat" }

   LOCAL aForcedLF := { ;
      "*.sh" }

   /* TOFIX: Harbour repo specific */
   LOCAL aNoProc := { ;
      "maskimag.png" }  /* libharu demo will fail if optimized */

   /* TOFIX: Harbour repo specific */
   LOCAL aNoCopyrightOk := { ;
      "tests/*", ;
      "*/tests/*", ;
      "src/codepage/*", ;
      "src/lang/*" }

   LOCAL hFlags
   LOCAL aCanHaveIdent
   LOCAL hAllowedExt := LoadGitattributes( cLocalRoot + ".gitattributes", @aCanHaveIdent, @hFlags )
   LOCAL nLines

   /* TODO: extend as you go */
   /* TOFIX: Harbour repo specific */
   LOCAL hDoNotProcess := { ;
      ".c" => { "3rd", "include", "dlmalloc", "hvm", "sha1", "sha2" }, ;
      ".h" => { "3rd", "include" } }

   hb_default( @lApplyFixes, .F. )

   aErr := {}

   cName := hb_DirSepToOS( cName )

   IF hb_FileExists( iif( lRebase, _HBROOT_, "" ) + cName ) .AND. ;
      ! FNameExc( cName, LoadGitignore( cLocalRoot + ".gitignore" ) )

      /* filename checks */

      IF "msdosfs" $ hFLags .AND. ( Len( hb_FNameName( cName ) ) > 8 .OR. Len( hb_FNameExt( cName ) ) > 4 ) .AND. ! FNameExc( cName, aCanBeLong )
         AAdd( aErr, "filename: non-8.3" )
      ENDIF

      IF "msdosfs" $ hFLags .AND. Left( hb_FNameName( cName ), 1 ) == "." .AND. ! FNameExc( cName, aCanBeDot )
         AAdd( aErr, "filename: non MS-DOS compatible" )
      ENDIF

      IF Empty( hb_FNameExt( cName ) )
         IF ! FNameExc( cName, aCanHaveNoExtension )
            AAdd( aErr, "filename: missing extension" )
         ENDIF
      ELSEIF ! Empty( hAllowedExt ) .AND. ! hb_FNameExt( cName ) $ hAllowedExt
         AAdd( aErr, "filename: unknown extension. Either change it or update .gitattributes." )
      ENDIF

      IF "casematters" $ hFLags .AND. !( cName == Lower( cName ) ) .AND. ! FNameExc( cName, aCanBeUpper )
         AAdd( aErr, "filename: non-lowercase" )
      ENDIF

      IF ! IsASCII7( cName )
         AAdd( aErr, "filename: non-ASCII-7" )
      ENDIF

      cFile := hb_MemoRead( iif( lRebase, _HBROOT_, "" ) + cName )

      IF ! IsBinary( cFile )

         IF hb_FileMatch( cName, "ChangeLog.txt" ) .AND. Len( cFile ) > 32768 .AND. ! lApplyFixes
            cFile := RTrimEOL( Left( cFile, 16384 ) ) + LTrim( Right( cFile, 16384 ) )
         ENDIF

         lReBuild := .F.

         /* text content checks */

         IF ! FNameExc( cName, aCanHaveTab ) .AND. e"\t" $ cFile
            AAdd( aErr, "content: has tab" )
         ENDIF

         IF hb_BLeft( cFile, hb_BLen( UTF8_BOM() ) ) == UTF8_BOM()
            AAdd( aErr, "content: has BOM" )
            IF lApplyFixes
               cFile := hb_BSubStr( cFile, hb_BLen( UTF8_BOM() ) + 1 )
            ENDIF
         ENDIF

         IF Right( cFile, 1 ) == Chr( 26 )
            AAdd( aErr, "content: has legacy EOF char" )
            IF lApplyFixes
               cFile := hb_StrShrink( cFile, 1 )
            ENDIF
         ENDIF

         cEOL := EOLDetect( cFile, @nLines )

         IF Len( cEOL ) == 0
            AAdd( aErr, "content: has mixed EOL types" )
            IF lApplyFixes
               lReBuild := .T.
            ENDIF
         ENDIF

         IF FNameExc( cName, aForcedCRLF ) .AND. !( cEOL == Chr( 13 ) + Chr( 10 ) )
            AAdd( aErr, "content: must use CRLF EOL for file type" )
            IF lApplyFixes
               cFile := StrTran( StrTran( cFile, Chr( 13 ) ), Chr( 10 ), cEOL := Chr( 13 ) + Chr( 10 ) )
            ENDIF
         ENDIF

         IF FNameExc( cName, aForcedLF ) .AND. !( cEOL == Chr( 10 ) )
            AAdd( aErr, "content: must use LF EOL for file type" )
            IF lApplyFixes
               cFile := StrTran( cFile, Chr( 13 ) )
               cEOL := Chr( 10 )
            ENDIF
         ENDIF

         IF ! FNameExc( cName, aCanHaveSpaceAtEol ) .AND. EndingWhitespace( cFile )
            AAdd( aErr, "content: has ending whitespace" )
            IF lApplyFixes
               lRemoveEndingWhitespace := .T.
               lReBuild := .T.
            ENDIF
         ENDIF

         IF lReBuild
            cFile := RemoveEndingWhitespace( cFile, iif( Empty( cEOL ), hb_eol(), cEOL ), lRemoveEndingWhitespace )
         ENDIF

         IF !( Right( cFile, Len( Chr( 10 ) ) ) == Chr( 10 ) )
            AAdd( aErr, "content: has no EOL at EOF" )
            IF lApplyFixes
               cFile += iif( Empty( cEOL ), hb_eol(), cEOL )
            ENDIF
         ENDIF

         IF Right( cFile, Len( Chr( 10 ) ) * 2 ) == Replicate( Chr( 10 ), 2 )
            AAdd( aErr, "content: has multiple EOL at EOF" )
            IF lApplyFixes
               DO WHILE Right( cFile, Len( Chr( 10 ) ) * 2 ) == Replicate( Chr( 10 ), 2 )
                  cFile := hb_StrShrink( cFile, Len( Chr( 10 ) ) )
               ENDDO
            ENDIF
         ELSEIF Right( cFile, Len( Chr( 13 ) + Chr( 10 ) ) * 2 ) == Replicate( Chr( 13 ) + Chr( 10 ), 2 )
            AAdd( aErr, "content: has multiple EOL at EOF" )
            IF lApplyFixes
               DO WHILE Right( cFile, Len( Chr( 13 ) + Chr( 10 ) ) * 2 ) == Replicate( Chr( 13 ) + Chr( 10 ), 2 )
                  cFile := hb_StrShrink( cFile, Len( Chr( 13 ) + Chr( 10 ) ) )
               ENDDO
            ENDIF
         ENDIF

         IF ! FNameExc( cName, aCanHaveAnyEncoding )
            tmp := -1
            IF ! IsASCII7( cFile, @tmp ) .AND. ! IsUTF8( cFile )
               AAdd( aErr, "content: is non-UTF-8/ASCII-7: " + hb_ntos( tmp ) )
            ENDIF
         ENDIF

         IF ! FNameExc( cName, aAnyIdent ) .AND. ;
            ( tmp := ( ( "$" + "Id" ) $ cFile ) ) != FNameExc( cName, aCanHaveIdent )
            IF tmp
               AAdd( aErr, "content: has " + "$" + "Id" )
            ELSE
               AAdd( aErr, "content: missing " + "$" + "Id" )
            ENDIF
         ENDIF

         /* TOFIX: Harbour repo specific */
         IF "|" + hb_FNameExt( cName ) + "|" $ "|.c|.h|.api|.prg|.hb|.ch|" .AND. ;
            nLines > 20 .AND. ;
            ! FNameExc( cName, aNoCopyrightOk ) .AND. ;
            ! "public domain" $ Lower( cFile ) .AND. ;
            ! "copyright" $ Lower( cFile ) .AND. ;
            ! "license" $ Lower( cFile )
            AAdd( aErr, "content: source code missing copyright/license" )
         ENDIF

         IF "|" + hb_FNameExt( cName ) + "|" $ "|.c|.h|.api|"
            IF "//" $ StripCStrings( StripCComments( cFile ) )
               AAdd( aErr, "content: C file with C++ comment" )
            ENDIF
         ENDIF
      ENDIF

      IF lApplyFixes
         lProcess := .T.
         FOR EACH tmp IN hb_HGetDef( hDoNotProcess, hb_FNameExt( cName ), {} )
            IF tmp $ cName
               lProcess := .F.
               EXIT
            ENDIF
         NEXT
         IF lProcess .AND. ! FNameExc( cName, aNoProc )
            OutStd( cName + ": " + "content: processing" + hb_eol() )
            ProcFile( cName )
         ENDIF
      ENDIF
   ENDIF

   RETURN Empty( aErr )

STATIC FUNCTION IsBinary( cFile )
   RETURN Chr( 0 ) $ cFile .OR. !( Chr( 10 ) $ cFile )

STATIC FUNCTION RTrimEOL( cFile )

   DO WHILE Right( cFile, 1 ) $ Chr( 13 ) + Chr( 10 )
      cFile := hb_StrShrink( cFile, 1 )
   ENDDO

   RETURN cFile

/*
 * UTF-8 encoding detection, based on filestr.cpp from Far Manager.
 * Harbour adaptation Copyright 2013 Viktor Szakats (vszakats.net/harbour)
 */

/*
Copyright (c) 1996 Eugene Roshal
Copyright (c) 2000 Far Group
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. The name of the authors may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
STATIC FUNCTION IsUTF8( cString )

   LOCAL lASCII := .T.
   LOCAL nOctets := 0
   LOCAL nChar
   LOCAL tmp

   FOR EACH tmp IN cString

      nChar := hb_BCode( tmp )

      IF hb_bitAnd( nChar, 0x80 ) != 0
         lASCII := .F.
      ENDIF

      IF nOctets != 0

         IF hb_bitAnd( nChar, 0xC0 ) != 0x80
            RETURN .F.
         ENDIF

         --nOctets

      ELSEIF hb_bitAnd( nChar, 0x80 ) != 0

         DO WHILE hb_bitAnd( nChar, 0x80 ) != 0
            nChar := hb_bitAnd( hb_bitShift( nChar, 1 ), 0xFF )
            ++nOctets
         ENDDO

         --nOctets

         IF nOctets == 0
            RETURN .F.
         ENDIF
      ENDIF
   NEXT

   RETURN !( nOctets > 0 .OR. lASCII )

STATIC FUNCTION IsASCII7( cString, /* @ */ nChar )

   LOCAL tmp

   FOR EACH tmp IN cString
      nChar := hb_BCode( tmp )
      IF ( nChar < 32 .OR. nChar > 126 ) .AND. ;
         nChar != 10 .AND. nChar != 13 .AND. nChar != 9 .AND. nChar != 12
         RETURN .F.
      ENDIF
   NEXT

   RETURN .T.

STATIC FUNCTION EOLDetect( cFile, /* @ */ nLines )

   LOCAL nCR := 0
   LOCAL nLF := 0
   LOCAL tmp

   FOR EACH tmp IN cFile
      IF tmp == Chr( 13 )
         ++nCR
      ELSEIF tmp == Chr( 10 )
         ++nLF
      ENDIF
   NEXT

   IF nCR > 0 .AND. nLF == 0
      nLines := nCR
      RETURN Chr( 13 )
   ELSEIF nCR == 0 .AND. nLF > 0
      nLines := nLF
      RETURN Chr( 10 )
   ELSEIF nCR == 0 .AND. nLF == 0
      nLines := 0
      RETURN "binary"
   ELSEIF nCR == nLF
      nLines := nCR
      RETURN Chr( 13 ) + Chr( 10 )
   ENDIF

   nLines := -1

   RETURN ""

STATIC FUNCTION EndingWhitespace( cFile )

   LOCAL cLine

   FOR EACH cLine IN hb_ATokens( StrTran( cFile, Chr( 13 ) ), Chr( 10 ) )
      IF Right( cLine, 1 ) == " "
         RETURN .T.
      ENDIF
   NEXT

   RETURN .F.

STATIC FUNCTION RemoveEndingWhitespace( cFile, cEOL, lRTrim )

   LOCAL cResult := ""
   LOCAL cLine

   FOR EACH cLine IN hb_ATokens( StrTran( cFile, Chr( 13 ) ), Chr( 10 ) )
      cResult += iif( lRTrim, RTrim( cLine ), cLine )
      IF ! cLine:__enumIsLast()
         cResult += cEOL
      ENDIF
   NEXT

   RETURN cResult

/* retains positions in file */
STATIC FUNCTION StripCStrings( cFile )

   LOCAL nPos := 1
   LOCAL aHits := {}
   LOCAL tmp

   DO WHILE ( tmp := hb_BAt( '"', cFile, nPos ) ) > 0
      /* TOFIX: imprecise escaped char detection */
      IF ( !( hb_BSubStr( cFile, tmp - 1, 1 ) == "\" ) .OR. ;
         hb_BSubStr( cFile, tmp - 2, 2 ) == "\\" ) .AND. ;
         !( hb_BSubStr( cFile, tmp - 1, 1 ) + hb_BSubStr( cFile, tmp + 1, 1 ) == "''" )
         AAdd( aHits, tmp )
      ENDIF
      nPos := tmp + 1
   ENDDO

   /* unbalanced */
   IF Len( aHits ) % 2 != 0
      AAdd( aHits, hb_BLen( cFile ) )
   ENDIF

   FOR tmp := 1 TO Len( aHits ) STEP 2
      cFile := hb_BLeft( cFile, aHits[ tmp ] ) + Replicate( " ", aHits[ tmp + 1 ] - aHits[ tmp ] - 1 ) + hb_BSubStr( cFile, aHits[ tmp + 1 ] )
   NEXT

   RETURN cFile

/* retains positions in file */
STATIC FUNCTION StripCComments( cFile )

   LOCAL nPos := 1
   LOCAL aHits := {}
   LOCAL tmp
   LOCAL tmp1
   LOCAL lStart := .T.

   /* bare bones */
   DO WHILE ( tmp := hb_BAt( iif( lStart, "/*", "*/" ), cFile, nPos ) ) > 0
      AAdd( aHits, tmp + iif( lStart, 0, 2 ) )
      nPos := tmp
      lStart := ! lStart
   ENDDO

   /* unbalanced */
   IF Len( aHits ) % 2 != 0
      AAdd( aHits, hb_BLen( cFile ) )
   ENDIF

   FOR tmp := 1 TO Len( aHits ) STEP 2
      FOR tmp1 := aHits[ tmp ] TO aHits[ tmp + 1 ]
         IF ! hb_BSubStr( cFile, tmp1, 1 ) $ Chr( 13 ) + Chr( 10 )
            hb_BPoke( @cFile, tmp1, hb_BCode( " " ) )
         ENDIF
      NEXT
   NEXT

   RETURN cFile

/* retains positions in file */
/* same as StripCComments() but gathers the comments in a new strings */
STATIC FUNCTION GetCComments( cFile )

   LOCAL nPos := 1
   LOCAL aHits := {}
   LOCAL tmp
   LOCAL tmp1
   LOCAL lStart := .T.

   LOCAL cComments

   /* bare bones */
   DO WHILE ( tmp := hb_BAt( iif( lStart, "/*", "*/" ), cFile, nPos ) ) > 0
      AAdd( aHits, tmp + iif( lStart, 0, 2 ) )
      nPos := tmp
      lStart := ! lStart
   ENDDO

   /* unbalanced */
   IF Len( aHits ) % 2 != 0
      AAdd( aHits, hb_BLen( cFile ) )
   ENDIF

   cComments := Space( hb_BLen( cFile ) )

   FOR tmp := 1 TO Len( aHits ) STEP 2
      FOR tmp1 := aHits[ tmp ] TO aHits[ tmp + 1 ]
         hb_BPoke( @cComments, tmp1, hb_BPeek( cFile, tmp1 ) )
      NEXT
   NEXT

   RETURN cComments

STATIC FUNCTION FNameExc( cName, aList )

   LOCAL tmp, tmp1

   FOR EACH tmp IN aList
      IF !( Left( tmp, 1 ) == "!" ) .AND. ;
         ( hb_FileMatch( cName, hb_DirSepToOS( tmp ) ) .OR. hb_FileMatch( hb_FNameNameExt( cName ), hb_DirSepToOS( tmp ) ) )
         FOR EACH tmp1 IN aList
            IF Left( tmp1, 1 ) == "!" .AND. hb_FileMatch( cName, hb_DirSepToOS( SubStr( tmp1, 2 ) ) )
               RETURN .F.
            ENDIF
         NEXT
         RETURN .T.
      ENDIF
   NEXT

   RETURN .F.

STATIC PROCEDURE ProcFile( cFileName )

   /* TOFIX: bin/harbour.ucf is in Harbour dir, not current project */
   LOCAL hProc := { ;
      ".png" => { "advpng -z -4 %1$s", "optipng -o7 %1$s" }, ;
      ".jpg" => { "jpegoptim --strip-all %1$s" }, ;
      ".c"   => { hb_StrFormat( "uncrustify -c %1$s %%1$s", hb_DirSepToOS( _HBROOT_ + "bin/harbour.ucf" ) ), @FixFuncCase() }, ;
      ".cpp" => ".c", ;
      ".h"   => ".c", ;
      ".api" => ".c", ;
      ".go"  => { "go fmt %1$s" }, ;
      ".txt" => { @FixFuncCase() }, ;
      ".md"  => ".txt", ;
      ".po"  => ".txt", ;
      ".prg" => { @FixFuncCase() /*, "hbformat %1$s" */ }, ;  /* NOTE: hbformat has bugs which make it unsuitable for unattended use */
      ".hb"  => ".prg", ;
      ".ch"  => ".prg" }

   LOCAL aProc := hb_FNameExt( cFileName )
   LOCAL xCmd

   DO WHILE HB_ISSTRING( aProc := hb_HGetDef( hProc, aProc, NIL ) )
   ENDDO

   IF HB_ISARRAY( aProc )
      FOR EACH xCmd IN aProc
         IF HB_ISSTRING( xCmd )
            hb_run( hb_StrFormat( hb_DirSepToOS( xCmd ), '"' + _HBROOT_ + cFileName + '"' ) )
         ELSEIF HB_ISEVALITEM( xCmd )
            Eval( xCmd, cFileName )
         ENDIF
      NEXT
   ENDIF

   RETURN

STATIC FUNCTION UTF8_BOM()
   RETURN ;
      hb_BChar( 0xEF ) + ;
      hb_BChar( 0xBB ) + ;
      hb_BChar( 0xBF )

STATIC FUNCTION LoadGitignore( cFileName )

   THREAD STATIC t_aIgnore := NIL

   LOCAL cLine

   IF t_aIgnore == NIL

      /* TOFIX: Harbour repo specific */
      t_aIgnore := { ;
         "*/3rd/*", ;
         "!*/3rd/*/*.hbc", ;
         "!*/3rd/*/*.hbp", ;
         "!*/3rd/*/Makefile" }

      FOR EACH cLine IN hb_ATokens( StrTran( hb_MemoRead( cFileName ), Chr( 13 ) ), Chr( 10 ) )
         IF ! Empty( cLine ) .AND. !( Left( cLine, 1 ) == "#" )
            /* TODO: clean this */
            AAdd( t_aIgnore, ;
               iif( Left( cLine, 1 ) $ "?*/!", "", "*/" ) + ;
               cLine + ;
               iif( Right( cLine, 1 ) == "/", "*", ;
               iif( Empty( hb_FNameExt( cLine ) ) .AND. !( Right( cLine, 2 ) == "*/" ), "/*", "" ) ) )
            IF !( ATail( t_aIgnore ) == cLine )
               IF Left( ATail( t_aIgnore ), 2 ) == "*/"
                  AAdd( t_aIgnore, SubStr( ATail( t_aIgnore ), 3 ) )
               ENDIF
               AAdd( t_aIgnore, cLine )
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN t_aIgnore

STATIC FUNCTION LoadGitattributes( cFileName, /* @ */ aIdent, /* @ */ hFlags )

   THREAD STATIC t_hExt := NIL
   THREAD STATIC t_aIdent := NIL
   THREAD STATIC t_hFlags := NIL

   LOCAL cLine
   LOCAL tmp

   IF t_hExt == NIL
      t_hExt := { => }
      t_aIdent := {}
      t_hFlags := { => }
      FOR EACH cLine IN hb_ATokens( StrTran( hb_MemoRead( cFileName ), Chr( 13 ) ), Chr( 10 ) )
         IF Left( cLine, 2 ) == "*."
             cLine := SubStr( cLine, 2 )
             IF ( tmp := At( " ", cLine ) ) > 0
                t_hExt[ RTrim( Left( cLine, tmp - 1 ) ) ] := NIL
             ENDIF
         ENDIF
         IF ( tmp := At( " ", cLine ) ) > 0 .AND. "ident" $ SubStr( cLine, tmp + 1 )
            AAdd( t_aIdent, RTrim( Left( cLine, tmp - 1 ) ) )
         ENDIF
         IF Left( cLine, 3 ) == "## "
            t_hFlags[ Lower( SubStr( cLine, 4 ) ) ] := NIL
         ENDIF
      NEXT
   ENDIF

   aIdent := t_aIdent
   hFlags := t_hFlags

   RETURN t_hExt

STATIC FUNCTION my_DirScan( cMask )
   RETURN my_DirScanWorker( cMask, {} )

STATIC FUNCTION my_DirScanWorker( cMask, aList )

   LOCAL file

   IF ! hb_FileExists( hb_FNameDir( cMask ) + ".git" )  /* skip Git submodules */
      FOR EACH file IN Directory( cMask, "D" )
         IF file[ F_NAME ] == "." .OR. file[ F_NAME ] == ".."
         ELSEIF "D" $ file[ F_ATTR ]
            my_DirScanWorker( hb_FNameDir( cMask ) + file[ F_NAME ] + hb_ps() + hb_FNameNameExt( cMask ), aList )
         ELSE
            AAdd( aList, hb_FNameDir( cMask ) + file[ F_NAME ] )
         ENDIF
      NEXT
   ENDIF

   RETURN aList

/* ---- */

STATIC FUNCTION FixFuncCase( cFileName, lVerbose, lRebase )

   STATIC sc_hInCommentOnly := { ;
      ".c"   =>, ;
      ".cpp" =>, ;
      ".h"   =>, ;
      ".api" => }

   /* TOFIX: Harbour repo specific */
   STATIC sc_hFileExceptions := { ;
      "ChangeLog.txt" =>, ;
      "std.ch"        =>, ;  /* compatibility */
      "big5_gen.prg"  =>, ;  /* new style code */
      "clsccast.prg"  =>, ;  /* new style code */
      "clsicast.prg"  =>, ;  /* new style code */
      "clsscast.prg"  =>, ;  /* new style code */
      "clsscope.prg"  =>, ;  /* new style code */
      "cpinfo.prg"    =>, ;  /* new style code */
      "foreach2.prg"  =>, ;  /* new style code */
      "keywords.prg"  =>, ;  /* new style code */
      "speedstr.prg"  =>, ;  /* new style code */
      "speedtst.prg"  =>, ;  /* new style code */
      "uc16_gen.prg"  =>, ;  /* new style code */
      "wcecon.prg"    =>, ;  /* new style code */
      "c_std.txt"     =>, ;  /* C level doc */
      "locks.txt"     =>, ;  /* C level doc */
      "pcode.txt"     =>, ;  /* C level doc */
      "tracing.txt"   =>, ;  /* C level doc */
      "xhb-diff.txt"  => }

   /* TOFIX: Harbour repo specific */
   STATIC sc_aMaskExceptions := { ;
      "*/3rd/*"          , ;  /* foreign code */
      "tests/hbpptest/*" , ;  /* test code, must be kept as is */
      "tests/mt/*"       , ;  /* new style code */
      "tests/multifnc/*" , ;
      "tests/rddtest/*"  }

   LOCAL hAll
   LOCAL cFile
   LOCAL cFileStripped

   LOCAL aMatchList
   LOCAL match
   LOCAL cProper
   LOCAL cOldCP

   LOCAL lInCommentOnly
   LOCAL nChanged := 0

   hb_default( @lVerbose, .F. )
   hb_default( @lRebase, .T. )

   IF Empty( hb_FNameExt( cFileName ) ) .OR. ;
      hb_FNameNameExt( cFileName ) $ sc_hFileExceptions .OR. ;
      AScan( sc_aMaskExceptions, {| tmp | hb_FileMatch( cFileName, hb_DirSepToOS( tmp ) ) } ) != 0
      RETURN .F.
   ENDIF

   hAll := __hbformat_BuildListOfFunctions()
   cFile := MemoRead( iif( lRebase, _HBROOT_, "" ) + cFileName )

   lInCommentOnly := hb_FNameExt( cFileName ) $ sc_hInCommentOnly
   cFileStripped := iif( lInCommentOnly, GetCComments( cFile ), cFile )

   #define _MATCH_cStr    1
   #define _MATCH_nStart  2
   #define _MATCH_nEnd    3

   cOldCP := hb_cdpSelect( "EN" )
   aMatchList := hb_regexAll( "([A-Za-z] |[^A-Za-z_:]|^)([A-Za-z_][A-Za-z0-9_]+\()", cFileStripped,,,,, .F. )
   hb_cdpSelect( cOldCP )

   FOR EACH match IN aMatchList
      IF Len( match[ 2 ][ _MATCH_cStr ] ) != 2 .OR. !( Left( match[ 2 ][ _MATCH_cStr ], 1 ) $ "D" /* "METHOD" */ )
         cProper := ProperCase( hAll, hb_StrShrink( match[ 3 ][ _MATCH_cStr ] ) ) + "("
         IF !( cProper == match[ 3 ][ _MATCH_cStr ] ) .AND. ;
            !( Upper( cProper ) == Upper( "FILE(" ) ) .AND. ;   /* interacts with "file(s)" text */
            !( Upper( cProper ) == Upper( "TOKEN(" ) ) .AND. ;  /* interacts with "token(s)" text */
            !( Upper( cProper ) == Upper( "INT(" ) ) .AND. ;    /* interacts with SQL statements */
            ( ! lInCommentOnly .OR. !( "|" + Lower( cProper ) + "|" $ Lower( "|Max(|Min(|FOpen(|Abs(|Log10(|GetEnv(|Sqrt(|Rand(|IsDigit(|IsAlpha(|" ) ) )
            cFile := Left( cFile, match[ 3 ][ _MATCH_nStart ] - 1 ) + cProper + SubStr( cFile, match[ 3 ][ _MATCH_nEnd ] + 1 )
            IF lVerbose
               OutStd( cFileName, match[ 3 ][ _MATCH_cStr ], cProper, "|" + match[ 1 ][ _MATCH_cStr ] + "|" + hb_eol() )
            ENDIF
            nChanged++
         ENDIF
      ENDIF
   NEXT

   IF ! lInCommentOnly
      cOldCP := hb_cdpSelect( "EN" )
      aMatchList := hb_regexAll( "(?:REQUEST|EXTERNAL|EXTERNA|EXTERN)[ \t]+([A-Za-z_][A-Za-z0-9_]+)", cFile,,,,, .F. )
      hb_cdpSelect( cOldCP )

      FOR EACH match IN aMatchList
         cProper := ProperCase( hAll, match[ 2 ][ _MATCH_cStr ] )
         IF !( cProper == match[ 2 ][ _MATCH_cStr ] )
            cFile := Left( cFile, match[ 2 ][ _MATCH_nStart ] - 1 ) + cProper + SubStr( cFile, match[ 2 ][ _MATCH_nEnd ] + 1 )
            OutStd( cFileName, match[ 2 ][ _MATCH_cStr ], cProper, "|" + match[ 1 ][ _MATCH_cStr ] + "|" + hb_eol() )
            nChanged++
         ENDIF
      NEXT
   ENDIF

   IF nChanged > 0
      OutStd( cFileName + ": Harbour function casings fixed: " + hb_ntos( nChanged ) + hb_eol() )
      hb_MemoWrit( iif( lRebase, _HBROOT_, "" ) + cFileName, cFile )
   ENDIF

   RETURN .T.

STATIC FUNCTION ProperCase( hAll, cName )

   IF cName $ hAll
      RETURN hb_HKeyAt( hAll, hb_HPos( hAll, cName ) )
   ENDIF

   RETURN cName

STATIC FUNCTION __hbformat_BuildListOfFunctions()

   THREAD STATIC t_hFunctions := NIL

   IF t_hFunctions == NIL
      t_hFunctions := { => }
      hb_HCaseMatch( t_hFunctions, .F. )

      WalkDir( hb_DirBase() + hb_DirSepToOS( _HBROOT_ + "include/" ), t_hFunctions )
      WalkDir( hb_DirBase() + hb_DirSepToOS( _HBROOT_ + "contrib/" ), t_hFunctions )
      WalkDir( hb_DirBase() + hb_DirSepToOS( _HBROOT_ + "extras/" ), t_hFunctions )
   ENDIF

   RETURN t_hFunctions

STATIC PROCEDURE WalkDir( cDir, hFunctions )

   LOCAL file
   LOCAL cLine

   FOR EACH file IN hb_DirScan( cDir, "*.hbx" )
      FOR EACH cLine IN hb_ATokens( StrTran( hb_MemoRead( cDir + file[ F_NAME ] ), Chr( 13 ) ), Chr( 10 ) )
         IF Left( cLine, Len( "DYNAMIC " ) ) == "DYNAMIC "
            hFunctions[ SubStr( cLine, Len( "DYNAMIC " ) + 1 ) ] := NIL
         ENDIF
      NEXT
   NEXT

   RETURN

/* ---- */
