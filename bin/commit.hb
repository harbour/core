#!/usr/bin/env hbmk2
/*
 * Commit preparer and source checker/fixer
 *
 * Copyright 2012-2013 Viktor Szakats (vszakats.net/harbour)
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
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 * (or visit their website at https://www.gnu.org/licenses/).
 *
 */

#define _CONFIGFIL_ ".hbcommit"
#define _CONFIGENV_ "HBCOMMIT_USER"

#pragma -w3
#pragma -km+
#pragma -ko+

#include "hbgtinfo.ch"

#define _COMMIT_HBROOT_  hb_PathNormalize( hb_DirSepToOS( hb_DirBase() + "../" ) )  /* must end with dirsep */

PROCEDURE Main( cParam1 )

   LOCAL cVCS
   LOCAL aFiles
   LOCAL aChanges
   LOCAL cLog
   LOCAL nStart, nEnd
   LOCAL cMyName
   LOCAL cLogName
   LOCAL lWasChangeLog

   InstallHook( "pre-commit"        , hb_StrFormat( "exec hbrun bin/%1$s.hb --check-only", hb_FNameName( hb_ProgName() ) ) )
// InstallHook( "prepare-commit-msg", hb_StrFormat( "exec hbrun bin/%1$s.hb $1 --prepare-commit", hb_FNameName( hb_ProgName() ) )

   cVCS := VCSDetect()
   aFiles := {}
   aChanges := DoctorChanges( cVCS, Changes( cVCS ), aFiles )

   IF Empty( aChanges )
      OutStd( hb_ProgName() + ": " + "no changes" + hb_eol() )
      ErrorLevel( 0 )
      RETURN
   ENDIF

   IF CheckFileList( aFiles )

      cLogName := FindChangeLog()
      IF Empty( cLogName )
         OutStd( hb_ProgName() + ": " + "cannot find ChangeLog file" + hb_eol() )
         ErrorLevel( 2 )
      ENDIF

      IF "--check-only" $ hb_CmdLine() .OR. ;
         "--prepare-commit" $ hb_CmdLine()

         IF AScan( aFiles, {| tmp | tmp == hb_FNameNameExt( cLogName ) } ) == 0
            OutStd( hb_ProgName() + ": " + hb_StrFormat( "%1$s not updated. Run 'hbrun bin/commit' and retry.", cLogName ) + hb_eol() )
            ErrorLevel( 3 )
            RETURN
         ELSE
            cLog := GetLastEntry( MemoRead( cLogName ), @nStart, @nEnd )
            IF ! Empty( cLog )
               IF "--prepare-commit" $ hb_CmdLine()
                  hb_MemoWrit( cParam1, EntryToCommitMsg( cLog ) + hb_MemoRead( cParam1 ) )
               ELSE
                  hbshell_gtSelect()
                  /* if clipboard already contains part of the entry, do not overwrite it */
                  IF ! hb_StrReplace( hb_gtInfo( HB_GTI_CLIPBOARDDATA ), Chr( 13 ) + Chr( 10 ), "" ) $ hb_StrReplace( cLog, Chr( 13 ) + Chr( 10 ), "" )
                     hb_gtInfo( HB_GTI_CLIPBOARDDATA, EntryToCommitMsg( cLog ) )
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
            ELSEIF hb_FileExists( _CONFIGFIL_ )
               cMyName := AllTrim( hb_MemoRead( _CONFIGFIL_ ) )
            ELSE
               cMyName := "Firstname Lastname (me domain.net)"
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

         OutStd( hb_ProgName() + ": " + hb_StrFormat( "Edit %1$s and commit", cLogName ) + hb_eol() )
//       LaunchCommand( GitEditor(), cLogName )
      ENDIF

      ErrorLevel( 0 )
   ELSE
      OutStd( hb_ProgName() + ": " + "Please correct errors listed above and re-run" + hb_eol() )
      ErrorLevel( 1 )
   ENDIF

   RETURN

STATIC FUNCTION InstallHook( cHookName, cCommand )

   LOCAL cName := _COMMIT_HBROOT_ + hb_DirSepToOS( ".git/hooks/" ) + cHookName
   LOCAL cFile := hb_MemoRead( cName )

   IF cCommand $ cFile
      RETURN .T.
   ENDIF

   IF Empty( cFile )
      cFile += "#!/bin/sh" + hb_eol()
   ENDIF

   RETURN hb_MemoWrit( cName, cFile + hb_eol() + cCommand + hb_eol() )

STATIC FUNCTION FindChangeLog()

   LOCAL cLogName

   IF ! hb_FileExists( cLogName := _COMMIT_HBROOT_ + "ChangeLog.txt" ) .AND. ;
      ! hb_FileExists( cLogName := _COMMIT_HBROOT_ + "ChangeLog" )
      RETURN ""
   ENDIF

   RETURN cLogName

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
      Int( Abs( nOffset ) % 3600 / 60 ), ;
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

   RETURN iif( nCount == 1, cMsg, cLog )

STATIC FUNCTION VCSDetect()

   DO CASE
   CASE hb_DirExists( _COMMIT_HBROOT_ + ".svn" ) ; RETURN "svn"
   CASE hb_DirExists( _COMMIT_HBROOT_ + ".git" ) ; RETURN "git"
   ENDCASE

   RETURN ""

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

#include "check.hb"
