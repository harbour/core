#!/usr/bin/hbmk2
/*
 * Harbour Project source code:
 * Commit preparer
 *
 * Copyright 2012-2013 Viktor Szakats (harbour syenar.net)
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
 * their web site at http://www.gnu.org/).
 *
 */

#define _CONFIGFIL_ ".hbcommit"
#define _CONFIGENV_ "HBCOMMIT_USER"

#pragma -w3
#pragma -km+
#pragma -ko+

PROCEDURE Main()

   LOCAL cVCS := VCSDetect()

   LOCAL aFiles := {}
   LOCAL aChanges := DoctorChanges( cVCS, Changes( cVCS ), aFiles )
   LOCAL cLog
   LOCAL cLogNew
   LOCAL cLine
   LOCAL nOffset
   LOCAL cHit
   LOCAL nPos
   LOCAL cMyName
   LOCAL cOldLang
   LOCAL cLogName

   IF Empty( aChanges )
      OutStd( hb_ProgName() + ": " + "no changes" + hb_eol() )
      ErrorLevel( 0 )
      RETURN
   ENDIF

   // ;

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

   nOffset := hb_UTCOffset()

   cLogNew := hb_StrFormat( "%1$s UTC%2$s%3$02d%4$02d %5$s", ;
      hb_TToC( hb_DateTime(), "YYYY-MM-DD", "HH:MM" ), ;
      iif( nOffset < 0, "-", "+" ), ;
      Int( nOffset / 3600 ), ;
      Int( ( ( nOffset / 3600 ) - Int( nOffset / 3600 ) ) * 60 ), ;
      cMyName ) + hb_eol()

   FOR EACH cLine IN aChanges
      cLogNew += cLine + hb_eol()
   NEXT

   // ;

   IF ! hb_FileExists( cLogName := "ChangeLog.txt" )
      IF ! hb_FileExists( cLogName := "ChangeLog" )
         OutStd( hb_ProgName() + ": " + "can't find ChangeLog file" + hb_eol() )
         ErrorLevel( 2 )
         RETURN
      ENDIF
   ENDIF

   IF CheckFileList( aFiles )

      cLog := MemoRead( cLogName )
      cOldLang := hb_cdpSelect( "EN" )
      cHit := hb_AtX( "\n[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9] UTC[\-+][0-1][0-9][0-5][0-9] ", cLog )
      IF Empty( cHit )
         cHit := ""
      ENDIF
      hb_cdpSelect( cOldLang )

      nPos := At( AllTrim( cHit ), cLog )
      IF nPos > 0
         cLog := Left( cLog, nPos - 1 ) + cLogNew + hb_eol() + SubStr( cLog, nPos )
      ELSE
         cLog += hb_eol() + cLogNew
      ENDIF

      hb_MemoWrit( cLogName, cLog )

      OutStd( hb_ProgName() + ": " + hb_StrFormat( "Edit %1$s and commit", cLogName ) + hb_eol() )
      ErrorLevel( 0 )
   ELSE
      OutStd( hb_ProgName() + ": " + "Please correct errors listed above and re-run" + hb_eol() )
      ErrorLevel( 1 )
   ENDIF

   RETURN

STATIC FUNCTION VCSDetect()

   DO CASE
   CASE hb_DirExists( ".svn" ) ; RETURN "svn"
   CASE hb_DirExists( ".git" ) ; RETURN "git"
   ENDCASE

   RETURN ""

STATIC FUNCTION DoctorChanges( cVCS, aChanges, aFiles )

   LOCAL cLine
   LOCAL cStart
   LOCAL aNew := {}

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
               AAdd( aFiles, SubStr( cLine, 8 + 1 ) )
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
               AAdd( aFiles, SubStr( cLine, 3 + 1 ) )
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

#include "check.hb"
