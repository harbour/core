/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SVN commit preparer
 *
 * Copyright 2012 Viktor Szakats (harbour syenar.net)
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

#define _MY_NAME_ "Firstname Lastname (me domain.net)"
#define _PCOMMIT_ ".pcommit"

#pragma warninglevel=3
#pragma -km+
#pragma -ko+

PROCEDURE Main()

   LOCAL aChanges := DoctorChanges( Changes() )
   LOCAL cLog
   LOCAL cLogNew
   LOCAL cLine
   LOCAL nOffset
   LOCAL cHit
   LOCAL nPos
   LOCAL cMyName

   IF Empty( aChanges )
      OutStd( hb_progname() + ": no changes" + hb_eol() )
      RETURN
   ENDIF

   // ;

   IF hb_FileExists( _PCOMMIT_ )
      cMyName := AllTrim( hb_MemoRead( _PCOMMIT_ ) )
   ELSE
      cMyName := _MY_NAME_
   ENDIF

   nOffset := hb_UTCOffset()

   cLogNew := hb_StrFormat( "%1$s UTC+%2$02d%3$02d %4$s",;
                 hb_TToC( hb_DateTime(), "YYYY-MM-DD", "HH:MM" ),;
                 Int( nOffset / 3600 ),;
                 Int( ( ( nOffset / 3600 ) - Int( nOffset / 3600 ) ) * 60 ),;
                 cMyName ) + hb_eol()

   FOR EACH cLine IN aChanges
      cLogNew += cLine + hb_eol()
   NEXT

   // ;

   cLog := MemoRead( "ChangeLog" )
   cHit := hb_AtX( "\n[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9] UTC[\-+][0-9][0-9][0-9][0-9] ", cLog )
   IF Empty( cHit )
      cHit := ""
   ENDIF

   nPos := At( AllTrim( cHit ), cLog )
   IF nPos > 0
      cLog := Left( cLog, nPos - 1 ) + cLogNew + hb_eol() + SubStr( cLog, nPos )
   ELSE
      cLog += hb_eol() + cLogNew
   ENDIF

   hb_MemoWrit( "ChangeLog", cLog )

   RETURN

STATIC FUNCTION DoctorChanges( aChanges )
   LOCAL cLine
   LOCAL cStart
   LOCAL aNew := {}

   ASort( aChanges,,, {| x, y | x < y } )

   FOR EACH cLine IN aChanges
      IF ! Empty( cLine ) .AND. SubStr( cLine, 8, 1 ) == " "
         cStart := Left( cLine, 1 )
         SWITCH cStart
         CASE "M"
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
         ENDIF
      ENDIF
   NEXT

   RETURN aNew

STATIC FUNCTION Changes()
   LOCAL cStdOut

   hb_processRun( "svn status -q",, @cStdOut )

   RETURN hb_ATokens( StrTran( cStdOut, Chr( 13 ) ), Chr( 10 ) )
