#!/usr/bin/hbmk2
/*
 * Harbour Project source code:
 * Various validations of filenames and file content, meant to be
 * run before committing to repository.
 *
 * Copyright 2013 Viktor Szakats (harbour syenar.net)
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

/* TODO: Apply transformations:
         Uncrustify, hbformat,
         pngout/pngcrush/optipng/advpng,
         jpgoptim/jpegtran/jpegrescan,
         css/html/xml format, etc */

#pragma -w3
#pragma -km+
#pragma -ko+

#include "directry.ch"

FUNCTION CheckFileList( xName )

   LOCAL lPassed := .T.

   LOCAL aErr, s
   LOCAL file

   IF HB_ISSTRING( xName )
      xName := { xName }
   ENDIF

   IF Empty( xName ) .OR. HB_ISARRAY( xName )
      IF Empty( xName )
         xName := hb_DirScan( "", hb_osFileMask() )
         FOR EACH file IN xName
            file := file[ F_NAME ]
         NEXT
      ENDIF
      FOR EACH file IN xName
         IF ! CheckFile( file, @aErr )
            lPassed := .F.
            FOR EACH s IN aErr
               OutStd( file + ": " + s + hb_eol() )
            NEXT
         ENDIF
      NEXT
   ENDIF

   RETURN lPassed

STATIC FUNCTION CheckFile( cName, /* @ */ aErr, lApplyFixes )

   LOCAL cFile
   LOCAL tmp
   LOCAL cEOL

   LOCAL lReBuild
   LOCAL lRemoveEndingWhitespace

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
      ".git*" }

   LOCAL aCanBeLong := { ;
      "ChangeLog.txt", ;
      ".git*", ;
      "*.po", ;
      "*.md", ;
      "*.html", ;
      "*/hb-charmap.def", ;  /* TOFIX: Use 8.3 name */
      "package/*", ;
      "*/3rd/*", ;
      "contrib/hbwin/*", ;
      "contrib/rddads/unixutils.h", ;
      "extras/httpsrv/*" }

   LOCAL aCanHaveTab := { ;
      "Makefile", ;
      "*.mk", ;
      "*.yyc", ;
      "*.dif", ;
      "*.xml", ;
      "*.css" }

   LOCAL aCanHaveSpaceAtEol := { ;
      "*.dif", ;
      "*.md" }

   LOCAL aCanHaveAnyEncoding := { ;
      "*.dif", ;
      "contrib/hbmisc/tests/sample.txt", ;  /* TOFIX: Not Unicode compatible component */
      "contrib/hbhpdf/tests/files/*.txt" }

   LOCAL aForcedCRLF := { ;
      "*.bat" }

   LOCAL aForcedLF := { ;
      "*.sh" }

   hb_default( @lApplyFixes, .F. )

   cName := hb_DirSepToOS( cName )
   cFile := hb_MemoRead( cName )

   aErr := {}

   /* filename checks */

   IF ! FNameExc( cName, LoadGitignore() )

      IF ( Len( hb_FNameName( cName ) ) > 8 .OR. Len( hb_FNameExt( cName ) ) > 4 ) .AND. ! FNameExc( cName, aCanBeLong )
         AAdd( aErr, "filename: non-8.3" )
      ENDIF

      IF Left( hb_FNameName( cName ), 1 ) == "." .AND. ! FNameExc( cName, aCanBeDot )
         AAdd( aErr, "filename: non MS-DOS compatible" )
      ENDIF

      IF !( cName == Lower( cName ) ) .AND. ! FNameExc( cName, aCanBeUpper )
         AAdd( aErr, "filename: non-lowercase" )
      ENDIF

      IF ! IsASCII7( cName )
         AAdd( aErr, "filename: non-ASCII-7" )
      ENDIF

      IF IsBinary( cFile )
         IF lApplyFixes
            IF hb_FNameExt( cFile ) == ".png"
               OutStd( cFile + ": " + "content: optimizing" + hb_eol() )
               hb_run( "optipng " + cFile )
            ENDIF
         ENDIF
      ELSE

         IF hb_FileMatch( cName, "ChangeLog.txt" ) .AND. Len( cFile ) > 32768 .AND. ! lApplyFixes
            cFile := Left( cFile, 16384 ) + Right( cFile, 16384 )
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

         cEOL := EOLDetect( cFile )

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
            cFile := RemoveEndingWhitespace( cFile, cEOL, lRemoveEndingWhitespace )
         ENDIF

         IF !( Right( cFile, Len( cEOL ) ) == cEOL )
            AAdd( aErr, "content: has no EOL at EOF" )
            IF lApplyFixes
               cFile += cEOL
            ENDIF
         ENDIF

         IF Right( cFile, Len( cEOL ) * 2 ) == Replicate( cEOL, 2 )
            AAdd( aErr, "content: has multiple EOL at EOF" )
            IF lApplyFixes
               DO WHILE Right( cFile, Len( cEOL ) * 2 ) == Replicate( cEOL, 2 )
                  cFile := hb_StrShrink( cFile, Len( cEOL ) )
               ENDDO
            ENDIF
         ENDIF

         IF ! FNameExc( cName, aCanHaveAnyEncoding )
            tmp := -1
            IF ! IsASCII7( cFile, @tmp ) .AND. ! IsUTF8( cFile )
               AAdd( aErr, "content: is non-UTF-8/ASCII-7: " + hb_ntos( tmp ) )
            ENDIF
         ENDIF

         IF "$" + "Id" $ cFile .AND. ! hb_FileMatch( cName, "ChangeLog.txt" )
            AAdd( aErr, "content: has " + "$" + "Id" )
         ENDIF
      ENDIF
   ENDIF

   RETURN Empty( aErr )

STATIC FUNCTION IsBinary( cFile )
   RETURN Chr( 0 ) $ cFile .OR. !( Chr( 10 ) $ cFile )

/*
 * UTF-8 encoding detection, based on filestr.cpp from Far Manager.
 * Harbour adaptation Copyright 2013 Viktor Szakats (harbour syenar.net)
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

STATIC FUNCTION EOLDetect( cFile )

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
      RETURN Chr( 13 )
   ELSEIF nCR == 0 .AND. nLF > 0
      RETURN Chr( 10 )
   ELSEIF nCR == 0 .AND. nLF == 0
      RETURN "binary"
   ELSEIF nCR == nLF
      RETURN Chr( 13 ) + Chr( 10 )
   ENDIF

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

STATIC FUNCTION UTF8_BOM()
   RETURN ;
      hb_BChar( 0xEF ) + ;
      hb_BChar( 0xBB ) + ;
      hb_BChar( 0xBF )

STATIC FUNCTION LoadGitignore()

   THREAD STATIC s_aIgnore := NIL

   LOCAL cLine

   IF s_aIgnore == NIL
      s_aIgnore := { "*/3rd/*" }
      FOR EACH cLine IN hb_ATokens( StrTran( hb_MemoRead( ".gitignore" ), Chr( 13 ) ), Chr( 10 ) )
         IF ! Empty( cLine ) .AND. !( Left( cLine, 1 ) == "#" )
            /* TODO: clean this */
            AAdd( s_aIgnore, ;
               iif( Left( cLine, 1 ) $ "?*/!", "", "*/" ) + ;
               cLine + ;
               iif( Right( cLine, 1 ) == "/", "*", ;
               iif( Empty( hb_FNameExt( cLine ) ) .AND. !( Right( cLine, 2 ) == "*/" ), "/*", "" ) ) )
            IF !( ATail( s_aIgnore ) == cLine )
               IF Left( ATail( s_aIgnore ), 2 ) == "*/"
                  AAdd( s_aIgnore, SubStr( ATail( s_aIgnore ), 3 ) )
               ENDIF
               AAdd( s_aIgnore, cLine )
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN s_aIgnore
