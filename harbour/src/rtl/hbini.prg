/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Handling .ini files
 *
 * Copyright 2002 Giancarlo Niccolai <gian@niccolai.ws>
 * www - http://www.xharbour.org
 *
 * this program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * this program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE.  See the
 * GNU General public License for more details.
 *
 * You should have received a copy of the GNU General public License
 * along with this software; see the file COPYING.  if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, xHarbour license gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * this exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General public License.
 *
 * this exception applies only to the code released with this xHarbour
 * explicit exception.  if you add/copy code from other sources,
 * as the General public License permits, the above exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * if you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * if you do not wish that, delete this exception notice.
 *
 */

/*
 * This small procedure reads a .ini file in the standard .ini format into
 * an hash array:
 *    ; A line starting with a ';' is a comment
 *    # Also, a '#' marks a comment up to the end of the line
 *    [NewSection]
 *    Variable = Value
 *    OtherVariable: Value
 *
 * You can pass a list of "potential" .ini files in a ';' separated path;
 * the first readable file will be loaded.
 *
 * On error, the function returns NIL. On success, you will have an hash
 * array of this form:
 *
 *    { 'MAIN' => { 'Key1' => 'Val1', ... ,'KeyN' => 'ValN'},
 *      'Section1' => { 'Key1' => 'Val1', ... ,'KeyN' => 'ValN'},
 *      ...
 *      'SectionN' => { 'Key1' => 'Val1', ... ,'KeyN' => 'ValN'}
 *    }
 *
 * Main is the default section (variables that are declared without a section).
 *
 */

#include "fileio.ch"


STATIC s_cLineComment := ";"
STATIC s_cHalfLineComment := "#"


PROCEDURE hb_IniSetComment( cLc, cHlc )

   IF HB_ISSTRING( cLc )
      s_cLineComment := cLc
   ENDIF

   IF HB_ISSTRING( cHlc )
      s_cHalfLineComment := cHlc
   ENDIF

   RETURN

FUNCTION HB_IniNew( lAutoMain )
   LOCAL hIni := { => }

   hb_HKeepOrder( hIni, .T. )

   IF ! HB_ISLOGICAL( lAutoMain )
      lAutoMain := .T.
   ENDIF

   IF lAutoMain
      hIni[ "MAIN" ] := { => }
      hb_HKeepOrder( hIni[ "MAIN" ], .T. )
   ENDIF

   RETURN hIni

FUNCTION hb_IniRead( cFileSpec, lKeyCaseSens, cSplitters, lAutoMain )
   RETURN hb_IniReadStr( iif( HB_ISSTRING( cFileSpec ), hb_IniFileLow( cFileSpec ), "" ), lKeyCaseSens, cSplitters, lAutoMain )

FUNCTION hb_IniReadStr( cData, lKeyCaseSens, cSplitters, lAutoMain )
   LOCAL hIni := { => }

   hb_HKeepOrder( hIni, .T. )

   /* Default case sensitiveness for keys */
   IF ! HB_ISLOGICAL( lKeyCaseSens )
      lKeyCaseSens := .T.
   ENDIF
   IF ! HB_ISSTRING( cSplitters )
      cSplitters := "="
   ENDIF
   IF ! HB_ISLOGICAL( lAutoMain )
      lAutoMain := .T.
   ENDIF
   IF ! HB_ISSTRING( cData )
      cData := ""
   ENDIF

   hb_HCaseMatch( hIni, lKeyCaseSens )

   IF lAutoMain
      hIni[ "MAIN" ] := { => }
      hb_HKeepOrder( hIni[ "MAIN" ], .T. )
   ENDIF

   RETURN hb_IniStringLow( hIni, cData, lKeyCaseSens, cSplitters, lAutoMain )

STATIC FUNCTION hb_IniFileLow( cFileSpec )
   LOCAL cFile, nLen
   LOCAL hFile
   LOCAL cData
   LOCAL aFiles := hb_aTokens( cFileSpec, hb_OSPathListSeparator() )

   IF Empty( aFiles )
      aFiles := { cFileSpec }
   ENDIF

   hFile := F_ERROR
   FOR EACH cFile IN aFiles
      IF ! Empty( cFile ) .AND. hb_FileExists( cFile )
         IF ( hFile := FOpen( cFile ) ) != F_ERROR
            EXIT
         ENDIF
      ENDIF
   NEXT

   IF hFile == F_ERROR
      RETURN NIL
   ENDIF

   /* we'll read the whole file, then we'll break it in lines. */
   cData := Space( FSeek( hFile, 0, FS_END ) )
   FSeek( hFile, 0, FS_SET )
   nLen := FRead( hFile, @cData, hb_BLen( cData ) )
   cData := hb_BLeft( cData, nLen )
   FClose( hFile )

   RETURN cData

STATIC FUNCTION hb_IniStringLow( hIni, cData, lKeyCaseSens, cSplitters, lAutoMain )
   LOCAL nLen
   LOCAL aKeyVal, hCurrentSection
   LOCAL nLineEnd
   LOCAL cLine
   LOCAL reComment, reInclude, reSection, reSplitters

   reComment := hb_regexComp( s_cHalfLineComment + "|^[ \t]*" + s_cLineComment )
   reInclude := hb_regexComp( "include (.*)" )
   reSection := hb_regexComp( "[[](.*)[]]" )
   reSplitters := hb_regexComp( cSplitters )

   /* Always begin with the MAIN section */
   IF lAutoMain
      hCurrentSection := hIni[ "MAIN" ]
   ELSE
      hCurrentSection := hIni
   ENDIF

   cLine := ""
   DO WHILE Len( cData ) > 0
      nLen := 2
      nLineEnd := At( Chr( 13 ) + Chr( 10 ), cData )
      IF nLineEnd == 0
         nLineEnd := At( Chr( 10 ) + Chr( 13 ), cData )
         IF nLineEnd == 0
            nLen := 1
            nLineEnd := At( Chr( 10 ), cData )
            IF nLineEnd == 0
               nLineEnd := At( Chr( 13 ), cData )
               IF nLineEnd == 0
                  nLineEnd := Len( cData ) + 1
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      /* Get the current line */
      cLine += AllTrim( SubStr( cData, 1, nLineEnd - 1 ) )
      /* remove current line */
      cData := SubStr( cData, nLineEnd + nLen )

      /* Skip void lines */
      IF Empty( cLine )
         LOOP
      ENDIF

      /* Sum up lines terminating with "<space>||" ...*/
      IF Len( cLine ) > 3 .AND. SubStr( cLine, -3, 3 ) == " ||"

         cLine := SubStr( cLine, 1, Len( cLine ) - 2 )
         /* ... but proceed if stream over */
         IF Len( cData ) > 0
            LOOP
         ENDIF

      ENDIF

      /* remove eventual comments */
      aKeyVal := hb_regexSplit( reComment, cLine )
      IF ! Empty( aKeyVal )
         cLine := AllTrim( aKeyVal[ 1 ] )
      ENDIF

      /* Skip all comment lines */
      IF Empty( cLine )
         LOOP
      ENDIF

      /* Is it an "INCLUDE" statement ? */
      aKeyVal := hb_regex( reInclude, cLine )
      IF ! Empty( aKeyVal )
         /* ignore void includes */
         aKeyVal[ 2 ] := AllTrim( aKeyVal[ 2 ] )
         IF Len( aKeyVal[ 2 ] ) == 0
            LOOP
         ENDIF
         hb_IniStringLow( hIni, hb_IniFileLow( aKeyVal[ 2 ] ), lKeyCaseSens, cSplitters, lAutoMain )
         cLine := ""
         LOOP
      ENDIF

      /* Is it a NEW section? */
      aKeyVal := hb_regex( reSection, cLine )
      IF ! Empty( aKeyVal )
         cLine := AllTrim( aKeyVal[ 2 ] )
         IF Len( cLine ) != 0
            hCurrentSection := { => }
            hb_HKeepOrder( hCurrentSection, .T. )
            IF ! lKeyCaseSens
               cLine := Upper( cLine )
            ENDIF
            hIni[ cLine ] := hCurrentSection
         ENDIF
         cLine := ""
         LOOP
      ENDIF

      /* Is it a valid key */
      aKeyVal := hb_regexSplit( reSplitters, cLine,,, 1 )
      IF Len( aKeyVal ) == 1
         /* TODO: Signal error */
         cLine := ""
         LOOP
      ENDIF

      /* If not case sensitive, use upper keys */
      IF ! lKeyCaseSens
         aKeyVal[ 1 ] := Upper( aKeyVal[ 1 ] )
      ENDIF

      hCurrentSection[ AllTrim( aKeyVal[ 1 ] ) ] := AllTrim( aKeyVal[ 2 ] )
      cLine := ""
   ENDDO

   RETURN hIni


FUNCTION hb_IniWrite( xFileName, hIni, cCommentBegin, cCommentEnd, lAutoMain )
   LOCAL hFile
   LOCAL lClose
   LOCAL cBuffer

   cBuffer := hb_IniWriteStr( hIni, cCommentBegin, cCommentEnd, lAutoMain )

   // if cBuffer == NIL I have to stop here
   IF ! HB_ISSTRING( cBuffer )
      RETURN .F.
   ENDIF

   IF HB_ISSTRING( xFileName )
      hFile := FCreate( xFileName )
      lClose := .T.
   ELSEIF HB_ISNUMERIC( xFileName )
      hFile := xFileName
      lClose := .F.
   ELSE
      RETURN .F.
   ENDIF

   IF hFile == F_ERROR
      RETURN .F.
   ENDIF

   IF FWrite( hFile, cBuffer ) != hb_BLen( cBuffer )
      IF lClose
         FClose( hFile )
      ENDIF
      RETURN .F.
   ENDIF

   IF lClose
      FClose( hFile )
   ENDIF

   RETURN .T.

FUNCTION hb_IniWriteStr( hIni, cCommentBegin, cCommentEnd, lAutoMain )
   LOCAL cNewLine := hb_eol()
   LOCAL cSection
   LOCAL cBuffer := ""

   IF ! HB_ISHASH( hIni )
      RETURN NIL
   ENDIF

   IF HB_ISSTRING( cCommentBegin ) .AND. ! Empty( cCommentBegin )
      cBuffer += cCommentBegin + cNewLine
   ENDIF

   IF ! HB_ISLOGICAL( lAutoMain )
      lAutoMain := .T.
   ENDIF

   // Fix if lAutoMain is .T. but I haven't a MAIN section
   IF lAutoMain .AND. ! hb_HHasKey( hIni, "MAIN" )
      lAutoMain := .F.
   ENDIF

   /* Write toplevel section */
   IF lAutoMain
      /* When automain is on, write the main section */
      hb_HEval( hIni[ "MAIN" ], ;
               {| cKey, xVal | cBuffer += hb_CStr( cKey ) + " = " + ;
                                             hb_CStr( xVal ) + cNewLine } )

   ELSE
      /* When automain is off, just write all the toplevel variables. */
      hb_HEval( hIni, {| cKey, xVal | iif( ! HB_ISHASH( xVal ),;
                cBuffer += hb_CStr( cKey ) + " = " + ;
                           hb_CStr( xVal ) + cNewLine, /* nothing */ ) } )
   ENDIF

   FOR EACH cSection IN hIni

      /* Avoid re-processing main section */
      IF lAutoMain
         /* When automain is on, skip section named MAIN */
         IF cSection:__enumKey == "MAIN"
            LOOP
         ENDIF
      ELSE
         /* When automain is off, skip all the toplevel variables. */
         IF ! HB_ISHASH( cSection )
            LOOP
         ENDIF
      ENDIF

      cBuffer += cNewLine + "[" + hb_CStr( cSection:__enumKey ) + "]" + cNewLine

      hb_HEval( cSection, ;
                {| cKey, xVal | cBuffer += hb_CStr( cKey ) + "=" + ;
                                           hb_CStr( xVal ) + cNewLine } )
   NEXT

   IF HB_ISSTRING( cCommentEnd ) .AND. ! Empty( cCommentEnd )
      cBuffer += cCommentEnd + cNewLine
   ENDIF

   RETURN iif( ! Empty( cBuffer ), cBuffer, NIL )
