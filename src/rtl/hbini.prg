/*
 * Handling .ini files
 *
 * Copyright 2002 Giancarlo Niccolai <gian@niccolai.ws>
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
 * along with this software; see the file COPYING.txt.  if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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
 *    Variable=Value
 *    OtherVariable: Value
 *
 * You can pass a list of "potential" .ini files in a ';' separated path;
 * the first readable file will be loaded.
 *
 * On error, the function returns NIL. On success, you will have an hash
 * array of this form:
 *
 *    { "MAIN" => { "Key1" => "Val1", ... , "KeyN" => "ValN" },
 *      "Section1" => { "Key1" => "Val1", ... , "KeyN" => "ValN" },
 *      ...
 *      "SectionN" => { "Key1" => "Val1", ... , "KeyN" => "ValN" }
 *    }
 *
 * Main is the default section (variables that are declared without a section).
 *
 */

#include "fileio.ch"

STATIC s_cLineComment := ";"
STATIC s_cHalfLineComment := "#"

PROCEDURE hb_iniSetComment( cLc, cHlc )

   IF HB_ISSTRING( cLc )
      s_cLineComment := cLc
   ENDIF

   IF HB_ISSTRING( cHlc )
      s_cHalfLineComment := cHlc
   ENDIF

   RETURN

FUNCTION hb_iniNew( lAutoMain )

   LOCAL hIni := { => }

   IF hb_defaultValue( lAutoMain, .T. )
      hIni[ "MAIN" ] := { => }
   ENDIF

   RETURN hIni

FUNCTION hb_iniRead( cFileSpec, lKeyCaseSens, cSplitters, lAutoMain )
   RETURN hb_iniReadStr( iif( HB_ISSTRING( cFileSpec ), hb_iniFileLow( cFileSpec ), "" ), lKeyCaseSens, cSplitters, lAutoMain )

FUNCTION hb_iniReadStr( cData, lKeyCaseSens, cSplitters, lAutoMain )

   LOCAL hIni := { => }

   /* Default case sensitiveness for keys */
   hb_default( @lKeyCaseSens, .T. )
   hb_default( @lAutoMain, .T. )

   hb_HCaseMatch( hIni, lKeyCaseSens )

   IF lAutoMain
      hIni[ "MAIN" ] := { => }
   ENDIF

   RETURN hb_iniStringLow( hIni, ;
      hb_defaultValue( cData, "" ), ;
      lKeyCaseSens, ;
      hb_defaultValue( cSplitters, "=" ), ;
      lAutoMain )

STATIC FUNCTION hb_iniFileLow( cFileSpec )

   LOCAL cFile, nLen
   LOCAL hFile
   LOCAL cData
   LOCAL aFiles := hb_ATokens( cFileSpec, hb_osPathListSeparator() )

   IF Empty( aFiles )
      aFiles := { cFileSpec }
   ENDIF

   hFile := NIL
   FOR EACH cFile IN aFiles
      IF ! HB_ISNULL( cFile ) .AND. hb_vfExists( cFile )
         IF ( hFile := hb_vfOpen( cFile, FO_READ ) ) != NIL
            EXIT
         ENDIF
      ENDIF
   NEXT

   IF hFile == NIL
      RETURN ""
   ENDIF

   /* we'll read the whole file, then we'll break it in lines. */
   cData := Space( hb_vfSize( hFile ) )
   hb_vfSeek( hFile, 0, FS_SET )
   nLen := hb_vfRead( hFile, @cData, hb_BLen( cData ) )
   cData := hb_BLeft( cData, nLen )
   hb_vfClose( hFile )

   RETURN cData

STATIC FUNCTION hb_iniStringLow( hIni, cData, lKeyCaseSens, cSplitters, lAutoMain )

   LOCAL aKeyVal, hCurrentSection
   LOCAL cLine
   LOCAL reComment, reInclude, reSection, reSplitters

   reComment := hb_regexComp( s_cHalfLineComment + "|^[ \t]*" + s_cLineComment )
   reInclude := hb_regexComp( "include (.*)" )
   reSection := hb_regexComp( "[[](.*)[]]" )
   reSplitters := hb_regexComp( cSplitters )

   /* Always begin with the MAIN section */
   hCurrentSection := iif( lAutoMain, hIni[ "MAIN" ], hIni )

   cLine := ""
   FOR EACH cData IN hb_ATokens( cData, .T. )
      cLine += AllTrim( cData )

      /* Sum up lines terminating with "<space>||" ...*/
      IF Right( cLine, 3 ) == " ||"
         cLine := hb_StrShrink( cLine, 2 )
         /* ... but proceed if stream over */
         IF ! cData:__enumIsLast()
            LOOP
         ENDIF
      ENDIF

      /* Skip void lines */
      IF Empty( cLine )
         LOOP
      ENDIF

      /* remove eventual comments */
      IF ! Empty( aKeyVal := hb_regexSplit( reComment, cLine ) )
         IF Empty( cLine := AllTrim( aKeyVal[ 1 ] ) )
            /* Skip all comment lines */
            LOOP
         ENDIF
      ENDIF

      /* Is it an "INCLUDE" statement ? */
      IF ! Empty( aKeyVal := hb_regex( reInclude, cLine ) )
         /* ignore void includes */
         aKeyVal[ 2 ] := AllTrim( aKeyVal[ 2 ] )
         IF HB_ISNULL( aKeyVal[ 2 ] )
            LOOP
         ENDIF
         hb_iniStringLow( hIni, hb_iniFileLow( aKeyVal[ 2 ] ), lKeyCaseSens, cSplitters, lAutoMain )
      /* Is it a NEW section? */
      ELSEIF ! Empty( aKeyVal := hb_regex( reSection, cLine ) )
         cLine := AllTrim( aKeyVal[ 2 ] )
         IF ! HB_ISNULL( cLine )
            hCurrentSection := { => }
            IF ! lKeyCaseSens
               cLine := Upper( cLine )
            ENDIF
            hIni[ cLine ] := hCurrentSection
         ENDIF
      /* Is it a valid key */
      ELSEIF Len( aKeyVal := hb_regexSplit( reSplitters, cLine,,, 1 ) ) == 1
         /* TODO: Signal error */
      ELSE
         /* If not case sensitive, use upper keys */
         IF ! lKeyCaseSens
            aKeyVal[ 1 ] := Upper( aKeyVal[ 1 ] )
         ENDIF
         hCurrentSection[ AllTrim( aKeyVal[ 1 ] ) ] := AllTrim( aKeyVal[ 2 ] )
      ENDIF

      cLine := ""
   NEXT

   RETURN hIni

FUNCTION hb_iniWrite( xFileName, hIni, cCommentBegin, cCommentEnd, lAutoMain )

   LOCAL hFile
   LOCAL lClose
   LOCAL cBuffer

   cBuffer := hb_iniWriteStr( hIni, cCommentBegin, cCommentEnd, lAutoMain )

   IF ! HB_ISSTRING( cBuffer )
      RETURN .F.
   ENDIF

   DO CASE
   CASE HB_ISSTRING( xFileName )
      IF ( hFile := hb_vfOpen( xFileName, FO_CREAT + FO_TRUNC + FO_WRITE + FO_EXCLUSIVE ) ) == NIL
         RETURN .F.
      ENDIF
      lClose := .T.
   CASE HB_ISPOINTER( xFileName )
      hFile := xFileName
      lClose := .F.
   OTHERWISE
      RETURN .F.
   ENDCASE

   IF hb_vfWrite( hFile, cBuffer ) != hb_BLen( cBuffer )
      IF lClose
         hb_vfClose( hFile )
      ENDIF
      RETURN .F.
   ENDIF

   IF lClose
      hb_vfClose( hFile )
   ENDIF

   RETURN .T.

FUNCTION hb_iniWriteStr( hIni, cCommentBegin, cCommentEnd, lAutoMain )

   LOCAL cEOL := Set( _SET_EOL )
   LOCAL cSection
   LOCAL cBuffer := ""

   IF ! HB_ISHASH( hIni )
      RETURN NIL
   ENDIF

   IF HB_ISSTRING( cCommentBegin ) .AND. ! Empty( cCommentBegin )
      cBuffer += cCommentBegin + cEOL
   ENDIF

   hb_default( @lAutoMain, .T. )

   // Fix if lAutoMain is .T. but I haven't a MAIN section
   IF lAutoMain .AND. !( "MAIN" $ hIni )
      lAutoMain := .F.
   ENDIF

   /* Write toplevel section */
   IF lAutoMain
      /* When automain is on, write the main section */
      hb_HEval( hIni[ "MAIN" ], {| cKey, xVal | ;
         cBuffer += hb_CStr( cKey ) + "=" + hb_CStr( xVal ) + cEOL } )
   ELSE
      /* When automain is off, just write all the toplevel variables. */
      hb_HEval( hIni, {| cKey, xVal | iif( HB_ISHASH( xVal ), /* nothing */, ;
         cBuffer += hb_CStr( cKey ) + "=" + hb_CStr( xVal ) + cEOL ) } )
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

      cBuffer += cEOL + "[" + hb_CStr( cSection:__enumKey ) + "]" + cEOL

      hb_HEval( cSection, ;
         {| cKey, xVal | cBuffer += hb_CStr( cKey ) + "=" + ;
         hb_CStr( xVal ) + cEOL } )
   NEXT

   IF HB_ISSTRING( cCommentEnd ) .AND. ! Empty( cCommentEnd )
      cBuffer += cCommentEnd + cEOL
   ENDIF

   RETURN iif( Empty( cBuffer ), NIL, cBuffer )
