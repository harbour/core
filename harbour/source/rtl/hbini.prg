/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * HB_ReadIni - Reading .ini files
 *
 * Copyright 2002 Giancarlo Niccolai [gian@niccolai.ws]
 * www - http://www.xharbour.org
 *
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
 * Copyright 2002 Giancarlo Niccolai [gian@niccolai.ws]
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

#include "fileio.ch"
#include "common.ch"

static s_cLineComment := ";"
static s_cHalfLineComment := "#"


PROCEDURE HB_SetIniComment( cLc, cHlc )
   s_cLineComment := cLc
   s_cHalfLineComment := cHlc
RETURN


FUNCTION HB_ReadIni( cFileSpec, bKeyCaseSens, cSplitters, bAutoMain )
   LOCAL hIni

   /* Default case sensitiveness for keys */
   IF bKeyCaseSens == NIL
      bKeyCaseSens := .T.
   ENDIF

   IF cSplitters == NIL
      cSplitters := "=|:"
   ENDIF

   IF bAutoMain == NIL
      bAutoMain := .T.
   END

   hIni := HB_Hash()
   HB_HCaseMatch( hIni, bKeyCaseSens )
   HB_HAutoAdd( hIni, HB_HAUTOADD_ASSIGN )

   IF bAutoMain
      hIni[ "MAIN" ] := hb_Hash()
   END

RETURN HB_ReadIni2( hIni, cFileSpec, bKeyCaseSens, cSplitters, bAutoMain )

STATIC FUNCTION HB_ReadIni2( hIni, cFileSpec, bKeyCaseSens, cSplitters, bAutoMain )
   LOCAL aFiles
   LOCAL cFile, nLen
   LOCAL aKeyVal, hCurrentSection
   LOCAL hFile, nLineEnd
   LOCAL cData, cBuffer, cLine
   LOCAL reComment, reInclude, reSection, reSplitters

   reComment := HB_RegexComp( s_cHalfLineComment + "|^[ \t]*" + s_cLineComment )
   reInclude := HB_RegexComp( "include (.*)" )
   reSection := HB_RegexComp( "[[](.*)[]]" )
   reSplitters := HB_RegexComp( cSplitters )

   aFiles := HB_aTokens( cFileSpec, HB_OSPATHLISTSEPARATOR() )
   IF Empty( aFiles )
      aFiles := { cFileSpec }
   ENDIF

   hFile := -1
   FOR EACH cFile IN aFiles
      IF !EMPTY( cFile ) .AND. File( cFile )
         IF ( hFile := FOpen( cFile ) ) != -1
            EXIT
         ENDIF
      ENDIF
   NEXT

   IF hFile == -1
      RETURN NIL
   ENDIF

   /* we'll read the whole file, then we'll break it in lines. */
   cBuffer := Space( FSeek( hFile, 0, FS_END ) )
   FSeek( hFile, 0, FS_SET )
   nLen := FRead( hFile, @cBuffer, Len( cBuffer ) )
   cBuffer := Left( cBuffer, nLen )
   FClose( hFile )

   /* Always begin with the MAIN section */
   IF bAutoMain
      hCurrentSection := hIni[ "MAIN" ]
   ELSE
      hCurrentSection := hIni
   END

   cLine := ""
   DO WHILE Len( cData ) > 0
      nLen := 2
      nLineEnd := At( chr( 13 ) + chr( 10 ), cData )
      IF nLineEnd == 0
         nLineEnd := At( chr( 10 ) + chr( 13 ), cData )
         IF nLineEnd == 0
            nLen := 1
            nLineEnd := At( chr( 10 ), cData )
            IF nLineEnd == 0
               nLineEnd := At( chr( 13 ), cData )
               IF nLineEnd == 0
                  nLineEnd := Len( cData )
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      /* Get the current line */
      cLine += AllTrim( Substr( cData, 1, nLineEnd - 1 ) )
      /* remove current line */
      cData := Substr( cData, nLineEnd + nLen )

      /* Skip void lines */
      IF Empty( cLine )
         LOOP
      ENDIF

      /* Sum up lines terminating with "<space>||" ...*/
      IF Len( cLine ) > 3 .and. SubStr( cLine, -3, 3 ) == " ||"

         cLine := Substr( cLine, 1, Len( cLine ) -2 )
         /* ... but proceed if stream over */
         IF Len( cData ) > 0
            LOOP
         ENDIF

      ENDIF

      /* remove eventual comments */
      aKeyVal := HB_RegexSplit( reComment, cLine )
      IF ! Empty( aKeyVal )
         cLine := AllTrim( aKeyVal[1] )
      ENDIF

      /* Skip all comment lines */
      IF Empty( cLine )
         LOOP
      ENDIF

      /* Is it an "INCLUDE" statement ? */
      aKeyVal := HB_RegEx( reInclude, cLine )
      IF ! Empty( aKeyVal )
         /* ignore void includes */
         aKeyVal[ 2 ] := AllTrim( aKeyVal[ 2 ] )
         IF Len( aKeyVal[ 2 ] ) == 0
            LOOP
         ENDIF
         HB_ReadIni2( hIni, aKeyVal[ 2 ], bKeyCaseSens, cSplitters, bAutoMain )
         cLine := ""
         LOOP
      ENDIF

      /* Is it a NEW section? */
      aKeyVal := HB_Regex( reSection, cLine )
      IF ! Empty( aKeyVal )
         cLine := AllTrim( aKeyVal[ 2 ] )
         IF Len( cLine ) != 0
            hCurrentSection := hb_Hash()
            IF ! bKeyCaseSens
               cLine := Upper( cLine )
            ENDIF
            hIni[ cLine ] := hCurrentSection
         ENDIF
         cLine := ""
         LOOP
      ENDIF

      /* Is it a valid key */
      aKeyVal := HB_RegexSplit( reSplitters, cLine,,, 2 )
      IF Len( aKeyVal ) == 1
         /* TODO: Signal error */
         cLine := ""
         LOOP
      ENDIF

      /* If not case sensitive, use upper keys */
      IF ! bKeyCaseSens
         aKeyVal[ 1 ] := Upper( aKeyVal[ 1 ] )
      ENDIF

      hCurrentSection[ AllTrim( aKeyVal[ 1 ] ) ] := AllTrim( aKeyVal[ 2 ] )
      cLine := ""
   ENDDO

RETURN hIni


FUNCTION HB_WriteIni( cFileName, hIni, cCommentBegin, cCommentEnd, bAutoMain )
   LOCAL hFile := -1
   LOCAL lClose := .F.
   LOCAL cNewLine := HB_OSNewLine()
   LOCAL cSection
   LOCAL cBuffer

   IF bAutoMain == NIL
      bAutoMain := .T.
   END

   IF VALTYPE( cFileName ) == "C"
      hFile := FCreate( cFileName )
      lClose := .T.
   ELSEIF VALTYPE( cFileName ) == "N"
      hFile := cFileName
   ENDIF

   IF hFile == -1
      RETURN .F.
   ENDIF

   IF !Empty( cCommentBegin )
      cBuffer := cCommentBegin + cNewLine
      IF FWrite( hFile, cBuffer ) != Len( cBuffer )
         IF lClose
            FClose( hFile )
         ENDIF
         RETURN .F.
      ENDIF
   ENDIF

   /* Write toplevel section */
   IF bAutoMain
      /* When automain is on, write the main section */
      hb_HEval( hIni[ "MAIN" ], ;
               { |cKey, xVal| FWrite( hFile, HB_CStr( cKey ) + " = " + ;
                                             HB_CStr( xVal ) + cNewLine ) } )
           
   ELSE
      /* When automain is off, just write all the toplevel variables. */
      hb_HEval( hIni, { |cKey, xVal| IIF( ! HB_IsHash( xVal ),;
                FWrite( hFile, HB_CStr( cKey ) + " = " + ;
                               HB_CStr( xVal ) + cNewLine ), /* nothing */ ) } )
   ENDIF

   FOR EACH cSection IN hIni

      /* Avoid re-processing main section */
      IF bAutoMain
         /* When automain is on, skip section named MAIN */
         IF cSection:__enumKey == "MAIN"
            LOOP
         ENDIF
      ELSE
         /* When automain is off, skip all the toplevel variables. */
         IF ! HB_IsHash( cSection )
            LOOP
         END
      ENDIF

      cBuffer := cNewLine + "[" + HB_CStr( cSection:__enumKey ) + "]" + cNewLine
      IF FWrite( hFile, cBuffer ) != Len( cBuffer )
         IF lClose
            FClose( hFile )
         ENDIF
         RETURN .F.
      ENDIF

      hb_HEval( cSection, ;
                { |cKey, xVal| FWrite( hFile, HB_CStr( cKey ) + "=" + ;
                                              HB_CStr( xVal ) + cNewLine ) } )
   NEXT

   IF !Empty( cCommentEnd )
      cBuffer := cCommentEnd + cNewLine
      IF FWrite( hFile, cBuffer ) != Len( cBuffer )
         IF lClose
            FClose( hFile )
         ENDIF
         RETURN .F.
      ENDIF
   ENDIF

   IF lClose
      FClose( hFile )
   ENDIF

RETURN .T.
