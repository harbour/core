/*
 * Harbour Project source code:
 * Manage translations and automatic doc generation
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

/*
 * Requires: curl (built with SSL) and Harbour in PATH
 * Reference: http://help.transifex.com/features/api/api-v2.1.html
 */

#pragma -w3

#include "directry.ch"

PROCEDURE Main( cCommand, ... )

   LOCAL hCommand := { ;
      "doc_make" => @doc_make(), ; /* Generate doc files for all languages */
      "src_push" => @src_push(), ; /* Upload translation source to Transifex localization service */
      "trs_pull" => @trs_pull(), ; /* Download translations from Transifex localization service */
      "trs_push" => @trs_push() }  /* Upload local translations to Transifex localization service */

   IF ! Empty( cCommand ) .AND. cCommand $ hCommand
      Set( _SET_DEFEXTENSIONS, .F. )
      Eval( hCommand[ cCommand ], ... )
   ELSE
      ? "unknown command"
   ENDIF

   RETURN

/* --------------------------------------------- */

STATIC PROCEDURE doc_make()

   LOCAL cBase := hb_DirBase()

   LOCAL file
   LOCAL cLang
   LOCAL cTemp

   LOCAL cMain := cBase + "hbmk2.prg"  /* must be runnable as script */
   LOCAL cDocOptions := "-lang={LANG} -longhelpmd"
   LOCAL cBaseLang := "en"
   LOCAL cPO_Dir := cBase + hb_DirSepToOS( "po/" )
   LOCAL cDoc_Dir := cBase + hb_DirSepToOS( "doc/" )

   cTemp := hb_FNameExtSet( cMain, ".hrb" )

   ? "generating documentation:"

   hb_run( hb_StrFormat( "hbmk2 -hbraw -q0 %1$s -gh -o%2$s", cMain, cTemp ) )

   FOR EACH cLang IN hb_ATokens( cBaseLang + "," + hb_regexAll( "-lng=([a-zA-Z0-9_\-,]*)", hb_MemoRead( hb_FNameExtSet( cMain, ".hbp" ) ),,,,, .T. )[ 1 ][ 2 ], "," )

      ?? "", cLang

      hb_run( hb_StrFormat( "hbi18n -q -g %1$s -o%2$s", ;
         cPO_Dir + hb_FNameName( cMain ) + "." + cLang + ".po", ;
         hb_FNameDir( cMain ) + hb_FNameName( cMain ) + "." + cLang + ".hbl" ) )

      file := cDoc_Dir + hb_FNameName( cMain ) + "." + cLang + ".md"
      hb_run( hb_StrFormat( "hbrun %1$s %2$s > %3$s", cTemp, StrTran( cDocOptions, "{LANG}", cLang ), file ) )
      FToNativeEOL( file )

      /* special case */
      IF hb_FNameName( cMain ) == "hbmk2"
         file := hb_FNameDir( cMain ) + hb_DirSepToOS( "../../contrib/hbrun/doc/" ) + "hbrun" + "." + cLang + ".md"
         hb_run( hb_StrFormat( "hbrun %1$s %2$s > %3$s", cTemp, StrTran( "-lang={LANG} -longhelpmdsh", "{LANG}", cLang ), file ) )
         FToNativeEOL( file )
      ENDIF

      FErase( hb_FNameDir( cMain ) + hb_FNameName( cMain ) + "." + cLang + ".hbl" )
   NEXT

   FErase( cTemp )

   RETURN

STATIC FUNCTION FToNativeEOL( cFile )
   RETURN hb_MemoWrit( cFile, StrTran( hb_MemoRead( cFile ), e"\n", hb_eol() ) )

/* --------------------------------------------- */

STATIC PROCEDURE src_push( cLogin )

   LOCAL cBase := hb_DirBase()

   LOCAL json
   LOCAL cTemp, cTemp2
   LOCAL cContent

   LOCAL cProject := "harbour"
   LOCAL cMain := cBase + "hbmk2.prg"
   LOCAL cBaseLang := "en"
   LOCAL cPO_Dir := cBase + hb_DirSepToOS( "po/" )

   IF Empty( cLogin )
      cLogin := GetEnv( "HB_TRANSIFEX_LOGIN" )  /* Format: username:password */
   ENDIF

   FClose( hb_FTempCreateEx( @cTemp, , , ".pot" ) )
   FClose( hb_FTempCreateEx( @cTemp2 ) )

   ? "generating translation source"

   hb_run( hb_StrFormat( "hbmk2 -hbraw -q0 %1$s -j%2$s -s", cMain, cTemp ) )

   POT_Sort( cTemp )

   ? "saving locally"

   cContent := hb_StrFormat( ;
      '#, c-format' + hb_eol() + ;
      'msgid ""' + hb_eol() + ;
      'msgstr ""' + hb_eol() + ;
      '"Project-Id-Version: %1$s\n"' + hb_eol() + ;
      '"Language: %2$s\n"' + hb_eol() + ;
      '"MIME-Version: 1.0\n"' + hb_eol() + ;
      '"Content-Type: text/plain; charset=UTF-8\n"' + hb_eol() + ;
      '"Content-Transfer-Encoding: 8bit\n"', hb_FNameName( cMain ), cBaseLang ) + hb_eol() + ;
      hb_eol() + ;
      hb_MemoRead( cTemp )

   hb_MemoWrit( cPO_Dir + hb_FNameName( cMain ) + "." + cBaseLang + ".po", cContent )

   ? "uploading", "size", hb_ntos( Len( cContent ) )

   hb_MemoWrit( cTemp, hb_jsonEncode( { "content" => StrTran( cContent, hb_eol(), e"\n" ) } ) )

   hb_run( hb_StrFormat( 'curl -s -i -L --user %1$s -X ' + ;
      'PUT -d @%2$s -H "Content-Type: application/json" ' + ;
      'https://www.transifex.com/api/2/project/%3$s/resource/%4$s/content/' + ;
      ' -o %5$s', ;
      cLogin, cTemp, cProject, hb_FNameName( cMain ), cTemp2 ) )

   IF hb_jsonDecode( GetJSON( hb_MemoRead( cTemp2 ) ), @json ) > 0
      ? hb_ValToExp( json )
   ELSE
      ? "API error"
   ENDIF

   FErase( cTemp )
   FErase( cTemp2 )

   RETURN

STATIC FUNCTION POT_Sort( cFileName )

   LOCAL aTrans
   LOCAL cErrorMsg

   IF ( aTrans := __i18n_potArrayLoad( cFileName, @cErrorMsg ) ) != NIL .AND. ;
      __i18n_potArraySave( cFileName, __i18n_potArraySort( aTrans ), @cErrorMsg )
      RETURN .T.
   ENDIF

   ? "i18n error", cErrorMsg

   RETURN .F.

/* --------------------------------------------- */

STATIC PROCEDURE trs_pull( cLogin )

   LOCAL cBase := hb_DirBase()

   LOCAL json
   LOCAL cLang
   LOCAL cTemp

   LOCAL cProject := "harbour"
   LOCAL cMain := cBase + "hbmk2.prg"
   LOCAL cPO_Dir := cBase + hb_DirSepToOS( "po/" )

   IF Empty( cLogin )
      cLogin := GetEnv( "HB_TRANSIFEX_LOGIN" )  /* Format: username:password */
   ENDIF

   FClose( hb_FTempCreateEx( @cTemp ) )

   ? "pulling translations:"

   FOR EACH cLang IN hb_ATokens( hb_regexAll( "-lng=([a-zA-Z0-9_\-,]*)", hb_MemoRead( hb_FNameExtSet( cMain, ".hbp" ) ),,,,, .T. )[ 1 ][ 2 ], "," )

      ?? "", cLang

      hb_run( hb_StrFormat( "curl -s -i -L --user %1$s -X " + ;
         "GET https://www.transifex.com/api/2/project/%2$s/resource/%3$s/translation/%4$s/" + ;
         " -o %5$s", ;
         cLogin, cProject, hb_FNameName( cMain ), cLang, cTemp ) )

      IF hb_jsonDecode( GetJSON( hb_MemoRead( cTemp ) ), @json ) > 0
         hb_MemoWrit( cTemp, json[ "content" ] )
         POT_Sort( cTemp )
         /* should only do this if the translation is primarily done
            on Transifex website. This encouraged and probably the case
            in practice. Delete source information, delete empty
            translations and apply some automatic transformation for
            common translation mistakes. */
         PO_Clean( cTemp, cPO_Dir + hb_FNameName( cMain ) + "." + cLang + ".po", .F., .F., @DoctorTranslation() )
      ELSE
         ? "API error"
      ENDIF
   NEXT

   FErase( cTemp )

   RETURN

STATIC FUNCTION DoctorTranslation( cString, cOri )

   LOCAL regex, hit

   cString := AllTrim( cString )

   /* Only if original doesn't have elongated spaces */
   IF cOri == StrUnspace( cOri )
      cString := StrUnspace( cString )
   ENDIF

   /* For Transifex: RETURN SYMBOL to real new line */
   cString := StrTran( cString, hb_UChar( 0x23CE ), e"\n" )

   /* Common typos: extra space or punctuation */
   cString := hb_StrReplace( cString, { ;
      e"\n "  => e"\n"  , ;
      e" .\n" => e".\n" , ;
      "( "    => "("    , ;
      " )"    => ")"    , ;
      " :"    => ":"    , ;
      " ,"    => ","    , ;
      " ;"    => ";"    , ;
      ":. "   => ": "   , ;
      ": . "  => ": "   , ;
      ":, "   => ": "   , ;
      ": , "  => ": "   } )

   /* Common typos: missing space */
   FOR EACH regex IN { ":([A-Za-z])", "[,;](\S)", "\)(\w)" }
      FOR EACH hit IN hb_regexAll( regex, cString,,,,, .T. )
         IF ! hit[ 1 ] $ cOri
            cString := StrTran( cString, hit[ 1 ], StrTran( hit[ 1 ], hit[ 2 ], " " + hit[ 2 ] ) )
         ENDIF
      NEXT
   NEXT

   RETURN cString

/* Converts multiple spaces to just one */
STATIC FUNCTION StrUnspace( cString )

   LOCAL cResult := ""
   LOCAL cChar, cCharPrev
   LOCAL tmp

   FOR tmp := 1 TO Len( cString )

      cChar := SubStr( cString, tmp, 1 )

      IF !( cChar == " " ) .OR. !( cCharPrev == " " )
         cResult += cChar
      ENDIF

      cCharPrev := cChar
   NEXT

   RETURN cResult

STATIC FUNCTION PO_Clean( cFNSource, cFNTarget, ... )

   LOCAL aTrans
   LOCAL cErrorMsg

   IF ( aTrans := __i18n_potArrayLoad( cFNSource, @cErrorMsg ) ) != NIL .AND. ;
      __i18n_potArraySave( cFNTarget, __i18n_potArrayClean( aTrans, ... ), @cErrorMsg )
      RETURN .T.
   ENDIF

   ? "i18n error", cErrorMsg

   RETURN .F.

/* --------------------------------------------- */

STATIC PROCEDURE trs_push( cLogin )

   LOCAL cBase := hb_DirBase()

   LOCAL json
   LOCAL cTemp, cTemp2
   LOCAL cContent

   LOCAL cProject := "harbour"
   LOCAL cMain := cBase + "hbmk2.prg"
   LOCAL cLang := "hu"
   LOCAL cPO_Dir := cBase + hb_DirSepToOS( "po/" )

   IF Empty( cLogin )
      cLogin := GetEnv( "HB_TRANSIFEX_LOGIN" )  /* Format: username:password */
   ENDIF

   FClose( hb_FTempCreateEx( @cTemp ) )
   FClose( hb_FTempCreateEx( @cTemp2 ) )

   cContent := hb_MemoRead( cPO_Dir + hb_FNameName( cMain ) + "." + cLang + ".po" )

   ? "uploading translation", "size", Len( cContent )

   hb_MemoWrit( cTemp, hb_jsonEncode( { "content" => StrTran( cContent, hb_eol(), e"\n" ) } ) )

   hb_run( hb_StrFormat( 'curl -s -i -L --user %1$s -X ' + ;
      'PUT -d @%2$s -H "Content-Type: application/json" ' + ;
      'https://www.transifex.com/api/2/project/%3$s/resource/%4$s/translation/%5$s/' + ;
      ' -o %6$s', ;
      cLogin, cTemp, cProject, hb_FNameName( cMain ), cLang, cTemp2 ) )

   IF hb_jsonDecode( GetJSON( hb_MemoRead( cTemp2 ) ), @json ) > 0
      ? hb_ValToExp( json )
   ELSE
      ? "API error"
   ENDIF

   FErase( cTemp )
   FErase( cTemp2 )

   RETURN

/* --------------------------------------------- */

STATIC FUNCTION GetJSON( cString )

   cString := SubStr( cString, At( "{", cString ) )
   cString := Left( cString, RAt( "}", cString ) )

   RETURN cString
