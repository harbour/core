/*
 * Converts .po files to lang modules
 *
 * Copyright 2013 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 */

#pragma -w3
#pragma -km+
#pragma -ko+

#include "hblang.ch"

PROCEDURE Main_po2lang()

   POToLang( "hu.po", "l_hu.c", "hu" )
   POToLang( "el.po", "l_el.c", "el" )

   RETURN

STATIC FUNCTION POToLang( cFileIn, cFileOut, cLang )

   LOCAL aTrans
   LOCAL cErrorMsg

   LOCAL cContent
   LOCAL cTranslator
   LOCAL nPos

   IF ( aTrans := __i18n_potArrayLoad( cFileIn, @cErrorMsg ) ) != NIL

      cContent := StrTran( _begin(), e"\n", hb_eol() )
      nPos := 0

      __i18n_potArrayClean( aTrans,,, {| cTrs, cOri | ProcessTrs( @cContent, cTrs, cOri, @cTranslator, @nPos, cLang ) } )

      cContent := "/* Last Translator: " + cTranslator + " */" + hb_eol() + ;
         Left( cContent, Len( cContent ) - Len( "," ) - Len( hb_eol() ) ) + hb_eol() + ;
         StrTran( StrTran( _end(), e"\n", hb_eol() ), "{LNG}", Upper( cLang ) )

      hb_MemoWrit( cFileOut, cContent )

      RETURN .T.
   ENDIF

   ? "i18n error", cErrorMsg

   RETURN .F.

STATIC FUNCTION ProcessTrs( /* @ */ cContent, cTrs, cOri, /* @ */ cTranslator, /* @ */ nPos, cLang )

   STATIC sc_hEmpty := { ;
      3  => { "", "UTF8", "" }, ;
      47 => { "", "" }, ;
      57 => { "" }, ;
      64 => { "", "", "", "" }, ;
      80 => { "", "", "" } }

   LOCAL tmp

   SWITCH nPos
   CASE HB_LANG_ITEM_BASE_ID      ; tmp := "/* Identification */" ; EXIT
   CASE HB_LANG_ITEM_BASE_MONTH   ; tmp := "/* Month names */" ; EXIT
   CASE HB_LANG_ITEM_BASE_DAY     ; tmp := "/* Day names */" ; EXIT
   CASE HB_LANG_ITEM_BASE_NATMSG  ; tmp := "/* CA-Cl*pper compatible natmsg items */" ; EXIT
   CASE HB_LANG_ITEM_BASE_ERRDESC ; tmp := "/* Error description names */" ; EXIT
   CASE HB_LANG_ITEM_BASE_ERRINTR ; tmp := "/* Internal error names */" ; EXIT
   CASE HB_LANG_ITEM_BASE_TEXT    ; tmp := "/* Texts */" ; EXIT
   OTHERWISE                      ; tmp := NIL
   ENDSWITCH

   IF tmp != NIL
      cContent += iif( nPos > 0, hb_eol(), "" ) + Space( 6 ) + tmp + hb_eol() + hb_eol()
   ENDIF

   IF nPos == 0
      cTranslator := hb_regexAll( "Last-Translator: ([^\n]*)", cTrs,,,,, .T. )[ 1 ][ 2 ]
      IF cTranslator == "foo bar <foo.bar@foobaz>"
         cTranslator := ""
      ENDIF
      cContent += Space( 6 ) + ConvToC( cLang ) + "," + hb_eol()
      ++nPos
   ELSE
      IF Len( cTrs ) == 0
         cTrs := cOri
      ENDIF
      cContent += Space( 6 ) + ConvToC( cTrs ) + "," + hb_eol()
      ++nPos

      IF nPos $ sc_hEmpty
         FOR tmp := 1 TO Len( sc_hEmpty[ nPos ] )
            cContent += Space( 6 ) + ConvToC( sc_hEmpty[ nPos ][ tmp ] ) + "," + hb_eol()
         NEXT
         nPos += Len( sc_hEmpty[ nPos ] )
      ENDIF
   ENDIF

   RETURN NIL

STATIC FUNCTION ConvToC( cStr )
   RETURN '"' + hb_StrReplace( cStr, { '"' => '\"' } ) + '"'

STATIC FUNCTION _begin()
#pragma __cstream | RETURN %s

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
#pragma __endtext

STATIC FUNCTION _end()
#pragma __cstream | RETURN %s
   }
};

#define HB_LANG_ID      {LNG}
#include "hbmsgreg.h"
#pragma __endtext
