
#pragma -w3
#pragma -km+
#pragma -ko+

#include "hblang.ch"

PROCEDURE Main()

   LOCAL cFileIn := "hu.po"
   LOCAL cFileOut := "l_hu.c"

   PO_2_C( cFileIn, cFileOut )

   RETURN

STATIC FUNCTION PO_2_C( cFileIn, cFileOut, ... )

   LOCAL aTrans
   LOCAL cErrorMsg

   LOCAL cContent
   LOCAL cTranslator
   LOCAL cID
   LOCAL nPos

   IF ( aTrans := __i18n_potArrayLoad( cFileIn, @cErrorMsg ) ) != NIL

      cContent := StrTran( _begin(), e"\n", hb_eol() )
      nPos := 0

      __i18n_potArrayClean( aTrans,,, {| cTrs, cOri | ProcessTrs( @cContent, cTrs, cOri, @cTranslator, @cID, @nPos ) } )

      cContent := "/* Last Translator: " + cTranslator + " */" + hb_eol() + ;
         Left( cContent, Len( cContent ) - Len( "," ) - Len( hb_eol() ) ) + hb_eol() + ;
         StrTran( StrTran( _end(), e"\n", hb_eol() ), "{LNG}", Upper( cID ) )

      hb_MemoWrit( cFileOut, cContent )

      RETURN .T.
   ENDIF

   ? "i18n error", cErrorMsg

   RETURN .F.

STATIC FUNCTION ProcessTrs( /* @ */ cContent, cTrs, cOri, /* @ */ cTranslator, /* @ */ cID, /* @ */ nPos )

   LOCAL tmp, tmp1

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
      IF cTranslator == "a b <a.b@c.d>"
         cTranslator := ""
      ENDIF
      FOR tmp := 0 TO 5
         cContent += Space( 6 ) + ConvToC( tmp1 := hb_regexAll( hb_StrFormat( "Harbour-Lang-Meta-%1$d: ([\S]*)", tmp ), cTrs,,,,, .T. )[ 1 ][ 2 ] ) + "," + hb_eol()
         ++nPos
         IF tmp == 0
            cID := tmp1
         ENDIF
      NEXT
   ELSE
      IF Len( cTrs ) == 0
         cTrs := cOri
      ENDIF
      cContent += Space( 6 ) + ConvToC( cTrs ) + "," + hb_eol()
      ++nPos
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
