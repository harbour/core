/*
 * Harbour Project source code:
 *    CP detection
 *
 * Copyright 2012 Viktor Szakats (vszakats.net/harbour)
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbextcdp.ch"

FUNCTION hb_cdpTerm()

   LOCAL cCP
   LOCAL cLang

#if defined( __PLATFORM__WINDOWS )
   cCP := __CPWinToCPStd( __wapi_GetConsoleOutputCP() )
   cLang := hb_UserLang()
#elif defined( __PLATFORM__UNIX )
   IF ! Empty( GetEnv( "LANG" ) )
      __UnixParseLangCP( GetEnv( "LANG" ), @cCP, @cLang )
   ELSE
      __UnixParseLangCP( GetEnv( "LC_CTYPE" ), @cCP, @cLang )
   ENDIF
   cCP := __CPUnixToCPStd( cCP )
#elif defined( __PLATFORM__DOS )
   /* TODO */
   cCP := NIL
   cLang := NIL
#elif defined( __PLATFORM__OS2 )
   /* TODO */
   cCP := NIL
   cLang := NIL
#endif

   IF ! Empty( cCP := __CPStdToHb( cCP, cLang ) )
      RETURN cCP
   ENDIF

   RETURN NIL

FUNCTION hb_cdpOS()

   LOCAL cCP
   LOCAL cLang

#if defined( __PLATFORM__WINDOWS )
   cCP := __CPWinToCPStd( __wapi_GetACP() )
   cLang := hb_UserLang()
#elif defined( __PLATFORM__UNIX )
   IF ! Empty( GetEnv( "LANG" ) )
      __UnixParseLangCP( GetEnv( "LANG" ), @cCP, @cLang )
   ELSE
      __UnixParseLangCP( GetEnv( "LC_CTYPE" ), @cCP, @cLang )
   ENDIF
   cCP := __CPUnixToCPStd( cCP )
#elif defined( __PLATFORM__DOS )
   /* TODO */
   cCP := NIL
   cLang := NIL
#elif defined( __PLATFORM__OS2 )
   /* TODO */
   cCP := NIL
   cLang := NIL
#endif

   IF ! Empty( cCP := __CPStdToHb( cCP, cLang ) )
      RETURN cCP
   ENDIF

   RETURN NIL

#if defined( __PLATFORM__WINDOWS )
STATIC FUNCTION __CPWinToCPStd( nCPWin )

   IF HB_ISNUMERIC( nCPWin )
      SWITCH nCPWin
      CASE 437   ; RETURN "cp437"
      CASE 737   ; RETURN "cp737"
      CASE 775   ; RETURN "cp775"
      CASE 850   ; RETURN "cp850"
      CASE 852   ; RETURN "cp852"
      CASE 857   ; RETURN "cp857"
      CASE 860   ; RETURN "cp860"
      CASE 861   ; RETURN "cp861"
      CASE 865   ; RETURN "cp865"
      CASE 866   ; RETURN "cp866"
      CASE 1200  ; RETURN "utf16"
      CASE 1250  ; RETURN "cp1250"
      CASE 1251  ; RETURN "cp1251"
      CASE 1252  ; RETURN "cp1252"
      CASE 1253  ; RETURN "cp1253"
      CASE 1254  ; RETURN "cp1254"
      CASE 1257  ; RETURN "cp1257"
      CASE 20866 ; RETURN "koi-8"
      CASE 21866 ; RETURN "koi-8u"
      CASE 28591 ; RETURN "iso8859-1"
      CASE 28592 ; RETURN "iso8859-2"
      CASE 28595 ; RETURN "iso8859-5"
      CASE 28597 ; RETURN "iso8859-7"
      CASE 28599 ; RETURN "iso8859-9"
      CASE 65001 ; RETURN "utf8"
      ENDSWITCH
   ENDIF

   RETURN ""

#elif defined( __PLATFORM__UNIX )

/* language[_territory][.codeset] */
/* [language[_territory][.codeset][@modifier]] */
/* TODO: handle "C"/"POSIX" values and values starting with "/" */
STATIC PROCEDURE __UnixParseLangCP( cString, /* @ */ cCP, /* @ */ cLang )

   LOCAL tmp

   IF ( tmp := At( ".", cString ) ) > 0
      cLang := Left( cString, tmp - 1 )
      cCP := SubStr( cString, tmp + 1 )
      IF ( tmp := At( "@", cString ) ) > 0
         cCP := Left( cString, tmp - 1 )
      ENDIF
   ELSE
      cLang := cString
      cCP := "UTF-8"
   ENDIF

   RETURN

STATIC FUNCTION __CPUnixToCPStd( cCPUnix )

   IF HB_ISSTRING( cCPUnix )
      /* TOFIX: update the list of std unix cp names */
      SWITCH Lower( hb_StrReplace( cCPUnix, "_-" ) )
      CASE "ibm437"
      CASE "cp437"       ; RETURN "cp437"
      CASE "ibm737"
      CASE "cp737"       ; RETURN "cp737"
      CASE "ibm775"
      CASE "cp775"       ; RETURN "cp775"
      CASE "ibm850"
      CASE "cp850"       ; RETURN "cp850"
      CASE "ibm852"
      CASE "cp852"       ; RETURN "cp852"
      CASE "ibm857"
      CASE "cp857"       ; RETURN "cp857"
      CASE "ibm860"
      CASE "cp860"       ; RETURN "cp860"
      CASE "ibm861"
      CASE "cp861"       ; RETURN "cp861"
      CASE "ibm865"
      CASE "cp865"       ; RETURN "cp865"
      CASE "ibm866"
      CASE "cp866"       ; RETURN "cp866"
      CASE "windows1250" ; RETURN "cp1250"
      CASE "windows1251" ; RETURN "cp1251"
      CASE "windows1252" ; RETURN "cp1252"
      CASE "windows1253" ; RETURN "cp1253"
      CASE "windows1254" ; RETURN "cp1254"
      CASE "windows1257" ; RETURN "cp1257"
      CASE "koi8r"       ; RETURN "koi-8"
      CASE "koi8u"       ; RETURN "koi-8u"
      CASE "iso88591"    ; RETURN "iso8859-1"
      CASE "iso88592"    ; RETURN "iso8859-2"
      CASE "iso88595"    ; RETURN "iso8859-5"
      CASE "iso88597"    ; RETURN "iso8859-7"
      CASE "iso88599"    ; RETURN "iso8859-9"
      CASE "utf8"        ; RETURN "utf8"
      ENDSWITCH
   ENDIF

   RETURN ""
#endif

STATIC FUNCTION __CPStdToHb( cCPStd, cCtryStd )

   LOCAL cCP
   LOCAL cCtryHb
   LOCAL cdp

   IF ! Empty( cCPStd )
      IF Lower( cCPStd ) == "utf8"
         cCP := "UTF8"
      ELSEIF Lower( cCPStd ) == "utf16"
         cCP := "UTF16LE"
      ELSE
         IF ! Empty( cCtryHb := __LangStdToCPCtryHb( cCtryStd ) )
            FOR EACH cdp IN hb_cdpList()
               IF Left( cdp, 2 ) == cCtryHb
                  IF Lower( cCPStd ) == hb_cdpUniID( cdp )
                     cCP := cdp
                     EXIT
                  ENDIF
               ENDIF
            NEXT
         ENDIF
         IF Empty( cCP )
            FOR EACH cdp IN hb_cdpList()
               IF Lower( cCPStd ) == hb_cdpUniID( cdp )
                  cCP := cdp
                  EXIT
               ENDIF
            NEXT
         ENDIF
      ENDIF
   ENDIF

   RETURN cCP

STATIC FUNCTION __LangStdToCPCtryHb( cCtryStd )

   IF HB_ISSTRING( cCtryStd )
      SWITCH Lower( cCtryStd )
#if 0
      CASE "af-za"      ; EXIT
      CASE "af"         ; EXIT
      CASE "ar-ae"      ; EXIT
      CASE "ar-bh"      ; EXIT
      CASE "ar-dz"      ; EXIT
      CASE "ar-eg"      ; EXIT
      CASE "ar-iq"      ; EXIT
      CASE "ar-jo"      ; EXIT
      CASE "ar-kw"      ; EXIT
      CASE "ar-lb"      ; EXIT
      CASE "ar-ly"      ; EXIT
      CASE "ar-ma"      ; EXIT
      CASE "ar-om"      ; EXIT
      CASE "ar-qa"      ; EXIT
      CASE "ar-sa"      ; EXIT
      CASE "ar-sy"      ; EXIT
      CASE "ar-tn"      ; EXIT
      CASE "ar-ye"      ; EXIT
      CASE "ar"         ; EXIT
      CASE "az-az-cyrl" ; EXIT
      CASE "az-az-latn" ; EXIT
      CASE "az"         ; EXIT
      CASE "be-by"      ; EXIT
      CASE "be"         ; EXIT
#endif
      CASE "bg-bg"
      CASE "bg"         ; RETURN "BG"
#if 0
      CASE "ca-es"      ; EXIT
      CASE "ca"         ; EXIT
      CASE "cy-gb"      ; EXIT
#endif
      CASE "cs-cz"
      CASE "cs"         ; RETURN "CS"
      CASE "da-dk"
      CASE "da"         ; RETURN "DK"
      CASE "de-at"
      CASE "de-ch"
      CASE "de-de"
      CASE "de-li"
      CASE "de-lu"
      CASE "de"         ; RETURN "DE"
#if 0
      CASE "div-mv"     ; EXIT
      CASE "div"        ; EXIT
#endif
      CASE "el-gr"
      CASE "el"         ; RETURN "EL"
      CASE "en-au"
      CASE "en-bz"
      CASE "en-ca"
      CASE "en-cb"
      CASE "en-gb"
      CASE "en-ie"
      CASE "en-jm"
      CASE "en-nz"
      CASE "en-ph"
      CASE "en-tt"
      CASE "en-us"
      CASE "en-za"
      CASE "en-zw"
      CASE "en"         ; RETURN "EN"
#if 0
      CASE "eo"         ; EXIT
#endif
      CASE "es-419"
      CASE "es-ar"
      CASE "es-bo"
      CASE "es-cl"
      CASE "es-co"
      CASE "es-cr"
      CASE "es-do"
      CASE "es-ec"
      CASE "es-es"
      CASE "es-gt"
      CASE "es-hn"
      CASE "es-mx"
      CASE "es-ni"
      CASE "es-pa"
      CASE "es-pe"
      CASE "es-pr"
      CASE "es-py"
      CASE "es-sv"
      CASE "es-uy"
      CASE "es-ve"
      CASE "es"         ; RETURN "ES"
#if 0
      CASE "et-ee"      ; EXIT
      CASE "et"         ; EXIT
      CASE "eu-es"      ; EXIT
      CASE "eu"         ; EXIT
      CASE "fa-ir"      ; EXIT
      CASE "fa"         ; EXIT
#endif
      CASE "fi-fi"
      CASE "fi"         ; RETURN "FI"
#if 0
      CASE "fo-fo"      ; EXIT
      CASE "fo"         ; EXIT
#endif
      CASE "fr-be"
      CASE "fr-ca"
      CASE "fr-ch"
      CASE "fr-fr"
      CASE "fr-lu"
      CASE "fr-mc"
      CASE "fr"         ; RETURN "FR"
#if 0
      CASE "gl-es"      ; EXIT
      CASE "gl"         ; EXIT
      CASE "gu-in"      ; EXIT
      CASE "gu"         ; EXIT
#endif
      CASE "he-il"
      CASE "he"         ; RETURN "HE"
#if 0
      CASE "hi-in"      ; EXIT
      CASE "hi"         ; EXIT
#endif
      CASE "hr-hr"
      CASE "hr"         ; RETURN "HR"
      CASE "hu-hu"
      CASE "hu"         ; RETURN "HU"
#if 0
      CASE "hy-am"      ; EXIT
      CASE "hy"         ; EXIT
      CASE "id-id"      ; EXIT
      CASE "id"         ; EXIT
#endif
      CASE "is-is"
      CASE "is"         ; RETURN "IS"
      CASE "it-ch"
      CASE "it-it"
      CASE "it"         ; RETURN "IT"
#if 0
      CASE "ja-jp"      ; EXIT
      CASE "ja"         ; EXIT
      CASE "ka-ge"      ; EXIT
      CASE "ka"         ; EXIT
      CASE "kk-kz"      ; EXIT
      CASE "kk"         ; EXIT
      CASE "kn-in"      ; EXIT
      CASE "kn"         ; EXIT
      CASE "ko-kr"      ; EXIT
      CASE "ko"         ; EXIT
      CASE "kok-in"     ; EXIT
      CASE "kok"        ; EXIT
      CASE "ky-kz"      ; EXIT
      CASE "ky"         ; EXIT
#endif
      CASE "lt-lt"
      CASE "lt"         ; RETURN "LT"
#if 0
      CASE "lv-lv"      ; EXIT
      CASE "lv"         ; EXIT
      CASE "mk-mk"      ; EXIT
      CASE "mk"         ; EXIT
      CASE "mn-mn"      ; EXIT
      CASE "mn"         ; EXIT
      CASE "mr-in"      ; EXIT
      CASE "mr"         ; EXIT
      CASE "ms-bn"      ; EXIT
      CASE "ms-my"      ; EXIT
      CASE "ms"         ; EXIT
      CASE "nb-no"      ; EXIT
#endif
      CASE "nl-be"
      CASE "nl-nl"
      CASE "nl"         ; RETURN "NL"
#if 0
      CASE "nn-no"      ; EXIT
#endif
      CASE "no"         ; RETURN "NO"
#if 0
      CASE "pa-in"      ; EXIT
      CASE "pa"         ; EXIT
#endif
      CASE "pl-pl"
      CASE "pl"         ; RETURN "PL"
      CASE "pt-br"
      CASE "pt-pt"
      CASE "pt"         ; RETURN "PT"
      CASE "ro-ro"
      CASE "ro"         ; RETURN "RO"
      CASE "ru-ru"
      CASE "ru"         ; RETURN "RU"
#if 0
      CASE "sa-in"      ; EXIT
      CASE "sa"         ; EXIT
#endif
      CASE "sk-sk"
      CASE "sk"         ; RETURN "SK"
      CASE "sl-si"
      CASE "sl"         ; RETURN "SL"
#if 0
      CASE "sq-al"      ; EXIT
      CASE "sq"         ; EXIT
#endif
      CASE "sr-sp-cyrl"
      CASE "sr-sp-latn" ; RETURN "SR"
      CASE "sv-fi"
      CASE "sv-se"
      CASE "sv"         ; RETURN "SV"
#if 0
      CASE "sw-ke"      ; EXIT
      CASE "sw"         ; EXIT
      CASE "syr-sy"     ; EXIT
      CASE "syr"        ; EXIT
      CASE "ta-in"      ; EXIT
      CASE "ta"         ; EXIT
      CASE "te-in"      ; EXIT
      CASE "te"         ; EXIT
      CASE "th-th"      ; EXIT
      CASE "th"         ; EXIT
#endif
      CASE "tr-tr"
      CASE "tr"         ; RETURN "TR"
#if 0
      CASE "tt-ru"      ; EXIT
      CASE "tt"         ; EXIT
#endif
      CASE "uk-ua"
      CASE "uk"         ; RETURN "UA"
#if 0
      CASE "ur-pk"      ; EXIT
      CASE "ur"         ; EXIT
      CASE "uz-uz-cyrl" ; EXIT
      CASE "uz-uz-latn" ; EXIT
      CASE "uz"         ; EXIT
      CASE "vi-vn"      ; EXIT
      CASE "vi"         ; EXIT
      CASE "zh-chs"     ; EXIT
      CASE "zh-cht"     ; EXIT
      CASE "zh-cn"      ; EXIT
      CASE "zh-hk"      ; EXIT
      CASE "zh-mo"      ; EXIT
      CASE "zh-sg"      ; EXIT
      CASE "zh-tw"      ; EXIT
      CASE "zh"         ; EXIT
#endif
      ENDSWITCH
   ENDIF

   RETURN Left( hb_cdpSelect(), 2 )
