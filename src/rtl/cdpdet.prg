/*
 * CP detection
 *
 * Copyright 2012 Viktor Szakats (vszakats.net/harbour)
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
   LOCAL tmp
   cCP := __CPWinToCPStd( iif( ( tmp := __wapi_GetConsoleOutputCP() ) == 0, __wapi_GetOEMCP(), tmp ) )
   cLang := hb_UserLang()
#elif defined( __PLATFORM__UNIX )
   LOCAL tmp
   cCP := __UnixParseLangCP( iif( HB_ISNULL( tmp := GetEnv( "LANG" ) ), ;
                                  GetEnv( "LC_CTYPE" ), tmp ), @cLang )
#elif defined( __PLATFORM__DOS )
   /* TODO */
   cCP := cLang := NIL
#elif defined( __PLATFORM__OS2 )
   /* TODO */
   cCP := cLang := NIL
#endif

   RETURN __CPStdToHb( cCP, cLang )

FUNCTION hb_cdpOS()

   LOCAL cCP
   LOCAL cLang

#if defined( __PLATFORM__WINDOWS )
   cCP := __CPWinToCPStd( __wapi_GetACP() )
   cLang := hb_UserLang()
#elif defined( __PLATFORM__UNIX )
   LOCAL tmp
   cCP := __UnixParseLangCP( iif( HB_ISNULL( tmp := GetEnv( "LANG" ) ), ;
                                  GetEnv( "LC_CTYPE" ), tmp ), @cLang )
#elif defined( __PLATFORM__DOS )
   /* TODO */
   cCP := cLang := NIL
#elif defined( __PLATFORM__OS2 )
   /* TODO */
   cCP := cLang := NIL
#endif

   RETURN __CPStdToHb( cCP, cLang )

#if defined( __PLATFORM__WINDOWS )
STATIC FUNCTION __CPWinToCPStd( nCPWin )

   SWITCH nCPWin
   CASE 65001 ; RETURN "utf8"
   CASE 1200  ; RETURN "utf16"
   CASE 437
   CASE 737
   CASE 775
   CASE 850
   CASE 852
   CASE 855
   CASE 856
   CASE 857
   CASE 858
   CASE 860
   CASE 861
   CASE 862
   CASE 863
   CASE 864
   CASE 865
   CASE 866
   CASE 869
   CASE 874
   CASE 1250
   CASE 1251
   CASE 1252
   CASE 1253
   CASE 1254
   CASE 1255
   CASE 1256
   CASE 1257
   CASE 1258  ; RETURN "cp" + hb_ntos( nCPWin )
   CASE 28591
   CASE 28592
   CASE 28593
   CASE 28594
   CASE 28595
   CASE 28596
   CASE 28597
   CASE 28598
   CASE 28599
   CASE 28603
   CASE 28605 ; RETURN "iso8859-" + hb_ntos( nCPWin - 28590 )
   CASE 20866 ; RETURN "koi-8"
   CASE 21866 ; RETURN "koi-8u"
   CASE 936   ; RETURN "GBK"
   CASE 950   ; RETURN "CP950"
   ENDSWITCH

   RETURN NIL

#elif defined( __PLATFORM__UNIX )

/* language[_territory][.codeset] */
/* [language[_territory][.codeset][@modifier]] */
/* TODO: handle "C"/"POSIX" values and values starting with "/" */
STATIC FUNCTION __UnixParseLangCP( cString, /* @ */ cLang )

   LOCAL tmp
   LOCAL cCP

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

   /* Tricks to make the manual translation table shorter */
   cCP := hb_StrReplace( Lower( cCP ), { "_" => "", "-" => "", "ibm" => "cp", "windows" => "cp" } )
   IF hb_LeftEq( cCP, "iso8859" )
      cCP := Stuff( cCP, Len( "iso8859" ) + 1, 0, "-" )
   ENDIF

   /* Convert UNIX CP name to Harbour CP ID */
   SWITCH cCP
   CASE "utf8"
   CASE "cp437"
   CASE "cp737"
   CASE "cp775"
   CASE "cp850"
   CASE "cp852"
   CASE "cp855"
   CASE "cp856"
   CASE "cp857"
   CASE "cp858"
   CASE "cp860"
   CASE "cp861"
   CASE "cp862"
   CASE "cp863"
   CASE "cp864"
   CASE "cp865"
   CASE "cp866"
   CASE "cp869"
   CASE "cp874"
   CASE "cp1250"
   CASE "cp1251"
   CASE "cp1252"
   CASE "cp1253"
   CASE "cp1254"
   CASE "cp1255"
   CASE "cp1256"
   CASE "cp1257"
   CASE "cp1258"
   CASE "iso8859-1"
   CASE "iso8859-2"
   CASE "iso8859-3"
   CASE "iso8859-4"
   CASE "iso8859-5"
   CASE "iso8859-6"
   CASE "iso8859-7"
   CASE "iso8859-8"
   CASE "iso8859-9"
   CASE "iso8859-13"
   CASE "iso8859-15" ; RETURN cCP
   CASE "koi8r"      ; RETURN "koi-8"
   CASE "koi8u"      ; RETURN "koi-8u"
   CASE "gbk"        ; RETURN "GBK"
   CASE "big5"       ; RETURN "BIG5"
   ENDSWITCH

   RETURN NIL

#endif

STATIC FUNCTION __CPStdToHb( cCPStd, cCtryStd )

   LOCAL cCtryHb
   LOCAL cdp
   LOCAL aCP

   IF cCPStd != NIL
      SWITCH cCPStd := Lower( cCPStd )
      CASE "utf8"
         RETURN "UTF8"
      CASE "utf16"
         RETURN "UTF16LE"
      OTHERWISE
         aCP := hb_cdpList()
         cCtryHb := __LangStdToCPCtryHb( cCtryStd )
         FOR EACH cdp IN aCP
            IF hb_LeftEq( cdp, cCtryHb ) .AND. cCPStd == hb_cdpUniID( cdp )
               RETURN cdp
            ENDIF
         NEXT
         FOR EACH cdp IN aCP
            IF cCPStd == hb_cdpUniID( cdp )
               RETURN hb_cdpUniID( cdp )
            ENDIF
         NEXT
      ENDSWITCH
   ENDIF

   RETURN NIL

STATIC FUNCTION __LangStdToCPCtryHb( cCtryStd )

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

   RETURN Left( hb_cdpSelect(), 2 )  /* Caller assumes this never returns strings shorter than two chars */
