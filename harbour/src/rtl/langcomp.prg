/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Language API compatibility layer
 *
 * Copyright 2012 Viktor Szakats (harbour syenar.net)
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

FUNCTION hb_langSelect( cLangID )
   LOCAL tmp

   IF HB_ISSTRING( cLangID )
      tmp := __CtryStdToBaseLangID( cLangID )
      IF ! Empty( tmp )
         hb_langNew( cLangID, hb_cdpSelect(), tmp, "UTF8" )
      ELSE
         /* For compatibility with legacy codepages */
         SWITCH cLangID
         CASE "BE866"  ; hb_langNew( cLangID, "BG866" , "BEUTF", "UTF8" ) ; EXIT
         CASE "BEWIN"  ; hb_langNew( cLangID, "BGWIN" , "BEUTF", "UTF8" ) ; EXIT
         CASE "BG866"  ; hb_langNew( cLangID, "BG866" , "BGUTF", "UTF8" ) ; EXIT
         CASE "BGISO"  ; hb_langNew( cLangID, "BGISO" , "BGUTF", "UTF8" ) ; EXIT
         CASE "BGMIK"  ; hb_langNew( cLangID, "BGMIK" , "BGUTF", "UTF8" ) ; EXIT
         CASE "BGWIN"  ; hb_langNew( cLangID, "BGWIN" , "BGUTF", "UTF8" ) ; EXIT
         CASE "CA"     ; hb_langNew( cLangID, "ES850" , "CAUTF", "UTF8" ) ; EXIT
         CASE "CS852"  ; hb_langNew( cLangID, "CS852" , "CSUTF", "UTF8" ) ; EXIT
         CASE "CSISO"  ; hb_langNew( cLangID, "CSISO" , "CSUTF", "UTF8" ) ; EXIT
         CASE "CSKAM"  ; hb_langNew( cLangID, "CSKAMC", "CSUTF", "UTF8" ) ; EXIT
         CASE "CSWIN"  ; hb_langNew( cLangID, "CSWIN" , "CSUTF", "UTF8" ) ; EXIT
         CASE "DE"     ; hb_langNew( cLangID, "DE850" , "DEUTF", "UTF8" ) ; EXIT
         CASE "DEWIN"  ; hb_langNew( cLangID, "DEWIN" , "DEUTF", "UTF8" ) ; EXIT
         CASE "EL"     ; hb_langNew( cLangID, "EL737" , "ELUTF", "UTF8" ) ; EXIT
         CASE "ELWIN"  ; hb_langNew( cLangID, "ELWIN" , "ELUTF", "UTF8" ) ; EXIT
         CASE "EO"     ; hb_langNew( cLangID, "ES850" , "EOUTF", "UTF8" ) ; EXIT
         CASE "ES"     ; hb_langNew( cLangID, "ES850" , "ESUTF", "UTF8" ) ; EXIT
         CASE "ESWIN"  ; hb_langNew( cLangID, "ESWIN" , "ESUTF", "UTF8" ) ; EXIT
         CASE "EU"     ; hb_langNew( cLangID, "ES850" , "EUUTF", "UTF8" ) ; EXIT
         CASE "FR"     ; hb_langNew( cLangID, "FR850" , "FRUTF", "UTF8" ) ; EXIT
         CASE "GL"     ; hb_langNew( cLangID, "ES850" , "GLUTF", "UTF8" ) ; EXIT
         CASE "HE862"  ; hb_langNew( cLangID, "HE862" , "HEUTF", "UTF8" ) ; EXIT
         CASE "HEWIN"  ; hb_langNew( cLangID, "HEWIN" , "HEUTF", "UTF8" ) ; EXIT
         CASE "HR646"  ; hb_langNew( cLangID, "HR646" , "HRUTF", "UTF8" ) ; EXIT
         CASE "HR852"  ; hb_langNew( cLangID, "HR852" , "HRUTF", "UTF8" ) ; EXIT
         CASE "HRISO"  ; hb_langNew( cLangID, "HRISO" , "HRUTF", "UTF8" ) ; EXIT
         CASE "HRWIN"  ; hb_langNew( cLangID, "HRWIN" , "HRUTF", "UTF8" ) ; EXIT
         CASE "HU852"  ; hb_langNew( cLangID, "HU852" , "HUUTF", "UTF8" ) ; EXIT
         CASE "HUISO"  ; hb_langNew( cLangID, "HUISO" , "HUUTF", "UTF8" ) ; EXIT
         CASE "HUWIN"  ; hb_langNew( cLangID, "HUWIN" , "HUUTF", "UTF8" ) ; EXIT
         CASE "ID"     ; hb_langNew( cLangID, "EN"    , "IDUTF", "UTF8" ) ; EXIT
         CASE "IS850"  ; hb_langNew( cLangID, "IS850" , "ISUTF", "UTF8" ) ; EXIT
         CASE "IT"     ; hb_langNew( cLangID, "IT850" , "ITUTF", "UTF8" ) ; EXIT
//       CASE "KO"     ; hb_langNew( cLangID, "?????" , "KOUTF", "UTF8" ) ; EXIT
         CASE "LTWIN"  ; hb_langNew( cLangID, "LTWIN" , "LTUTF", "UTF8" ) ; EXIT
         CASE "NL"     ; hb_langNew( cLangID, "EN"    , "NLUTF", "UTF8" ) ; EXIT
         CASE "PL852"  ; hb_langNew( cLangID, "PL852" , "PLUTF", "UTF8" ) ; EXIT
         CASE "PLISO"  ; hb_langNew( cLangID, "PLISO" , "PLUTF", "UTF8" ) ; EXIT
         CASE "PLMAZ"  ; hb_langNew( cLangID, "PLMAZ" , "PLUTF", "UTF8" ) ; EXIT
         CASE "PLWIN"  ; hb_langNew( cLangID, "PLWIN" , "PLUTF", "UTF8" ) ; EXIT
         CASE "PT"     ; hb_langNew( cLangID, "PT850" , "PTUTF", "UTF8" ) ; EXIT
         CASE "PTISO"  ; hb_langNew( cLangID, "PTISO" , "PTUTF", "UTF8" ) ; EXIT
         CASE "RO"     ; hb_langNew( cLangID, "RO852" , "ROUTF", "UTF8" ) ; EXIT
         CASE "RU866"  ; hb_langNew( cLangID, "RU866" , "RUUTF", "UTF8" ) ; EXIT
         CASE "RUKOI8" ; hb_langNew( cLangID, "RUKOI8", "RUUTF", "UTF8" ) ; EXIT
         CASE "RUWIN"  ; hb_langNew( cLangID, "RU1251", "RUUTF", "UTF8" ) ; EXIT
         CASE "SK852"  ; hb_langNew( cLangID, "SK852" , "SKUTF", "UTF8" ) ; EXIT
         CASE "SKISO"  ; hb_langNew( cLangID, "SKISO" , "SKUTF", "UTF8" ) ; EXIT
         CASE "SKKAM"  ; hb_langNew( cLangID, "SKKAMC", "SKUTF", "UTF8" ) ; EXIT
         CASE "SKWIN"  ; hb_langNew( cLangID, "SKWIN" , "SKUTF", "UTF8" ) ; EXIT
         CASE "SL646"  ; hb_langNew( cLangID, "SL646" , "SLUTF", "UTF8" ) ; EXIT
         CASE "SL852"  ; hb_langNew( cLangID, "SL852" , "SLUTF", "UTF8" ) ; EXIT
         CASE "SLISO"  ; hb_langNew( cLangID, "SLISO" , "SLUTF", "UTF8" ) ; EXIT
         CASE "SLWIN"  ; hb_langNew( cLangID, "SLWIN" , "SLUTF", "UTF8" ) ; EXIT
         CASE "SR852"  ; hb_langNew( cLangID, "SL852" , "SRLAT", "UTF8" ) ; EXIT
         CASE "SRISO"  ; hb_langNew( cLangID, "SLISO" , "SRLAT", "UTF8" ) ; EXIT
         CASE "SRWIN"  ; hb_langNew( cLangID, "SRWIN" , "SRUTF", "UTF8" ) ; EXIT
         CASE "SV"     ; hb_langNew( cLangID, "SV850" , "SVUTF", "UTF8" ) ; EXIT
         CASE "SVWIN"  ; hb_langNew( cLangID, "SVWIN" , "SVUTF", "UTF8" ) ; EXIT
         CASE "TR857"  ; hb_langNew( cLangID, "TR857" , "TRUTF", "UTF8" ) ; EXIT
         CASE "TRWIN"  ; hb_langNew( cLangID, "TRWIN" , "TRUTF", "UTF8" ) ; EXIT
         CASE "UA866"  ; hb_langNew( cLangID, "UA866" , "UAUTF", "UTF8" ) ; EXIT
         CASE "UADOS"  ; hb_langNew( cLangID, "UA1125", "UAUTF", "UTF8" ) ; EXIT
         CASE "UAKOI8" ; hb_langNew( cLangID, "UAKOI8", "UAUTF", "UTF8" ) ; EXIT
         CASE "UAWIN"  ; hb_langNew( cLangID, "UA1125", "UAUTF", "UTF8" ) ; EXIT
         CASE "ZHB5"   ; hb_langNew( cLangID, "BIG5"  , "ZHUTF", "UTF8" ) ; EXIT
//       CASE "ZHGB"   ; hb_langNew( cLangID, "?????" , "ZHUTF", "UTF8" ) ; EXIT
         ENDSWITCH
      ENDIF
   ENDIF

   RETURN __hb_langSelect( cLangID )

STATIC FUNCTION __CtryStdToBaseLangID( cCtryStd )
   LOCAL cCtryHb := ""

   IF HB_ISSTRING( cCtryStd )
      SWITCH Lower( cCtryStd )
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
      CASE "be"         ; cCtryHb := "BEUTF" ; EXIT
      CASE "bg-bg"
      CASE "bg"         ; cCtryHb := "BGUTF" ; EXIT
      CASE "ca-es"
      CASE "ca"         ; cCtryHb := "CAUTF" ; EXIT
      CASE "cy-gb"      ; EXIT
      CASE "cs-cz"
      CASE "cs"         ; cCtryHb := "CSUTF" ; EXIT
      CASE "da-dk"      ; EXIT
      CASE "da"         ; EXIT
      CASE "de-at"
      CASE "de-ch"
      CASE "de-de"
      CASE "de-li"
      CASE "de-lu"
      CASE "de"         ; cCtryHb := "DEUTF" ; EXIT
      CASE "div-mv"     ; EXIT
      CASE "div"        ; EXIT
      CASE "el-gr"
      CASE "el"         ; cCtryHb := "ELUTF" ; EXIT
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
      CASE "en"         ; cCtryHb := "EN" ; EXIT
      CASE "eo"         ; cCtryHb := "EOUTF" ; EXIT
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
      CASE "es"         ; cCtryHb := "ESUTF" ; EXIT
      CASE "et-ee"      ; EXIT
      CASE "et"         ; EXIT
      CASE "eu-es"
      CASE "eu"         ; cCtryHb := "EUUTF" ; EXIT
      CASE "fa-ir"      ; EXIT
      CASE "fa"         ; EXIT
      CASE "fi-fi"      ; EXIT
      CASE "fi"         ; EXIT
      CASE "fo-fo"      ; EXIT
      CASE "fo"         ; EXIT
      CASE "fr-be"
      CASE "fr-ca"
      CASE "fr-ch"
      CASE "fr-fr"
      CASE "fr-lu"
      CASE "fr-mc"
      CASE "fr"         ; cCtryHb := "FRUTF" ; EXIT
      CASE "gl-es"
      CASE "gl"         ; cCtryHb := "GLUTF" ; EXIT
      CASE "gu-in"      ; EXIT
      CASE "gu"         ; EXIT
      CASE "he-il"
      CASE "he"         ; cCtryHb := "HEUTF" ; EXIT
      CASE "hi-in"      ; EXIT
      CASE "hi"         ; EXIT
      CASE "hr-hr"
      CASE "hr"         ; cCtryHb := "HRUTF" ; EXIT
      CASE "hu-hu"
      CASE "hu"         ; cCtryHb := "HUUTF" ; EXIT
      CASE "hy-am"      ; EXIT
      CASE "hy"         ; EXIT
      CASE "id-id"
      CASE "id"         ; cCtryHb := "IDUTF" ; EXIT
      CASE "is-is"
      CASE "is"         ; cCtryHb := "ISUTF" ; EXIT
      CASE "it-ch"
      CASE "it-it"
      CASE "it"         ; cCtryHb := "ITUTF" ; EXIT
      CASE "ja-jp"      ; EXIT
      CASE "ja"         ; EXIT
      CASE "ka-ge"      ; EXIT
      CASE "ka"         ; EXIT
      CASE "kk-kz"      ; EXIT
      CASE "kk"         ; EXIT
      CASE "kn-in"      ; EXIT
      CASE "kn"         ; EXIT
      CASE "ko-kr"
      CASE "ko"         ; cCtryHb := "KOUTF" ; EXIT
      CASE "kok-in"     ; EXIT
      CASE "kok"        ; EXIT
      CASE "ky-kz"      ; EXIT
      CASE "ky"         ; EXIT
      CASE "lt-lt"
      CASE "lt"         ; cCtryHb := "LTUTF" ; EXIT
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
      CASE "nl-be"
      CASE "nl-nl"
      CASE "nl"         ; cCtryHb := "NLUTF" ; EXIT
      CASE "nn-no"      ; EXIT
      CASE "no"         ; EXIT
      CASE "pa-in"      ; EXIT
      CASE "pa"         ; EXIT
      CASE "pl-pl"
      CASE "pl"         ; cCtryHb := "PLUTF" ; EXIT
      CASE "pt-br"
      CASE "pt-pt"
      CASE "pt"         ; cCtryHb := "PTUTF" ; EXIT
      CASE "ro-ro"
      CASE "ro"         ; cCtryHb := "ROUTF" ; EXIT
      CASE "ru-ru"
      CASE "ru"         ; cCtryHb := "RUUTF" ; EXIT
      CASE "sa-in"      ; EXIT
      CASE "sa"         ; EXIT
      CASE "sk-sk"
      CASE "sk"         ; cCtryHb := "SKUTF" ; EXIT
      CASE "sl-si"
      CASE "sl"         ; cCtryHb := "SLUTF" ; EXIT
      CASE "sq-al"      ; EXIT
      CASE "sq"         ; EXIT
      CASE "sr-sp-cyrl" ; cCtryHb := "SRUTF" ; EXIT
      CASE "sr-sp-latn" ; cCtryHb := "SRLAT" ; EXIT
      CASE "sv-fi"
      CASE "sv-se"
      CASE "sv"         ; cCtryHb := "SVUTF" ; EXIT
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
      CASE "tr-tr"
      CASE "tr"         ; cCtryHb := "TRUTF" ; EXIT
      CASE "tt-ru"      ; EXIT
      CASE "tt"         ; EXIT
      CASE "uk-ua"
      CASE "uk"         ; cCtryHb := "UAUTF" ; EXIT
      CASE "ur-pk"      ; EXIT
      CASE "ur"         ; EXIT
      CASE "uz-uz-cyrl" ; EXIT
      CASE "uz-uz-latn" ; EXIT
      CASE "uz"         ; EXIT
      CASE "vi-vn"      ; EXIT
      CASE "vi"         ; EXIT
      CASE "zh-chs"
      CASE "zh-cht"
      CASE "zh-cn"
      CASE "zh-hk"
      CASE "zh-mo"
      CASE "zh-sg"
      CASE "zh-tw"      ; cCtryHb := "ZHUTF" ; EXIT
      ENDSWITCH
   ENDIF

   RETURN cCtryHb
