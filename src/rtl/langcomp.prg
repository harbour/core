/*
 * Harbour Project source code:
 * The Language API module selection
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

#ifdef HB_LEGACY_LEVEL4
   /* Required for legacy language modules with a two character ID.
      These cannot have a compatibility puller symbol in langlgcy.prg,
      which in turn pulls all CP modules, so we're pulling them from
      here. */
REQUEST HB_CODEPAGE_CS852
REQUEST HB_CODEPAGE_DE850
REQUEST HB_CODEPAGE_EL737
#endif

FUNCTION hb_langSelect( cLangID, cCP )

   LOCAL tmp
   LOCAL cCPDef
   LOCAL cLangIDBase

   IF HB_ISSTRING( cLangID )

      cCPDef := hb_cdpSelect()

#ifdef HB_LEGACY_LEVEL4

      /* Emulate legacy Harbour language modules for compatibility */
      SWITCH Upper( cLangID )
      CASE "BE866"  ; cCPDef := "BG866" ; cLangIDBase := "be" ; EXIT
      CASE "BEWIN"  ; cCPDef := "BGWIN" ; cLangIDBase := "be" ; EXIT
      CASE "BG866"  ; cCPDef := "BG866" ; cLangIDBase := "bg" ; EXIT
      CASE "BGISO"  ; cCPDef := "BGISO" ; cLangIDBase := "bg" ; EXIT
      CASE "BGMIK"  ; cCPDef := "BGMIK" ; cLangIDBase := "bg" ; EXIT
      CASE "BGWIN"  ; cCPDef := "BGWIN" ; cLangIDBase := "bg" ; EXIT
      CASE "CS852"  ; cCPDef := "CS852" ; cLangIDBase := "cs" ; EXIT
      CASE "CSISO"  ; cCPDef := "CSISO" ; cLangIDBase := "cs" ; EXIT
      CASE "CSKAM"  ; cCPDef := "CSKAMC"; cLangIDBase := "cs" ; EXIT
      CASE "CSWIN"  ; cCPDef := "CSWIN" ; cLangIDBase := "cs" ; EXIT
      CASE "DEWIN"  ; cCPDef := "DEWIN" ; cLangIDBase := "de" ; EXIT
      CASE "ELWIN"  ; cCPDef := "ELWIN" ; cLangIDBase := "el" ; EXIT
      CASE "ESWIN"  ; cCPDef := "ESWIN" ; cLangIDBase := "es" ; EXIT
      CASE "HE862"  ; cCPDef := "HE862" ; cLangIDBase := "he" ; EXIT
      CASE "HEWIN"  ; cCPDef := "HEWIN" ; cLangIDBase := "he" ; EXIT
      CASE "HR646"  ; cCPDef := "HR646" ; cLangIDBase := "hr" ; EXIT
      CASE "HR852"  ; cCPDef := "HR852" ; cLangIDBase := "hr" ; EXIT
      CASE "HRISO"  ; cCPDef := "HRISO" ; cLangIDBase := "hr" ; EXIT
      CASE "HRWIN"  ; cCPDef := "HRWIN" ; cLangIDBase := "hr" ; EXIT
      CASE "HU852"  ; cCPDef := "HU852" ; cLangIDBase := "hu" ; EXIT
      CASE "HUISO"  ; cCPDef := "HUISO" ; cLangIDBase := "hu" ; EXIT
      CASE "HUWIN"  ; cCPDef := "HUWIN" ; cLangIDBase := "hu" ; EXIT
      CASE "IS850"  ; cCPDef := "IS850" ; cLangIDBase := "is" ; EXIT
      CASE "LTWIN"  ; cCPDef := "LTWIN" ; cLangIDBase := "lt" ; EXIT
      CASE "PL852"  ; cCPDef := "PL852" ; cLangIDBase := "pl" ; EXIT
      CASE "PLISO"  ; cCPDef := "PLISO" ; cLangIDBase := "pl" ; EXIT
      CASE "PLMAZ"  ; cCPDef := "PLMAZ" ; cLangIDBase := "pl" ; EXIT
      CASE "PLWIN"  ; cCPDef := "PLWIN" ; cLangIDBase := "pl" ; EXIT
      CASE "PTISO"  ; cCPDef := "PTISO" ; cLangIDBase := "pt" ; EXIT
      CASE "RU866"  ; cCPDef := "RU866" ; cLangIDBase := "ru" ; EXIT
      CASE "RUKOI8" ; cCPDef := "RUKOI8"; cLangIDBase := "ru" ; EXIT
      CASE "RUWIN"  ; cCPDef := "RU1251"; cLangIDBase := "ru" ; EXIT
      CASE "SK852"  ; cCPDef := "SK852" ; cLangIDBase := "sk" ; EXIT
      CASE "SKISO"  ; cCPDef := "SKISO" ; cLangIDBase := "sk" ; EXIT
      CASE "SKKAM"  ; cCPDef := "SKKAMC"; cLangIDBase := "sk" ; EXIT
      CASE "SKWIN"  ; cCPDef := "SKWIN" ; cLangIDBase := "sk" ; EXIT
      CASE "SL646"  ; cCPDef := "SL646" ; cLangIDBase := "sl" ; EXIT
      CASE "SL852"  ; cCPDef := "SL852" ; cLangIDBase := "sl" ; EXIT
      CASE "SLISO"  ; cCPDef := "SLISO" ; cLangIDBase := "sl" ; EXIT
      CASE "SLWIN"  ; cCPDef := "SLWIN" ; cLangIDBase := "sl" ; EXIT
      CASE "SR852"  ; cCPDef := "SL852" ; cLangIDBase := "sr_lat" ; EXIT
      CASE "SRISO"  ; cCPDef := "SLISO" ; cLangIDBase := "sr_lat" ; EXIT
      CASE "SRWIN"  ; cCPDef := "SRWIN" ; cLangIDBase := "sr_cyr" ; EXIT
      CASE "SVWIN"  ; cCPDef := "SVWIN" ; cLangIDBase := "sv" ; EXIT
      CASE "TR857"  ; cCPDef := "TR857" ; cLangIDBase := "tr" ; EXIT
      CASE "TRWIN"  ; cCPDef := "TRWIN" ; cLangIDBase := "tr" ; EXIT
      CASE "UA866"  ; cCPDef := "UA866" ; cLangIDBase := "uk" ; EXIT
      CASE "UADOS"  ; cCPDef := "UA1125"; cLangIDBase := "uk" ; EXIT
      CASE "UAKOI8" ; cCPDef := "UAKOI8"; cLangIDBase := "uk" ; EXIT
      CASE "UAWIN"  ; cCPDef := "UA1251"; cLangIDBase := "uk" ; EXIT
      CASE "ZHB5"   ; cCPDef := "BIG5"  ; cLangIDBase := "zh" ; EXIT
      CASE "ZHGB"   ; cCPDef := "GBK"   ; cLangIDBase := "zh_sim" ; EXIT
      OTHERWISE
         /* Case sensitive legacy IDs. Lowercase flavours denote new
            language module IDs, so they won't be recognized as
            compatibility ones. INCOMPATIBLE. */
         SWITCH cLangID
         CASE "CA"     ; cCPDef := "DE850" ; cLangIDBase := "ca" ; EXIT
         CASE "DE"     ; cCPDef := "DE850" ; cLangIDBase := "de" ; EXIT
         CASE "EL"     ; cCPDef := "EL737" ; cLangIDBase := "el" ; EXIT
         CASE "EO"     ; cCPDef := "DE850" ; cLangIDBase := "eo" ; EXIT
         CASE "ES"     ; cCPDef := "DE850" ; cLangIDBase := "es" ; EXIT
         CASE "EU"     ; cCPDef := "DE850" ; cLangIDBase := "eu" ; EXIT
         CASE "FR"     ; cCPDef := "DE850" ; cLangIDBase := "fr" ; EXIT
         CASE "GL"     ; cCPDef := "DE850" ; cLangIDBase := "gl" ; EXIT
         CASE "ID"     ; cCPDef := "EN"    ; cLangIDBase := "id" ; EXIT
         CASE "IT"     ; cCPDef := "DE850" ; cLangIDBase := "it" ; EXIT
         /* INCOMPATIBILITY: "KO" (Korean) using CP949 is not supported anymore. */
         CASE "NL"     ; cCPDef := "EN"    ; cLangIDBase := "nl" ; EXIT
         CASE "PT"     ; cCPDef := "DE850" ; cLangIDBase := "pt" ; EXIT
         CASE "RO"     ; cCPDef := "CS852" ; cLangIDBase := "ro" ; EXIT
         CASE "SV"     ; cCPDef := "DE850" ; cLangIDBase := "sv" ; EXIT
         ENDSWITCH
      ENDSWITCH

#endif

      IF ! HB_ISSTRING( cCP )
         cCP := cCPDef
      ENDIF

#ifdef HB_LEGACY_LEVEL4
      IF ! Empty( cLangIDBase )
         /* Legacy emulation */
         cLangID := cLangIDBase
      ELSE
#endif
         /* Support standard ISO language IDs */
         IF ! Empty( tmp := __LangStdToLangHb( cLangID ) )
            cLangID := cLangIDBase := tmp
         ELSE
            /* Normal case */
            cLangIDBase := cLangID
         ENDIF
#ifdef HB_LEGACY_LEVEL4
      ENDIF
#endif

      IF ! hb_cdpIsUTF8( cCP )
         cLangID += "." + cCP
         hb_langNew( cLangID, cCP, cLangIDBase, "UTF8" )
      ENDIF
   ENDIF

   RETURN __hb_langSelect( cLangID )

STATIC FUNCTION __LangStdToLangHb( cLangStd )

   LOCAL cLangHb := ""

   IF HB_ISSTRING( cLangStd )
      SWITCH Lower( StrTran( cLangStd, "_", "-" ) )
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
#endif
      CASE "be"         ; cLangHb := "be" ; EXIT
      CASE "bg-bg"
      CASE "bg"         ; cLangHb := "bg" ; EXIT
      CASE "ca-es"
      CASE "ca"         ; cLangHb := "ca" ; EXIT
#if 0
      CASE "cy-gb"      ; EXIT
#endif
      CASE "cs-cz"
      CASE "cs"         ; cLangHb := "cs" ; EXIT
#if 0
      CASE "da-dk"      ; EXIT
      CASE "da"         ; EXIT
#endif
      CASE "de-at"
      CASE "de-ch"
      CASE "de-de"
      CASE "de-li"
      CASE "de-lu"
      CASE "de"         ; cLangHb := "de" ; EXIT
#if 0
      CASE "div-mv"     ; EXIT
      CASE "div"        ; EXIT
#endif
      CASE "el-gr"
      CASE "el"         ; cLangHb := "el" ; EXIT
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
      CASE "en"         ; cLangHb := "en" ; EXIT
      CASE "eo"         ; cLangHb := "eo" ; EXIT
      CASE "es-419"     ; cLangHb := "es_419" ; EXIT
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
      CASE "es"         ; cLangHb := "es" ; EXIT
#if 0
      CASE "et-ee"      ; EXIT
      CASE "et"         ; EXIT
#endif
      CASE "eu-es"
      CASE "eu"         ; cLangHb := "eu" ; EXIT
#if 0
      CASE "fa-ir"      ; EXIT
      CASE "fa"         ; EXIT
      CASE "fi-fi"      ; EXIT
      CASE "fi"         ; EXIT
      CASE "fo-fo"      ; EXIT
      CASE "fo"         ; EXIT
#endif
      CASE "fr-be"
      CASE "fr-ca"
      CASE "fr-ch"
      CASE "fr-fr"
      CASE "fr-lu"
      CASE "fr-mc"
      CASE "fr"         ; cLangHb := "fr" ; EXIT
      CASE "gl-es"
      CASE "gl"         ; cLangHb := "gl" ; EXIT
#if 0
      CASE "gu-in"      ; EXIT
      CASE "gu"         ; EXIT
#endif
      CASE "he-il"
      CASE "he"         ; cLangHb := "he" ; EXIT
#if 0
      CASE "hi-in"      ; EXIT
      CASE "hi"         ; EXIT
#endif
      CASE "hr-hr"
      CASE "hr"         ; cLangHb := "hr" ; EXIT
      CASE "hu-hu"
      CASE "hu"         ; cLangHb := "hu" ; EXIT
#if 0
      CASE "hy-am"      ; EXIT
      CASE "hy"         ; EXIT
#endif
      CASE "id-id"
      CASE "id"         ; cLangHb := "id" ; EXIT
      CASE "is-is"
      CASE "is"         ; cLangHb := "is" ; EXIT
      CASE "it-ch"
      CASE "it-it"
      CASE "it"         ; cLangHb := "it" ; EXIT
#if 0
      CASE "ja-jp"      ; EXIT
      CASE "ja"         ; EXIT
      CASE "ka-ge"      ; EXIT
      CASE "ka"         ; EXIT
      CASE "kk-kz"      ; EXIT
      CASE "kk"         ; EXIT
      CASE "kn-in"      ; EXIT
      CASE "kn"         ; EXIT
#endif
      CASE "ko-kr"
      CASE "ko"         ; cLangHb := "ko" ; EXIT
#if 0
      CASE "kok-in"     ; EXIT
      CASE "kok"        ; EXIT
      CASE "ky-kz"      ; EXIT
      CASE "ky"         ; EXIT
#endif
      CASE "lt-lt"
      CASE "lt"         ; cLangHb := "lt" ; EXIT
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
      CASE "nl"         ; cLangHb := "nl" ; EXIT
#if 0
      CASE "nn-no"      ; EXIT
      CASE "no"         ; EXIT
      CASE "pa-in"      ; EXIT
      CASE "pa"         ; EXIT
#endif
      CASE "pl-pl"
      CASE "pl"         ; cLangHb := "pl" ; EXIT
      CASE "pt-br"      ; cLangHb := "pt_br" ; EXIT
      CASE "pt-pt"
      CASE "pt"         ; cLangHb := "pt" ; EXIT
      CASE "ro-ro"
      CASE "ro"         ; cLangHb := "ro" ; EXIT
      CASE "ru-ru"
      CASE "ru"         ; cLangHb := "ru" ; EXIT
#if 0
      CASE "sa-in"      ; EXIT
      CASE "sa"         ; EXIT
#endif
      CASE "sk-sk"
      CASE "sk"         ; cLangHb := "sk" ; EXIT
      CASE "sl-si"
      CASE "sl"         ; cLangHb := "sl" ; EXIT
#if 0
      CASE "sq-al"      ; EXIT
      CASE "sq"         ; EXIT
#endif
      CASE "sr-sp-cyrl" ; cLangHb := "sr_cyr" ; EXIT
      CASE "sr-sp-latn" ; cLangHb := "sr_lat" ; EXIT
      CASE "sv-fi"
      CASE "sv-se"
      CASE "sv"         ; cLangHb := "sv" ; EXIT
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
      CASE "tr"         ; cLangHb := "tr" ; EXIT
#if 0
      CASE "tt-ru"      ; EXIT
      CASE "tt"         ; EXIT
#endif
      CASE "uk-ua"
      CASE "uk"         ; cLangHb := "uk" ; EXIT
#if 0
      CASE "ur-pk"      ; EXIT
      CASE "ur"         ; EXIT
      CASE "uz-uz-cyrl" ; EXIT
      CASE "uz-uz-latn" ; EXIT
      CASE "uz"         ; EXIT
      CASE "vi-vn"      ; EXIT
      CASE "vi"         ; EXIT
#endif
      CASE "zh-chs"     ; cLangHb := "zh_sim" ; EXIT
      CASE "zh-cht"
      CASE "zh-cn"
      CASE "zh-hk"
      CASE "zh-mo"
      CASE "zh-sg"
      CASE "zh-tw"
      CASE "zh"         ; cLangHb := "zh" ; EXIT
      ENDSWITCH
   ENDIF

   RETURN cLangHb
