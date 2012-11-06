/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language related functions
 *
 * Copyright 2009 Viktor Szakats (harbour syenar.net)
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

#include "hbapi.h"

#if defined( HB_OS_WIN )
   #include <windows.h>
#endif

HB_FUNC( HB_USERLANG )
{
   char * ietf;

   ietf = hb_getenv( "LC_ALL" );
   if( ietf == NULL )
   {
      ietf = hb_getenv( "LC_MESSAGES" );
      if( ietf == NULL )
         ietf = hb_getenv( "LANG" );
   }

   if( ietf != NULL )
   {
      HB_ISIZ tmp;

      for( tmp = 0; ietf[ tmp ] && ietf[ tmp ] != '.'; tmp++ )
      {
         if( ietf[ tmp ] == '_' )
            ietf[ tmp ] = '-';
      }

      hb_retclen_buffer( ietf, tmp );
   }
   else
   {
#if defined( HB_OS_WIN )
      const char * ietfc;

      switch( GetUserDefaultLangID() )
      {
         case 0x0036: ietfc = "af"         ; break;
         case 0x0436: ietfc = "af-ZA"      ; break;
         case 0x001C: ietfc = "sq"         ; break;
         case 0x041C: ietfc = "sq-AL"      ; break;
         case 0x0001: ietfc = "ar"         ; break;
         case 0x1401: ietfc = "ar-DZ"      ; break;
         case 0x3C01: ietfc = "ar-BH"      ; break;
         case 0x0C01: ietfc = "ar-EG"      ; break;
         case 0x0801: ietfc = "ar-IQ"      ; break;
         case 0x2C01: ietfc = "ar-JO"      ; break;
         case 0x3401: ietfc = "ar-KW"      ; break;
         case 0x3001: ietfc = "ar-LB"      ; break;
         case 0x1001: ietfc = "ar-LY"      ; break;
         case 0x1801: ietfc = "ar-MA"      ; break;
         case 0x2001: ietfc = "ar-OM"      ; break;
         case 0x4001: ietfc = "ar-QA"      ; break;
         case 0x0401: ietfc = "ar-SA"      ; break;
         case 0x2801: ietfc = "ar-SY"      ; break;
         case 0x1C01: ietfc = "ar-TN"      ; break;
         case 0x3801: ietfc = "ar-AE"      ; break;
         case 0x2401: ietfc = "ar-YE"      ; break;
         case 0x002B: ietfc = "hy"         ; break;
         case 0x042B: ietfc = "hy-AM"      ; break;
         case 0x002C: ietfc = "az"         ; break;
         case 0x082C: ietfc = "az-AZ-Cyrl" ; break;
         case 0x042C: ietfc = "az-AZ-Latn" ; break;
         case 0x002D: ietfc = "eu"         ; break;
         case 0x042D: ietfc = "eu-ES"      ; break;
         case 0x0023: ietfc = "be"         ; break;
         case 0x0423: ietfc = "be-BY"      ; break;
         case 0x0002: ietfc = "bg"         ; break;
         case 0x0402: ietfc = "bg-BG"      ; break;
         case 0x0003: ietfc = "ca"         ; break;
         case 0x0403: ietfc = "ca-ES"      ; break;
         case 0x0452: ietfc = "cy-GB"      ; break;
         case 0x0C04: ietfc = "zh-HK"      ; break;
         case 0x1404: ietfc = "zh-MO"      ; break;
         case 0x0804: ietfc = "zh-CN"      ; break;
         case 0x0004: ietfc = "zh-CHS"     ; break;
         case 0x1004: ietfc = "zh-SG"      ; break;
         case 0x0404: ietfc = "zh-TW"      ; break;
         case 0x7C04: ietfc = "zh-CHT"     ; break;
         case 0x001A: ietfc = "hr"         ; break;
         case 0x041A: ietfc = "hr-HR"      ; break;
         case 0x0005: ietfc = "cs"         ; break;
         case 0x0405: ietfc = "cs-CZ"      ; break;
         case 0x0006: ietfc = "da"         ; break;
         case 0x0406: ietfc = "da-DK"      ; break;
         case 0x0065: ietfc = "div"        ; break;
         case 0x0465: ietfc = "div-MV"     ; break;
         case 0x0013: ietfc = "nl"         ; break;
         case 0x0813: ietfc = "nl-BE"      ; break;
         case 0x0413: ietfc = "nl-NL"      ; break;
         case 0x0009: ietfc = "en"         ; break;
         case 0x0C09: ietfc = "en-AU"      ; break;
         case 0x2809: ietfc = "en-BZ"      ; break;
         case 0x1009: ietfc = "en-CA"      ; break;
         case 0x2409: ietfc = "en-CB"      ; break;
         case 0x1809: ietfc = "en-IE"      ; break;
         case 0x2009: ietfc = "en-JM"      ; break;
         case 0x1409: ietfc = "en-NZ"      ; break;
         case 0x3409: ietfc = "en-PH"      ; break;
         case 0x1C09: ietfc = "en-ZA"      ; break;
         case 0x2C09: ietfc = "en-TT"      ; break;
         case 0x0809: ietfc = "en-GB"      ; break;
         case 0x0409: ietfc = "en-US"      ; break;
         case 0x3009: ietfc = "en-ZW"      ; break;
         case 0x0025: ietfc = "et"         ; break;
         case 0x0425: ietfc = "et-EE"      ; break;
         case 0x0038: ietfc = "fo"         ; break;
         case 0x0438: ietfc = "fo-FO"      ; break;
         case 0x0029: ietfc = "fa"         ; break;
         case 0x0429: ietfc = "fa-IR"      ; break;
         case 0x000B: ietfc = "fi"         ; break;
         case 0x040B: ietfc = "fi-FI"      ; break;
         case 0x000C: ietfc = "fr"         ; break;
         case 0x080C: ietfc = "fr-BE"      ; break;
         case 0x0C0C: ietfc = "fr-CA"      ; break;
         case 0x040C: ietfc = "fr-FR"      ; break;
         case 0x140C: ietfc = "fr-LU"      ; break;
         case 0x180C: ietfc = "fr-MC"      ; break;
         case 0x100C: ietfc = "fr-CH"      ; break;
         case 0x0056: ietfc = "gl"         ; break;
         case 0x0456: ietfc = "gl-ES"      ; break;
         case 0x0037: ietfc = "ka"         ; break;
         case 0x0437: ietfc = "ka-GE"      ; break;
         case 0x0007: ietfc = "de"         ; break;
         case 0x0C07: ietfc = "de-AT"      ; break;
         case 0x0407: ietfc = "de-DE"      ; break;
         case 0x1407: ietfc = "de-LI"      ; break;
         case 0x1007: ietfc = "de-LU"      ; break;
         case 0x0807: ietfc = "de-CH"      ; break;
         case 0x0008: ietfc = "el"         ; break;
         case 0x0408: ietfc = "el-GR"      ; break;
         case 0x0047: ietfc = "gu"         ; break;
         case 0x0447: ietfc = "gu-IN"      ; break;
         case 0x000D: ietfc = "he"         ; break;
         case 0x040D: ietfc = "he-IL"      ; break;
         case 0x0039: ietfc = "hi"         ; break;
         case 0x0439: ietfc = "hi-IN"      ; break;
         case 0x000E: ietfc = "hu"         ; break;
         case 0x040E: ietfc = "hu-HU"      ; break;
         case 0x000F: ietfc = "is"         ; break;
         case 0x040F: ietfc = "is-IS"      ; break;
         case 0x0021: ietfc = "id"         ; break;
         case 0x0421: ietfc = "id-ID"      ; break;
         case 0x0010: ietfc = "it"         ; break;
         case 0x0410: ietfc = "it-IT"      ; break;
         case 0x0810: ietfc = "it-CH"      ; break;
         case 0x0011: ietfc = "ja"         ; break;
         case 0x0411: ietfc = "ja-JP"      ; break;
         case 0x004B: ietfc = "kn"         ; break;
         case 0x044B: ietfc = "kn-IN"      ; break;
         case 0x003F: ietfc = "kk"         ; break;
         case 0x043F: ietfc = "kk-KZ"      ; break;
         case 0x0057: ietfc = "kok"        ; break;
         case 0x0457: ietfc = "kok-IN"     ; break;
         case 0x0012: ietfc = "ko"         ; break;
         case 0x0412: ietfc = "ko-KR"      ; break;
         case 0x0040: ietfc = "ky"         ; break;
         case 0x0440: ietfc = "ky-KZ"      ; break;
         case 0x0026: ietfc = "lv"         ; break;
         case 0x0426: ietfc = "lv-LV"      ; break;
         case 0x0027: ietfc = "lt"         ; break;
         case 0x0427: ietfc = "lt-LT"      ; break;
         case 0x002F: ietfc = "mk"         ; break;
         case 0x042F: ietfc = "mk-MK"      ; break;
         case 0x003E: ietfc = "ms"         ; break;
         case 0x083E: ietfc = "ms-BN"      ; break;
         case 0x043E: ietfc = "ms-MY"      ; break;
         case 0x004E: ietfc = "mr"         ; break;
         case 0x044E: ietfc = "mr-IN"      ; break;
         case 0x0050: ietfc = "mn"         ; break;
         case 0x0450: ietfc = "mn-MN"      ; break;
         case 0x0014: ietfc = "no"         ; break;
         case 0x0414: ietfc = "nb-NO"      ; break;
         case 0x0814: ietfc = "nn-NO"      ; break;
         case 0x0015: ietfc = "pl"         ; break;
         case 0x0415: ietfc = "pl-PL"      ; break;
         case 0x0016: ietfc = "pt"         ; break;
         case 0x0416: ietfc = "pt-BR"      ; break;
         case 0x0816: ietfc = "pt-PT"      ; break;
         case 0x0046: ietfc = "pa"         ; break;
         case 0x0446: ietfc = "pa-IN"      ; break;
         case 0x0018: ietfc = "ro"         ; break;
         case 0x0418: ietfc = "ro-RO"      ; break;
         case 0x0019: ietfc = "ru"         ; break;
         case 0x0419: ietfc = "ru-RU"      ; break;
         case 0x004F: ietfc = "sa"         ; break;
         case 0x044F: ietfc = "sa-IN"      ; break;
         case 0x0C1A: ietfc = "sr-SP-Cyrl" ; break;
         case 0x081A: ietfc = "sr-SP-Latn" ; break;
         case 0x001B: ietfc = "sk"         ; break;
         case 0x041B: ietfc = "sk-SK"      ; break;
         case 0x0024: ietfc = "sl"         ; break;
         case 0x0424: ietfc = "sl-SI"      ; break;
         case 0x000A: ietfc = "es"         ; break;
         case 0x2C0A: ietfc = "es-AR"      ; break;
         case 0x400A: ietfc = "es-BO"      ; break;
         case 0x340A: ietfc = "es-CL"      ; break;
         case 0x240A: ietfc = "es-CO"      ; break;
         case 0x140A: ietfc = "es-CR"      ; break;
         case 0x1C0A: ietfc = "es-DO"      ; break;
         case 0x300A: ietfc = "es-EC"      ; break;
         case 0x440A: ietfc = "es-SV"      ; break;
         case 0x100A: ietfc = "es-GT"      ; break;
         case 0x480A: ietfc = "es-HN"      ; break;
         case 0x080A: ietfc = "es-MX"      ; break;
         case 0x4C0A: ietfc = "es-NI"      ; break;
         case 0x180A: ietfc = "es-PA"      ; break;
         case 0x3C0A: ietfc = "es-PY"      ; break;
         case 0x280A: ietfc = "es-PE"      ; break;
         case 0x500A: ietfc = "es-PR"      ; break;
         case 0x0C0A: ietfc = "es-ES"      ; break;
         case 0x380A: ietfc = "es-UY"      ; break;
         case 0x200A: ietfc = "es-VE"      ; break;
         case 0x0041: ietfc = "sw"         ; break;
         case 0x0441: ietfc = "sw-KE"      ; break;
         case 0x001D: ietfc = "sv"         ; break;
         case 0x081D: ietfc = "sv-FI"      ; break;
         case 0x041D: ietfc = "sv-SE"      ; break;
         case 0x005A: ietfc = "syr"        ; break;
         case 0x045A: ietfc = "syr-SY"     ; break;
         case 0x0049: ietfc = "ta"         ; break;
         case 0x0449: ietfc = "ta-IN"      ; break;
         case 0x0044: ietfc = "tt"         ; break;
         case 0x0444: ietfc = "tt-RU"      ; break;
         case 0x004A: ietfc = "te"         ; break;
         case 0x044A: ietfc = "te-IN"      ; break;
         case 0x001E: ietfc = "th"         ; break;
         case 0x041E: ietfc = "th-TH"      ; break;
         case 0x001F: ietfc = "tr"         ; break;
         case 0x041F: ietfc = "tr-TR"      ; break;
         case 0x0022: ietfc = "uk"         ; break;
         case 0x0422: ietfc = "uk-UA"      ; break;
         case 0x0020: ietfc = "ur"         ; break;
         case 0x0420: ietfc = "ur-PK"      ; break;
         case 0x0043: ietfc = "uz"         ; break;
         case 0x0843: ietfc = "uz-UZ-Cyrl" ; break;
         case 0x0443: ietfc = "uz-UZ-Latn" ; break;
         case 0x002A: ietfc = "vi"         ; break;
         case 0x042A: ietfc = "vi-VN"      ; break;
         default:     ietfc = NULL         ; break;
      }
      hb_retc_const( ietfc );
#else
      hb_retc_null();
#endif
   }
}
