/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language related functions
 *
 * Copyright 2009 Viktor Szakats <harbour.01 syenar.hu>
 * www - http://www.harbour-project.org
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

#define HB_OS_WIN_USED

#include "hbapi.h"

HB_FUNC( HB_USERLANG )
{
   char * ietf;
   BOOL bFree;

   ietf = hb_getenv( "LC_ALL" );
   if( ! ietf )
   {
      ietf = hb_getenv( "LC_MESSAGES" );
      if( ! ietf )
         ietf = hb_getenv( "LANG" );
   }
   bFree = ( ietf != NULL );

#if defined(HB_OS_WIN)
   if( ! ietf )
   {
      switch( GetUserDefaultLangID() )
      {
      case 0x0036: ietf = "af"         ; break;
      case 0x0436: ietf = "af-ZA"      ; break;
      case 0x001C: ietf = "sq"         ; break;
      case 0x041C: ietf = "sq-AL"      ; break;
      case 0x0001: ietf = "ar"         ; break;
      case 0x1401: ietf = "ar-DZ"      ; break;
      case 0x3C01: ietf = "ar-BH"      ; break;
      case 0x0C01: ietf = "ar-EG"      ; break;
      case 0x0801: ietf = "ar-IQ"      ; break;
      case 0x2C01: ietf = "ar-JO"      ; break;
      case 0x3401: ietf = "ar-KW"      ; break;
      case 0x3001: ietf = "ar-LB"      ; break;
      case 0x1001: ietf = "ar-LY"      ; break;
      case 0x1801: ietf = "ar-MA"      ; break;
      case 0x2001: ietf = "ar-OM"      ; break;
      case 0x4001: ietf = "ar-QA"      ; break;
      case 0x0401: ietf = "ar-SA"      ; break;
      case 0x2801: ietf = "ar-SY"      ; break;
      case 0x1C01: ietf = "ar-TN"      ; break;
      case 0x3801: ietf = "ar-AE"      ; break;
      case 0x2401: ietf = "ar-YE"      ; break;
      case 0x002B: ietf = "hy"         ; break;
      case 0x042B: ietf = "hy-AM"      ; break;
      case 0x002C: ietf = "az"         ; break;
      case 0x082C: ietf = "az-AZ-Cyrl" ; break;
      case 0x042C: ietf = "az-AZ-Latn" ; break;
      case 0x002D: ietf = "eu"         ; break;
      case 0x042D: ietf = "eu-ES"      ; break;
      case 0x0023: ietf = "be"         ; break;
      case 0x0423: ietf = "be-BY"      ; break;
      case 0x0002: ietf = "bg"         ; break;
      case 0x0402: ietf = "bg-BG"      ; break;
      case 0x0003: ietf = "ca"         ; break;
      case 0x0403: ietf = "ca-ES"      ; break;
      case 0x0452: ietf = "cy-GB"      ; break;
      case 0x0C04: ietf = "zh-HK"      ; break;
      case 0x1404: ietf = "zh-MO"      ; break;
      case 0x0804: ietf = "zh-CN"      ; break;
      case 0x0004: ietf = "zh-CHS"     ; break;
      case 0x1004: ietf = "zh-SG"      ; break;
      case 0x0404: ietf = "zh-TW"      ; break;
      case 0x7C04: ietf = "zh-CHT"     ; break;
      case 0x001A: ietf = "hr"         ; break;
      case 0x041A: ietf = "hr-HR"      ; break;
      case 0x0005: ietf = "cs"         ; break;
      case 0x0405: ietf = "cs-CZ"      ; break;
      case 0x0006: ietf = "da"         ; break;
      case 0x0406: ietf = "da-DK"      ; break;
      case 0x0065: ietf = "div"        ; break;
      case 0x0465: ietf = "div-MV"     ; break;
      case 0x0013: ietf = "nl"         ; break;
      case 0x0813: ietf = "nl-BE"      ; break;
      case 0x0413: ietf = "nl-NL"      ; break;
      case 0x0009: ietf = "en"         ; break;
      case 0x0C09: ietf = "en-AU"      ; break;
      case 0x2809: ietf = "en-BZ"      ; break;
      case 0x1009: ietf = "en-CA"      ; break;
      case 0x2409: ietf = "en-CB"      ; break;
      case 0x1809: ietf = "en-IE"      ; break;
      case 0x2009: ietf = "en-JM"      ; break;
      case 0x1409: ietf = "en-NZ"      ; break;
      case 0x3409: ietf = "en-PH"      ; break;
      case 0x1C09: ietf = "en-ZA"      ; break;
      case 0x2C09: ietf = "en-TT"      ; break;
      case 0x0809: ietf = "en-GB"      ; break;
      case 0x0409: ietf = "en-US"      ; break;
      case 0x3009: ietf = "en-ZW"      ; break;
      case 0x0025: ietf = "et"         ; break;
      case 0x0425: ietf = "et-EE"      ; break;
      case 0x0038: ietf = "fo"         ; break;
      case 0x0438: ietf = "fo-FO"      ; break;
      case 0x0029: ietf = "fa"         ; break;
      case 0x0429: ietf = "fa-IR"      ; break;
      case 0x000B: ietf = "fi"         ; break;
      case 0x040B: ietf = "fi-FI"      ; break;
      case 0x000C: ietf = "fr"         ; break;
      case 0x080C: ietf = "fr-BE"      ; break;
      case 0x0C0C: ietf = "fr-CA"      ; break;
      case 0x040C: ietf = "fr-FR"      ; break;
      case 0x140C: ietf = "fr-LU"      ; break;
      case 0x180C: ietf = "fr-MC"      ; break;
      case 0x100C: ietf = "fr-CH"      ; break;
      case 0x0056: ietf = "gl"         ; break;
      case 0x0456: ietf = "gl-ES"      ; break;
      case 0x0037: ietf = "ka"         ; break;
      case 0x0437: ietf = "ka-GE"      ; break;
      case 0x0007: ietf = "de"         ; break;
      case 0x0C07: ietf = "de-AT"      ; break;
      case 0x0407: ietf = "de-DE"      ; break;
      case 0x1407: ietf = "de-LI"      ; break;
      case 0x1007: ietf = "de-LU"      ; break;
      case 0x0807: ietf = "de-CH"      ; break;
      case 0x0008: ietf = "el"         ; break;
      case 0x0408: ietf = "el-GR"      ; break;
      case 0x0047: ietf = "gu"         ; break;
      case 0x0447: ietf = "gu-IN"      ; break;
      case 0x000D: ietf = "he"         ; break;
      case 0x040D: ietf = "he-IL"      ; break;
      case 0x0039: ietf = "hi"         ; break;
      case 0x0439: ietf = "hi-IN"      ; break;
      case 0x000E: ietf = "hu"         ; break;
      case 0x040E: ietf = "hu-HU"      ; break;
      case 0x000F: ietf = "is"         ; break;
      case 0x040F: ietf = "is-IS"      ; break;
      case 0x0021: ietf = "id"         ; break;
      case 0x0421: ietf = "id-ID"      ; break;
      case 0x0010: ietf = "it"         ; break;
      case 0x0410: ietf = "it-IT"      ; break;
      case 0x0810: ietf = "it-CH"      ; break;
      case 0x0011: ietf = "ja"         ; break;
      case 0x0411: ietf = "ja-JP"      ; break;
      case 0x004B: ietf = "kn"         ; break;
      case 0x044B: ietf = "kn-IN"      ; break;
      case 0x003F: ietf = "kk"         ; break;
      case 0x043F: ietf = "kk-KZ"      ; break;
      case 0x0057: ietf = "kok"        ; break;
      case 0x0457: ietf = "kok-IN"     ; break;
      case 0x0012: ietf = "ko"         ; break;
      case 0x0412: ietf = "ko-KR"      ; break;
      case 0x0040: ietf = "ky"         ; break;
      case 0x0440: ietf = "ky-KZ"      ; break;
      case 0x0026: ietf = "lv"         ; break;
      case 0x0426: ietf = "lv-LV"      ; break;
      case 0x0027: ietf = "lt"         ; break;
      case 0x0427: ietf = "lt-LT"      ; break;
      case 0x002F: ietf = "mk"         ; break;
      case 0x042F: ietf = "mk-MK"      ; break;
      case 0x003E: ietf = "ms"         ; break;
      case 0x083E: ietf = "ms-BN"      ; break;
      case 0x043E: ietf = "ms-MY"      ; break;
      case 0x004E: ietf = "mr"         ; break;
      case 0x044E: ietf = "mr-IN"      ; break;
      case 0x0050: ietf = "mn"         ; break;
      case 0x0450: ietf = "mn-MN"      ; break;
      case 0x0014: ietf = "no"         ; break;
      case 0x0414: ietf = "nb-NO"      ; break;
      case 0x0814: ietf = "nn-NO"      ; break;
      case 0x0015: ietf = "pl"         ; break;
      case 0x0415: ietf = "pl-PL"      ; break;
      case 0x0016: ietf = "pt"         ; break;
      case 0x0416: ietf = "pt-BR"      ; break;
      case 0x0816: ietf = "pt-PT"      ; break;
      case 0x0046: ietf = "pa"         ; break;
      case 0x0446: ietf = "pa-IN"      ; break;
      case 0x0018: ietf = "ro"         ; break;
      case 0x0418: ietf = "ro-RO"      ; break;
      case 0x0019: ietf = "ru"         ; break;
      case 0x0419: ietf = "ru-RU"      ; break;
      case 0x004F: ietf = "sa"         ; break;
      case 0x044F: ietf = "sa-IN"      ; break;
      case 0x0C1A: ietf = "sr-SP-Cyrl" ; break;
      case 0x081A: ietf = "sr-SP-Latn" ; break;
      case 0x001B: ietf = "sk"         ; break;
      case 0x041B: ietf = "sk-SK"      ; break;
      case 0x0024: ietf = "sl"         ; break;
      case 0x0424: ietf = "sl-SI"      ; break;
      case 0x000A: ietf = "es"         ; break;
      case 0x2C0A: ietf = "es-AR"      ; break;
      case 0x400A: ietf = "es-BO"      ; break;
      case 0x340A: ietf = "es-CL"      ; break;
      case 0x240A: ietf = "es-CO"      ; break;
      case 0x140A: ietf = "es-CR"      ; break;
      case 0x1C0A: ietf = "es-DO"      ; break;
      case 0x300A: ietf = "es-EC"      ; break;
      case 0x440A: ietf = "es-SV"      ; break;
      case 0x100A: ietf = "es-GT"      ; break;
      case 0x480A: ietf = "es-HN"      ; break;
      case 0x080A: ietf = "es-MX"      ; break;
      case 0x4C0A: ietf = "es-NI"      ; break;
      case 0x180A: ietf = "es-PA"      ; break;
      case 0x3C0A: ietf = "es-PY"      ; break;
      case 0x280A: ietf = "es-PE"      ; break;
      case 0x500A: ietf = "es-PR"      ; break;
      case 0x0C0A: ietf = "es-ES"      ; break;
      case 0x380A: ietf = "es-UY"      ; break;
      case 0x200A: ietf = "es-VE"      ; break;
      case 0x0041: ietf = "sw"         ; break;
      case 0x0441: ietf = "sw-KE"      ; break;
      case 0x001D: ietf = "sv"         ; break;
      case 0x081D: ietf = "sv-FI"      ; break;
      case 0x041D: ietf = "sv-SE"      ; break;
      case 0x005A: ietf = "syr"        ; break;
      case 0x045A: ietf = "syr-SY"     ; break;
      case 0x0049: ietf = "ta"         ; break;
      case 0x0449: ietf = "ta-IN"      ; break;
      case 0x0044: ietf = "tt"         ; break;
      case 0x0444: ietf = "tt-RU"      ; break;
      case 0x004A: ietf = "te"         ; break;
      case 0x044A: ietf = "te-IN"      ; break;
      case 0x001E: ietf = "th"         ; break;
      case 0x041E: ietf = "th-TH"      ; break;
      case 0x001F: ietf = "tr"         ; break;
      case 0x041F: ietf = "tr-TR"      ; break;
      case 0x0022: ietf = "uk"         ; break;
      case 0x0422: ietf = "uk-UA"      ; break;
      case 0x0020: ietf = "ur"         ; break;
      case 0x0420: ietf = "ur-PK"      ; break;
      case 0x0043: ietf = "uz"         ; break;
      case 0x0843: ietf = "uz-UZ-Cyrl" ; break;
      case 0x0443: ietf = "uz-UZ-Latn" ; break;
      case 0x002A: ietf = "vi"         ; break;
      case 0x042A: ietf = "vi-VN"      ; break;
      }
   }
#endif

   if( bFree )
   {
      long tmp, len = strlen( ietf );

      for( tmp = 0; tmp < len; tmp++ )
      {
         if( ietf[ tmp ] == '.' )
            break;
         else if( ietf[ tmp ] == '_' )
            ietf[ tmp ] = '-';
      }

      hb_retclen_buffer( ietf, tmp );
   }
   else
      hb_retc_const( ietf );
}
