/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (Template)
 *
 * Copyright 2006 { Tranlation by : Bicahi Esgici <esgici@yahoo.com> }
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

/* Language name: Turkish */
/* ISO language code : TR  */
/* Codepage: 857 ( OEM ) */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "TR",                        /* ID */
      "Turkish",                   /* Name (in English) */
      "Trk‡e",                    /* Name (in native language) */
      "TR",                        /* RFC ID */
      "857",                      /* Codepage */
      "$Revision$ $Date$",         /* Version */

      /* Month names */

      "Ocak",
      "žubat",
      "Mart",
      "Nisan",
      "Mays",
      "Haziran",
      "Temmuz",
      "A§ustos",
      "Eyll",
      "Ekim",
      "Kasm",
      "Aralk",

      /* Day names */

      "Pazar",
      "Pazartesi",
      "Sal",
      "€arŸamba",
      "PerŸembe",
      "Cuma",
      "Cumartesi",

      /* CA-Cl*pper compatible natmsg items */

      "Database Dosyas  # Kayt      Son Gncelleme  Boyut", 
      "Daha ”rnek ister misiniz?",
      "Sayfa No.",
      "** Alttoplam **",
      "* Altalttoplam *",
      "*** Toplam ***",
      "Ins",
      "   ",
      "Ge‡ersiz Tarih",
      "Snr: ",
      " - ",
      "E/H",
      "GE€ERS˜Z ˜FADE",

      /* Error description names */

      "Bilinmeyen hata",
      "Argman hatas",
      "Snr hatas",
      "Katar taŸma",
      "Saysal taŸma",
      "Sfr B”lc",
      "Saysal hata",
      "˜mlƒ hatas",
      "˜Ÿlem ‡ok karmaŸk",
      "",
      "",
      "Hafza yetersiz",
      "TanmlanmamŸ fonksiyon",
      "Eksport metodu yok",
      "De§iŸken yok",
      "Alyas yok",
      "Eksport de§iŸkeni yok",
      "Alyasta ge‡ersiz karakter",
      "Alyas zaten kullanmda",
      "",
      "OluŸturma hatas",
      "A‡ma hatas",
      "Kapatma hatas",
      "Okuma hatas",
      "Yazma hatas",
      "Print hatas",
      "",
      "",
      "",
      "",
      "Desteklenmeyen iŸlem",
      "Snr aŸld",
      "Bozukluk var",
      "Data tip hatas",
      "Data boyut hatas",
      "€alŸma alan kullanmda de§il",
      "€alŸma alan indeksli de§il",
      "Exclusive gerekiyor",
      "Kilit gerekiyor",
      "Yazma izni yok",
      "Append kilidi kurulamad",
      "Kilit kurulamad",
      "",
      "",
      "",
      "",
      "array eriŸim",
      "array atama",
      "array boyut",
      "array de§il",
      "Ÿart",

      /* Internal error names */

      "Kurtarlamaz hata%lu: ",
      "Hata kurtarma baŸarsz",
      "Hata i‡in ERRORBLOCK() yok",
      "€ok fazla i‡i‡e hata tutucu ‡a§rs",
      "RDD ge‡ersiz veya yklemenedi",
      "%s i‡in ge‡ersiz metot tipi",
      "hb_xgrab hafza atayamad",
      "hb_xrealloc NULL pointer tarafndan ‡a§rld",
      "hb_xrealloc ge‡ersiz bir pointer tarafndan ‡a§rld",
      "hb_xrealloc tekrar hafza atayamad",
      "hb_xfree ge‡ersiz bir pointer tarafndan ‡a§rld",
      "hb_xfree NULL pointer tarafndan ‡a§rld",
      "BaŸlama prosedr bulunamd : \'%s\'",
      "BaŸlama prosedr yok",
      "Desteklenmeyen VP iŸlem kodu",
      "%s i‡in sembol maddesi lƒzm",
      "%s i‡in ge‡ersiz sembol tipi ",
      "%s i‡in kodblok lazm",
      "%s i‡in stack pop edilirken yanlŸ madde tipi ",
      "Stack taŸmas",
      "%s i‡inde bir madde kendi stne kopyalanmak istendi",
      "%s hafza de§iŸkeni olarak ge‡ersiz sembol maddesi aktarld",
      "Hafza buffer taŸmas",
      "hb_xgrab sfr bayt atamas istendi",
      "hb_xrealloc sfr bayta boyutlandrmak istendi",
      "hb_xalloc sfr bayt atamas istendi",

      /* Texts */

      "DD.MM.YYYY",
      "E",
      "H"
   }
};

HB_LANG_ANNOUNCE( TR857 );

HB_CALL_ON_STARTUP_BEGIN( hb_lang_Init_TR857 )
   hb_langRegister( &s_lang );
HB_CALL_ON_STARTUP_END( hb_lang_Init_TR857 )

#if defined(HB_PRAGMA_STARTUP)
   #pragma startup hb_lang_Init_TR857
#elif defined(HB_MSC_STARTUP)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto_hb_lang_Init_TR857 = hb_lang_Init_TR857;
   #pragma data_seg()
#endif
