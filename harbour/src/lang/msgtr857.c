/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (TR857)
 *
 * Copyright 2006 { Translation by : Bicahi Esgici <esgici@yahoo.com> }
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

/* Language name: Turkish */
/* ISO language code (2 chars): TR */
/* Codepage: CP-857 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "TR857",                     /* ID */
      "Turkish",                   /* Name (in English) */
      "TÅrkáe",                    /* Name (in native language) */
      "TR",                        /* RFC ID */
      "CP-857",                    /* Codepage */
      "",                          /* Version */

      /* Month names */

      "Ocak",
      "ûubat",
      "Mart",
      "Nisan",
      "Mayçs",
      "Haziran",
      "Temmuz",
      "Aßustos",
      "EylÅl",
      "Ekim",
      "Kasçm",
      "Aralçk",

      /* Day names */

      "Pazar",
      "Pazartesi",
      "Salç",
      "Äarüamba",
      "Perüembe",
      "Cuma",
      "Cumartesi",

      /* CA-Cl*pper compatible natmsg items */

      "Database Dosyasç  # Kayçt      Son G_ncelleme  Boyut",
      "Daha îrnek ister misiniz?",
      "Sayfa No.",
      "** Alttoplam **",
      "* Altalttoplam *",
      "*** Toplam ***",
      "Ins",
      "   ",
      "Geáersiz Tarih",
      "Sçnçr: ",
      " - ",
      "E/H",
      "GEÄERSòZ òFADE",

      /* Error description names */

      "Bilinmeyen hata",
      "ArgÅman hatasç",
      "Sçnçr hatasç",
      "Katar taüma",
      "Sayçsal taüma",
      "Sçfçr BîlÅcÅ",
      "Sayçsal hata",
      "òmlÉ hatasç",
      "òülem áok karmaüçk",
      "",
      "",
      "Hafçza yetersiz",
      "Tançmlanmamçü fonksiyon",
      "Eksport metodu yok",
      "Deßiüken yok",
      "Alyas yok",
      "Eksport deßiükeni yok",
      "Alyasta geáersiz karakter",
      "Alyas zaten kullançmda",
      "",
      "Oluüturma hatasç",
      "Aáma hatasç",
      "Kapatma hatasç",
      "Okuma hatasç",
      "Yazma hatasç",
      "Print hatasç",
      "",
      "",
      "",
      "",
      "Desteklenmeyen iülem",
      "Sçnçr aüçldç",
      "Bozukluk var",
      "Data tip hatasç",
      "Data boyut hatasç",
      "Äalçüma alanç kullançmda deßil",
      "Äalçüma alanç indeksli deßil",
      "Exclusive gerekiyor",
      "Kilit gerekiyor",
      "Yazma izni yok",
      "Append kilidi kurulamadç",
      "Kilit kurulamadç",
      "",
      "",
      "",
      "",
      "array eriüim",
      "array atama",
      "array boyut",
      "array deßil",
      "üart",

      /* Internal error names */

      "Kurtarçlamaz hata%d: ",
      "Hata kurtarma baüarçsçz",
      "Hata iáin ERRORBLOCK() yok",
      "Äok fazla iáiáe hata tutucu áaßrçsç",
      "RDD geáersiz veya yÅklemenedi",
      "%s iáin geáersiz metot tipi",
      "hb_xgrab hafçza atayamadç",
      "hb_xrealloc NULL pointer tarafçndan áaßrçldç",
      "hb_xrealloc geáersiz bir pointer tarafçndan áaßrçldç",
      "hb_xrealloc tekrar hafçza atayamadç",
      "hb_xfree geáersiz bir pointer tarafçndan áaßrçldç",
      "hb_xfree NULL pointer tarafçndan áaßrçldç",
      "Baülama prosedÅrÅ bulunamdç : \'%s\'",
      "Baülama prosedÅrÅ yok",
      "Desteklenmeyen VP iülem kodu",
      "%s iáin sembol maddesi lÉzçm",
      "%s iáin geáersiz sembol tipi ",
      "%s iáin kodblok lazçm",
      "%s iáin stack pop edilirken yanlçü madde tipi ",
      "Stack taümasç",
      "%s iáinde bir madde kendi ÅstÅne kopyalanmak istendi",
      "%s hafçza deßiükeni olarak geáersiz sembol maddesi aktarçldç",
      "Hafçza buffer taümasç",
      "hb_xgrab sçfçr bayt atamasç istendi",
      "hb_xrealloc sçfçr bayta boyutlandçrmak istendi",
      "hb_xalloc sçfçr bayt atamasç istendi",

      /* Texts */

      "DD.MM.YYYY",
      "E",
      "H"
   }
};

#define HB_LANG_ID      TR857
#include "hbmsgreg.h"
