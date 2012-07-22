/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (TRWIN)
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
/* Codepage: Windows-1254 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "TRWIN",                     /* ID */
      "Turkish",                   /* Name (in English) */
      "Türkçe",                    /* Name (in native language) */
      "TR",                        /* RFC ID */
      "Windows-1254",              /* Codepage */
      "",                          /* Version */

      /* Month names */

      "Ocak",
      "Þubat",
      "Mart",
      "Nisan",
      "Mayýs",
      "Haziran",
      "Temmuz",
      "Aðustos",
      "Eylül",
      "Ekim",
      "Kasým",
      "Aralýk",

      /* Day names */

      "Pazar",
      "Pazartesi",
      "Salý",
      "Çarþamba",
      "Perþembe",
      "Cuma",
      "Cumartesi",

      /* CA-Cl*pper compatible natmsg items */

      "Database Dosyasý  # Kayýt      Son Güncelleme  Boyut",
      "Daha örnek ister misiniz?",
      "Sayfa No.",
      "** Alttoplam **",
      "* Altalttoplam *",
      "*** Toplam ***",
      "Ins",
      "   ",
      "Geçersiz Tarih",
      "Sýnýr: ",
      " - ",
      "E/H",
      "GEÇERSÝZ ÝFADE",

      /* Error description names */

      "Bilinmeyen hata",
      "Argüman hatasý",
      "Sýnýr hatasý",
      "Katar taþma",
      "Sayýsal taþma",
      "Sýfýr Bölücü",
      "Sayýsal hata",
      "Ýmlâ hatasý",
      "Ýþlem çok karmaþýk",
      "",
      "",
      "Hafýza yetersiz",
      "Tanýmlanmamýþ fonksiyon",
      "Eksport metodu yok",
      "Deðiþken yok",
      "Alyas yok",
      "Eksport deðiþkeni yok",
      "Alyasta geçersiz karakter",
      "Alyas zaten kullanýmda",
      "",
      "Oluþturma hatasý",
      "Açma hatasý",
      "Kapatma hatasý",
      "Okuma hatasý",
      "Yazma hatasý",
      "Print hatasý",
      "",
      "",
      "",
      "",
      "Desteklenmeyen iþlem",
      "Sýnýr aþýldý",
      "Bozukluk var",
      "Data tip hatasý",
      "Data boyut hatasý",
      "Çalýþma alaný kullanýmda deðil",
      "Çalýþma alaný indeksli deðil",
      "Exclusive gerekiyor",
      "Kilit gerekiyor",
      "Yazma izni yok",
      "Append kilidi kurulamadý",
      "Kilit kurulamadý",
      "",
      "",
      "",
      "",
      "array eriþim",
      "array atama",
      "array boyut",
      "array deðil",
      "þart",

      /* Internal error names */

      "Kurtarýlamaz hata%d: ",
      "Hata kurtarma baþarýsýz",
      "Hata için ERRORBLOCK() yok",
      "Çok fazla içiçe hata tutucu çaðrýsý",
      "RDD geçersiz veya yüklemenedi",
      "%s için geçersiz metot tipi",
      "hb_xgrab hafýza atayamadý",
      "hb_xrealloc NULL pointer tarafýndan çaðrýldý",
      "hb_xrealloc geçersiz bir pointer tarafýndan çaðrýldý",
      "hb_xrealloc tekrar hafýza atayamadý",
      "hb_xfree geçersiz bir pointer tarafýndan çaðrýldý",
      "hb_xfree NULL pointer tarafýndan çaðrýldý",
      "Baþlama prosedürü bulunamdý : \'%s\'",
      "Baþlama prosedürü yok",
      "Desteklenmeyen VP iþlem kodu",
      "%s için sembol maddesi lâzým",
      "%s için geçersiz sembol tipi ",
      "%s için kodblok lazým",
      "%s için stack pop edilirken yanlýþ madde tipi ",
      "Stack taþmasý",
      "%s içinde bir madde kendi üstüne kopyalanmak istendi",
      "%s hafýza deðiþkeni olarak geçersiz sembol maddesi aktarýldý",
      "Hafýza buffer taþmasý",
      "hb_xgrab sýfýr bayt atamasý istendi",
      "hb_xrealloc sýfýr bayta boyutlandýrmak istendi",
      "hb_xalloc sýfýr bayt atamasý istendi",

      /* Texts */

      "DD.MM.YYYY",
      "E",
      "H"
   }
};

#define HB_LANG_ID      TRWIN
#include "hbmsgreg.h"
