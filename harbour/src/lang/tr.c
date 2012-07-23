/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (TRUTF)
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
/* Codepage: UTF-8 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "TRUTF",                     /* ID */
      "Turkish",                   /* Name (in English) */
      "Türkçe",                    /* Name (in native language) */
      "TR",                        /* RFC ID */
      "UTF-8",                     /* Codepage */
      "",                          /* Version */

      /* Month names */

      "Ocak",
      "Şubat",
      "Mart",
      "Nisan",
      "Mayıs",
      "Haziran",
      "Temmuz",
      "Ağustos",
      "Eylül",
      "Ekim",
      "Kasım",
      "Aralık",

      /* Day names */

      "Pazar",
      "Pazartesi",
      "Salı",
      "Çarşamba",
      "Perşembe",
      "Cuma",
      "Cumartesi",

      /* CA-Cl*pper compatible natmsg items */

      "Database Dosyası  # Kayıt      Son Güncelleme  Boyut",
      "Daha örnek ister misiniz?",
      "Sayfa No.",
      "** Alttoplam **",
      "* Altalttoplam *",
      "*** Toplam ***",
      "Ins",
      "   ",
      "Geçersiz Tarih",
      "Sınır: ",
      " - ",
      "E/H",
      "GEÇERSİZ İFADE",

      /* Error description names */

      "Bilinmeyen hata",
      "Argüman hatası",
      "Sınır hatası",
      "Katar taşma",
      "Sayısal taşma",
      "Sıfır Bölücü",
      "Sayısal hata",
      "İmlâ hatası",
      "İşlem çok karmaşık",
      "",
      "",
      "Hafıza yetersiz",
      "Tanımlanmamış fonksiyon",
      "Eksport metodu yok",
      "Değişken yok",
      "Alyas yok",
      "Eksport değişkeni yok",
      "Alyasta geçersiz karakter",
      "Alyas zaten kullanımda",
      "",
      "Oluşturma hatası",
      "Açma hatası",
      "Kapatma hatası",
      "Okuma hatası",
      "Yazma hatası",
      "Print hatası",
      "",
      "",
      "",
      "",
      "Desteklenmeyen işlem",
      "Sınır aşıldı",
      "Bozukluk var",
      "Data tip hatası",
      "Data boyut hatası",
      "Çalışma alanı kullanımda değil",
      "Çalışma alanı indeksli değil",
      "Exclusive gerekiyor",
      "Kilit gerekiyor",
      "Yazma izni yok",
      "Append kilidi kurulamadı",
      "Kilit kurulamadı",
      "",
      "",
      "",
      "",
      "array erişim",
      "array atama",
      "array boyut",
      "array değil",
      "şart",

      /* Internal error names */

      "Kurtarılamaz hata%d: ",
      "Hata kurtarma başarısız",
      "Hata için ERRORBLOCK() yok",
      "Çok fazla içiçe hata tutucu çağrısı",
      "RDD geçersiz veya yüklemenedi",
      "%s için geçersiz metot tipi",
      "hb_xgrab hafıza atayamadı",
      "hb_xrealloc NULL pointer tarafından çağrıldı",
      "hb_xrealloc geçersiz bir pointer tarafından çağrıldı",
      "hb_xrealloc tekrar hafıza atayamadı",
      "hb_xfree geçersiz bir pointer tarafından çağrıldı",
      "hb_xfree NULL pointer tarafından çağrıldı",
      "Başlama prosedürü bulunamdı : \'%s\'",
      "Başlama prosedürü yok",
      "Desteklenmeyen VP işlem kodu",
      "%s için sembol maddesi lâzım",
      "%s için geçersiz sembol tipi ",
      "%s için kodblok lazım",
      "%s için stack pop edilirken yanlış madde tipi ",
      "Stack taşması",
      "%s içinde bir madde kendi üstüne kopyalanmak istendi",
      "%s hafıza değişkeni olarak geçersiz sembol maddesi aktarıldı",
      "Hafıza buffer taşması",
      "hb_xgrab sıfır bayt ataması istendi",
      "hb_xrealloc sıfır bayta boyutlandırmak istendi",
      "hb_xalloc sıfır bayt ataması istendi",

      /* Texts */

      "DD.MM.YYYY",
      "E",
      "H"
   }
};

#define HB_LANG_ID      TRUTF
#include "hbmsgreg.h"
