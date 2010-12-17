/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (PL Windows CP1250)
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

/* Language name: Polish */
/* ISO language code (2 chars): PL */
/* Codepage: 1250 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "PLWIN",                     /* ID */
      "Polish",                    /* Name (in English) */
      "Polski",                    /* Name (in native language) */
      "PL",                        /* RFC ID */
      "1250",                      /* Codepage */
      "",                          /* Version */

      /* Month names */

       "Styczeñ",
       "Luty",
       "Marzec",
       "Kwiecieñ",
       "Maj",
       "Czerwiec",
       "Lipiec",
       "Sierpieñ",
       "Wrzesieñ",
       "PaŸdziernik",
       "Listopad",
       "Grudzieñ",

      /* Day names */

       "Niedziela",
       "Poniedzia³ek",
       "Wtorek",
       "Œroda",
       "Czwartek",
       "Pi¹tek",
       "Sobota",

      /* CA-Cl*pper compatible natmsg items */

      "Baza danych       #Rekordów    Uaktualniona Rozmiar",
      "Wiêcej przyk³adów?",
      "Strona",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Wst",    /* wstaw */
      "Zas",    /* zastap */
      "Nieprawid³owa data",
      "Zakres:",
      " - ",
      "T/N",
      "B³êdne wyra¿enie",

      /* Error description names */

       "B³¹d bez opisu",
       "Nieprawid³owy argument",
       "B³¹d zakresu tablicy",
       "Za du¿y string",
       "Przepe³nienie numeryczne",
       "Dzielenie przez zero",
       "B³¹d numeryczny",
       "Nieprawid³owa sk³adnia",
       "Operacja zbyt z³o¿ona",
      "",
      "",
       "Za ma³o pamiêci",
       "Niezdefiniowana funkcja",
       "Metoda jest niedostêpna",
       "Zmienna nie istnieje",
       "Alias bazy nie istnieje",
       "Zmienna jest niedostêpna",
       "Nieprawid³owy alias bazy",
       "Podany alias ju¿ istnieje",
      "",
       "B³¹d podczas tworzenia zbioru",
       "B³¹d podczas otwarcia zbioru",
       "B³¹d podczas zamkniêcia zbioru",
       "B³¹d podczas odczytu ze zbioru",
       "B³¹d podczas zapisu do zbioru",
       "B³¹d wydruku",
      "",
      "",
      "",
      "",
       "Nieprawid³owa operacja",
       "Przekroczony limit",
       "Wykryto uszkodzenie danych",
       "Niezgodny typ danych",
       "Wartoœæ poza zakresem",
       "Baza jest nie otwarta",
       "Baza nie ma indeksu",
       "Wymagany jest wy³¹czny dostêp do bazy",
       "Wymagana blokada dostêpu",
       "Zapis niedozwolony",
       "Brak blokady dostêpu podczas dodawania rekordu",
       "Nie uda³o siê zablokowaæ dostêpu",
      "",
      "",
      "",
       "B³¹d w destruktorze obiektu",
       "Nieprawid³owa liczba argumentów",
       "pobranie elementu tablicy",
       "zmiana wartoœci elementu tablicy",
       "wymagana jest tablica",
       "wymagany typ: logiczny",

      /* Internal error names */

      "Nienaprawialny b³¹d nr %d: ",
      "Nieudana próba naprawy b³êdu",
      "Brak kodu obs³ugi ERRORBLOCK()",
      "Zbyt wiele zagnie¿d¿onych b³êdów",
      "Nieza³adowany lub z³y RDD",
      "Z³y typ metody wo³anej z %s",
      "hb_xgrab nie mo¿e zarezerwowaæ pamiêci",
      "hb_xrealloc wywo³any ze wskaŸnikiem NULL",
      "hb_xrealloc wywo³any ze z³ym wskaŸnikiem",
      "hb_xrealloc nie mo¿e powiêkszyæ bloku pamiêci",
      "hb_xfree wywo³any ze z³ym wskaŸnikiem",
      "hb_xfree wywo³any ze wskaŸnikiem NULL",
      "Brak definicji procedury startowej: \'%s\'",
      "Brak procedury startowej",
      "Nieprawid³owa wartoœæ VM opcode",
      "W %s wymagany jest item typu \'Symbol\'",
      "W %s podano z³y item dla SELF",
      "W %s oczekiwany jest item typu \'Codeblock\'",
      "Funkcja %s wymaga innego typu na stosie",
      "Stos poni¿ej dna",
      "Item nie mo¿e byæ skopiowany w %s",
      "W %s podano z³y item jako memvar",
      "Zapis poza przydzielonym obszarem",
      "hb_xgrab requested to allocate zero bytes",
      "hb_xrealloc requested to resize to zero bytes",
      "hb_xalloc requested to allocate zero bytes",

      /* Texts */

      "YYYY.MM.DD",
      "T",
      "N"
   }
};

#define HB_LANG_ID      PLWIN
#include "hbmsgreg.h"
