/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (PL Mazowia)
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
/* Codepage: Mazowia */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "PLMAZ",                     /* ID */
      "Polish",                    /* Name (in English) */
      "Polski",                    /* Name (in native language) */
      "PL",                        /* RFC ID */
      "Mazowia",                   /* Codepage */
      "",                          /* Version */

      /* Month names */

       "Stycze§",
       "Luty",
       "Marzec",
       "Kwiecie§",
       "Maj",
       "Czerwiec",
       "Lipiec",
       "Sierpie§",
       "Wrzesie§",
       "Pa¶dziernik",
       "Listopad",
       "Grudzie§",

      /* Day names */

       "Niedziela",
       "Poniedziaíek",
       "Wtorek",
       "òroda",
       "Czwartek",
       "PiÜtek",
       "Sobota",

      /* CA-Cl*pper compatible natmsg items */

      "Baza danych       #Rekord¢w    Uaktualniona Rozmiar",
      "Wiëcej przykíad¢w?",
      "Strona",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Wst",    /* wstaw */
      "Zas",    /* zastap */
      "Nieprawidíowa data",
      "Zakres:",
      " - ",
      "T/N",
      "Bíëdne wyraßenie",

      /* Error description names */

       "BíÜd bez opisu",
       "Nieprawidíowy argument",
       "BíÜd zakresu tablicy",
       "Za dußy string",
       "Przepeínienie numeryczne",
       "Dzielenie przez zero",
       "BíÜd numeryczny",
       "Nieprawidíowa skíadnia",
       "Operacja zbyt zíoßona",
      "",
      "",
       "Za maío pamiëci",
       "Niezdefiniowana funkcja",
       "Metoda jest niedostëpna",
       "Zmienna nie istnieje",
       "Alias bazy nie istnieje",
       "Zmienna jest niedostëpna",
       "Nieprawidíowy alias bazy",
       "Podany alias juß istnieje",
      "",
       "BíÜd podczas tworzenia zbioru",
       "BíÜd podczas otwarcia zbioru",
       "BíÜd podczas zamkniëcia zbioru",
       "BíÜd podczas odczytu ze zbioru",
       "BíÜd podczas zapisu do zbioru",
       "BíÜd wydruku",
      "",
      "",
      "",
      "",
       "Nieprawidíowa operacja",
       "Przekroczony limit",
       "Wykryto uszkodzenie danych",
       "Niezgodny typ danych",
       "Wartoûç poza zakresem",
       "Baza jest nie otwarta",
       "Baza nie ma indeksu",
       "Wymagany jest wyíÜczny dostëp do bazy",
       "Wymagana blokada dostëpu",
       "Zapis niedozwolony",
       "Brak blokady dostëpu podczas dodawania rekordu",
       "Nie udaío sië zablokowaç dostëpu",
      "",
      "",
      "",
       "BíÜd w destruktorze obiektu",
       "Nieprawidíowa liczba argument¢w",
       "pobranie elementu tablicy",
       "zmiana wartoûci elementu tablicy",
       "wymagana jest tablica",
       "wymagany typ: logiczny",

      /* Internal error names */

      "Nienaprawialny bíÜd nr %d: ",
      "Nieudana pr¢ba naprawy bíëdu",
      "Brak kodu obsíugi ERRORBLOCK()",
      "Zbyt wiele zagnießdßonych bíëd¢w",
      "Niezaíadowany lub zíy RDD",
      "Zíy typ metody woíanej z %s",
      "hb_xgrab nie moße zarezerwowaç pamiëci",
      "hb_xrealloc wywoíany ze wska¶nikiem NULL",
      "hb_xrealloc wywoíany ze zíym wska¶nikiem",
      "hb_xrealloc nie moße powiëkszyç bloku pamiëci",
      "hb_xfree wywoíany ze zíym wska¶nikiem",
      "hb_xfree wywoíany ze wska¶nikiem NULL",
      "Brak definicji procedury startowej: \'%s\'",
      "Brak procedury startowej",
      "Nieprawidíowa wartoûç VM opcode",
      "W %s wymagany jest item typu \'Symbol\'",
      "W %s podano zíy item dla SELF",
      "W %s oczekiwany jest item typu \'Codeblock\'",
      "Funkcja %s wymaga innego typu na stosie",
      "Stos ponißej dna",
      "Item nie moße byç skopiowany w %s",
      "W %s podano zíy item jako memvar",
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

#define HB_LANG_ID      PLMAZ
#include "hbmsgreg.h"
