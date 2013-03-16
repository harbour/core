/*
 * Harbour Project source code:
 * Language Support Module (pl)
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "pl",                        /* ISO ID (2 chars) */
      "Polish",                    /* Name (in English) */
      "Polski",                    /* Name (in native language) */
      "PL",                        /* RFC ID */
      "UTF8",                      /* Codepage */
      "",                          /* Version */

      /* Month names */

      "Styczeń",
      "Luty",
      "Marzec",
      "Kwiecień",
      "Maj",
      "Czerwiec",
      "Lipiec",
      "Sierpień",
      "Wrzesień",
      "Październik",
      "Listopad",
      "Grudzień",

      /* Day names */

      "Niedziela",
      "Poniedziałek",
      "Wtorek",
      "Środa",
      "Czwartek",
      "Piątek",
      "Sobota",

      /* CA-Cl*pper compatible natmsg items */

      "Baza danych       # Rekordów   Uaktualniona    Rozmiar",
      "Więcej przykładów?",
      "Strona",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Wst",    /* wstaw */
      "Zas",    /* zastap */
      "Nieprawidłowa data",
      "Zakres:",
      " - ",
      "T/N",
      "Błędne wyrażenie",

      /* Error description names */

      "Błąd bez opisu",
      "Nieprawidłowy argument",
      "Błąd zakresu tablicy",
      "Za duży string",
      "Przepełnienie numeryczne",
      "Dzielenie przez zero",
      "Błąd numeryczny",
      "Nieprawidłowa składnia",
      "Operacja zbyt złożona",
      "",
      "",
      "Za mało pamięci",
      "Niezdefiniowana funkcja",
      "Metoda jest niedostępna",
      "Zmienna nie istnieje",
      "Alias bazy nie istnieje",
      "Zmienna jest niedostępna",
      "Nieprawidłowy alias bazy",
      "Podany alias już istnieje",
      "",
      "Błąd podczas tworzenia zbioru",
      "Błąd podczas otwarcia zbioru",
      "Błąd podczas zamknięcia zbioru",
      "Błąd podczas odczytu ze zbioru",
      "Błąd podczas zapisu do zbioru",
      "Błąd wydruku",
      "",
      "",
      "",
      "",
      "Nieprawidłowa operacja",
      "Przekroczony limit",
      "Wykryto uszkodzenie danych",
      "Niezgodny typ danych",
      "Wartość poza zakresem",
      "Baza jest nie otwarta",
      "Baza nie ma indeksu",
      "Wymagany jest wyłączny dostęp do bazy",
      "Wymagana blokada dostępu",
      "Zapis niedozwolony",
      "Brak blokady dostępu podczas dodawania rekordu",
      "Nie udało się zablokować dostępu",
      "",
      "",
      "",
      "Błąd w destruktorze obiektu",
      "Nieprawidłowa liczba argumentów",
      "pobranie elementu tablicy",
      "zmiana wartości elementu tablicy",
      "wymagana jest tablica",
      "wymagany typ: logiczny",

      /* Internal error names */

      "Nienaprawialny błąd nr %d: ",
      "Nieudana próba naprawy błędu",
      "Brak kodu obsługi ERRORBLOCK()",
      "Zbyt wiele zagnieżdżonych błędów",
      "Niezaładowany lub zły RDD",
      "Zły typ metody wołanej z %s",
      "hb_xgrab nie może zarezerwować pamięci",
      "hb_xrealloc wywołany ze wskaźnikiem NULL",
      "hb_xrealloc wywołany ze złym wskaźnikiem",
      "hb_xrealloc nie może powiększyć bloku pamięci",
      "hb_xfree wywołany ze złym wskaźnikiem",
      "hb_xfree wywołany ze wskaźnikiem NULL",
      "Brak definicji procedury startowej: \'%s\'",
      "Brak procedury startowej",
      "Nieprawidłowa wartość VM opcode",
      "W %s wymagany jest item typu \'Symbol\'",
      "W %s podano zły item dla SELF",
      "W %s oczekiwany jest item typu \'Codeblock\'",
      "Funkcja %s wymaga innego typu na stosie",
      "Stos poniżej dna",
      "Item nie może być skopiowany w %s",
      "W %s podano zły item jako memvar",
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

#define HB_LANG_ID      PL
#include "hbmsgreg.h"
