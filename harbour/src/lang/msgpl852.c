/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (PL CP-852)
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
/* Codepage: 852 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "PL852",                     /* ID */
      "Polish",                    /* Name (in English) */
      "Polski",                    /* Name (in native language) */
      "PL",                        /* RFC ID */
      "852",                       /* Codepage */
      "",                          /* Version */

      /* Month names */

       "Styczeä",
       "Luty",
       "Marzec",
       "Kwiecieä",
       "Maj",
       "Czerwiec",
       "Lipiec",
       "Sierpieä",
       "Wrzesieä",
       "Pa«dziernik",
       "Listopad",
       "Grudzieä",

      /* Day names */

       "Niedziela",
       "Poniedziaˆek",
       "Wtorek",
       "—roda",
       "Czwartek",
       "Pi¥tek",
       "Sobota",

      /* CA-Cl*pper compatible natmsg items */

      "Baza danych       #Rekord¢w    Uaktualniona Rozmiar",
      "Wi©cej przykˆad¢w?",
      "Strona",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Wst",    /* wstaw */
      "Zas",    /* zastap */
      "Nieprawidˆowa data",
      "Zakres:",
      " - ",
      "T/N",
      "Bˆ©dne wyra¾enie",

      /* Error description names */

       "Bˆ¥d bez opisu",
       "Nieprawidˆowy argument",
       "Bˆ¥d zakresu tablicy",
       "Za du¾y string",
       "Przepeˆnienie numeryczne",
       "Dzielenie przez zero",
       "Bˆ¥d numeryczny",
       "Nieprawidˆowa skˆadnia",
       "Operacja zbyt zˆo¾ona",
      "",
      "",
       "Za maˆo pami©ci",
       "Niezdefiniowana funkcja",
       "Metoda jest niedost©pna",
       "Zmienna nie istnieje",
       "Alias bazy nie istnieje",
       "Zmienna jest niedost©pna",
       "Nieprawidˆowy alias bazy",
       "Podany alias ju¾ istnieje",
      "",
       "Bˆ¥d podczas tworzenia zbioru",
       "Bˆ¥d podczas otwarcia zbioru",
       "Bˆ¥d podczas zamkni©cia zbioru",
       "Bˆ¥d podczas odczytu ze zbioru",
       "Bˆ¥d podczas zapisu do zbioru",
       "Bˆ¥d wydruku",
      "",
      "",
      "",
      "",
       "Nieprawidˆowa operacja",
       "Przekroczony limit",
       "Wykryto uszkodzenie danych",
       "Niezgodny typ danych",
       "Warto˜† poza zakresem",
       "Baza nie jest otwarta",
       "Baza nie ma indeksu",
       "Wymagany jest wyˆ¥czny dost©p do bazy",
       "Wymagana blokada dost©pu",
       "Zapis niedozwolony",
       "Brak blokady dost©pu podczas dodawania rekordu",
       "Nie udaˆo si© zablokowa† dost©pu",
      "",
      "",
      "",
       "Bˆ¥d w destruktorze obiektu",
       "Nieprawidˆowa liczba argument¢w",
       "pobranie elementu tablicy",
       "zmiana warto˜ci elementu tablicy",
       "wymagana jest tablica",
       "wymagany typ: logiczny",

      /* Internal error names */

      "Nienaprawialny bˆ¥d nr %d: ",
      "Nieudana pr¢ba naprawy bˆ©du",
      "Brak kodu obsˆugi ERRORBLOCK()",
      "Zbyt wiele zagnie¾d¾onych bˆ©d¢w",
      "Niezaˆadowany lub zˆy RDD",
      "Zˆy typ metody woˆanej z %s",
      "hb_xgrab nie mo¾e zarezerwowa† pami©ci",
      "hb_xrealloc wywoˆany ze wska«nikiem NULL",
      "hb_xrealloc wywoˆany ze zˆym wska«nikiem",
      "hb_xrealloc nie mo¾e powi©kszy† bloku pami©ci",
      "hb_xfree wywoˆany ze zˆym wska«nikiem",
      "hb_xfree wywoˆany ze wska«nikiem NULL",
      "Brak definicji procedury startowej: \'%s\'",
      "Brak procedury startowej",
      "Nieprawidˆowa warto˜† VM opcode",
      "W %s wymagany jest item typu \'Symbol\'",
      "W %s podano zˆy item dla SELF",
      "W %s oczekiwany jest item typu \'Codeblock\'",
      "Funkcja %s wymaga innego typu na stosie",
      "Stos poni¾ej dna",
      "Item nie mo¾e by† skopiowany w %s",
      "W %s podano zˆy item jako memvar",
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

#define HB_LANG_ID      PL852
#include "hbmsgreg.h"
