/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (PL ISO-8859-2)
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/* Language name: Polish */
/* ISO language code (2 chars): PL */
/* Codepage: ISO-8859-2 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "PLISO",                     /* ID */
      "Polish",                    /* Name (in English) */
      "Polski",                    /* Name (in native language) */
      "PL",                        /* RFC ID */
      "ISO-8859-2",                /* Codepage */
      "$Revision$ $Date$",         /* Version */

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
       "Pa¼dziernik",
       "Listopad",
       "Grudzieñ",

      /* Day names */

       "Niedziela",
       "Poniedzia³ek",
       "Wtorek",
       "¦roda",
       "Czwartek",
       "Pi±tek",
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

       "B³±d bez opisu",
       "Nieprawid³owy argument",
       "B³±d zakresu tablicy",
       "Za du¿y string",
       "Przepe³nienie numeryczne",
       "Dzielenie przez zero",
       "B³±d numeryczny",
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
       "B³±d podczas tworzenia zbioru",
       "B³±d podczas otwarcia zbioru",
       "B³±d podczas zamkniêcia zbioru",
       "B³±d podczas odczytu ze zbioru",
       "B³±d podczas zapisu do zbioru",
       "B³±d wydruku",
      "",
      "",
      "",
      "",
       "Nieprawid³owa operacja",
       "Przekroczony limit",
       "Uszkodzony indeks bazy",
       "Niezgodny typ danych",
       "Warto¶æ poza zakresem",
       "Baza jest nie otwarta",
       "Baza nie ma indeksu",
       "Wymagany jest wy³±czny dostêp do bazy",
       "Wymagana blokada dostêpu",
       "Zapis niedozwolony",
       "Brak blokady dostêpu podczas dodawania rekordu",
       "Nie uda³o siê zablokowaæ dostêpu",
      "",
      "",
      "",
      "",
       "Nieprawid³owa liczba argumentów",
       "pobranie elementu tablicy",
       "zmiana warto¶ci elementu tablicy",
       "wymagana jest tablica",
       "wymagany typ: logiczny",

      /* Internal error names */

      "Nienaprawialny b³±d nr %lu: ",
      "Nieudana próba naprawy b³êdu",
      "Brak kodu obs³ugi ERRORBLOCK()",
      "Zbyt wiele zagnie¿d¿onych b³êdów",
      "Nieza³adowany lub z³y RDD",
      "Z³y typ metody wo³anej z %s",
      "hb_xgrab nie mo¿e zarezerwowaæ pamiêci",
      "hb_xrealloc wywo³any ze wska¼nikiem NULL",
      "hb_xrealloc wywo³any ze z³ym wska¼nikiem",
      "hb_xrealloc nie mo¿e powiekszyæ bloku pamiêci",
      "hb_xfree wywo³any ze z³ym wska¼nikiem",
      "hb_xfree wywo³any ze wska¼nikiem NULL",
      "Brak definicja procedury startowej: \'%s\'",
      "Brak procedury startowej",
      "Nieprawid³owa warto¶æ VM opcode",
      "W %s wymagany jest item typu \'Symbol\'",
      "W %s podano z³y item dla SELF",
      "W %s oczekiwany jest item typu \'Codeblock\'",
      "Funkcja %s wymaga innego typu na stosie",
      "Stos poni¿ej dna",
      "Item nie mo¿e byæ skopiowany w %s",
      "W %s podano z³y item jako memvar",
      "Zapis poza przydzielonym obszarem",

      /* Texts */

      "YYYY.MM.DD",
      "T",
      "N"
   }
};

HB_LANG_ANNOUNCE( PLISO );

HB_CALL_ON_STARTUP_BEGIN( hb_lang_Init_PLISO )
   hb_langRegister( &s_lang );
HB_CALL_ON_STARTUP_END( hb_lang_Init_PLISO )
#if ! defined(__GNUC__) && ! defined(_MSC_VER)
   #pragma startup hb_lang_Init_PLISO
#endif

