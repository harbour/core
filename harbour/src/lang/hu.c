/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (hu)
 *
 * Copyright 2012 Viktor Szakats (harbour syenar.net)
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

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "hu",                        /* ISO ID (2 chars) */
      "Hungarian",                 /* Name (in English) */
      "Magyar",                    /* Name (in native language) */
      "HU",                        /* RFC ID */
      "UTF8",                      /* Codepage */
      "",                          /* Version */

      /* Month names */

      "január",
      "február",
      "március",
      "április",
      "május",
      "június",
      "július",
      "augusztus",
      "szeptember",
      "október",
      "november",
      "december",

      /* Day names */

      "vasárnap",
      "hétfő",
      "kedd",
      "szerda",
      "csütörtök",
      "péntek",
      "szombat",

      /* CA-Cl*pper compatible natmsg items */

      "Adatbázisok       Tételszám    Utolsó mód.     Méret",
      "Kéri a további részeket?",
      "lapszám",
      "** Összesen **",
      "* Részösszesen *",
      "*** Mindösszesen ***",
      "Ins",
      "   ",
      "Rossz dátum",
      " Határok ",
      " - ",
      "I/N",
      "INVALID EXPRESSION",

      /* Error description names */

      "Ismeretlen hiba",
      "Paraméter hiba",
      "Tömbindex hiba",
      "Karakteres változó túlcsordulás",
      "Numerikus túlcsordulás",
      "Nullával való osztás",
      "Numerikus hiba",
      "Szintaktikus hiba",
      "Túl összetett művelet",
      "",
      "",
      "Kevés memória",
      "Nem definiált függvény",
      "Nem exportált metódus",
      "Nem létező változó",
      "Nem létező munkaterület név",
      "Nem exportált változó",
      "Helytelen munkaterület név",
      "Már használt munkaterület név",
      "",
      "Létrehozási hiba",
      "Megnyitási hiba",
      "Lezárási hiba",
      "Olvasási hiba",
      "Írás hiba",
      "Nyomtatási hiba",
      "",
      "",
      "",
      "",
      "Nem támogatott művelet",
      "Korlát túllépve",
      "Index hiba felfedezve",
      "Nem megfelelő adattípus",
      "Túl széles adat",
      "Nem megnyitott munkaterület",
      "Nem indexelt munkaterület",
      "Kizárólagos megnyitási mód szükséges",
      "Zárolás szükséges",
      "Írás nem megengedett",
      "Zárolás nem sikerült új rekord felvitelekor",
      "Zárolás nem sikerült",
      "",
      "",
      "",
      "",
      "tömbelem hozzáférés",
      "tömbelem értékadás",
      "tömbelem dimenzió",
      "nem tömb",
      "feltételes",

      /* Internal error names */

      "Kezelhetetlen hiba %d: ",
      "Hiba kezelési hiba",
      "Nincs ERRORBLOCK() a hiba kezeléséhez",
      "Túl sok rekurzív hiba kezelő hívás",
      "Az RDD hibás vagy nem sikerült betölteni",
      "Nem megfelelő típusú metódus (%s)",
      "hb_xgrab can't allocate memory",
      "hb_xrealloc called with a NULL pointer",
      "hb_xrealloc called with an invalid pointer",
      "hb_xrealloc can't reallocate memory",
      "hb_xfree called with an invalid pointer",
      "hb_xfree called with a NULL pointer",
      "Can\'t locate the starting procedure: \'%s\'",
      "No starting procedure",
      "Unsupported VM opcode",
      "Symbol item expected from %s",
      "Invalid symbol type for self from %s",
      "Codeblock expected from %s",
      "Incorrect item type on the stack trying to pop from %s",
      "Stack underflow",
      "An item was going to be copied to itself from %s",
      "Invalid symbol item passed as memvar %s",
      "Memory buffer overflow",
      "hb_xgrab requested to allocate zero bytes",
      "hb_xrealloc requested to resize to zero bytes",
      "hb_xalloc requested to allocate zero bytes",

      /* Texts */

      "YYYY.MM.DD",
      "I",
      "N"
   }
};

#define HB_LANG_ID      HU
#include "hbmsgreg.h"
