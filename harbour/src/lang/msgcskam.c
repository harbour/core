/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (CSKAM)
 *
 * Copyright 2000 Viktor Szakats (harbour syenar.net) (English, from msg_tpl.c)
 * Copyright 2000 Davor Siklic <siki@msoft.cz>
 * Copyright 2006 Vojtech Obrdlik <vobrdlik@centrum.cz>
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

/* Language name: Czech */
/* ISO language code (2 chars): CS */
/* Codepage:  Kamenickych (CS2) */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "CSKAM",                     /* ID */
      "Czech",                     /* Name (in English) */
      "Äesky",                     /* Name (in native language) */
      "CS",                        /* RFC ID */
      "895(KAM)",                  /* Codepage */
      "",                          /* Version */

      /* Month names */

      "leden",
      "£nor",
      "b©ezen",
      "duben",
      "kvàten",
      "áerven",
      "áervenec",
      "srpen",
      "z†©°",
      "©°jen",
      "listopad",
      "prosinec",

      /* Day names */

      "nedàle",
      "pondàl°",
      "£terò",
      "st©eda",
      "átvrtek",
      "p†tek",
      "sobota",

      /* CA-Cl*pper compatible natmsg items */

      "Datab†ze          # Z†znamñ    Aktualizace Velikost",
      "Chcete v°ce p©°kladñ?",
      "Strana",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Ins",
      "   ",
      "ChybnÇ datum",
      "Rozsah: ",
      " - ",
      "A/N",
      "CHYBNù VùRAZ",

      /* Error description names */

      "Nezn†m† chyba",
      "Chyba argumentu",
      "Chyba mez°",
      "P©eteáen° ©etàzce",
      "P©eteáen° á°sla",
      "Dàlen° nulou",
      "Numerick† chyba",
      "Chyba syntaxe",
      "Operace p©°li® komplexn°",
      "",
      "",
      "Nedostatek pamàti",
      "Nedefinovan† funkce",
      "Nezn†m† metoda",
      "Promànn† neexistuje",
      "Alias neexistuje",
      "Nezn†m† promànn†",
      "NepovolenÇ znaky v aliasu",
      "Alias jië pouëit",
      "",
      "Chyba vytvo©en°",
      "Chyba otev©en°",
      "Chyba zav©en°",
      "Chyba áten°",
      "Chyba z†pisu",
      "Chyba tisku",
      "",
      "",
      "",
      "",
      "Operace nen° podporov†na",
      "P©ekroáen limit",
      "Index po®kozen",
      "Typ dat se neshoduje",
      "Chyba ®°©ky dat",
      "Pracovn° oblast nen° pouëita",
      "Nen° otev©en index",
      "Poëadov†no uzamknut°",
      "Z†mek p©i p©id†n° z†znamu selhal",
      "Z†mek selhal",
      "",
      "",
      "",
      "",
      "p©°stup k poli",
      "p©i©azen° pole",
      "zmàna dimenze pole",
      "nen° pole",
      "podm°nka",

      /* Internal error names */

      "Unrecoverable error %d: ",
      "Error recovery failure",
      "No ERRORBLOCK() for error",
      "Too many recursive error handler calls",
      "RDD invalid or failed to load",
      "Invalid method type from %s",
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

      "DD.MM.YYYY",
      "A",
      "N"
   }
};

#define HB_LANG_ID      CSKAM
#include "hbmsgreg.h"
