/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (SK Kamenicky ("895"))
 *
 * Copyright 2008,2012 Gyula Bartal <gybartal@gmail.com> (from msgskwin.c)
 * Update December 25, 2011 Jaroslav Janik <Jaroslav.Janik@siemens.com>
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

/* Language name: Slovak */
/* ISO language code (2 chars): SK */
/* Codepage: Kamenicky ("895") */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "SKKAM",                     /* ID */
      "Slovak",                    /* Name (in English) */
      "Slovensky",                 /* Name (in native language) */
      "SK",                        /* RFC ID */
      "Kamenicky",                 /* Codepage */
      "",                          /* Version */

      /* Month names */

      "janu†r",
      "febru†r",
      "marec",
      "apr°l",
      "m†j",
      "j£n",
      "j£l",
      "august",
      "september",
      "okt¢ber",
      "november",
      "december",

      /* Day names */

      "nedeåa",
      "pondelok",
      "utorok",
      "streda",
      "®tvrtok",
      "piatok",
      "sobota",

      /* CA-Cl*pper compatible natmsg items */

      "D†tab†ze          #  Vety      Aktualiz†cia Veåkosü",
      "Chcete viac pr°kladov?",
      "Strana",
      "** Medzis£áet **",
      "* Medzimedzis£áet *",
      "*** S£áet ***",
      "Ins",
      "   ",
      "Chybnò d†tum",
      "Rozsah: ",
      " - ",
      "A/N",
      "CHYBNù VùRAZ",

      /* Error description names */

      "Nezn†m† chyba",
      "Chyba argumentu",
      "Chyba medz°",
      "Preplnenie reüazca",
      "Preplnenie á°sla",
      "Delenie nulou",
      "Numerick† chyba",
      "Syntaktick† chyba",
      "Oper†cia pr°li® komplexn†",
      "",
      "",
      "Nedostatok pamÑte",
      "Nedefinovan† funkcia",
      "Nezn†ma met¢da",
      "Premenn† neexistuje",
      "Oblasü neexistuje",
      "Nezn†ma premenn†",
      "NepovolenÇ znaky v oblasti",
      "Oblasü je uë pouëit†",
      "",
      "Chyba vytvorenia",
      "Chyba otvorenia",
      "Chyba zatvorenia",
      "Chyba á°tania",
      "Chyba z†pisu",
      "Chyba tlaáe",
      "",
      "",
      "",
      "",
      "Nepodporovan† oper†cia",
      "Prekroáenò limit",
      "Index po®kodenò",
      "Chyba typu d†t",
      "Chyba dçëky d†t",
      "Nepouëit† pracovn† oblasü ",
      "Nezoraden† pracovn† oblasü",
      "Nutnò vòhradnò pr°stup",
      "Uzamknutie nutnÇ",
      "Zlyhanie uzamkutia pri prid†van°",
      "Zlyhanie uzamknutia",
      "",
      "",
      "",
      "",
      "pr°stup k poåu",
      "priradenie k poåu",
      "zmena dimenze poåa",
      "nie je to pole",
      "podmienka",

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

#define HB_LANG_ID      SKKAM
#include "hbmsgreg.h"
