/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (HU852)
 *
 * Copyright 1999-2005 Viktor Szakats (harbour syenar.net)
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

/* Language name: Hungarian */
/* ISO language code (2 chars): HU */
/* Codepage: CP-852 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "HU852",                     /* ID */
      "Hungarian",                 /* Name (in English) */
      "Magyar",                    /* Name (in native language) */
      "HU",                        /* RFC ID */
      "CP-852",                    /* Codepage */
      "",                          /* Version */

      /* Month names */

      "janu†r",
      "febru†r",
      "m†rcius",
      "†prilis",
      "m†jus",
      "j£nius",
      "j£lius",
      "augusztus",
      "szeptember",
      "okt¢ber",
      "november",
      "december",

      /* Day names */

      "vas†rnap",
      "hÇtfã",
      "kedd",
      "szerda",
      "csÅtîrtîk",
      "pÇntek",
      "szombat",

      /* CA-Cl*pper compatible natmsg items */

      "Adatb†zisok       TÇtelsz†m    Utols¢ m¢d.     MÇret",
      "KÇri a tov†bbi rÇszeket?",
      "lapsz†m",
      "** ôsszesen **",
      "* RÇszîsszesen *",
      "*** Mindîsszesen ***",
      "Ins",
      "   ",
      "Rossz d†tum",
      " Hat†rok ",
      " - ",
      "I/N",
      "INVALID EXPRESSION",

      /* Error description names */

      "Ismeretlen hiba",
      "ParamÇter hiba",
      "Tîmbindex hiba",
      "Karakteres v†ltoz¢ t£lcsordul†s",
      "Numerikus t£lcsordul†s",
      "Null†val val¢ oszt†s",
      "Numerikus hiba",
      "Szintaktikus hiba",
      "T£l îsszetett m˚velet",
      "",
      "",
      "KevÇs mem¢ria",
      "Nem defini†lt fÅggvÇny",
      "Nem export†lt met¢dus",
      "Nem lÇtezã v†ltoz¢",
      "Nem lÇtezã munkaterÅlet nÇv",
      "Nem export†lt v†ltoz¢",
      "Helytelen munkaterÅlet nÇv",
      "M†r haszn†lt munkaterÅlet nÇv",
      "",
      "LÇtrehoz†si hiba",
      "Megnyit†si hiba",
      "Lez†r†si hiba",
      "Olvas†si hiba",
      "÷r†s hiba",
      "Nyomtat†si hiba",
      "",
      "",
      "",
      "",
      "Nem t†mogatott m˚velet",
      "Korl†t t£llÇpve",
      "Index hiba felfedezve",
      "Nem megfelelã adatt°pus",
      "T£l szÇles adat",
      "Nem megnyitott munkaterÅlet",
      "Nem indexelt munkaterÅlet",
      "Kiz†r¢lagos megnyit†si m¢d szÅksÇges",
      "Z†rol†s szÅksÇges",
      "÷r†s nem megengedett",
      "Z†rol†s nem sikerÅlt £j rekord felvitelekor",
      "Z†rol†s nem sikerÅlt",
      "",
      "",
      "",
      "",
      "tîmbelem hozz†fÇrÇs",
      "tîmbelem ÇrtÇkad†s",
      "tîmbelem dimenzi¢",
      "nem tîmb",
      "feltÇteles",

      /* Internal error names */

      "Kezelhetetlen hiba %d: ",
      "Hiba kezelÇsi hiba",
      "Nincs ERRORBLOCK() a hiba kezelÇsÇhez",
      "T£l sok rekurz°v hiba kezelã h°v†s",
      "Az RDD hib†s vagy nem sikerÅlt betîlteni",
      "Nem megfelelã t°pus£ met¢dus (%s)",
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

#define HB_LANG_ID      HU852
#include "hbmsgreg.h"
