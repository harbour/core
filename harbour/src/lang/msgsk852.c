/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (SK852) SK852
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
/* Codepage: 852 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "SK852",                     /* ID */
      "Slovak",                    /* Name (in English) */
      "Slovensky",                 /* Name (in native language) */
      "SK",                        /* RFC ID */
      "852",                       /* Codepage */
      "",                          /* Version */

      /* Month names */

      "janu r",
      "febru r",
      "marec",
      "apr¡l",
      "m j",
      "j£n",
      "j£l",
      "august",
      "september",
      "okt¢ber",
      "november",
      "december",

      /* Day names */

      "nede–a",
      "pondelok",
      "utorok",
      "streda",
      "çtvrtok",
      "piatok",
      "sobota",

      /* CA-Cl*pper compatible natmsg items */

      "D tab ze          #  Vety      Aktualiz cia Ve–kosœ",
      "Chcete viac pr¡kladov?",
      "Strana",
      "** Medzis£Ÿet **",
      "* Medzimedzis£Ÿet *",
      "*** S£Ÿet ***",
      "Ins",
      "   ",
      "Chybnì d tum",
      "Rozsah: ",
      " - ",
      "A/N",
      "CHYBNí VíRAZ",

      /* Error description names */

      "Nezn m  chyba",
      "Chyba argumentu",
      "Chyba medz¡",
      "Preplnenie reœazca",
      "Preplnenie Ÿ¡sla",
      "Delenie nulou",
      "Numerick  chyba",
      "Syntaktick  chyba",
      "Oper cia pr¡liç komplexn ",
      "",
      "",
      "Nedostatok pam„te",
      "Nedefinovan  funkcia",
      "Nezn ma met¢da",
      "Premenn  neexistuje",
      "Oblasœ neexistuje",
      "Nezn ma premenn ",
      "Nepovolen‚ znaky v oblasti",
      "Oblasœ je u§ pou§it ",
      "",
      "Chyba vytvorenia",
      "Chyba otvorenia",
      "Chyba zatvorenia",
      "Chyba Ÿ¡tania",
      "Chyba z pisu",
      "Chyba tlaŸe",
      "",
      "",
      "",
      "",
      "Nepodporovan  oper cia",
      "PrekroŸenì limit",
      "Index poçkodenì",
      "Chyba typu d t",
      "Chyba d’§ky d t",
      "Nepou§it  pracovn  oblasœ ",
      "Nezoraden  pracovn  oblasœ",
      "Nutnì vìhradnì pr¡stup",
      "Uzamknutie nutn‚",
      "Zlyhanie uzamkutia pri prid van¡",
      "Zlyhanie uzamknutia",
      "",
      "",
      "",
      "",
      "pr¡stup k po–u",
      "priradenie k po–u",
      "zmena dimenze po–a",
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

#define HB_LANG_ID      SK852
#include "hbmsgreg.h"
