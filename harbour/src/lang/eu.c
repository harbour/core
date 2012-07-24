/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (EUUTF)
 *
 * Copyright 2000 Nicolas del Pozo <niko@geroa.com>
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

      "EUUTF",                     /* ISO ID (2 chars) */
      "Basque",                    /* Name (in English) */
      "English",                   /* Name (in native language) */
      "EU",                        /* RFC ID */
      "UTF8",                      /* Codepage */
      "",                          /* Version */

      /* Month names */

      "Urtarrila",
      "Otsaila",
      "Martxoa",
      "Apirila",
      "Maitza",
      "Ekaina",
      "Uztaila",
      "Abuztua",
      "Iraila",
      "Urria",
      "Azaroa",
      "Abendua",

      /* Day names */

      "Igandea",
      "Astelehena",
      "Asteartea",
      "Asteazkena",
      "Osteguna",
      "Ostirala",
      "Larunbata",

      /* CA-Cl*pper compatible natmsg items */

      "Database Files    # Records    Last Update     Size",
      "Do you want more samples?",
      "Page No.",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Ins",
      "   ",
      "Invalid date",
      "Range: ",
      " - ",
      "Y/N",
      "INVALID EXPRESSION",

      /* Error description names */

      "Errore ezezaguna",
      "Argumentu-errore",
      "Maila-errore",
      "Kate-gainezkatze",
      "Zenbaki-gainezkatze",
      "Zero-zatiketa",
      "Zenbaki-errore",
      "Sintaxi-errore",
      "Eragiketa konplexuegia",
      "",
      "",
      "Memoria gutxi",
      "Funtzio-definitugabea",
      "Modu-esportagabea",
      "Aldagai-ezezaguna",
      "Aldagai-",
      "Alias-okerra",
      "Illegal characters in alias",
      "Alias-bikoiztua",
      "",
      "Sortze-errore",
      "Irekitze-errore",
      "Itxiera-errore",
      "Irrakurketa-errore",
      "Idazketa-errore",
      "Imprimaketa-errore",
      "",
      "",
      "",
      "",
      "Onartugabeko eragiketa",
      "Muga gainditua",
      "Indize-hondaketa igarria",
      "Datu-mota okerra",
      "Datu-zabalera luzeegia",
      "Laneko area erabili gabea",
      "Laneko area ez indexatua",
      "Modu-esklusiboa behar da",
      "Blokeoa behar da",
      "Idazkera-debekatua",
      "Erregistro eransketan blokeo errorea",
      "Blokeo-errore",
      "",
      "",
      "",
      "",
      "taula-atzipena",
      "yaula-esleipen",
      "array dimension",
      "ez da taula",
      "baldintza-sententzia",

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

      "DD/MM/YYYY",
      "E",
      "N"
   }
};

#define HB_LANG_ID      EUUTF
#include "hbmsgreg.h"
