/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (EU)
 *
 * Copyright 2000 Nicolas del Pozo <niko@geroa.com>
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

/* Language name: Basque */
/* ISO language code (2 chars): EU */
/* Codepage: ???? */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "EU",                        /* ID */
      "Basque",                    /* Name (in English) */
      "English",                   /* Name (in native language) */
      "EU",                        /* RFC ID */
      "850",                       /* Codepage */
      "$Revision$ $Date$",         /* Version */

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

      "Unrecoverable error %lu: ",
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

      /* Texts */

      "DD/MM/YYYY",
      "E",
      "N"
   }
};

HB_LANG_ANNOUNCE( EU );

HB_CALL_ON_STARTUP_BEGIN( hb_lang_Init_EU )
   hb_langRegister( &s_lang );
HB_CALL_ON_STARTUP_END( hb_lang_Init_EU )
#if ! defined(__GNUC__) && ! defined(_MSC_VER)
   #pragma startup hb_lang_Init_EU
#endif

