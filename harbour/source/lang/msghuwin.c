/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (HUWIN)
 *
 * Copyright 2000 Victor Szakats <info@szelvesz.hu>
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

/* Language name: Hungarian */
/* ISO language code (2 chars): HU */
/* Codepage: Windows-1 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */
   
      "HUWIN",                     /* ID */
      "Hungarian",                 /* Name (in English) */
      "Magyar",                    /* Name (in native language) */
      "HU",                        /* RFC ID */
      "Windows-1",                 /* Codepage */
      "$Revision$ $Date$",         /* Version */
   
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
      "hétfõ",
      "kedd",
      "szerda",
      "csütörtök",
      "péntek",
      "szombat",
   
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
   
      "Ismeretlen hiba",
      "Paraméter hiba",
      "Tömbindex hiba",
      "Karakteres változó túlcsordulás",
      "Numerikus túlcsordulás",
      "Nullával való osztás",
      "Numerikus hiba",
      "Szintaktikus hiba",
      "Túl összetett mûvelet",
      "",
      "",
      "Kevés memória",
      "Nem definiált függvény",
      "Nem exportált metódus",
      "Nem létezõ változó",
      "Nem létezõ munkaterület név",
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
      "Nem támogatott mûvelet",
      "Korlát túllépve",
      "Index hiba felfedezve",
      "Nem megfelelõ adattípus",
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
   
      "Can't locate starting procedure",
      "Can't allocate memory (%s)",
      "Can't reallocate memory (%s)",
      "Free called with null pointer", /* DEBUG */
      "Not implemented opcode (%s)",
      "Not implemented (%s)",
   
      /* Texts */
   
      "YYYY.MM.DD",
      "I",
      "N"
   }
};

HB_LANG_ANNOUNCE( HUWIN );

HB_CALL_ON_STARTUP_BEGIN( hb_lang_Init_HUWIN )
   hb_langRegister( &s_lang );
HB_CALL_ON_STARTUP_END( hb_lang_Init_HUWIN )
#if ! defined(__GNUC__) && ! defined(_MSC_VER)
   #pragma startup hb_lang_Init_HUWIN
#endif

