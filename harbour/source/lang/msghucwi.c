/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (HUCWI)
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
/* Codepage: CWI-2 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */
   
      "HUCWI",                     /* ID */
      "Hungarian",                 /* Name (in English) */
      "Magyar",                    /* Name (in native language) */
      "HU",                        /* RFC ID */
      "CWI-2",                     /* Codepage */
      "$Revision$ $Date$",         /* Version */
   
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
      "hÇtfì",
      "kedd",
      "szerda",
      "csÅtîrtîk",
      "pÇntek",
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
      "ParamÇter hiba",
      "Tîmbindex hiba",
      "Karakteres v†ltoz¢ t£lcsordul†s",
      "Numerikus t£lcsordul†s",
      "Null†val val¢ oszt†s",
      "Numerikus hiba",
      "Szintaktikus hiba",
      "T£l îsszetett mñvelet",
      "",
      "",
      "KevÇs mem¢ria",
      "Nem defini†lt fÅggvÇny",
      "Nem export†lt met¢dus",
      "Nem lÇtezì v†ltoz¢",
      "Nem lÇtezì munkaterÅlet nÇv",
      "Nem export†lt v†ltoz¢",
      "Helytelen munkaterÅlet nÇv",
      "M†r haszn†lt munkaterÅlet nÇv",
      "",
      "LÇtrehoz†si hiba",
      "Megnyit†si hiba",
      "Lez†r†si hiba",
      "Olvas†si hiba",
      "çr†s hiba",
      "Nyomtat†si hiba",
      "",
      "",
      "",
      "",
      "Nem t†mogatott mñvelet",
      "Korl†t t£llÇpve",
      "Index hiba felfedezve",
      "Nem megfelelì adatt°pus",
      "T£l szÇles adat",
      "Nem megnyitott munkaterÅlet",
      "Nem indexelt munkaterÅlet",
      "Kiz†r¢lagos megnyit†si m¢d szÅksÇges",
      "Z†rol†s szÅksÇges",
      "çr†s nem megengedett",
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

HB_LANG_ANNOUNCE( HUCWI );

HB_CALL_ON_STARTUP_BEGIN( hb_lang_Init_HUCWI )
   hb_langRegister( &s_lang );
HB_CALL_ON_STARTUP_END( hb_lang_Init_HUCWI )
#if ! defined(__GNUC__) && ! defined(_MSC_VER)
   #pragma startup hb_lang_Init_HUCWI
#endif

