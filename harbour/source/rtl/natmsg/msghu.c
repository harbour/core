/* $Id$

   Harbour Project source code

   Hungarian language module (2 char. ISO language code: HU)
   Codepage: 852

   Copyright (C) 1999  Victor Szel <info@szelvesz.hu>
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).
*/

/* TODO: Decide which codepage to use, and how to implement the whole
         codepage issue */

char *hb_monthsname[ 12 ] =
{
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
   "december"
};

char *hb_daysname[ 7 ] =
{
   "vas†rnap",
   "hÇtfã",
   "kedd",
   "szerda",
   "csÅtîrtîk",
   "pÇntek",
   "szombat"
};

static char *genericErrors[] =
{
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
   "Nem megfelelã sz†m£ paramÇter",
   "tîmbelem hozz†fÇrÇs",
   "tîmbelem ÇrtÇkad†s",
   "nem tîmb",
   "feltÇteles"
};

char *hb_ErrorNatDescription( ULONG ulGenError )
{
   if( ulGenError <= sizeof(genericErrors)/sizeof(char*) )
      return genericErrors[ ulGenError ];
   else
      return genericErrors[ 0 ];
}
