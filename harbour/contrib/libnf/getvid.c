/*
 * $Id$
 */

/*
   Harbour Project source code

   Getvid.c Support functions for Nanfor Library

   Copyright 2000  Luiz Rafael Culik <Culik@sl.conex.net>
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

#include "hbapi.h"
#include "dos.h"

HB_FUNC(_FT_GETVPG)
{
   int iPage;
#if defined(HB_OS_DOS)
   {
      union REGS registers;
      regs.h.ah=0x0F;
      HB_DOS_INT86(0x10,&registers,&registers);
      iPage=regs.h.bh;
   }
#else
   {
      iPage=0;
   }
#endif
   {
      hb_retni(iPage);
   }
}

HB_FUNC(_V_SETVPG)
{
   int iPage;
#if defined(HB_OS_DOS)
   {
      union REGS registers;
      iPage=hb_parni(1);
      regs.h.ah=0x05;
      regs.h.al=iPage;
      HB_DOS_INT86(0x10,&registers,&registers);
   }
#endif
}

