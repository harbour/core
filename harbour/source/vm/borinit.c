/*
 * $Id$
 *
 * Harbour Project source code:
 * Borland _INIT_ segment initialization when not using Borland C startup
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

typedef struct
{
   char c1, c2;
   void ( * pInitFunc )( void );
} HB_BORINITSYMBOL, * HB_PBORINITSYMBOL;

extern HB_BORINITSYMBOL hb_BorFirstSymbol, hb_BorLastSymbol;

static void hb_vmProcessBorlandInitSegment( void )
{
   HB_PBORINITSYMBOL pFirst = &hb_BorFirstSymbol;

   while( pFirst < ( &hb_BorLastSymbol - 1 ) )
      ( pFirst++ )->pInitFunc();
}