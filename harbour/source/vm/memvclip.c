/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * CA-Cl*pper compatibility memvar support
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbcomp.h" /* for VS_* macros */

extern HB_FUNC( __MVCLEAR );
extern HB_FUNC( __MVRELEASE );
extern HB_FUNC( __MVXRELEASE );
extern HB_FUNC( __MVSAVE );
extern HB_FUNC( __MVRESTORE );

/* NOTE: Undocumented Clipper internal function */

HB_FUNC( __QQPUB )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );

   if( pItem )
      hb_memvarCreateFromItem( pItem, VS_PUBLIC, NULL );
}

/* CA-Clipper 5.2e compatibility functions. */

HB_FUNC( __MCLEAR )
{
   HB_FUNCNAME( __MVCLEAR )();
}

HB_FUNC( __MRELEASE )
{
   HB_FUNCNAME( __MVRELEASE )();
}

HB_FUNC( __MXRELEASE )
{
   HB_FUNCNAME( __MVXRELEASE )();
}

HB_FUNC( __MSAVE )
{
   HB_FUNCNAME( __MVSAVE )();
}

HB_FUNC( __MRESTORE )
{
   HB_FUNCNAME( __MVRESTORE )();
}

