/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * GET system active getlist handler for default TGETLIST/GETSYS/READVAR
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

/* NOTE: Harbour internal function to set and get the active GetList */

STATIC s_oGetListActive

PROCEDURE __GetListSetActive( oGetList )

   IF s_oGetListActive != NIL
      s_oGetListActive:lHasFocus := .F.
   ENDIF

   s_oGetListActive := oGetList
   s_oGetListActive:lHasFocus := .T.

   RETURN

/* NOTE: Using a separate function for maximum speed */

FUNCTION __GetListActive()
   RETURN s_oGetListActive

