/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Old style order management functions
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

#include "common.ch"

/* NOTE: The fifth parameters (cOrderName) is undocumented. */

FUNCTION dbCreateIndex( cOrderBagName, cKeyExpr, bKeyExpr, lUnique, cOrderName )
   RETURN ordCreate( cOrderBagName, cOrderName, cKeyExpr, bKeyExpr, lUnique )

FUNCTION dbSetIndex( cIndexName )
   RETURN ordListAdd( cIndexName )

FUNCTION dbClearIndex()
   RETURN ordListClear()

FUNCTION dbReindex()
   RETURN ordListRebuild()

FUNCTION dbSetOrder( nOrderNum )

   IF ISCHARACTER( nOrderNum ) .AND. !Empty( Val( nOrderNum ) )
      nOrderNum := Val( nOrderNum )
   ENDIF

   ordSetFocus( nOrderNum )

   RETURN NIL

FUNCTION IndexExt()
   RETURN ordBagExt()

FUNCTION IndexKey( nOrder )

   IF !ISNUMBER( nOrder )
      RETURN ordKey( nOrder )
   ENDIF

   IF Used()
      RETURN ordKey( nOrder )
   ENDIF

   RETURN ""

/* TODO: Dummy functions, should be removed when implemented. */

STATIC FUNCTION ordCreate()
   RETURN ""

STATIC FUNCTION ordListAdd()
   RETURN NIL

STATIC FUNCTION ordListClear()
   RETURN NIL

STATIC FUNCTION ordListRebuild()
   RETURN NIL

STATIC FUNCTION ordSetFocus()
   RETURN ""

STATIC FUNCTION ordBagExt()
   RETURN ""

STATIC FUNCTION ordKey()
   RETURN ""

