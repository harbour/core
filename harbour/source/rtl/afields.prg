/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * AFIELDS() function
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
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
#include "dbstruct.ch"

FUNCTION AFields( aFieldNames, aTypes, aWidths, aDecimals )

   LOCAL aStruct := dbStruct()
   LOCAL nLen := Len( aStruct )

   IF nLen > 0

      IF ISARRAY( aFieldNames )
         nLen := Min( nLen, Len( aFieldNames ) )
      ENDIF

      IF ISARRAY( aTypes )
         nLen := Min( nLen, Len( aTypes ) )
      ENDIF

      IF ISARRAY( aWidths )
         nLen := Min( nLen, Len( aWidths ) )
      ENDIF

      IF ISARRAY( aDecimals )
         nLen := Min( nLen, Len( aDecimals ) )
      ENDIF

      IF ISARRAY( aFieldNames )
         aEval( aStruct, {| aField, nIndex | aFieldNames[ nIndex ] := aField[ DBS_NAME ] }, 1, nLen )
      ENDIF

      IF ISARRAY( aTypes )
         aEval( aStruct, {| aField, nIndex | aTypes[ nIndex ] := aField[ DBS_TYPE ] }, 1, nLen )
      ENDIF

      IF ISARRAY( aWidths )
         aEval( aStruct, {| aField, nIndex | aWidths[ nIndex ] := aField[ DBS_LEN ] }, 1, nLen )
      ENDIF

      IF ISARRAY( aDecimals )
         aEval( aStruct, {| aField, nIndex | aDecimals[ nIndex ] := aField[ DBS_DEC ] }, 1, nLen )
      ENDIF

   ENDIF

   RETURN nLen
