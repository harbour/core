/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Get Class
 *
 * Copyright 1999 Ignacio Ortiz de Z£niga <ignacio@fivetech.com>
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

#include "hbclass.ch"
#include "hbsetup.ch"


//---------------------------------------------------------------------------//

Function GetNew( nRow, nCol, bVarBlock, cVarName, cPicture, cColor )

return Get():New( nRow, nCol, bVarBlock, cVarName, cPicture, cColor )

//---------------------------------------------------------------------------//

FUNCTION __GET( bSetGet, cVarName, cPicture, bValid, bWhen )

   LOCAL oGet

   IF( bSetGet == NIL )
      //Alert( "No Block for: " + cVarName )

      IF __ISMV( cVarName )
         bSetGet := {|_1| IIF( _1 == NIL,  __MVGET( cVarName ), __MVPUT( cVarName, _1 ) ) }
      ELSE
         //Alert( "Not MemVar: " + cVarName )

         /* "{|_1| IIF( _1 == NIL, &cVarName, &cVarName := _1 )" */
         bSetGet := &( "{|_1| IIF( _1 == NIL, " + cVarName + ", " + cVarName + " := _1 ) }" )
      ENDIF
   ENDIF

   oGet := Get():New( , ,bSetGet, cVarName, cPicture )

   oGet:PreBlock := bWhen
   oGet:PostBlock := bValid

   RETURN oGet


FUNCTION __GETA( aVar, cVarName, cPicture, bValid, bWhen, aIndex )

   LOCAL oGet, nDim := Len( aIndex ), bSetGet, aGetVar, Counter

   aGetVar := aVar
   FOR Counter := 1 TO nDim - 1
      aGetVar := aGetVar[aIndex[Counter]]
   NEXT
   bSetGet := {|xValue| IIF( PCOUNT() == 0, aGetVar[aIndex[Counter]], aGetVar[aIndex[Counter]] := xValue )}

   oGet := Get():New( , ,bSetGet, cVarName, cPicture )
   oGet:SubScript := aIndex

   oGet:PreBlock := bWhen
   oGet:PostBlock := bValid

RETURN oGet
