/*
 * $Id$
 */

/*
 * ASORT - Sort an array
 *
 * Copyright (C) 1999 Eddie Runia (eddie@runia.com)
 * Part of the Harbour Project www.harbour-project.org
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
 */

//
// <aSorted> aSort( <aUnsorted>, [nStart], [nCount], [bBlock] )
//
// Sort an array
//
function aSort( aIn, nStart, nCount, bBlock )

   IF !( ValType( aIn ) == "A" )
      RETURN NIL
   ENDIF

   IF Len( aIn ) == 0
      RETURN aIn
   ENDIF

   IF !( ValType( nStart ) == "N" )
      nStart := 1
   ENDIF

   IF nStart > Len( aIn )
      nStart := Len( aIn )
   ENDIF

   IF !( ValType( nCount ) == "N" )
      nCount := Len( aIn ) - nStart + 1
   ELSEIF nCount > Len( aIn ) - nStart
      nCount := Len( aIn ) - nStart
   ENDIF

   IF !( ValType( bBlock ) == "B" )
      bBlock := {| x, y | x < y }
   ENDIF

   QuickSort( aIn, nStart, nCount, bBlock )

return aIn


//
// QuickSort( <aSort>, <nLeft>, <nRight>, <bOrder> )
//
// Perform a QuickSort of <aSort>.
//
// For instructions :
// http://monty.cnri.reston.va.us/grail/demo/quicksort/quicksort.htm
//
static function QuickSort( aSort, nLeft, nRight, bOrder )

   local nUp     := nLeft
   local nDown   := nRight
   local xMiddle := aSort[ int ( ( nLeft + nRight ) / 2 ) ]
   local xTemp
   local lOk     := .T.

   do while lOk
      do while Eval( bOrder, aSort[ nUp ], xMiddle   )
         nUp++
      enddo

      do while Eval( bOrder, xMiddle, aSort[ nDown ] )
         nDown--
      enddo

      if nUp <= nDown
         if nUp != nDown
            xTemp          := aSort[ nUp ]
            aSort[ nUp   ] := aSort[ nDown ]
            aSort[ nDown ] := xTemp
         endif
         nUp++
         nDown--
      endif

      lOk := nUp <= nDown
   enddo

   if nLeft < nDown
      QuickSort( aSort, nLeft, nDown , bOrder )
   endif

   if nUp < nRight
      QuickSort( aSort, nUp  , nRight, bOrder )
   endif

return nil

