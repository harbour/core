/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ASORT() function
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Chen Kedem <niki@actcom.co.il>
 *    Documentation
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "common.ch"

/*  $DOC$
 *  $FUNCNAME$
 *      ASORT()
 *  $CATEGORY$
 *      Array
 *  $ONELINER$
 *      Sort an array
 *  $SYNTAX$
 *      ASORT( <aArray>, [<nStart>], [<nCount>], [<bSort>] ) --> aArray
 *  $ARGUMENTS$
 *      <aArray> Array to be sorted.
 *
 *      <nStart> The first element to start the sort from, default is 1.
 *
 *      <nCount> Number of elements starting from <nStart> to sort, default
 *      is all elements.
 *
 *      <bSort> Code block for sorting order, default is ascending order
 *      {| x, y | x < y }. The code block should accept two parameters and
 *      must return .T. if the sort is in order, .F. if not.
 *  $RETURNS$
 *      ASORT() return reference to the now sorted <aArray> or NIL if the
 *      passed <aArray> is not an array.
 *  $DESCRIPTION$
 *      ASORT() sort all or part of a given array. If <bSort> is omitted,
 *      the function expect <aArray> to be one dimensional array containing
 *      single data type (one of: Character, Date, Logical, Numeric) and
 *      sort this array in ascending order: Character are sorted by their
 *      ASCII value, Dates are sorted chronologically, Logical put .F.
 *      values before .T., Numeric are sorted by their value.
 *
 *      If <bSort> is specified, it is used to handle the sorting order.
 *      With each time the block is evaluate, two array elements are passed
 *      to the code block, and <bSort> must return a logical value that
 *      state if those elements are in order (.T.) or not (.F.). Using
 *      this block you can sort multidimensional array, descending orders
 *      or even (but why would you want to do that) sort array that contain
 *      different data type.
 *  $EXAMPLES$
 *     // sort numeric values in ascending order
 *     ASORT( { 3, 1, 4, 42, 5, 9 } )      // result: { 1, 3, 4, 5, 9, 42 }
 *
 *     // sort character strings in descending lexical order
 *     aKeys := { "Ctrl", "Alt", "Delete" }
 *     bSort := {| x, y | UPPER( x ) > UPPER( y ) }
 *     ASORT( aKeys,,, bSort )       // result: { "Delete", "Ctrl", "Alt" }
 *
 *     // sort two-dimensional array according to 2nd element of each pair
 *     aPair :=   { {"Sun",8}, {"Mon",1}, {"Tue",57}, {"Wed",-6} }
 *     ASORT( aPair,,, {| x, y | x[2] < y[2] } )
 *     // result: { {"Wed",-6}, {"Mon",1}, {"Sun",8}, {"Tue",57} }
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      For speed we are checking the return type of the passed code block.
 *      This will result in a small incompatibility since the code block
 *      will be called one more time for the first logical element than in
 *      Clipper. But this is block calling frequency and order differs from
 *      Clipper anyway, since they use different sorting algorithm.
 *  $SEEALSO$
 *      ASCAN() EVAL() SORT
 *  $END$
 */

FUNCTION ASort( aArray, nStart, nCount, bBlock )

   IF !ISARRAY( aArray )
      RETURN NIL
   ENDIF

   IF Len( aArray ) >= 1

      IF !ISNUMBER( nStart ) .OR. nStart == 0
         nStart := 1
      ENDIF

      IF nStart >= 1

         IF nStart > Len( aArray )
            nStart := Len( aArray )
         ENDIF

         IF !ISNUMBER( nCount ) .OR. nCount < 1 .OR. nCount > ( Len( aArray ) - nStart + 1 )
            nCount := Len( aArray ) - nStart + 1
         ENDIF

         /* NOTE: For speed we are checking the return type of the passed
                  codeblock here. This will result in a small incompatibility
                  since the codeblock will be called one more time for the
                  first logical element than in Clipper.
                  But this is block calling frequency and order differs from
                  Clipper anyway, since they use different sorting sorting
                  algorhythms. */

         IF !ISBLOCK( bBlock ) .OR. !( ISLOGICAL( Eval( bBlock, aArray[ nStart ], aArray[ nStart ] ) ) )
            bBlock := {| x, y | x < y }
         ENDIF

         QuickSort( aArray, nStart, nStart + nCount - 1, bBlock )

      ENDIF

   ENDIF

   RETURN aArray

/*
 * QuickSort( <aArray>, <nLeft>, <nRight>, <bBlock> )
 *
 * Perform a QuickSort of <aArray>.
 *
 * For instructions :
 * http://monty.cnri.reston.va.us/grail/demo/quicksort/quicksort.htm
 */
STATIC PROCEDURE QuickSort( aArray, nLeft, nRight, bBlock )

   LOCAL nUp     := nLeft
   LOCAL nDown   := nRight
   LOCAL xMiddle := aArray[ Int( ( nLeft + nRight ) / 2 ) ]
   LOCAL xTemp

   DO WHILE .T.

      DO WHILE Eval( bBlock, aArray[ nUp ], xMiddle )
         nUp++
      ENDDO

      DO WHILE Eval( bBlock, xMiddle, aArray[ nDown ] )
         nDown--
      ENDDO

      IF nUp <= nDown
         IF nUp != nDown
            xTemp           := aArray[ nUp ]
            aArray[ nUp ]   := aArray[ nDown ]
            aArray[ nDown ] := xTemp
         ENDIF
         nUp++
         nDown--
      ENDIF

      IF nUp > nDown
         EXIT
      ENDIF

   ENDDO

   IF nLeft < nDown
      QuickSort( aArray, nLeft, nDown , bBlock )
   ENDIF

   IF nUp < nRight
      QuickSort( aArray, nUp  , nRight, bBlock )
   ENDIF

   RETURN

