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

#include "common.ch"

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

