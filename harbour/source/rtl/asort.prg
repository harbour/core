/*
 * $Id$
 */

/*
 * ASORT - Sort an array
 *
 * Copyright (C) 1999 Eddie Runia (eddie@runia.com)
 */

//
// <aSorted> aSort( <aUnsorted>, [nStart], [nCount], [bBlock] )
//
// Sort an array
//
function aSort( aIn, nStart, nCount, bBlock )

   IF !(ValType(nStart) == "N")
        nStart := 1
   ENDIF

   IF !(ValType(nCount) == "N")
        nCount := Len(aIn) - nStart + 1
   ENDIF

   IF !(ValType(bBlock) == "B")
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

