//
// <aSorted> aSort( <aUnsorted>, [nStart], [nCount], [bBlock] )
//
// Sort an array
//
function aSort( aIn, nStart, nCount, bBlock )

   nStart := Default( nStart, 1 )
   QuickSort( aIn,                                      ;
              nStart,                                   ;
              Default( nCount, Len(aIn) - nStart + 1 ), ;
              Default( bBlock, {| x, y | x < y } ) )
return aIn


//
// QuickSort( <aSort>, <nLeft>, <nRight>, <bOrder> )
//
// Perform a QuickSort of <aSort>.
//
// For instructions :
// http://monty.cnri.reston.va.us/grail/demo/quicksort/quicksort.htm
//
function QuickSort( aSort, nLeft, nRight, bOrder )

   local nUp     := nLeft
   local nDown   := nRight
   local xMiddle := aSort[ ( nLeft + nRight ) / 2 ]
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

