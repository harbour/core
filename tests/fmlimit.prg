#include "hbmemory.ch"

procedure main()

   local s, a, i

   if Memory( HB_MEM_ISLIMIT ) > 0
      ? "FM supports used memory limit."
   else
      ? "FM does not support used memory limit !!!"
      ? "Program won't be interrupted."
      wait
   endif
   ?

   ? "max used memory:", Memory( HB_MEM_USEDMAX )
   ? "    used memory:", Memory( HB_MEM_USED )
   ?
   s := Space( 100000000 ); HB_SYMBOL_UNUSED( s )
   ? "max used memory:", Memory( HB_MEM_USEDMAX )
   ? "    used memory:", Memory( HB_MEM_USED )
   ?
   s := ""; HB_SYMBOL_UNUSED( s )
   ? "max used memory:", Memory( HB_MEM_USEDMAX )
   ? "    used memory:", Memory( HB_MEM_USED )
   ?
   __fm_AllocLimit()
   ? "max used memory:", Memory( HB_MEM_USEDMAX )
   ? "    used memory:", Memory( HB_MEM_USED )
   ?
   ?
   ? "    alloc limit:", __fm_allocLimit()
   ? "    used memory:", Memory( HB_MEM_USED )
   s := Space( 100000000 ); HB_SYMBOL_UNUSED( s )
   ? "    used memory:", Memory( HB_MEM_USED )
   s := ""; HB_SYMBOL_UNUSED( s )
   ? "    used memory:", Memory( HB_MEM_USED )
   ?
   wait
   ?
   __fm_allocLimit( 50000000 )
   ? "    alloc limit:", __fm_allocLimit()
   a := Array( 10 )
   for i := 1 to Len( a )
      a[ i ] := Space( 5000000 )
      ? "(" + Str( i, 2 ) + ") used memory:", Memory( HB_MEM_USED )
   next

   return
