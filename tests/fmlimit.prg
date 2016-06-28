#include "hbmemory.ch"

procedure Main()

   local s, a, i

   if Memory( HB_MEM_CANLIMIT ) > 0
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
   ? "    alloc limit:", __fm_AllocLimit()
   ? "    used memory:", Memory( HB_MEM_USED )
   s := Space( 100000000 ); HB_SYMBOL_UNUSED( s )
   ? "    used memory:", Memory( HB_MEM_USED )
   s := ""; HB_SYMBOL_UNUSED( s )
   ? "    used memory:", Memory( HB_MEM_USED )
   ?
   wait
   ?
   __fm_AllocLimit( 50000000 )
   ? "    alloc limit:", __fm_AllocLimit()
   a := Array( 10 )
   for i := 1 to Len( a )
      a[ i ] := Space( 5000000 )
      ? "(" + Str( i, 2 ) + ") used memory:", Memory( HB_MEM_USED )
   next

   return
