/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    a small memory mangaer test code
 */

#define N_LOOPS   100000

#ifdef __HARBOUR__
   #include "hbmemory.ch"
#endif

proc main()
local nCPUSec, nRealSec, i, a

#ifdef __HARBOUR__
if MEMORY( HB_MEM_USEDMAX ) != 0
   ?
   ? "Warning !!! Memory statistic enabled."
endif
#endif
?
? date(), time(), VERSION()+build_mode()+", "+OS()

?
? "testing single large memory blocks allocation and freeing..."
nRealSec := seconds()
nCPUSec := secondsCPU()
for i := 1 to N_LOOPS
    a := space( 50000 )
next
a := NIL
nCPUSec := secondsCPU() - nCPUSec
nRealSec := seconds() - nRealSec
? " CPU time:", nCPUSec, "sec."
? "real time:", nRealSec, "sec."

?
? "testing many large memory blocks allocation and freeing..."
nRealSec := seconds()
nCPUSec := secondsCPU()
a := array(100)
for i := 1 to N_LOOPS
    a[ i % 100 + 1 ] := space( 50000 )
    if i % 200 == 0
       afill(a,"")
    endif
next
a := NIL
nCPUSec := secondsCPU() - nCPUSec
nRealSec := seconds() - nRealSec
? " CPU time:", nCPUSec, "sec."
? "real time:", nRealSec, "sec."

?
? "testing large memory block reallocation with intermediate allocations..."
? "Warning!!! some compilers may badly fail here"
wait

nRealSec := seconds()
nCPUSec := secondsCPU()
a := {}
for i := 1 to N_LOOPS
   aadd( a, {} )
   if i%1000 == 0
      ?? i
   endif
next
nCPUSec := secondsCPU() - nCPUSec
nRealSec := seconds() - nRealSec
? " CPU time:", nCPUSec, "sec."
? "real time:", nRealSec, "sec."
wait

return


function build_mode()
#ifdef __CLIP__
   return " (MT)"
#else
   #ifdef __XHARBOUR__
      return iif( HB_MULTITHREAD(), " (MT)", "" ) + ;
             iif( MEMORY( HB_MEM_USEDMAX ) != 0, " (FMSTAT)", "" )
   #else
      #ifdef __HARBOUR__
         return iif( HB_MTVM(), " (MT)", "" ) + ;
                iif( MEMORY( HB_MEM_USEDMAX ) != 0, " (FMSTAT)", "" )
      #else
         #ifdef __XPP__
            return " (MT)"
         #else
            return ""
         #endif
      #endif
   #endif
#endif
