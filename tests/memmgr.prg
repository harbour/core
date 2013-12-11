/*
 * Harbour Project source code:
 *    a small memory manager test code
 */

#include "simpleio.ch"

#define N_LOOPS   100000

#ifdef __HARBOUR__
#include "hbmemory.ch"
#endif

PROCEDURE Main()

   LOCAL nCPUSec, nRealSec, i, a

#ifdef __HARBOUR__

   IF Memory( HB_MEM_USEDMAX ) != 0
      ?
      ? "Warning !!! Memory statistics enabled."
   ENDIF
#endif
   ?
   ? Date(), Time(), Version() + build_mode() + ",", OS()

   ?
   ? "testing single large memory blocks allocation and freeing..."
   nRealSec := Seconds()
   nCPUSec := hb_SecondsCPU()
   FOR i := 1 TO N_LOOPS
      a := Space( 50000 )
   NEXT
   a := NIL
   nCPUSec := hb_SecondsCPU() - nCPUSec
   nRealSec := Seconds() - nRealSec
   ? " CPU time:", nCPUSec, "sec."
   ? "real time:", nRealSec, "sec."

   ?
   ? "testing many large memory blocks allocation and freeing..."
   nRealSec := Seconds()
   nCPUSec := hb_SecondsCPU()
   a := Array( 100 )
   FOR i := 1 TO N_LOOPS
      a[ i % 100 + 1 ] := Space( 50000 )
      IF i % 200 == 0
         AFill( a, "" )
      ENDIF
   NEXT
   a := NIL
   nCPUSec := hb_SecondsCPU() - nCPUSec
   nRealSec := Seconds() - nRealSec
   ? " CPU time:", nCPUSec, "sec."
   ? "real time:", nRealSec, "sec."

   ?
   ? "testing large memory block reallocation with intermediate allocations..."
   ? "Warning!!! some compilers may badly fail here"
   WAIT

   nRealSec := Seconds()
   nCPUSec := hb_SecondsCPU()
   a := {}
   FOR i := 1 TO N_LOOPS
      AAdd( a, {} )
      IF i % 1000 == 0
         ?? i
      ENDIF
   NEXT
   nCPUSec := hb_SecondsCPU() - nCPUSec
   nRealSec := Seconds() - nRealSec
   ? " CPU time:", nCPUSec, "sec."
   ? "real time:", nRealSec, "sec."
   WAIT

   RETURN

FUNCTION build_mode()
#ifdef __CLIP__
   RETURN " (MT)"
#else
#ifdef __XHARBOUR__
   RETURN iif( hb_MultiThread(), " (MT)", "" ) + ;
      iif( Memory( HB_MEM_USEDMAX ) != 0, " (FMSTAT)", "" )
#else
#ifdef __HARBOUR__
   RETURN iif( hb_mtvm(), " (MT)", "" ) + ;
      iif( Memory( HB_MEM_USEDMAX ) != 0, " (FMSTAT)", "" )
#else
#ifdef __XPP__
   RETURN " (MT)"
#else
   RETURN ""
#endif
#endif
#endif
#endif

#if __HARBOUR__ < 0x010100

FUNCTION hb_mtvm()
   RETURN .F.

#endif
