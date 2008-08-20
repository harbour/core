//
// $Id$
//

#include "hbmemory.ch"

FUNCTION MAIN
LOCAL nH1, nH2, nH3, nH4
LOCAL n:=0
LOCAL aSign:={"|", "/", "-", "\" }
LOCAL nPrev:=SECONDS()

  CLS
  ? "   Time:        Memory used:                          Miliseconds elapsed"
  ?
  ? "Can you see it ??? :) Press any key or wait 30 seconds"
  ?
  ?
  @ 10,2 SAY "Memory before TEST() call" + STR( MEMORY(HB_MEM_USED) )
  TEST()
  @ 11,2 SAY "Memory after TEST() and before collecting" + STR( MEMORY(HB_MEM_USED) )
  HB_GCALL()
  @ 12,2 SAY "Memory after collecting" + STR( MEMORY(HB_MEM_USED) )
  nH1 := HB_IDLEADD( {|| DEVPOS(0,01), DEVOUT( TIME() ) } )
  nH2 := HB_IDLEADD( {|| DEVPOS(0,21), TEST(), DEVOUT( MEMORY(HB_MEM_USED) ) } )
  nH3 := HB_IDLEADD( {|| DEVPOS(0,41), IIF(n==4,n:=1,n++),DEVOUT(aSign[n]) } )
  nH4 := HB_IDLEADD( {|| DEVPOS(0,61), DEVOUT( 1000*(SECONDS()-nPrev) ), nPrev:=SECONDS() } )

  ? VALTYPE(nH1), nH1, VALTYPE(nH2), nH2, VALTYPE(nH3), nH3, VALTYPE(nH4), nH4
  
  INKEY( 30 )
  IF !EMPTY(nH3)
     @ 14,2 SAY "Delete task 3: " + HB_VALTOSTR(nH3)
     HB_IDLEDEL( nH3 )
  ENDIF
  IF !EMPTY(nH2)
     @ 15,2 SAY "Delete task 2: " + HB_VALTOSTR(nH2)
     HB_IDLEDEL( nH2 )
  ENDIF
  IF !EMPTY(nH1)
     @ 16,2 SAY "Delete task 1: " + HB_VALTOSTR(nH1)
     HB_IDLEDEL( nH1 )
  ENDIF
  IF !EMPTY(nH4)
     @ 17,2 SAY "Delete task 4: " + HB_VALTOSTR(nH4)
     HB_IDLEDEL( nH4 )
  ENDIF

  @ 18,2 SAY "Memory after idle states" + STR( MEMORY(HB_MEM_USED) )
  HB_GCALL()
  @ 19,2 SAY "Memory after collecting" + STR( MEMORY(HB_MEM_USED) )

RETURN 1

PROC TEST()
LOCAL a, b, c
LOCAL cb

  a := ARRAY( 3 )
  b := ARRAY( 3 )
  c := ARRAY( 3 )
  a[1] :=a
  a[2] :=b
  a[3] :=c
  b[1] :=a
  b[2] :=b
  b[3] :=c
  c[1] :=a
  c[2] :=b
  c[3] :=c

  cb := {|x| x:=cb}
  EVAL( cb )

RETURN
