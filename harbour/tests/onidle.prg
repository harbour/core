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
  ? "Can you see it ??? :) Press ESC or wait 5 seconds"
  ?
  ?
  nH1 = HB_IDLEADD( {|| DEVPOS(0,01), DEVOUT( TIME() )} )
  nH2 = HB_IDLEADD( {|| DEVPOS(0,21), TEST(), DEVOUT( MEMORY(HB_MEM_USED) )} )
  nH3 = HB_IDLEADD( {|| DEVPOS(0,41), IIF(n=4,n:=1,n++),DEVOUT(aSign[n]) } )
  nH4 = HB_IDLEADD( {|| DEVPOS(0,61), DEVOUT( 1000*(SECONDS()-nPrev) ), nPrev:=SECONDS()} )
  
  INKEY( 5 )
  HB_IDLEDEL( nH3 )
  HB_IDLEDEL( nH2 )
  HB_IDLEDEL( nH1 )
  HB_IDLEDEL( nH4 )
  
RETURN 1

PROC TEST()
LOCAL a, b
LOCAL cb

  a := ARRAY( 2 )
  b := ARRAY( 2 )
  a[1] :=a
  a[2] :=b
  b[1] :=b
  b[2] :=a
  
//  cb := {|x| IIF( x>10, IIF(x=0,0,EVAL(cb,x-1)), DEVOUT(x))}
//  EVAL( cb, 20 )
//  INKEY( .5 )
  
RETURN

