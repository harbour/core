/*
 * $Id$
 */

// see also hrbext.prg

#include "hbhrb.ch"

PROCEDURE Main( x )

   LOCAL pHrb, cExe := "Msg2()"

   LOCAL n := iif( x == NIL, 0, Val( x ) )

   ? "calling Msg ... From exe here !"
   Msg()
   ? "========================="

#if 0
   ? "Loading( 'hrbext.hrb' )"
   pHrb := hb_hrbLoad( "hrbext.hrb" )

   ? "Loading( HB_HRB_BIND_DEFAULT, 'hrbext.hrb' )"
   pHrb := hb_hrbLoad( HB_HRB_BIND_DEFAULT, "hrbext.hrb" )

   ? "Loading( HB_HRB_BIND_LOCAL, 'hrbext.hrb' )"
   pHrb := hb_hrbLoad( HB_HRB_BIND_LOCAL, "hrbext.hrb" )
#endif

   ? "Loading(" + iif( n == 0, "HB_HRB_BIND_DEFAULT", iif( n == 1, "HB_HRB_BIND_LOCAL", "HB_HRB_BIND_OVERLOAD" ) ) + ", 'hrbext.hrb' )"
   pHrb := hb_hrbLoad( n, "hrbext.hrb" )

   ? "========================="

   ? "calling Msg ... DEFAULT=From exe, LOCAL=From exe, OVERLOAD=From HRB"
   Msg()
   ? "========================="

   ? "calling Msg ... DEFAULT=From exe, LOCAL=From HRB, OVERLOAD=From HRB"
   &cExe  //
   ? "========================="

   hb_hrbUnload( pHrb ) // should do nothing in case of OVERLOAD

   ? "calling Msg ... DEFAULT=From exe, LOCAL=From exe, OVERLOAD=From HRB"
   Msg() // test unload protection when using OVERLOAD ... then .hrb not anymore unloadable
   ? "========================="

   ? "END"

   RETURN

FUNCTION Msg()

   ? "Function called from Exe"

   RETURN .T.
