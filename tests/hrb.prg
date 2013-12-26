// see also hrbext.prg

#include "hbhrb.ch"

PROCEDURE Main( n )

   LOCAL pHrb
   LOCAL cExe := "Msg2()"

   hb_default( @n, "0" )
   n := Val( n )

   IF ! hb_FileExists( "hrbext.hrb" )
      hb_run( "harbour hrbext.prg -gh" )
   ENDIF

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

   ? "Loading( " + iif( n == 0, "HB_HRB_BIND_DEFAULT", iif( n == 1, "HB_HRB_BIND_LOCAL", "HB_HRB_BIND_OVERLOAD" ) ) + ", 'hrbext.hrb' )"
   pHrb := hb_hrbLoad( n, "hrbext.hrb" )

   ? "========================="

   ? "calling Msg ... DEFAULT=From exe, LOCAL=From exe, OVERLOAD=From hrb"
   Msg()
   ? "========================="

   ? "calling Msg ... DEFAULT=From exe, LOCAL=From hrb, OVERLOAD=From hrb"
   &cExe  //
   ? "========================="

   hb_hrbUnload( pHrb ) // should do nothing in case of OVERLOAD

   ? "calling Msg ... DEFAULT=From exe, LOCAL=From exe, OVERLOAD=From hrb"
   Msg() // test unload protection when using OVERLOAD ... then .hrb not anymore unloadable
   ? "========================="

   ? "END"

   RETURN

FUNCTION Msg()

   ? "Function called from .exe"

   RETURN .T.
