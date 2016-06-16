PROCEDURE Main()

   ? C_FUNC()

   ? EndDumpTest()

   RETURN

#pragma begindump

#include "hbapi.h"

HB_FUNC( C_FUNC )
{
   hb_retc( "returned from C_FUNC()\n" );
}

#pragma enddump

STATIC FUNCTION EndDumpTest()
   RETURN "End Dump Test"
