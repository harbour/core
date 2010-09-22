/*
 * $Id$
 */

PROCEDURE Main()

   QOut( C_FUNC() )

   QOut( EndDumpTest() )

   RETURN

#pragma BEGINDUMP

#include "hbapi.h"

HB_FUNC( C_FUNC )
{
   hb_retc( "returned from C_FUNC()\n" );
}

#pragma ENDDUMP

FUNCTION EndDumpTest()
   RETURN "End Dump Test"
