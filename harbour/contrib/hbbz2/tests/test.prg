/*
 * $Id$
 */

#include "simpleio.ch"

PROCEDURE Main()
   LOCAL cI, cJ, nErr
   cI := "Hello"
   cJ := HB_BZ2_COMPRESS( cI,, @nErr )
   ? nErr, LEN( cJ ), HB_STRTOHEX( cJ )
   RETURN
