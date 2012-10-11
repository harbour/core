/*
 * $Id$
 */

#require "hbbz2"

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL cI, cJ, nErr

   cI := "Hello"
   cJ := HB_BZ2_COMPRESS( cI, , @nErr )
   ? nErr, Len( cJ ), hb_StrToHex( cJ )

   RETURN
