#require "hbbz2"

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL cI, cJ, nErr

   cI := "Hello"
   cJ := hb_bz2_Compress( cI, , @nErr )
   ? nErr, Len( cJ ), hb_StrToHex( cJ )

   RETURN
