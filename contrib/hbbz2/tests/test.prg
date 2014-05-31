#require "hbbz2"

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL cJ, nErr

   cJ := hb_bz2_Compress( "Hello", , @nErr )
   ? nErr, Len( cJ ), hb_StrToHex( cJ )

   RETURN
