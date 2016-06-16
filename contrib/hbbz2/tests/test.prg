#require "hbbz2"

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL nErr
   LOCAL cJ := hb_bz2_Compress( "Hello",, @nErr )

   ? nErr, hb_BLen( cJ ), hb_StrToHex( cJ )

   RETURN
