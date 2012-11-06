/*
 * $Id$
 */

#require "hbamf"

REQUEST HB_CODEPAGE_UTF8EX

#command TEST [<explist,...>] => _TEST( <explist> )

PROCEDURE Main()

   hb_cdpSelect( "UTF8EX" )

   TEST {}, "8352"
   TEST "a", "F248"
   TEST "ą", "96F0"
   TEST 1, "AE79"
   TEST 1000, "278B"
   TEST 1000000, "A752"
   TEST 268435455, "4907"
   TEST 268435456, "E677"
   TEST 268435456000, "4271"
   TEST -1, "FE11"
   TEST 9007199254740990, "0009"
   TEST amf3_Decode( amf3_Encode( 9007199254740990 ) ), "0009"
   TEST 9007199254740991, "8918"
   TEST amf3_Decode( amf3_Encode( 9007199254740991 ) ), "8918"
   TEST 9007199254740991.00, "8918"
   TEST 6969.69, "10AF"
   TEST NIL, "F1E1"
   TEST .T., "E3C2"
   TEST .F., "6AD3"
   TEST { 1, -1 }, "0560"
   TEST { "ONE" => 0xcafe, "TWO" => 0xbabe }, "CE93"

   RETURN

STATIC PROCEDURE _TEST( a, cChkOK )

   LOCAL x := amf3_Encode( a )
   LOCAL cChk := hb_StrToHex( I2Bin( hb_CRC( x ) ) )

   ? PadL( hb_ValToExp( a ), 18 ), hb_StrToHex( x, " " ), "CHECKSUM", cChk, iif( ! Empty( cChkOK ) .AND. !( cChk == cChkOK ), "!TEST FAILED!, should be " + cChkOK, " " )

   RETURN
