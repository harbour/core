/*
 * $Id$
 */

REQUEST HB_CODEPAGE_UTF8EX

#uncommand ? [<explist,...>] =>
#command ? [<explist,...>]  => A( <explist> )

PROCEDURE Main()

   hb_cdpSelect( "UTF8EX" )

   ? { }, "8352"
   ? "a", "F248"
   ? "¥", "96F0"
   ? 1,   "AE79"
   ? 1000, "278B"
   ? 1000000, "A752"
   ? 268435455, "4907"
   ? 268435456, "E677"
   ? 268435456000, "4271"
   ? - 1, "FE11"
   ? 9007199254740990, "0009"
   ? AMF3_DECODE( AMF3_ENCODE( 9007199254740990 ) ), "0009"
   ? 9007199254740991, "8918"
   ? AMF3_DECODE( AMF3_ENCODE( 9007199254740991 ) ), "8918"
   ? 9007199254740991.00, "8918"
   ? 6969.69, "10AF"
   ? NIL, "F1E1"
   ? .T. , "E3C2"
   ? .F. , "6AD3"
   ? { 1, - 1 }, "0560"
   ? { "ONE" => 0xcafe, "TWO" => 0xbabe }, "CE93"

   RETURN

PROCEDURE A( a, cChkOK )

   LOCAL x := AMF3_ENCODE( a )
   LOCAL cChk := hb_StrToHex( I2Bin( hb_CRC( x ) ) )

   QOut( PadL( hb_ValToExp( a ), 18 ), hb_StrToHex( x, " " ), "CHECKSUM", cChk, iif( !Empty( cChkOK ) .AND. !( cChk == cChkOK ), "!TEST FAILED!, should be " + cChkOK, " " ) )

   RETURN
