/*
 * $Id$
 */

#require "hbamf"
#require "hbtest"

REQUEST HB_CODEPAGE_UTF8EX

PROCEDURE Main()

   hb_cdpSelect( "UTF8EX" )

   HBTEST _enc( {}                                             ) IS "09 01 01"
   HBTEST _enc( "a"                                            ) IS "06 03 61"
   HBTEST _enc( "ą"                                            ) IS "06 05 C4 85"
   HBTEST _enc( 1                                              ) IS "04 01"
   HBTEST _enc( 1000                                           ) IS "04 87 68"
   HBTEST _enc( 1000000                                        ) IS "04 BD 84 40"
   HBTEST _enc( 268435455                                      ) IS "04 BF FF FF FF"
   HBTEST _enc( 268435456                                      ) IS "05 41 B0 00 00 00 00 00 00"
   HBTEST _enc( 268435456000                                   ) IS "05 42 4F 40 00 00 00 00 00"
   HBTEST _enc( -1                                             ) IS "04 FF FF FF FF"
   HBTEST _enc( 9007199254740990                               ) IS "05 43 3F FF FF FF FF FF FE"
   HBTEST _enc( amf3_Decode( amf3_Encode( 9007199254740990 ) ) ) IS "05 43 3F FF FF FF FF FF FE"
   HBTEST _enc( 9007199254740991                               ) IS "05 43 3F FF FF FF FF FF FF"
   HBTEST _enc( amf3_Decode( amf3_Encode( 9007199254740991 ) ) ) IS "05 43 3F FF FF FF FF FF FF"
   HBTEST _enc( 9007199254740991.00                            ) IS "05 43 3F FF FF FF FF FF FF"
   HBTEST _enc( 6969.69                                        ) IS "05 40 BB 39 B0 A3 D7 0A 3D"
   HBTEST _enc( NIL                                            ) IS "01"
   HBTEST _enc( .T.                                            ) IS "03"
   HBTEST _enc( .F.                                            ) IS "02"
   HBTEST _enc( { 1, -1 }                                      ) IS "09 05 01 04 01 04 FF FF FF FF"
   HBTEST _enc( { "ONE" => 0xcafe, "TWO" => 0xbabe }           ) IS "09 01 07 4F 4E 45 04 83 95 7E 07 54 57 4F 04 82 F5 3E 01"

   RETURN

STATIC FUNCTION _enc( a )
   RETURN hb_StrToHex( amf3_Encode( a ), " " )

#if 0
STATIC FUNCTION _encsum( a ) /* for longer values */
   RETURN hb_NumToHex( hb_CRC( amf3_Encode( a ) ), 4 )
#endif
