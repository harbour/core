/*
 * $Id$
 */

#require "hbamf"
#require "hbtest"

REQUEST HB_CODEPAGE_UTF8EX

PROCEDURE Main()

   hb_cdpSelect( "UTF8EX" )

   HBTEST amf3_Encode( {}                                             ) IS e"\011\001\001"
   HBTEST amf3_Encode( "a"                                            ) IS e"\006\003a"
   HBTEST amf3_Encode( "ą"                                            ) IS e"\006\005\304\205"
   HBTEST amf3_Encode( 1                                              ) IS e"\004\001"
   HBTEST amf3_Encode( 1000                                           ) IS e"\004\207h"
   HBTEST amf3_Encode( 1000000                                        ) IS e"\004\275\204@"
   HBTEST amf3_Encode( 268435455                                      ) IS e"\004\277\377\377\377"
   HBTEST amf3_Encode( 268435456                                      ) IS e"\005A\260\000\000\000\000\000\000"
   HBTEST amf3_Encode( 268435456000                                   ) IS e"\005BO@\000\000\000\000\000"
   HBTEST amf3_Encode( -1                                             ) IS e"\004\377\377\377\377"
   HBTEST amf3_Encode( 9007199254740990                               ) IS e"\005C?\377\377\377\377\377\376"
   HBTEST amf3_Encode( amf3_Decode( amf3_Encode( 9007199254740990 ) ) ) IS e"\005C?\377\377\377\377\377\376"
   HBTEST amf3_Encode( 9007199254740991                               ) IS e"\005C?\377\377\377\377\377\377"
   HBTEST amf3_Encode( amf3_Decode( amf3_Encode( 9007199254740991 ) ) ) IS e"\005C?\377\377\377\377\377\377"
   HBTEST amf3_Encode( 9007199254740991.00                            ) IS e"\005C?\377\377\377\377\377\377"
   HBTEST amf3_Encode( 6969.69                                        ) IS e"\005@\2739\260\243\327\012="
   HBTEST amf3_Encode( NIL                                            ) IS e"\001"
   HBTEST amf3_Encode( .T.                                            ) IS e"\003"
   HBTEST amf3_Encode( .F.                                            ) IS e"\002"
   HBTEST amf3_Encode( { 1, -1 }                                      ) IS e"\011\005\001\004\001\004\377\377\377\377"
   HBTEST amf3_Encode( { "ONE" => 0xCAFE, "TWO" => 0xBABE }           ) IS e"\011\001\007ONE\004\203\225~\007TWO\004\202\365>\001"

   RETURN

#if 0
STATIC FUNCTION _enchex( a )  /* for hex notation */
   RETURN hb_StrToHex( amf3_Encode( a ), " " )

STATIC FUNCTION _encsum( a )  /* for longer values */
   RETURN hb_NumToHex( hb_CRC( amf3_Encode( a ) ), 4 )
#endif
