/* RFC4648 test vectors for base64 */

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL cVector, cStr

   LOCAL hTestVectors := { ;
      ""       => "", ;
      "f"      => "Zg==", ;
      "fo"     => "Zm8=", ;
      "foo"    => "Zm9v", ;
      "foob"   => "Zm9vYg==", ;
      "fooba"  => "Zm9vYmE=", ;
      "foobar" => "Zm9vYmFy" }

   FOR EACH cVector IN hTestVectors

      cStr := hb_base64Encode( cVector:__enumKey )
      IF !( cStr == cVector )
         ? hb_StrFormat( "hb_base64Encode(): expected '%s' got '%s' while encoding '%s'", ;
            cVector:__enumKey(), cStr, cVector )
      ELSE
         ? hb_StrFormat( "hb_base64Encode(): passed '%s'", cVector:__enumKey )
      ENDIF

      cStr := hb_base64Decode( cVector )
      IF !( cStr == cVector:__enumKey() )
         ? hb_StrFormat( "hb_base64Decode(): expected '%s' got '%s' while decoding '%s'", ;
            cVector, cStr, cVector:__enumKey() )
      ELSE
         ? hb_StrFormat( "hb_base64Decode(): passed '%s'", cVector )
      ENDIF
   NEXT

   RETURN
