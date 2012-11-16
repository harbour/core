/*
 * $Id$
 */

/* RFC4648 test vectors for base64 */

REQUEST HB_GT_CGI_DEFAULT

PROCEDURE Main()

   LOCAL aTestVectors, aVector, cStr

   aTestVectors := {          ;
      ""       => "",         ;
      "f"      => "Zg==",     ;
      "fo"     => "Zm8=",     ;
      "foo"    => "Zm9v",     ;
      "foob"   => "Zm9vYg==", ;
      "fooba"  => "Zm9vYmE=", ;
      "foobar" => "Zm9vYmFy" }

   FOR EACH aVector IN aTestVectors

      cStr := hb_base64Encode( aVector:__enumKey )
      IF cStr != aVector
         ? hb_StrFormat( "hb_base64Encode(): expected '%s' got '%s' while encoding '%s'", ;
            aVector:__enumKey(), cStr, aVector )
      ELSE
         ? hb_StrFormat( "hb_base64Encode(): passed '%s'", aVector:__enumKey )
      ENDIF

      cStr := hb_base64Decode( aVector )
      IF cStr != aVector:__enumKey()
         ? hb_StrFormat( "hb_base64Decode(): expected '%s' got '%s' while decoding '%s'", ;
            aVector, cStr, aVector:__enumKey() )
      ELSE
         ? hb_StrFormat( "hb_base64Decode(): passed '%s'", aVector )
      ENDIF
   NEXT

   RETURN
