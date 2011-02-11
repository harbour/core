/*
 * $Id$
 */

/* RFC4648 test vectors for base64 */

#pragma warninglevel=3
#pragma exitseverity=2

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

      cStr := hb_base64encode( aVector:__enumKey )
      IF cStr != aVector
         OutStd( hb_strFormat( "hb_base64encode(): expected '%s' got '%s' while encoding '%s'" + hb_eol(), ;
                 aVector:__enumKey(), cStr, aVector ) )
      ELSE
         OutStd( hb_strFormat( "hb_base64encode(): passed '%s'" + hb_eol(), aVector:__enumKey ) )
      ENDIF

      cStr := hb_base64decode( aVector )
      IF cStr != aVector:__enumKey()
         OutStd( hb_strFormat( "hb_base64decode(): expected '%s' got '%s' while decoding '%s'" + hb_eol(), ;
                 aVector, cStr, aVector:__enumKey() ) )
      ELSE
         OutStd( hb_strFormat( "hb_base64decode(): passed '%s'" + hb_eol(), aVector ) )
      ENDIF 
   NEXT

   RETURN


