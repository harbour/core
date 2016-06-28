/* Copyright 2015 Viktor Szakats (vszakats.net/harbour) */

#require "hbcrypto"
#require "hbtest"

PROCEDURE Main()

   LOCAL cBuf, cKey, cEnc, tmp

   cBuf := "My testing clear text"
   cKey := "aserghjerg4w5ygb8JHBr8ySuhrergiE"  /* 32-byte key */

   HBTEST Lower( hb_StrToHex( cEnc := hb_aes256_encrypt_ecb( cBuf, cKey ) ) ) IS "77f6121c51b179e26135d31e534ba4aca00ff746c2d12f94a445b3d55ff13a4e"
   HBTEST                             hb_aes256_decrypt_ecb( cEnc, cKey )     IS cBuf

   cBuf := ""
   FOR tmp := 0 TO 15
      cBuf += hb_BChar( tmp * 16 + tmp )
   NEXT
   cKey := ""
   FOR tmp := 0 TO 31
      cKey += hb_BChar( tmp )
   NEXT

   HBTEST Lower( hb_StrToHex( cEnc := hb_aes256_encrypt_ecb( cBuf, cKey ) ) ) IS "8ea2b7ca516745bfeafc49904b4960892b347c88e5c9c8ff0b7a121b687bd06d"
   HBTEST Lower( hb_StrToHex(         hb_aes256_decrypt_ecb( cEnc, cKey ) ) ) IS Lower( hb_StrToHex( cBuf ) )

   RETURN
