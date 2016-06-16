PROCEDURE Main()

   LOCAL cText := "This is my secret message."
   LOCAL cKey := hb_blowfishKey( "Top Secret ;-)" )

   LOCAL cEncrypted

   ? "Original:", hb_StrToExp( cText )

   /* encrypt data */
   ? "Encrypted:", hb_StrToHex( cEncrypted := hb_blowfishEncrypt( cKey, cText ) )

   /* decrypt data */
   ? "Decrypted:", hb_StrToExp( hb_blowfishDecrypt( cKey, cEncrypted ) )

   RETURN
