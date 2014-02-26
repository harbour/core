/* Copyright 2009 Viktor Szakats (vszakats.net/harbour) */

#require "hbssl"

PROCEDURE Main()

   LOCAL ctx
   LOCAL digest

   LOCAL key
   LOCAL iv

   SSL_init()

   OpenSSL_add_all_digests()
   OpenSSL_add_all_ciphers()

   ? "Version built against:", hb_NumToHex( OPENSSL_VERSION() )
   ? "Version loaded:", hb_NumToHex( SSLeay() )

   ctx := EVP_MD_CTX_create()
   EVP_MD_CTX_init( ctx )

   EVP_DigestInit_ex( ctx, "SHA256" )
   EVP_DigestUpdate( ctx, "sample text" )
   digest := ""
   EVP_DigestFinal( ctx, @digest )
   ? "SHA256", ">" + hb_StrToHex( digest ) + "<"

   EVP_DigestInit_ex( ctx, HB_EVP_MD_SHA256 )
   EVP_DigestUpdate( ctx, "sample text" )
   digest := ""
   EVP_DigestFinal( ctx, @digest )
   ? "SHA256", ">" + hb_StrToHex( digest ) + "<"

   EVP_MD_CTX_cleanup( ctx )

   EVP_DigestInit_ex( ctx, HB_EVP_MD_RIPEMD160 )
   EVP_DigestUpdate( ctx, "sample text" )
   digest := ""
   EVP_DigestFinal( ctx, @digest )
   ? "RIPEMD160", ">" + hb_StrToHex( digest ) + "<"

   key := ""
   iv := ""
   ? "EVP_BytesToKey", EVP_BytesToKey( HB_EVP_CIPHER_AES_192_OFB, HB_EVP_MD_SHA256, "salt1234", "data", 2, @key, @iv )
   ? "KEY", hb_StrToHex( key )
   ? ">" + key + "<"
   ? "IV", hb_StrToHex( iv )
   ? ">" + iv + "<"

   key := ""
   iv := ""
   ? "EVP_BytesToKey", EVP_BytesToKey( "AES-192-OFB", "SHA256", "salt1234", "data", 2, @key, @iv )
   ? "KEY", hb_StrToHex( key )
   ? ">" + key + "<"
   ? "IV", hb_StrToHex( iv )
   ? ">" + iv + "<"

   key := ""
   iv := ""
   ? "EVP_BytesToKey", EVP_BytesToKey( "AES-192-OFB", "SHA256",, "data", 2, @key, @iv )
   ? "KEY", hb_StrToHex( key )
   ? ">" + key + "<"
   ? "IV", hb_StrToHex( iv )
   ? ">" + iv + "<"

   EVP_cleanup()

   RETURN
