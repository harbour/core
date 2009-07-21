/*
 * $Id$
 */

/*
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://www.harbour-project.org
 */

#include "simpleio.ch"

#include "hbssl.ch"

PROCEDURE Main()

   LOCAL ctx
   LOCAL result
   LOCAL encrypted
   LOCAL decrypted

   LOCAL cKey := "key"

   OpenSSL_add_all_ciphers()

   ctx := hb_EVP_CIPHER_CTX_create()
   EVP_CIPHER_CTX_init( ctx )

   EVP_EncryptInit( ctx, "AES-192-OFB", cKey )
   ? EVP_CIPHER_CTX_cipher( ctx )
   ? EVP_CIPHER_block_size( EVP_CIPHER_CTX_cipher( ctx ) )

   encrypted := ""
   result := ""
   EVP_EncryptUpdate( ctx, @result, "sample text" )
   encrypted += result
   EVP_EncryptFinal( ctx, @result )
   encrypted += result
   ? "ENCRYTPTED", ">" + hb_StrToHex( encrypted ) + "<"

   EVP_CIPHER_CTX_init( ctx )
   EVP_CIPHER_CTX_cleanup( ctx )

   EVP_DecryptInit( ctx, "AES-192-OFB", cKey )

   decrypted := ""
   result := ""
   EVP_DecryptUpdate( ctx, @result, encrypted )
   decrypted += result
   EVP_DecryptFinal( ctx, @result )
   decrypted += result
   ? "DECRYTPTED", ">" + decrypted + "<"

   EVP_cleanup()

   RETURN
