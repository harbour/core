/*
 * $Id$
 */

/*
 * Copyright 2009 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 */

#require "hbssl"

PROCEDURE Main()

   LOCAL ctx
   LOCAL result
   LOCAL encrypted
   LOCAL decrypted

   SSL_init()

   OpenSSL_add_all_ciphers()

   ctx := hb_EVP_ENCODE_ctx_create()

   EVP_EncodeInit( ctx )

   encrypted := ""
   result := ""
   EVP_EncodeUpdate( ctx, @result, "sample text" )
   encrypted += result
   EVP_EncodeFinal( ctx, @result )
   encrypted += result
   ? "ENCRYTPTED", ">" + encrypted + "<"

   ctx := hb_EVP_ENCODE_ctx_create()

   EVP_DecodeInit( ctx )

   decrypted := ""
   result := ""
   EVP_DecodeUpdate( ctx, @result, encrypted )
   decrypted += result
   EVP_DecodeFinal( ctx, @result )
   decrypted += result
   ? "DECRYTPTED", ">" + decrypted + "<"

   RETURN
