/* Copyright 2009 Viktor Szakats (vszakats.net/harbour) */

#require "hbssl"

PROCEDURE Main()

   LOCAL ctx
   LOCAL result
   LOCAL encoded
   LOCAL decoded

   SSL_init()

   OpenSSL_add_all_ciphers()

   ctx := hb_EVP_ENCODE_ctx_create()

   EVP_EncodeInit( ctx )

   encoded := ""
   result := ""
   EVP_EncodeUpdate( ctx, @result, "sample text" )
   encoded += result
   EVP_EncodeFinal( ctx, @result )
   encoded += result
   ? "ENCODED", ">" + hb_StrToHex( encoded ) + "<"

   ctx := hb_EVP_ENCODE_ctx_create()

   EVP_DecodeInit( ctx )

   decoded := ""
   result := ""
   EVP_DecodeUpdate( ctx, @result, encoded )
   decoded += result
   EVP_DecodeFinal( ctx, @result )
   decoded += result
   ? "DECODED", ">" + decoded + "<"

   RETURN
