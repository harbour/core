/* Copyright 2009-2016 Viktor Szakats (vszakats.net/harbour) */

#require "hbssl"

PROCEDURE Main()

   LOCAL ctx
   LOCAL result
   LOCAL encoded
   LOCAL decoded

   SSL_init()

   OpenSSL_add_all_ciphers()

   ctx := EVP_ENCODE_CTX_new()

   EVP_EncodeInit( ctx )

   encoded := ""
   result := ""
   EVP_EncodeUpdate( ctx, @result, "sample text" )
   encoded += result
   EVP_EncodeFinal( ctx, @result )
   encoded += result
   ? "ENCODED", ">" + hb_StrToHex( encoded ) + "<"

   ctx := EVP_ENCODE_CTX_new()

   EVP_DecodeInit( ctx )

   decoded := ""
   result := ""
   EVP_DecodeUpdate( ctx, @result, encoded )
   decoded += result
   EVP_DecodeFinal( ctx, @result )
   decoded += result
   ? "DECODED", ">" + decoded + "<"

   RETURN
