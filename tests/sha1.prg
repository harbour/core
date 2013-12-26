/* Copyright 2009 Viktor Szakats (vszakats.net/harbour) */

PROCEDURE Main()

   ? hb_SHA1( "hello" )
   ? hb_SHA1( "hello", .F. ) == Lower( hb_StrToHex( hb_SHA1( "hello", .T. ) ) )

   ? Lower( hb_SHA1( "abc"                                                                               ) ) == "a9993e364706816aba3e25717850c26c9cd0d89d"
   ? Lower( hb_SHA1( "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"                          ) ) == "84983e441c3bd26ebaae4aa1f95129e5e54670f1"
   ? Lower( hb_SHA1( Replicate( "a", 1000000 )                                                           ) ) == "34aa973cd4c4daa4f61eeb2bdbad27316534016f"
   ? Lower( hb_SHA1( Replicate( "0123456701234567012345670123456701234567012345670123456701234567", 10 ) ) ) == "dea356a2cddd90c7a7ecedc5ebb563934f460452"

   ? hb_HMAC_SHA1( "hello", "key" )
   ? hb_HMAC_SHA1( "hello", "key", .F. ) == Lower( hb_StrToHex( hb_HMAC_SHA1( "hello", "key", .T. ) ) )

   RETURN
