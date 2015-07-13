/* Copyright 2015 Viktor Szakats (vszakats.net/harbour) */

#require "hbcrypto"
#require "hbtest"

PROCEDURE Main()

   LOCAL message
   LOCAL private_key
   LOCAL public_key
   LOCAL signature
   LOCAL shared_secret

   /* http://ed25519.cr.yp.to */

   HBTEST ed25519_create_keypair( @public_key, @private_key )                    IS NIL
   HBTEST ValType( public_key )                                                  IS "C"
   HBTEST hb_BLen( public_key )                                                  IS 32
   HBTEST ValType( private_key )                                                 IS "C"
   HBTEST hb_BLen( private_key )                                                 IS 64
   HBTEST ed25519_sign( @signature )                                             IS "E 1 BASE 3013 Argument error (ED25519_SIGN) OS:0 #:0 F:S"
   message := hb_MemoRead( __FILE__ )
   HBTEST ed25519_sign( @signature, message, public_key, private_key )           IS NIL
   HBTEST ValType( signature )                                                   IS "C"
   HBTEST hb_BLen( signature )                                                   IS 64
   HBTEST ed25519_verify( signature, message, public_key )                       IS .T.
   HBTEST ed25519_verify( signature, message + "a", public_key )                 IS .F.
   HBTEST ed25519_verify( hb_BLeft( signature, 63 ) + "a", message, public_key ) IS .F.
   HBTEST ed25519_verify( signature, message, hb_BLeft( signature, 31 ) + "a" )  IS .F.
   HBTEST ed25519_key_exchange( @shared_secret, public_key, private_key )        IS NIL
   HBTEST ValType( shared_secret )                                               IS "C"
   HBTEST hb_BLen( shared_secret )                                               IS 32

   public_key := hb_base64Decode( "WHPNZbNuFk9ZlYCMxCLnWAKH936bj0ITv5RSKXhRoZ8=" )
   private_key := hb_base64Decode( "4MJZ7MmqUmX5zUFLPScFa5PmctrHoUKv0Ah9aU6kREbYPhqR3UWGlQ8af0F9fSXOHaSDimjiTi3R8CQ0GhnxtQ==" )
   message := "TEST MESSAGE"
   HBTEST ed25519_sign( @signature, message, public_key, private_key )           IS NIL
   HBTEST hb_base64Encode( signature )                                           IS "VZuUsB2NEaMEbPilujIupXd8WxNEffSC5r9v0xO5gEmcF4g1mqxVJU8CsOYiAZPFE6h7nlQz2840K/JiwcblAQ=="

#if 1
   hb_MemoWrit( "pub.key", hb_base64Encode( public_key ) )
   hb_MemoWrit( "prv.key", hb_base64Encode( private_key ) )
   hb_MemoWrit( "msg.bin", message )
   hb_MemoWrit( "sign.bin", hb_base64Encode( signature ) )
#endif

   RETURN
