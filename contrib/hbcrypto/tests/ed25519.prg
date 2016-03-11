/* Copyright 2015 Viktor Szakats (vszakats.net/harbour) */

#require "hbcrypto"
#require "hbtest"

PROCEDURE Main()

   LOCAL message
   LOCAL secret_key
   LOCAL public_key
   LOCAL signature
   LOCAL shared_secret

   /* https://ed25519.cr.yp.to */

   HBTEST hb_ed25519_create_keypair( @public_key, @secret_key )                     IS NIL
   HBTEST ValType( public_key )                                                     IS "C"
   HBTEST hb_BLen( public_key )                                                     IS 32
   HBTEST ValType( secret_key )                                                     IS "C"
   HBTEST hb_BLen( secret_key )                                                     IS 64
   HBTEST hb_ed25519_sign()                                                         IS "E 1 BASE 3013 Argument error (HB_ED25519_SIGN) OS:0 #:0 F:S"
   message := hb_MemoRead( __FILE__ )
   HBTEST hb_ed25519_get_pubkey( secret_key ) == public_key                         IS .T.
   signature := hb_ed25519_sign( message, public_key, secret_key )
   HBTEST ValType( signature )                                                      IS "C"
   HBTEST hb_BLen( signature )                                                      IS 64
   HBTEST hb_ed25519_verify( signature, message, public_key )                       IS .T.
   HBTEST hb_ed25519_verify( signature, message + "a", public_key )                 IS .F.
   HBTEST hb_ed25519_verify( hb_BLeft( signature, 63 ) + "a", message, public_key ) IS .F.
   HBTEST hb_ed25519_verify( signature, message, hb_BLeft( signature, 31 ) + "a" )  IS .F.
#if 0
   shared_secret := hb_ed25519_key_exchange( public_key, secret_key )
   HBTEST ValType( shared_secret )                                                  IS "C"
   HBTEST hb_BLen( shared_secret )                                                  IS 32
#endif

   message := "TEST MESSAGE"
   public_key := hb_base64Decode( "WHPNZbNuFk9ZlYCMxCLnWAKH936bj0ITv5RSKXhRoZ8=" )
   secret_key := hb_base64Decode( "4MJZ7MmqUmX5zUFLPScFa5PmctrHoUKv0Ah9aU6kREbYPhqR3UWGlQ8af0F9fSXOHaSDimjiTi3R8CQ0GhnxtQ==" )
   HBTEST hb_base64Encode( hb_ed25519_sign( message, public_key, secret_key ) )     IS "VZuUsB2NEaMEbPilujIupXd8WxNEffSC5r9v0xO5gEmcF4g1mqxVJU8CsOYiAZPFE6h7nlQz2840K/JiwcblAQ=="

   RETURN
