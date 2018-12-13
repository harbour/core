/*
 *
 * Rewritten from C: Viktor Szakats (vszakats.net/harbour)
 */

PROCEDURE Main()

   ? ">" + hb_SHA1( "hello" ) + "<"
   ? ">" + hb_SHA1( "hello", .F. ) + "<"
   ? ">" + hb_SHA1( "hello", .T. ) + "<"

   ? ">" + hb_HMAC_SHA1( "hello", "key" ) + "<"
   ? ">" + hb_HMAC_SHA1( "hello", "key", .F. ) + "<"
   ? ">" + hb_HMAC_SHA1( "hello", "key", .T. ) + "<"

   RETURN
