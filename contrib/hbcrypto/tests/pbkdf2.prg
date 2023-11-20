/* Copyright 2013-2016 Viktor Szakats */

#require "hbcrypto"
#require "hbtest"

PROCEDURE Main()

   /* https://tools.ietf.org/html/rfc7914 */

   HBTEST Lower( hb_StrToHex( hb_pbkdf2_sha256( "passwd"  , "salt",     1, 64 ) ) ) IS "55ac046e56e3089fec1691c22544b605f94185216dde0465e68b9d57c20dacbc49ca9cccf179b645991664b39d77ef317c71b845b1e30bd509112041d3a19783"
   HBTEST Lower( hb_StrToHex( hb_pbkdf2_sha256( "Password", "NaCl", 80000, 64 ) ) ) IS "4ddcd8f60b98be21830cee5ef22701f9641a4418d04c0414aeff08876b34ab56a1d425a1225833549adb841b51c9b3176a272bdebba1d078478f62b397f33c8d"

   RETURN
