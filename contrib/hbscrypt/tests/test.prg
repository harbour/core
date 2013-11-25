/* Copyright 2013 Viktor Szakats (vszakats.net/harbour) */

#require "hbscrypt"

PROCEDURE Main()

   LOCAL tmp

   /* https://tools.ietf.org/html/draft-josefsson-scrypt-kdf-00 */

   ? "hb_pbkdf2_sha256()"
   ? Lower( hb_StrToHex( tmp := hb_pbkdf2_sha256( "passwd"  , "salt",     1, 64 ) ) ) == "55ac046e56e3089fec1691c22544b605f94185216dde0465e68b9d57c20dacbc49ca9cccf179b645991664b39d77ef317c71b845b1e30bd509112041d3a19783", hb_BLen( tmp )
   ? Lower( hb_StrToHex( tmp := hb_pbkdf2_sha256( "Password", "NaCl", 80000, 64 ) ) ) == "4ddcd8f60b98be21830cee5ef22701f9641a4418d04c0414aeff08876b34ab56a1d425a1225833549adb841b51c9b3176a272bdebba1d078478f62b397f33c8d", hb_BLen( tmp )
   ?

   ? "hb_scrypt()"
   ? Lower( hb_StrToHex( tmp := hb_scrypt( ""             , ""              ,      16, 1,  1, 64 ) ) ) == "77d6576238657b203b19ca42c18a0497f16b4844e3074ae8dfdffa3fede21442fcd0069ded0948f8326a753a0fc81f17e8d3e0fb2e0d3628cf35e20c38d18906", hb_BLen( tmp )
   ? Lower( hb_StrToHex( tmp := hb_scrypt( "password"     , "NaCl"          ,    1024, 8, 16, 64 ) ) ) == "fdbabe1c9d3472007856e7190d01e9fe7c6ad7cbc8237830e77376634b3731622eaf30d92e22a3886ff109279d9830dac727afb94a83ee6d8360cbdfa2cc0640", hb_BLen( tmp )
   ? Lower( hb_StrToHex( tmp := hb_scrypt( "pleaseletmein", "SodiumChloride",   16384, 8,  1, 64 ) ) ) == "7023bdcb3afd7348461c06cd81fd38ebfda8fbba904f8e3ea9b543f6545da1f2d5432955613f0fcf62d49705242a9af9e61e85dc0d651e40dfcf017b45575887", hb_BLen( tmp )
   // takes very long time
// ? Lower( hb_StrToHex( tmp := hb_scrypt( "pleaseletmein", "SodiumChloride", 1048576, 8,  1, 64 ) ) ) == "2101cb9b6a511aaeaddbbe09cf70f881ec568d574a2ffd4dabe5ee9820adaa478e56fd8f4ba5d09ffa1c6d927c40f4c337304049e8a952fbcbf45c6fa77a41a4", hb_BLen( tmp )
   ?

   ? "hb_strcmpc()"
   ? .F. == hb_strcmpc( NIL, NIL )
   ? .F. == hb_strcmpc( NIL, "" )
   ? .F. == hb_strcmpc( "1", "12" )
   ? .F. == hb_strcmpc( "12", "1" )
   ? .T. == hb_strcmpc( "12", "12" )
   ? .F. == hb_strcmpc( "12", "13" )
   ? .F. == hb_strcmpc( "22", "12" )
   ? .T. == hb_strcmpc( "", "" )

   RETURN
