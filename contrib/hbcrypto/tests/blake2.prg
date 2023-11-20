/* Copyright 2015 Viktor Szakats */

#require "hbcrypto"
#require "hbtest"

PROCEDURE Main()

   LOCAL cKey

   /* https://blake2.net/blake2b-test.txt */

   cKey := hb_HexToStr( "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f" )

   HBTEST hb_blake2b( "" )                                            IS "786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce"
   HBTEST hb_blake2b( "The quick brown fox jumps over the lazy dog" ) IS "a8add4bdddfd93e4877d2746e62817b116364a1fa7bc148d95090bc7333b3673f82401cf7aa2e4cb1ecd90296e3f14cb5413f8ed77be73045b13914cdcd6a918"
   HBTEST hb_blake2b( "", cKey )                                      IS "10ebb67700b1868efb4417987acf4690ae9d972fb7a590c2f02871799aaa4786b5e996e8f0f4eb981fc214b005f42d2ff4233499391653df7aefcbc13fc51568"

   /* https://blake2.net/blake2s-test.txt */

   cKey := hb_HexToStr( "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f" )

   HBTEST hb_blake2s( "" )                                            IS "69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9"
   HBTEST hb_blake2s( "The quick brown fox jumps over the lazy dog" ) IS "606beeec743ccbeff6cbcdf5d5302aa855c256c29b88c8ed331ea1a6bf3c8812"
   HBTEST hb_blake2s( "", cKey )                                      IS "48a8997da407876b3d79c0d92325ad3b89cbb754d86ab71aee047ad345fd2c49"

   HBTEST hb_blake2s( "",, 16 )                                            IS "64550d6ffe2c0a01a14aba1eade0200c"
   HBTEST hb_blake2s( "The quick brown fox jumps over the lazy dog",, 16 ) IS "96fd07258925748a0d2fb1c8a1167a73"

   RETURN
