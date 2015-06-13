/* Copyright 2015 Viktor Szakats (vszakats.net/harbour) */

#require "hbb2"
#require "hbtest"

PROCEDURE Main()

   /* https://tools.ietf.org/html/draft-saarinen-blake2-03 */

   HBTEST hb_BLAKE2b( "" )                                            IS "786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce"
   HBTEST hb_BLAKE2b( "The quick brown fox jumps over the lazy dog" ) IS "a8add4bdddfd93e4877d2746e62817b116364a1fa7bc148d95090bc7333b3673f82401cf7aa2e4cb1ecd90296e3f14cb5413f8ed77be73045b13914cdcd6a918"

   RETURN
