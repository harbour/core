/* Copyright 2014 Viktor Szakats (vszakats.net/harbour) */

#require "hbcity"
#require "hbtest"

PROCEDURE Main()

   /* test data taken from: https://github.com/nashby/cityhash */

   LOCAL cString := "test"
   LOCAL nSeed1 := 12345
   LOCAL nSeed2 := 54321

   HBTEST cityhash32( cString )                 IS 1633095781
   HBTEST cityhash64( cString )                 IS 8581389452482819506
   HBTEST cityhash64( cString, nSeed1 )         IS 9154302171269876511
   HBTEST cityhash64( cString, nSeed1, nSeed2 ) IS 4854399283587686019
   HBTEST cityhash128( cString )                IS 124124989950401219618153994964897029896
   HBTEST cityhash128( cString, nSeed1 )        IS 101668641288246442316643001405184598611
#if 0
   HBTEST cityhash128crc( cString )             IS "124124989950401219618153994964897029896"
   HBTEST cityhash128crc( cString, nSeed1 )     IS "101668641288246442316643001405184598611"
#endif

   RETURN
