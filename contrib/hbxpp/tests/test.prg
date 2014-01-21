/* Copyright 2014 Viktor Szakats (vszakats.net/harbour) */

#require "hbtest"
#require "hbxpp"

PROCEDURE Main()

   /* TODO: verify results against Xbase++ */
   HBTEST ARemove()                      IS NIL
   HBTEST ARemove( { 1, 2, 3 } )         IS "{3}"
   HBTEST ARemove( { 1, 2, 3 }, -1 )     IS "E 1 BASE 1003 Argument error (AREMOVE) OS:0 #:0"
   HBTEST ARemove( { 1, 2, 3 }, 1 )      IS "{1}"
   HBTEST ARemove( { 1, 2, 3 }, 2 )      IS "{2}"
   HBTEST ARemove( { 1, 2, 3 }, 2, -1 )  IS "E 1 BASE 1003 Argument error (AREMOVE) OS:0 #:0"
   HBTEST ARemove( { 1, 2, 3 }, 2, 0 )   IS "{}"
   HBTEST ARemove( { 1, 2, 3 }, 2, 1 )   IS "{2}"
   HBTEST ARemove( { 1, 2, 3 }, 2, 2 )   IS "{2, 3}"
   HBTEST ARemove( { 1, 2, 3 }, 2, 3 )   IS "{2, 3}"
   HBTEST ARemove( { 1, 2, 3 }, 1, 3 )   IS "{1, 2, 3}"
   HBTEST ARemove( { 1, 2, 3 }, 1, 2 )   IS "{1, 2}"
   HBTEST ARemove( { 1, 2, 3 }, -1, -2 ) IS "E 1 BASE 1003 Argument error (AREMOVE) OS:0 #:0"

   RETURN
