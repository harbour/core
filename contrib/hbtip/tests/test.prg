/* Copyright 2014 Viktor Szakats (vszakats.net/harbour) */

#require "hbtip"
#require "hbtest"

PROCEDURE Main()

   HBTEST tip_MimeType( "MZ" )                  IS "application/x-dosexec"
   HBTEST tip_MimeType( "hello" )               IS "unknown"
   HBTEST tip_MimeType( "hello", "my-unknown" ) IS "my-unknown"

   RETURN
