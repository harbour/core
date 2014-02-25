/* Copyright 2014 Viktor Szakats (vszakats.net/harbour) */

#require "hbosx"

PROCEDURE Main()

   ? osx_ProxyGet()
   ? osx_ProxyGet( "http" )
   ? osx_ProxyGet( "https" )
   ? osx_ProxyGet( "ftp" )
   ? osx_ProxyGet( "gopher" )
   ? osx_ProxyGet( "garbage" )

   RETURN
