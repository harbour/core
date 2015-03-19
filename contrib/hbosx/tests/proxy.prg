/* Copyright 2014 Viktor Szakats (vszakats.net/harbour) */

#require "hbosx"

PROCEDURE Main()

   ? osx_ProxyDetect()
   ? osx_ProxyDetect( "http" )
   ? osx_ProxyDetect( "https" )
   ? osx_ProxyDetect( "ftp" )
   ? osx_ProxyDetect( "gopher" )
   ? osx_ProxyDetect( "garbage" )

   RETURN
