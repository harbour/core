/* Copyright 2014-2016 Viktor Szakats (vszakats.net/harbour) */

#require "hbmac"

PROCEDURE Main()

   ? mac_ProxyDetect()
   ? mac_ProxyDetect( "http" )
   ? mac_ProxyDetect( "https" )
   ? mac_ProxyDetect( "ftp" )
   ? mac_ProxyDetect( "gopher" )
   ? mac_ProxyDetect( "garbage" )

   RETURN
