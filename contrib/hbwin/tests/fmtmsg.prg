/* Copyright 2013 Viktor Szakats (vszakats.net/harbour) */

#require "hbwin"

PROCEDURE Main()

   LOCAL cMsg

   cMsg := Space( 0 )  ; ? wapi_FormatMessage( ,, 1324,, @cMsg ), ">" + cMsg + "<"
   cMsg := Space( 128 ); ? wapi_FormatMessage( ,, 1324,, @cMsg ), ">" + cMsg + "<"
   cMsg := Space( 128 ); ? wapi_FormatMessage( ,, 1244,, @cMsg ), ">" + cMsg + "<"

   RETURN
