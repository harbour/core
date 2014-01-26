/* Copyright 2013 Viktor Szakats (vszakats.net/harbour) */

#require "hbwin"

PROCEDURE Main()

   LOCAL cMsg

   cMsg := Space( 0 )  ; ? wapi_FormatMessage( ,, 1324,, @cMsg ), ">" + cMsg + "<"
   cMsg := Space( 128 ); ? wapi_FormatMessage( ,, 1324,, @cMsg ), ">" + cMsg + "<"
   cMsg := Space( 128 ); ? wapi_FormatMessage( ,, 1244,, @cMsg ), ">" + cMsg + "<"

   ? "|" + win_ErrorDesc() + "|"
   ? "|" + win_ErrorDesc( 5 ) + "|"
   ? "|" + win_ErrorDesc( 1324 ) + "|"
   ? "|" + win_ErrorDesc( 1244 ) + "|"

   RETURN
