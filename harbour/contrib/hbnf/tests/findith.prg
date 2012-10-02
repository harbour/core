/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main( cCk, cStr, nOcc, xCase )

   LOCAL nFind

   IF PCount() != 4
      ? "usage: findith cCk cStr nOcc xCase"
      QUIT
   ENDIF

   xCase := iif( xCase == "Y", .T. , .F. )
   nOcc  := Val( nOcc )
   ? iif( xCase, "Ignoring ", "Observing " ) + "case:"

   ? cStr
   nFind := FT_FINDITH( cCk, cStr, nOcc, xCase )
   ? iif( nFind > 0, Space( nFind - 1 ) + "^" , "Not found" )

   RETURN
