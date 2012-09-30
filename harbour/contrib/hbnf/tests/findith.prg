/*
 * $Id$
 */

PROCEDURE Main( cCk, cStr, nOcc, xCase )

   LOCAL nFind

   IF PCount() != 4
      QOut( "usage: findith cCk cStr nOcc xCase" )
      QUIT
   ENDIF

   xCase := iif( xCase == "Y", .T. , .F. )
   nOcc  := Val( nOcc )
   QOut( iif( xCase, "Ignoring ", "Observing " ) + "case:" )

   QOut( cStr )
   nFind := FT_FINDITH( cCk, cStr, nOcc, xCase )
   QOut( iif( nFind > 0, Space( nFind - 1 ) + "^" , "Not found" ) )

   RETURN
