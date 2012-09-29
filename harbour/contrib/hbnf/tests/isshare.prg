/*
 * $Id$ 
 */

PROCEDURE Main()

   LOCAL nLoaded := ft_isshare()

   DO CASE
   CASE nLoaded == 0
      QOut( "Share not loaded, but ok to load" )
   CASE nLoaded == 1
      QOut( "Share not loaded, but NOT ok to load!" )
   CASE nLoaded == 255
      QOut( "Share is loaded!" )
   ENDCASE

   QOut( "Retcode: " + Str( nLoaded ) )

   RETURN

