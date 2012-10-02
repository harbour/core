/*
 * $Id$
 */

PROCEDURE Main()
   LOCAL nCount := __dynsCount()
   LOCAL nPos

   FOR nPos := 1 TO nCount
      ? __dynsGetName( nPos )
   NEXT

   nPos := __dynsGetIndex( "MAIN" )
   ? "MAIN", nPos

   ? __dynsGetName( nPos )
   ? __dynsGetName()
   ? __dynsGetName( 0 )
   ? __dynsGetName( 100000 )
   ? __dynsGetName( __dynsGetIndex( "HB_THISDOESNTEXIST_" ) )

   RETURN
