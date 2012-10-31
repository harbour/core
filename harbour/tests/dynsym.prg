/*
 * $Id$
 */

PROCEDURE Main()

   LOCAL nCount := __dynSCount()
   LOCAL nPos

   FOR nPos := 1 TO nCount
      ? __dynSGetName( nPos )
   NEXT

   nPos := __dynSGetIndex( "MAIN" )
   ? "MAIN", nPos

   ? __dynSGetName( nPos )
   ? __dynSGetName()
   ? __dynSGetName( 0 )
   ? __dynSGetName( 100000 )
   ? __dynSGetName( __dynSGetIndex( "HB_THISDOESNTEXIST_" ) )

   RETURN
