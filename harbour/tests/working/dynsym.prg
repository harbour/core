/*
 * $Id$
 */

FUNCTION Main()
   LOCAL nCount := __dynsCount()
   LOCAL nPos

   FOR nPos := 1 TO nCount
      OutStd( __dynsGetName( nPos ), Chr(13) + Chr(10) )
   NEXT

   nPos := __dynsGetIndex( "MAIN" )
   ? "MAIN", nPos

   ? __dynsGetName( nPos )
   
   RETURN NIL
