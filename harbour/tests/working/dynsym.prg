/*
 * $Id$
 */

FUNCTION Main()
   LOCAL nCount := __dynsymCount()
   LOCAL nPos

   FOR nPos := 1 TO nCount
      OutStd( __dynsymGetName( nPos ), Chr(13) + Chr(10) )
   NEXT

   RETURN NIL
