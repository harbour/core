/*
 * $Id$
 */

// Testing Harbour ProcName() and ProcLine()

PROCEDURE Main()

   Two()

   RETURN

FUNCTION Two()

   Three()

   RETURN NIL

FUNCTION Three()

   Four()

   RETURN NIL

FUNCTION Four()

   Five()

   RETURN NIL

FUNCTION Five()

   LOCAL n := 0

   WHILE ! Empty( ProcName( n ) )
      ?? "Called from: ", ProcName( n ), ProcLine( n++ ), hb_eol()
   ENDDO

   RETURN NIL
