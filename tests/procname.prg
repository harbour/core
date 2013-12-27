// Testing Harbour ProcName() and ProcLine()

PROCEDURE Main()

   Two()

   RETURN

STATIC FUNCTION Two()

   Three()

   RETURN NIL

STATIC FUNCTION Three()

   Four()

   RETURN NIL

STATIC FUNCTION Four()

   Five()

   RETURN NIL

STATIC FUNCTION Five()

   LOCAL n := 0

   WHILE ! Empty( ProcName( n ) )
      ?? "Called from:", ProcName( n ), ProcLine( n++ ), hb_eol()
   ENDDO

   RETURN NIL
