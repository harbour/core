// Testing Harbour ProcName() and ProcLine()

PROCEDURE Main()

   Two()

   RETURN

STATIC PROCEDURE Two()

   Three()

   RETURN

STATIC PROCEDURE Three()

   Four()

   RETURN

STATIC PROCEDURE Four()

   Five()

   RETURN

STATIC PROCEDURE Five()

   LOCAL n := 0

   DO WHILE ! Empty( ProcName( n ) )
      ?? "Called from:", ProcName( n ), ProcLine( n++ ), hb_eol()
   ENDDO

   RETURN
