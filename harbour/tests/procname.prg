/*
 * $Id$
 */

// Testing Harbour ProcName() and ProcLine()

#define CRLF Chr( 13 ) + Chr( 10 )

PROCEDURE Main()

   Two()

   RETURN

FUNCTION Two()

   Three()

   RETURN nil

FUNCTION Three()

   Four()

   RETURN nil

FUNCTION Four()

   Five()

   RETURN nil

FUNCTION Five()

   LOCAL n := 0

   WHILE ! Empty( ProcName( n ) )
      QQOut( "Called from: ", ProcName( n ), ProcLine( n ++ ), CRLF )
   ENDDO

   RETURN nil
