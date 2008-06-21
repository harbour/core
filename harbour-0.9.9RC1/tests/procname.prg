//
// $Id$
//

// Testing Harbour ProcName() and ProcLine()

#define CRLF Chr( 13 ) + Chr( 10 )

function Main()

   Two()

return nil

function Two()

   Three()

return nil

function Three()

   Four()

return nil

function Four()

   Five()

return nil

function Five()

   local n := 0

   while ! Empty( ProcName( n ) )
     QQOut( "Called from: ", ProcName( n ), ProcLine( n++ ), CRLF )
   end

return nil

