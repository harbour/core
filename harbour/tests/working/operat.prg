//
// $Id$
//

// Testing Harbour operators management

#define CRLF  Chr( 13 ) + Chr( 10 )

function Main()

   local a := 1
   local b := 2
   local c := 3

   QOut( "testing Harbour operators management" )

   QQOut( "a = ", a, CRLF )
   QQOut( "b = ", b, CRLF )
   QQOut( "c = ", c, CRLF )

   QQOut( "a + b = ", a + b, CRLF )      // 3
   QQOut( "b - a = ", b - a, CRLF )      // 1
   QQOut( "b * c = ", b * c, CRLF )      // 6
   QQOut( "b * c / 2 = ", b * c / 2, CRLF ) // 3

   QQOut( "a += b = ", a += b, CRLF )    // 3
   QQOut( "a = ", a, CRLF )              // 3
   QQOut( "a -= b = ", a -= b, CRLF )    // 1
   QQOut( "a = ", a, CRLF )              // 1

   QQOut( "a < b ", a < b, CRLF )       // TRUE
   QQOut( "a > b ", a > b, CRLF )       // FALSE
   QQOut( "a + b <= c ", a + b <= c, CRLF)  // TRUE
   QQOut( "a + b >= c ", a + b >= c, CRLF) // TRUE

   QQOut( "a *= b = ", a *= b, CRLF )    // 2
   QQOut( "a /= b = ", a /= b, CRLF )    // 1
   QQOut( "a = ", a, CRLF )              // 1

   QQOut( "b ** 3 = ", b ** 3, CRLF )    // 8
   QQOut( "b ^ 3 = ", b ^ 3, CRLF )      // 8

   QQOut( "8 % 3 = ", 8 % 3, CRLF )      // 2

return nil
