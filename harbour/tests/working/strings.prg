//
// $Id$
//

// Testing Harbour strings management runtime library

function Main()

   QOut( "Testing Harbour strings management runtime library" )

   QOut( "Chr( 97 ) = ", Chr( 97 ) )

   QOut( "Lower( 'ABCdE' ) = ", Lower( "ABCdE" ) )

   QOut( "Replicate( 'abc', 5 ) = ", Replicate( "abc", 5 ) )

   QOut( '"*" + Space( 5 ) + "*" = ', "*" + Space( 5 ) + "*" )

   QOut( '"*" + LTrim( "   test" ) + "*" = ', "*" + LTrim( "   test" ) + "*" )

   QOut( 'SubStr( "abcdef", 2, 3 ) = ', SubStr( "abcdef", 2, 3 ) )

   QOut( 'Asc( "a" ) = ', Asc( "a" ) )

   QOut( 'IsDigit( "123" ) = ', IsDigit( "123" ) )

   QOut( 'Right( "world", 2 ) = ', Right( "world", 2 ) )

   QOut( 'Left( "world", 2 ) = ', Left( "world", 2 ) )

return nil
