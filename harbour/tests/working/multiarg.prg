//
// $Id$
//

//
// MultiArg
//
// Testing of multiple arguments
//
// Date : 1999/05/24
//
// Written by Eddie Runia <eddie@runia.com>
// www - http://www.harbour-project.org
//
// Placed in the public domain
//
function Main()

   ShoutArg( 1, "1", 2.5, .T. )
   ShoutArg( 2, "1", 2.5, .T. )
   ShoutArg( 3, "1", 2.5, .T. )
   ShoutArg( 4, "1", 2.5, .T. )
   ShoutArg( 5, "1", 2.5, .T. )
return nil


function ShoutArg( nArg, x1,x2,x3,x4,x5,x6,x7,x8,x9,x10 )

  QOut( nArg, "==", PValue( nArg ) )
return nil
