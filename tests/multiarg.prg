// Testing of multiple arguments
//
// Written by Eddie Runia <eddie@runia.com>
// www - http://harbour-project.org
//
// Placed in the public domain
//

PROCEDURE Main()

   ShoutArg( 1, "1", 2.5, .T. )
   ShoutArg( 2, "1", 2.5, .T. )
   ShoutArg( 3, "1", 2.5, .T. )
   ShoutArg( 4, "1", 2.5, .T. )
   ShoutArg( 5, "1", 2.5, .T. )

   RETURN

STATIC PROCEDURE ShoutArg( nArg, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10 )

   HB_SYMBOL_UNUSED( x1 )
   HB_SYMBOL_UNUSED( x2 )
   HB_SYMBOL_UNUSED( x3 )
   HB_SYMBOL_UNUSED( x4 )
   HB_SYMBOL_UNUSED( x5 )
   HB_SYMBOL_UNUSED( x6 )
   HB_SYMBOL_UNUSED( x7 )
   HB_SYMBOL_UNUSED( x8 )
   HB_SYMBOL_UNUSED( x9 )
   HB_SYMBOL_UNUSED( x10 )

   ? nArg, "==", hb_PValue( nArg )

   RETURN
