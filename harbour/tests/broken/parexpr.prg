//
// $Id$
//

// The following code tests harbour's ability to cope with parenthesized
// expressions.

// These tests were written by Dave Pearson <davep@hagbard.demon.co.uk> and
// are placed into the public domain.

Function Main()
Local x
Local y

   // Simple one to start with.
   x := ( 1 )
   ? x

   // Now with a little more complex:
   x := ( 1, 2 )
   ? x
   
   // And a little more, this is really the same as the previous one.
   x := ( 1, 2, 3 )
   ? x
   
   // Expression within expression
   x := ( ( 1, 2, 3 ) )
   ? x
   
   // And a little more:
   x := ( ( 1, 2, 3 ), ( 1, 2, 3 ) )
   ? x
   
   // Some inline assignments
   x := ( y := 10, y )
   ? x
   
   x := ( ( y := ( 1, 2, 3) ), y * ( 10, 20, 30 ) )
   ? x
   
   // Now mix with statements and functions
   ? ( 1, 2, 3 )

   If ( y := .t. )
      ? "Working"
   Else
      ? "Borken"
   EndIf
   
   If ( x := 10, y := ( x == 10 ) )
      ? "Working"
   Else
      ? "Broken"
   EndIf
   
   If ( Something( 1, 2, 3 ), .T. )
      ? "Working"
   Else
      ? "Broken"
   EndIf
   
   ?
   
Return( NIL )

Static Function Something( x, y, z )

  // This does something and it does it well/
  
Return( NIL )
