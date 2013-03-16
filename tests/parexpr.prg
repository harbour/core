// The following code tests harbour's ability to cope with parenthesized
// expressions.

// These tests were written by Dave Pearson <davep@hagbard.demon.co.uk> and
// are placed into the public domain.

// This file is OK to have warnings.
#ifdef __HARBOUR__
   #pragma -es0
#endif

PROCEDURE Main()

   LOCAL x
   LOCAL y

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

   x := ( ( y := ( 1, 2, 3 ) ), y * ( 10, 20, 30 ) )
   ? x

   // Now mix with statements and functions
   ? ( 1, 2, 3 )

   IF ( y := .T. )
      ? "Working"
   ELSE
      ? "Borken"
   ENDIF

   IF ( x := 10, y := ( x == 10 ) )
      ? "Working"
   ELSE
      ? "Broken"
   ENDIF

   IF ( Something( 1, 2, 3 ), .T. )
      ? "Working"
   ELSE
      ? "Broken"
   ENDIF

   ?

   // Now even some more testing of related code
   // placed into public domain by Ryszard Glab

   ? IF( (.F.,0,.T.), ("some", "text", "IF Working"), ("some", "text", "Broken") )
   ? IF( (.T.,1,.F.), ("some", "text", "Broken"), ("some", "text", "IF Working") )

   ? IF( (.T. .OR. .F.), IF( .T., "Working", "Broken" ), IF( .F., "Broken", "Working" ) )

   /* The following code should generate syntax error if uncommented
    * because IF token followed by any three expressions is interpreted
    * as IIF inline
    */
// IF( .T., .F., .T. )
//    ? "Working"
// ELSE
//    ? "Broken"
// ENDIF

   RETURN

STATIC FUNCTION Something( x, y, z )

   // This does something and it does it well/

   Return( NIL )
