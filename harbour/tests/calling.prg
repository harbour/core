/*
 * $Id$
 */

// Calling different PRG functions

PROCEDURE Main()

   SecondOne()

   DO Fourth WITH "from Fourth() function"    // Testing the old fashion DO ...

   QOut( "Ok, back to Main()" )

   RETURN

FUNCTION SecondOne()

   QOut( "Ok, this is from Second() function call" )

   Third()

   RETURN nil

FUNCTION Third()

   QOut( "Ok, this is from Third() function call" )

   RETURN nil

FUNCTION Fourth( cText )

   QOut( cText )

   RETURN nil
