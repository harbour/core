// Calling different PRG functions

PROCEDURE Main()

   SecondOne()

   DO Fourth WITH "from Fourth() function"    // Testing the old fashioned 'DO'...

   ? "Ok, back to Main()"

   RETURN

STATIC FUNCTION SecondOne()

   ? "Ok, this is from SecondOne() function call"

   Third()

   RETURN NIL

STATIC FUNCTION Third()

   ? "Ok, this is from Third() function call"

   RETURN NIL

STATIC FUNCTION Fourth( cText )

   ? cText

   RETURN NIL
