// Calling different PRG functions

PROCEDURE Main()

   SecondOne()

   DO Fourth WITH "from Fourth() function"    // Testing the old fashioned 'DO'...

   ? "Ok, back to Main()"

   RETURN

STATIC PROCEDURE SecondOne()

   ? "Ok, this is from SecondOne() function call"

   Third()

   RETURN

STATIC PROCEDURE Third()

   ? "Ok, this is from Third() function call"

   RETURN

STATIC PROCEDURE Fourth( cText )

   ? cText

   RETURN
