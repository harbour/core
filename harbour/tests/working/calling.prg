// Calling different PRG functions

function Main()

   Second()

   DO Fourth WITH "from Fourth() function"    // Testing the old fashion DO ...

   QOut( "Ok, back to Main()" )

return nil

function Second()

   QOut( "Ok, this is from Second() function call" )

   Third()

return nil

function Third()

   QOut( "Ok, this is from Third() function call" )

return nil

function Fourth( cText )

   QOut( cText )

return nil
