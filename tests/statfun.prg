// Testing a static function call

PROCEDURE Main()

   ? "From Main()"

   SecondOne()

   ? "From Main() again"

   RETURN

STATIC FUNCTION SecondOne()

   ? "From SecondOne()"

   RETURN NIL
