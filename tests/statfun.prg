// Testing a static function call

PROCEDURE Main()

   ? "From Main()"

   SecondOne()

   ? "From Main() again"

   RETURN

STATIC PROCEDURE SecondOne()

   ? "From SecondOne()"

   RETURN
