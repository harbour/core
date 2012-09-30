/*
 * $Id$
 */

// Testing a static function call

PROCEDURE Main()

   QOut( "From Main()" )

   SecondOne()

   QOut( "From Main() again" )

   RETURN

STATIC FUNCTION SecondOne()

   QOut( "From Second()" )

   RETURN NIL
