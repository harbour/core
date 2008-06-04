//
// $Id$
//

// Testing a static function call

function Main()

   QOut( "From Main()" )

   SecondOne()

   QOut( "From Main() again" )

return nil

static function SecondOne()

   QOut( "From Second()" )

return nil
