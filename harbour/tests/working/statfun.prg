// Testing a static function call

function Main()

   QOut( "From Main()" )

   Second()

   QOut( "From Main() again" )

return nil

static function Second()

   QOut( "From Second()" )

return nil
