// Testing Harbour file io features

function Main()

   local h := FCreate( "test.txt" )

   FWrite( h, "This is a test" )

   FClose( h )

return nil