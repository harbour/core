//NOTEST
//
// Spawn2 called from Spawn
//
// Function should be static to avoid collision with Spawn/Main
//
static function Main()

   local n := 0

   while !Empty( ProcName( n ) )
      n++
   end
   if ProcName( n - 1 ) != "HB_RUN"
      QOut( "Please compile me with /gHRB" )
      QOut()
      QOut( "Then : hbrun spawn" )
   else
      QOut( "Hi, I am Spawn2" )
      QOut( "Let's call a function from Spawn()" )
      SomeWhereElse()
      QOut( "Back to Spawn2" )
   endif
return nil


