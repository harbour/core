// Standard Harbour ErrorSys system

//----------------------------------------------------------------------------//

init procedure ClipInit

   // public getlist := {}    TODO!

   ErrorSys()

return

//----------------------------------------------------------------------------//

static function DefError( oError )

   local cInfo := ""
   local n := 2

   while ! Empty( ProcName( n ) )
      cInfo += Chr( 13 ) + Chr( 10 ) + "Called from " + ProcName( n ) + ;
               "(" + AllTrim( Str( ProcLine( n++ ) ) ) + ")"
   end

   QOut( oError:Description + Chr( 13 ) + Chr( 10 ) + cInfo )

   __Quit()

return .t.

//----------------------------------------------------------------------------//

procedure ErrorSys

   ErrorBlock( { | oError | DefError( oError ) } )

return

//----------------------------------------------------------------------------//
