Procedure Main()

   LOCAL aCompiledProcs := {}, nProcId := 0, sPPed

   ErrorBlock( {|oErr| RP_Run_Err( oErr, aCompiledProcs ) } )

   PP_RunInit()

   sPPed := PP_PreProLine( "Proc Test" )
   PP_CompileLine( sPPed, 1, aCompiledProcs, NIL, @nProcId )

   sPPed := PP_PreProLine( "Alert( 'Hello from embedded PP' )" )
   PP_CompileLine( sPPed, 2, aCompiledProcs, NIL, @nProcId )

   sPPed := PP_PreProLine( "RETURN" )
   PP_CompileLine( sPPed, 3, aCompiledProcs, NIL, @nProcId )

   PP_ExecuteProcedure( aCompiledProcs[1] )

   // OR :

   PP_RunInit()
   aCompiledProcs := {}; nProcId := 0

   sPPed := PP_PreProLine( "Proc EmbeddedMain" )
   sPPed += ";" + PP_PreProLine( "Alert( EmbeddedTest() )" )
   sPPed += ";" + PP_PreProLine( "return " )
   sPPed += ";" + PP_PreProLine( "Proc EmbeddedTest" )
   sPPEd += ";" + PP_PreProLine( "return 'Hello Again'" )

   PP_CompileLine( sPPed, 0, aCompiledProcs, NIL, @nProcId )
   PP_ExecuteProcedure( aCompiledProcs[1] )

return
