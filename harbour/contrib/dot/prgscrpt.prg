Procedure Main()

   LOCAL aCompiledProcs := {}, nProcId := 0, sPPed

   // Note, aCompiledProcs MUST be an array.
   PP_RunInit( aCompiledProcs )

   // Note the return value from PP_ExecProcedure() and no Procedure declaration in this 1st sample...
   Alert( PP_ExecProcedure( ;
        PP_CompileLine( ;
          PP_PreProLine( "Private cVar := 'Hi there'; ? cVar; Return 'Returned from embedded script!'" ), ;
                         0, aCompiledProcs, NIL, @nProcId )[1] ) )

   // OR:
   nProcId := 0
   PP_RunInit( aCompiledProcs )

   sPPed := PP_PreProLine( "Proc Test" )
   PP_CompileLine( sPPed, 2, aCompiledProcs, NIL, @nProcId )

   sPPed := PP_PreProLine( "Alert( 'Hello from embedded PP' )" )
   PP_CompileLine( sPPed, 2, aCompiledProcs, NIL, @nProcId )

   sPPed := PP_PreProLine( "RETURN" )
   PP_CompileLine( sPPed, 3, aCompiledProcs, NIL, @nProcId )

   PP_ExecProcedure( aCompiledProcs[1] )

   // OR :

   nProcId := 0
   PP_RunInit( aCompiledProcs )

   sPPed := PP_PreProLine( "Proc EmbeddedMain" )
   sPPed += ";" + PP_PreProLine( "Alert( EmbeddedTest() )" )
   sPPed += ";" + PP_PreProLine( "return " )
   sPPed += ";" + PP_PreProLine( "Proc EmbeddedTest" )
   sPPEd += ";" + PP_PreProLine( "return 'Hello Again'" )

   PP_CompileLine( sPPed, 0, aCompiledProcs, NIL, @nProcId )
   PP_ExecProcedure( aCompiledProcs[1] )

return
