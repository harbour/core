#COMMAND MEMVAR <*x*> =>

#COMMAND BROWSE => Browse( 1, 0, MaxRow() - 1, MaxCol() )

#TRANSLATE _GET_( <var>, <varname>, <pic>, <valid>, <when> ) => __GET( MEMVARBLOCK(<varname>), <varname>, <pic>, <valid>, <when> )
#TRANSLATE __GET( <parlist,...>):Display() => __GET(<parlist>)

#COMMAND IF <ifExp>         => __SetIf( <ifExp> )
#COMMAND ELSEIF <elseifExp> => __SetElseIf( <elseifExp> )
#COMMAND ELSE               => __SetElse()
#COMMAND ENDIF [<*x*>]      => __SetEnd()
#COMMAND END [<*x*>]        => __SetEnd()

#COMMAND DO CASE         => __SetDoCase()
#COMMAND CASE <caseExp>  => __SetCase( <caseExp> )
#COMMAND OTHERWISE       => __SetOtherwise()
#COMMAND ENDCASE [<*x*>] => __SetEndCase()

#COMMAND DO <file>.prg => PP_Run( #<file> + ".prg" )

#COMMAND INIT PROCEDURE <name>[()]            => PP_PROC_INIT <name>
#COMMAND EXIT PROCEDURE <name>[()]            => PP_PROC_EXIT <name>

#COMMAND STATIC PROCEDURE <name>[()]          => PP_PROC_PRG <name>
#COMMAND STATIC FUNCTION <name>[()]           => PP_PROC_PRG <name>
#COMMAND STATIC PROCEDURE <name>( <par,...> ) => PP_PROC_PRG <name> ; PP_LocalParams( { <"par"> } )
#COMMAND STATIC FUNCTION <name>( <par,...> )  => PP_PROC_PRG <name> ; PP_LocalParams( { <"par"> } )

#COMMAND PROCEDURE <name>[()]          => PP_PROC <name>
#COMMAND FUNCTION <name>[()]           => PP_PROC <name>
#COMMAND PROCEDURE <name>( <par,...> ) => PP_PROC <name> ; PP_LocalParams( { <"par"> } )
#COMMAND FUNCTION <name>( <par,...> )  => PP_PROC <name> ; PP_LocalParams( { <"par"> } )

#COMMAND RETURN [<retExp>]             => PP_SetReturn( <retExp> )

#COMMAND PARAMETERS <par,...> => PP_Params( { <"par"> } )
#COMMAND PRIVATE <var,...>    => PP_Privates( { <"var"> } )
#COMMAND PUBLIC <var,...>     => PP_Publics( { <"var"> } )
#COMMAND LOCAL <var,...>      => PP_Locals( { <"var"> } )
#COMMAND STATIC <var,...>     => PP_Statics( { <"var"> } )

#TRANSLATE ProcName( [<n>] ) => PP_ProcName( <n> )
#TRANSLATE ProcLine( [<n>] ) => PP_ProcLine( <n> )
