#COMMAND BROWSE => Browse( 1, 0, MaxRow() - 1, MaxCol() )

#ifdef __HARBOUR__
   #TRANSLATE _GET_( <var>, <varname>, <pic>, <valid>, <when> ) => __GET( <var>, <varname>, <pic>, <valid>, <when>, MEMVARBLOCK(<varname>) )
#else
   #TRANSLATE _GET_( <var>, <varname>, <pic>, <valid>, <when> ) => __GET( MEMVARBLOCK(<varname>), <varname>, <pic>, <valid>, <when> )
   #TRANSLATE __GET( <parlist,...>):Display() => __GET(<parlist>)
#endif

#COMMAND IF <ifExp> => __SetIf( <ifExp> )
#COMMAND ELSEIF <elseifExp> => __SetElseIf( <elseifExp> )
#COMMAND ELSE => __SetElse()
#COMMAND ENDIF [<*x*>] => __SetEnd()
#COMMAND END [<*x*>] => __SetEnd()

#COMMAND DO CASE => __SetDoCase()
#COMMAND CASE <caseExp> => __SetCase( <caseExp> )
#COMMAND OTHERWISE => __SetOtherwise()
#COMMAND ENDCASE [<*x*>] => __SetEndCase()

#COMMAND DO <file>.prg => PP_Run( #<file> + ".prg" )

#COMMAND PROCEDURE <name>()            => PROCEDURE <name>
#COMMAND FUNCTION <name>()             => PROCEDURE <name>
#COMMAND PROCEDURE <name>( <par,...> ) => PROCEDURE <name> ; PP_LocalParams( { <"par"> } )
#COMMAND FUNCTION <name>( <par,...> )  => PROCEDURE <name> ; PP_LocalParams( { <"par"> } )
#COMMAND FUNCTION <name>               => PROCEDURE <name>
#COMMAND RETURN [<retExp>]             => PP_SetReturn( <retExp> )

#COMMAND PARAMETERS <par,...> => PP_SetParams( { <"par"> } )
#COMMAND PRIVATE <var,...>    => PP_Privates( { <"var"> } )
#COMMAND PUBLIC <var,...>     => PP_Publics( { <"var"> } )
#COMMAND LOCAL <var,...>      => PP_Locals( { <"var"> } )
#COMMAND STATIC <var,...>     => PP_Statics( { <"var"> } )

#TRANSLATE ProcName( [<n>] ) => PP_ProcName( <n> )
#TRANSLATE ProcLine( [<n>] ) => PP_ProcLine( <n> )
