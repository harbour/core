#command CLS                                                            ;
      => Scroll( 2, 0, MaxRow() - 1, MaxCol() )                         ;
       ; SetPos( 2, 0 )

#COMMAND BROWSE => Browse( 1, 0, MaxRow() - 1, MaxCol() )
#COMMAND EXIT => __QUIT()

#TRANSLATE _GET_( <var>, <varname>, [<pic>], [<valid>], [<when>] ) => __GET( MEMVARBLOCK(<varname>), <varname>, <pic>, <valid>, <when> )
#ifndef __HARBOUR__
   #TRANSLATE __GET( <parlist,...>):Display() => __GET(<parlist>)
   #TRANSLATE aAdd( GetList, __GET(<parlist,...>) ) => __oGet := __GET(<parlist>) ; aAdd( GetList, __oGet ) ; __oGet:Display()
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

#command CD <(dir)> => DirChange( <(dir)> )
