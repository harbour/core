#command CLS                                                            ;
      => Scroll( 1, 0, MaxRow() - 1, MaxCol() )                         ;
       ; SetPos(1,0)

#COMMAND BROWSE => Browse( 1, 0, MaxRow() - 1, MaxCol() )
#COMMAND EXIT => __QUIT()

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
