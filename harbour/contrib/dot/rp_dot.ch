#COMMAND BROWSE => Browse( 1, 0, 23, 79 )
#COMMAND EXIT => __QUIT()

#ifdef __HARBOUR__
   #TRANSLATE _GET_( <var>, <varname>, <pic>, <valid>, <when> ) => __GET( <var>, <varname>, <pic>, <valid>, <when>, MEMVARBLOCK(<varname>) )
#else
   #TRANSLATE _GET_( <var>, <varname>, <pic>, <valid>, <when> ) => __GET( MEMVARBLOCK(<varname>), <varname>, <pic>, <valid>, <when> )
   #TRANSLATE __GET( <parlist,...>):Display() => __GET(<parlist>)
#endif

__get(1,2,3,4,5):display()
