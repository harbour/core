FUNCTION MAIN()
LOCAL a, b, c

  c =2
  b =Detach( 5, @c )
  OUTSTD( EVAL( b, 6, 1 ) )
  QOUT("")
  
  b =Detach( c, 15 )
  OUTSTD( EVAL( b, 8 ) )
  QOUT("")
  
  b =Call1( b )
  QOUT( EVAL( b, 10 ) )

RETURN nil

FUNCTION Detach( x, y )
RETURN( {|z| OutStd("z="),OutStd(z), OutStd("  x="),OutStd(x), OutStd("  y="),Outstd(y), Outstd("  z*y+x="), z*x+y} )

FUNCTION Call1( cb )
RETURN( Call2( cb ) )

FUNCTION Call2( cb )
RETURN( Call3( cb ) )

FUNCTION Call3( cb )
RETURN( {|x| EVAL(cb,2) *x} )
