//
// $Id$
//

func main()
qout( 1 / 0 )
qout( 1 % 0 )
   qout( sin( 33 ) )
   qout( cos( 43 ) )
   qout( tan( 54 ) )
   qout( log10( 112 ) )
   qout( log( 12 ) )
   qout( sqrt( 16 ) )
   qout( asin( 33 ) )
   qout( acos( 43 ) )
   qout( atan( 54 ) ) 
   qout( abs( 10 ) )
   qout( exp( 15 ) )
   qout( 454.14 )
   qout( int( 454.14 ) )
   qout( int( 454 ) )
   qout( min( 1, 1.0 ) )
   qout( min( 1, 10 ) )
   qout( max( 1.0, 1 ) )
   qout( max( 1, 10 ) )
   qout( min( stod( "19990101" ), stod( "20000101" ) ) )
   qout( max( stod( "19990101" ), stod( "20000101" ) ), "An argument error will appear on the next line.")
   qout( min( stod( "19990101" ), 20000101 ) )
return nil
