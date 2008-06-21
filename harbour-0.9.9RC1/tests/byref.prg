//
// $Id$
//

// Managing variables by reference

function Main()
STATIC s:=10

   local x := 0

   QOut( "Managing LOCAL variables by reference" )
   Qout( 'In main before ref1 x=', x )
   ref1( @x )
   Qout( ' In main after ref1 x=', x )


   QOut( "Managing STATIC variables by reference" )
   Qout( 'In main before ref1 s=', s )
   ref1( @s )
   Qout( ' In main after ref1 s=', s )

return nil

function ref1( x )

  x++
  Qout( ' In ref1 before ref2 =', x )
  Ref2( @x )
  Qout( ' In ref1 after ref2 =', x )

return nil

function ref2( x )

  x++
  Qout( '  In ref2 before ref3 =', x )
  Ref3( @x )
  Qout( '  In ref2 after ref3 =', x )

return nil

function ref3( x )
STATIC a

  x++
  Qout( '   In ref3 before ref4 =', x )
  a ={ x, x }
  Ref4( @a )
  Qout( '   In ref3 after ref4 =', x )

return nil

function ref4( a )

  a[ 1 ]++
  Qout( '    In ref4 =', a[ 1 ] )

return nil
