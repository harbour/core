/*
 * $Id$
 */

// Managing variables by reference

PROCEDURE Main()

   STATIC s := 10

   LOCAL x := 0

   QOut( "Managing LOCAL variables by reference" )
   QOut( 'In main before ref1 x=', x )
   ref1( @x )
   QOut( ' In main after ref1 x=', x )


   QOut( "Managing STATIC variables by reference" )
   QOut( 'In main before ref1 s=', s )
   ref1( @s )
   QOut( ' In main after ref1 s=', s )

   RETURN

FUNCTION ref1( x )

   x ++
   QOut( ' In ref1 before ref2 =', x )
   Ref2( @x )
   QOut( ' In ref1 after ref2 =', x )

   RETURN nil

FUNCTION ref2( x )

   x ++
   QOut( '  In ref2 before ref3 =', x )
   Ref3( @x )
   QOut( '  In ref2 after ref3 =', x )

   RETURN nil

FUNCTION ref3( x )

   STATIC a

   x ++
   QOut( '   In ref3 before ref4 =', x )
   a = { x, x }
   Ref4( @a )
   QOut( '   In ref3 after ref4 =', x )

   RETURN nil

FUNCTION ref4( a )

   a[ 1 ] ++
   QOut( '    In ref4 =', a[ 1 ] )

   RETURN nil
