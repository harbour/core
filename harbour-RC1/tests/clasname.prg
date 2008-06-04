//NOTEST
//
// $Id$
//

// The following code tests the application of the className message
// against the "fundemental" types of Clipper.

// These tests were written by Dave Pearson <davep@hagbard.demon.co.uk>
// and are placed into the public domain.

Function Main()

   // First, try all the types. This checks that the VM can cope.

   aeval( { NIL, {}, "", 0, ctod( "" ), .F., {|| NIL }, ErrorNew() },;
            {|x| qout( x:className ) } )

   // Now try against values "in the code". This checks that the
   // compiler can cope.

   ?

   qout( NIL:className )
   qout( {}:className )
   qout( "":className )
   qout( 0:className )
   qout( ctod( "" ):className )
   qout( .f.:className )
   qout( {|| nil }:className )
   qout( ErrorNew():className )

   // For fun, do it again while ensuring the parser doesn't care about
   // whitespace.

   ?

   qout( NIL         :  className )
   qout( {}          :  className )
   qout( ""          :  className )
   qout( 0           :  className )
   qout( ctod( "" )  :  className )
   qout( .f.         :  className )
   qout( {|| nil }   :  className )
   qout( ErrorNew()  :  className )

   // Now for some sillier ones. If the above work the following should
   // work too.

   ?
   qout( ( NIL:className ):className )

   ?
   qout( ( ( NIL:className ):className ):className )

Return( NIL )

