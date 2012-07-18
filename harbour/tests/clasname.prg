//NOTEST
/*
 * $Id$
 */

// The following code tests the application of the className message
// against the "fundemental" types of Clipper.

// These tests were written by Dave Pearson <davep@hagbard.demon.co.uk>
// and are placed into the public domain.

PROCEDURE Main()

   // First, try all the types. This checks that the VM can cope.

   AEval( { NIL, {}, "", 0, CToD( "" ), .F. , {|| NIL }, ErrorNew() }, ;
      {| x | QOut( x:className ) } )

   // Now try against values "in the code". This checks that the
   // compiler can cope.

   ?

   QOut( NIL:className )
   QOut( {}:className )
   QOut( "":className )
   QOut( 0:className )
   QOut( CToD( "" ):className )
   QOut( .F. :className )
   QOut( {|| NIL }:className )
   QOut( ErrorNew():className )

   // For fun, do it again while ensuring the parser doesn't care about
   // whitespace.

   ?

   QOut( NIL         :  className )
   QOut( {}          :  className )
   QOut( ""          :  className )
   QOut( 0           :  className )
   QOut( CToD( "" )  :  className )
   QOut( .F.         :  className )
   QOut( {|| NIL }   :  className )
   QOut( ErrorNew()  :  className )

   // Now for some sillier ones. If the above work the following should
   // work too.

   ?
   QOut( ( NIL:className ):className )

   ?
   QOut( ( ( NIL:className ):className ):className )

   RETURN
