/*
 * $Id$
 */

//
// Function Array syntax test
//
// Written by Eddie Runia <eddie@runia.com>
// www - http://harbour-project.org
//
// Placed in the public domain
//

PROCEDURE Main()

   LOCAL a

   QOut( "Direct reference : ", aFunc()[1] )

   a := aFunc()
   QOut( "Ref via array    : ", a[1] )

   aFunc()[1] := "Something different"
   QOut( "Assign new text  : ", aFunc()[1] )

   aFunc()[1] := 4
   QOut( "Assign 4         : ", aFunc()[1] )

   QOut( "Post increment   : ", aFunc()[1] ++ )
   QOut( "After            : ", aFunc()[1] )
   QOut( "Pre decrement    : ", -- aFunc()[1] )
   QOut( "After            : ", aFunc()[1] )

   aFunc()[1] += 2
   QOut( "Plus 2           : ", aFunc()[1] )

   aFunc()[1] -= 3
   QOut( "Minus 3          : ", aFunc()[1] )

   aFunc()[1] *= 3
   QOut( "Times 3          : ", aFunc()[1] )

   aFunc()[1] /= 1.5
   QOut( "Divide by 1.5    : ", aFunc()[1] )

   aFunc()[1] %= 4
   QOut( "Modulus 4        : ", aFunc()[1] )

   aFunc()[1] ^= 3
   QOut( "To the power 3   : ", aFunc()[1] )

   QOut( "Global stack" )
   Debug( __dbgVMStkGList() )        // Please note a is a reference to aArray !
   QOut( "Statics" )
   Debug( __dbgVMVarSList() )

   RETURN

FUNCTION aFunc()

   STATIC aArray := { [Test] }

   RETURN aArray
