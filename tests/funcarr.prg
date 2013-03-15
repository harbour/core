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

   ? "Direct reference : ", aFunc()[ 1 ]

   a := aFunc()
   ? "Ref via array    : ", a[ 1 ]

   aFunc()[ 1 ] := "Something different"
   ? "Assign new text  : ", aFunc()[ 1 ]

   aFunc()[ 1 ] := 4
   ? "Assign 4         : ", aFunc()[ 1 ]

   ? "Post increment   : ", aFunc()[ 1 ]++
   ? "After            : ", aFunc()[ 1 ]
   ? "Pre decrement    : ", --aFunc()[ 1 ]
   ? "After            : ", aFunc()[ 1 ]

   aFunc()[ 1 ] += 2
   ? "Plus 2           : ", aFunc()[ 1 ]

   aFunc()[ 1 ] -= 3
   ? "Minus 3          : ", aFunc()[ 1 ]

   aFunc()[ 1 ] *= 3
   ? "Times 3          : ", aFunc()[ 1 ]

   aFunc()[ 1 ] /= 1.5
   ? "Divide by 1.5    : ", aFunc()[ 1 ]

   aFunc()[ 1 ] %= 4
   ? "Modulus 4        : ", aFunc()[ 1 ]

   aFunc()[ 1 ] ^= 3
   ? "To the power 3   : ", aFunc()[ 1 ]

   ? "Global stack"
   ? hb_ValToExp( __dbgVMStkGList() )        // Please note a is a reference to aArray !
   ? "Statics"
   ? hb_ValToExp( __dbgVMVarSList() )

   RETURN

FUNCTION aFunc()

   STATIC s_aArray := { [Test] }

   RETURN s_aArray
