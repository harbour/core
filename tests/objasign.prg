// Object Array syntax test
//
// Written by Eddie Runia <eddie@runia.com>
// www - http://harbour-project.org
//
// Placed in the public domain

#include "hbclass.ch"

PROCEDURE Main()

   LOCAL o := TNumber():New()

   ? "Direct reference :", o:x

   o:x := "I am a data"
   ? "Assign text      :", o:x

   o:x := 4
   ? "Assign 4         :", o:x

   ? "Post increment   :", o:x++
   ? "After            :", o:x
   ? "Pre decrement    :", --o:x
   ? "After            :", o:x

   o:x += 2
   ? "Plus 2           :", o:x

   o:x -= 3
   ? "Minus 3          :", o:x

   o:x *= 3
   ? "Times 3          :", o:x

   o:x /= 1.5
   ? "Divide by 1.5    :", o:x

   o:x %= 4
   ? "Modulus 4        :", o:x

   o:x ^= 3
   ? "To the power 3   :", o:x

   ? "Global stack"
   ? hb_ValToExp( __dbgVMStkGList() )
   ? "Statics"
   ? hb_ValToExp( __dbgVMVarSList() )

   RETURN

CREATE CLASS TNumber STATIC         // Very simple class

   VAR x

   METHOD New()

END CLASS

METHOD New() CLASS TNumber

   ::x := 1

   RETURN self
