/*
 * $Id$
 */

//
// Object Array syntax test
//
// Written by Eddie Runia <eddie@runia.com>
// www - http://harbour-project.org
//
// Placed in the public domain
//

PROCEDURE Main()

   LOCAL o := TNumber():New()

   QOut( "Direct reference : ", o:x )

   o:x := "I am a data"
   QOut( "Assign text      : ", o:x )

   o:x := 4
   QOut( "Assign 4         : ", o:x )

   QOut( "Post increment   : ", o:x++ )
   QOut( "After            : ", o:x   )
   QOut( "Pre decrement    : ", --o:x )
   QOut( "After            : ", o:x   )

   o:x += 2
   QOut( "Plus 2           : ", o:x )

   o:x -= 3
   QOut( "Minus 3          : ", o:x )

   o:x *= 3
   QOut( "Times 3          : ", o:x )

   o:x /= 1.5
   QOut( "Divide by 1.5    : ", o:x )

   o:x %= 4
   QOut( "Modulus 4        : ", o:x )

   o:x ^= 3
   QOut( "To the power 3   : ", o:x )

   QOut( "Global stack" )
   Debug( __dbgVMStkGList() )
   QOut( "Statics" )
   Debug( __dbgVMVarSList() )

   RETURN

FUNCTION TNumber()                              // Very simple class

   STATIC oNumber

   IF oNumber == NIL
      oNumber := HBClass():New( "TNumber" )

      oNumber:AddData( "x" )
      oNumber:AddMethod( "New", @New() )
      oNumber:Create()
   ENDIF

   RETURN oNumber:Instance()

STATIC FUNCTION New()

   LOCAL self := QSelf()

   ::x := 1

   RETURN self
