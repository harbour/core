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

   ? "Direct reference :", hb_ValToStr( o:x )

   o:x[ 1 ]     := "I am a data"
   o:Get()[ 2 ] := "I am a method"
   ? "Assign text      :", hb_ValToStr( o:x )

   o:x[ 1 ]     := 4
   o:Get()[ 2 ] := 4
   ? "Assign 4         :", hb_ValToStr( o:x )

   ? "Post increment   :", o:x[ 1 ]++ , o:Get()[ 2 ]++
   ? "After            :", o:x[ 1 ]   , o:Get()[ 2 ]
   ? "Pre decrement    :", --o:x[ 1 ] , --o:Get()[ 2 ]
   ? "After            :", o:x[ 1 ]   , o:Get()[ 2 ]

   o:x[ 1 ]     += 2
   o:Get()[ 2 ] += 2
   ? "Plus 2           :", hb_ValToStr( o:x )

   o:x[ 1 ]     -= 3
   o:Get()[ 2 ] -= 3
   ? "Minus 3          :", hb_ValToStr( o:x )

   o:x[ 1 ]     *= 3
   o:Get()[ 2 ] *= 3
   ? "Times 3          :", hb_ValToStr( o:x )

   o:x[ 1 ]     /= 1.5
   o:Get()[ 2 ] /= 1.5
   ? "Divide by 1.5    :", hb_ValToStr( o:x )

   o:x[ 1 ]     %= 4
   o:Get()[ 2 ] %= 4
   ? "Modulus 4        :", hb_ValToStr( o:x )

   o:x[ 1 ]     ^= 3
   o:Get()[ 2 ] ^= 3
   ? "To the power 3   :", hb_ValToStr( o:x )

   ? "Global stack"
   ? hb_ValToExp( __dbgVMStkGList() )
   ? "Statics"
   ? hb_ValToExp( __dbgVMVarSList() )

   RETURN

STATIC FUNCTION TNumber()                       // Very simple class

   STATIC s_oNumber

   IF s_oNumber == NIL
      s_oNumber := HBClass():New( "TNumber" )

      s_oNumber:AddData( "x" )
      s_oNumber:AddMethod( "Get", @Get() )
      s_oNumber:AddMethod( "New", @New() )
      s_oNumber:Create()
   ENDIF

   RETURN s_oNumber:Instance()

STATIC FUNCTION New()

   LOCAL self := QSelf()

   ::x := { 1, 1 }

   RETURN self

STATIC FUNCTION Get()

   LOCAL self := QSelf()

   RETURN ::x
