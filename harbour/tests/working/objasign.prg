//
// Object Array syntax test
//

Function Main

   local o := TNumber():New()

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
   HBDebug( __aGlobalStack() )        // Please note a is a reference to aArray !
   QOut( "Statics")
   HBDebug( __aStatic() )
return NIL

Function TNumber()                              // Very simple class

   static oNumber

   if oNumber == NIL
      oNumber := TClass():New( "TNumber" )

      oNumber:AddData  ( "x"   )
      oNumber:AddMethod( "New", @New() )
      oNumber:Create()
   endif
return oNumber:Instance()


static function New()

   local self := QSelf()

   ::x := 1
return self


