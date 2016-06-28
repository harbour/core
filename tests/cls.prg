// Testing Harbour classes ON ERROR feature

#include "hbclass.ch"

PROCEDURE Main()

   LOCAL o := Test()

   CLS

   // "Another" message is not defined for Class Test, but
   // it will invoke ON ERROR MyErrorManager() method
   o:Another( "Hello" )

   // Notice how __GetMessage() shows a underscored message
   // as we are setting a DATA value.
   o:Another := 5

   RETURN

CREATE CLASS Test STATIC

   ON ERROR MyErrorManager( uParam1 )

ENDCLASS

METHOD PROCEDURE MyErrorManager( uParam1 ) CLASS Test

   IF PCount() > 0
      Alert( uParam1 )
   ENDIF

   Alert( __GetMessage() )  // Shows the message that was sent to the object

   RETURN
