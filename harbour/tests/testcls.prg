/*
 * $Id$
 */

// Testing Harbour classes ON ERROR feature

#include "hbclass.ch"

PROCEDURE Main()

   LOCAL o := Test()

   o:Another( "Hello" ) // "Another" message is not defined for Class Test, but
   // it will invoke ON ERROR MyErrorManager() method

   o:Another := 5  // Notice how __GetMessage() shows a underscored message
   // as we are setting a DATA value.

   RETURN

CREATE CLASS Test

   ON ERROR MyErrorManager( uParam1 )

ENDCLASS

METHOD MyErrorManager( uParam1 ) CLASS Test

   IF PCount() > 0
      Alert( uParam1 )
   ENDIF

   Alert( __GetMessage() )  // Shows the message that was sent to the object

   RETURN NIL
