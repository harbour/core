/* Class HBPersistent test */

#include "hbclass.ch"

PROCEDURE Main()

   LOCAL oTest := Test():New()
   LOCAL oTest2 := Test2():New()

   LOCAL tmp

   oTest:One   := "hello"
   oTest:Two   := 123
   oTest:Three := "this value is not persistent"
   oTest:Four  := oTest2   // We store another persistent object here
   oTest:Four2 := oTest2

   oTest2:Five := "some more text"

   ? tmp := oTest:SaveToText()     // We save it to a text

   oTest:SaveToFile( "test.txt" )  // We save it to a file

   tmp := StrTran( tmp, "some more text", "changed text" )
   tmp := StrTran( tmp, "hello", "welcome" )

   ? "before load:", oTest:Four:Five
   ? "load:", oTest:LoadFromText( tmp, .T. )
   ? "after load:", oTest:Four:Five

   RETURN

CREATE CLASS Test FROM HBPersistent

   VAR    One        PROPERTY
   VAR    Two        PROPERTY
   VAR    Three
   VAR    Four       PROPERTY
   VAR    Four2      PROPERTY

   METHOD Another()  INLINE { 1, { "One", "Two" }, 0d20121231 } PROPERTY
   METHOD Another2() INLINE { 2, { "AAA", "BBB" }, 0d20121231 } PROPERTY
   METHOD More()     VIRTUAL

ENDCLASS

CREATE CLASS Test2 FROM HBPersistent

   VAR Five  PROPERTY

ENDCLASS
