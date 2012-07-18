/*
 * $Id$
 */

// Class HBPersistent test

#include "hbclass.ch"

PROCEDURE Main()

   LOCAL oTest := Test():New()
   LOCAL oTest2 := Test2():New()

   oTest:One   := "hello"
   oTest:Two   := 123
   oTest:Three := "this value is not persistent"
   oTest:Four  := oTest2   // We store another persistent object here

   oTest2:Five := "some more text"

   ? oTest:SaveToText()            // We save it to a text

   oTest:SaveToFile( "test.txt" )  // We save it to a file

   RETURN

CREATE CLASS Test FROM HBPersistent

   VAR    One       PROPERTY
   VAR    Two       PROPERTY
   VAR    Three
   VAR    Four      PROPERTY

   METHOD Another() INLINE { 1, { "One", "Two" }, Date() } PROPERTY
   METHOD More()    VIRTUAL

ENDCLASS

CREATE CLASS Test2 FROM HBPersistent

   VAR Five  PROPERTY

ENDCLASS
