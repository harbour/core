//
// $Id$
//
// Class HBPersistent test

#include "hbclass.ch"

function Main()

   local oTest := Test():New()
   local oTest2 := Test2():New()

   oTest:cName = "oTest"  // We set a name for the persistent object to save

   oTest:One   = "hello"
   oTest:Two   = 123
   oTest:Three = "this value is not persistent"
   oTest:Four  = oTest2   // We store another persistent object here

   oTest2:Five = "some more text"

   ? oTest:SaveToText()            // We save it to a text

   oTest:SaveToFile( "test.txt" )  // We save it to a file

return nil

CLASS Test FROM HBPersistent

   DATA   One       PROPERTY
   DATA   Two       PROPERTY
   DATA   Three
   DATA   Four      PROPERTY

   METHOD Another() INLINE { 1, { "One", "Two" }, Date() } PROPERTY
   METHOD More()    VIRTUAL

ENDCLASS

CLASS Test2 FROM HBPersistent

   DATA Five  PROPERTY

ENDCLASS