//
// $Id$
//
// Inheriting from Class HBForm sample

#include "hbclass.ch"

function Main()

   local oForm2 := HBForm2():New()

   oForm2:ShowModal()

return nil

CLASS HBForm2 FROM HBForm

   METHOD New()

   METHOD TestClick( oSender )
   METHOD ExitClick( oSender ) INLINE ::Close()

ENDCLASS

METHOD New() CLASS HBForm2

   local oMenu, oMenuItem

   Super:New()

   ::Caption = "Harbour GUI demo"

   oMenu = HBMenu():New( Self )

   oMenuItem = HBMenuItem():New( oMenu )
   oMenuitem:Caption = "Test"
   oMenuitem:Name    = "Test"
   oMenuItem:OnClick  = "TestClick"  // The container method to execute
   oMenu:Add( oMenuItem )

   oMenuItem = HBMenuItem():New( oMenu )
   oMenuitem:Caption = "Exit"
   oMenuitem:Name    = "Exit"
   oMenuItem:OnClick  = "ExitClick"  // The container method to execute
   oMenu:Add( oMenuItem )

   ::Menu = oMenu

return Self

METHOD TestClick( oSender ) CLASS HBForm2

   MsgInfo( "This event has been fired by a " + oSender:ClassName() + " object",;
            "Welcome to Harbour GUI power" )

return nil