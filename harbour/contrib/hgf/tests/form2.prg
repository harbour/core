//
// $Id$
//
// Inheriting from Class TForm sample

#include "hbclass.ch"

function Main()

   local oForm2 := TForm2():New()

   oForm2:ShowModal()

return nil

CLASS TForm2 FROM TForm

   METHOD New()

   METHOD TestClick( oSender )
   METHOD ExitClick( oSender ) INLINE ::Close()

ENDCLASS

METHOD New() CLASS TForm2

   local oMenu, oMenuItem

   Super:New()

   ::Caption = "Harbour GUI demo"

   oMenu = TMenu():New( Self )

   oMenuItem = TMenuItem():New( oMenu )
   oMenuitem:Caption = "Test"
   oMenuitem:Name    = "Test"
   oMenuItem:OnClick  = "TestClick"  // The container method to execute
   oMenu:Add( oMenuItem )

   oMenuItem = TMenuItem():New( oMenu )
   oMenuitem:Caption = "Exit"
   oMenuitem:Name    = "Exit"
   oMenuItem:OnClick  = "ExitClick"  // The container method to execute
   oMenu:Add( oMenuItem )

   ::Menu = oMenu

return Self

METHOD TestClick( oSender ) CLASS TForm2

   MsgInfo( "This event has been fired by a " + oSender:ClassName() + " object",;
            "Welcome to Harbour GUI power" )

return nil