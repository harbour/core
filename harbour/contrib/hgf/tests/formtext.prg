//
// $Id$
//
// Testing forms persistence

#include "hbclass.ch"

function Main()

   local oForm2 := HBForm2():New()

   oForm2:OnClick = "Form2Click"

   oForm2:ShowModal()

return nil

CLASS HBForm2 FROM HBForm

   METHOD New()

   METHOD ViewAsTextClick( oSender ) INLINE MsgInfo( ::SaveToText() )
   METHOD ExitClick( oSender ) INLINE ::Close()

   METHOD Form2Click( oSender, nXPos, nYPos )

ENDCLASS

METHOD New() CLASS HBForm2

   local oMenu, oMenuItem

   Super:New()

   ::Caption = "Harbour GUI demo"

   oMenu = HBMenu():New( Self )

   oMenuItem = HBMenuItem():New( oMenu )
   oMenuitem:Caption = "View as Text"
   oMenuitem:Name    = "ViewAsText"
   oMenuItem:OnClick  = "ViewAsTextClick"  // The container method to execute
   oMenu:Add( oMenuItem )

   oMenuItem = HBMenuItem():New( oMenu )
   oMenuitem:Caption = "Exit"
   oMenuitem:Name    = "Exit"
   oMenuItem:OnClick  = "ExitClick"  // The container method to execute
   oMenu:Add( oMenuItem )

   ::Menu = oMenu

return Self

METHOD Form2Click( oSender, nXPos, nYPos ) CLASS HBForm2

   MsgInfo( "Click at " + AllTrim( Str( nXPos ) ) + ", " + ;
            AllTrim( Str( nYPos ) ) )

return nil