//
// $Id$
//
// Testing forms persistence

#include "hbclass.ch"

function Main()

   local oForm2 := TForm2():New()

   oForm2:OnClick = "Form2Click"

   oForm2:ShowModal()

return nil

CLASS TForm2 FROM TForm

   METHOD New()

   METHOD ViewAsTextClick( oSender )
   METHOD ExitClick( oSender ) INLINE ::Close()

   METHOD Form2Click( oSender, nXPos, nYPos )

ENDCLASS

METHOD New() CLASS TForm2

   local oMenu, oMenuItem

   Super:New()

   ::cName   = "oForm2"
   ::Caption = "Harbour GUI demo"

   oMenu = TMenu():New( Self )

   oMenuItem = TMenuItem():New( oMenu )
   oMenuitem:Caption = "View as Text"
   oMenuitem:Name    = "ViewAsText"
   oMenuItem:OnClick  = "ViewAsTextClick"  // The container method to execute
   oMenu:Add( oMenuItem )

   oMenuItem = TMenuItem():New( oMenu )
   oMenuitem:Caption = "Exit"
   oMenuitem:Name    = "Exit"
   oMenuItem:OnClick  = "ExitClick"  // The container method to execute
   oMenu:Add( oMenuItem )

   ::Menu = oMenu

return Self

METHOD ViewAsTextClick( oSender ) CLASS TForm2

   MsgInfo( ::SaveToText() )

return nil

METHOD Form2Click( oSender, nXPos, nYPos ) CLASS TForm2

   MsgInfo( "Click at " + AllTrim( Str( nXPos ) ) + ", " + ;
            AllTrim( Str( nYPos ) ) )

return nil