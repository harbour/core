//
// $Id$
//

// Testing Harbour GUI framework

function Main()

   local oForm := HBForm():New()
   local oMenu := HBMenu():New( oForm )
   local oMenuItem, oSubItem, oSubItem2

   oMenuItem = HBMenuItem():New( oMenu )
   oMenuItem:Caption = "One"
   oMenu:Add( oMenuItem )

   oSubItem = HBMenuItem():New( oMenuItem )
   oSubItem:Caption = "First"
   oMenuItem:Add( oSubItem )

   oSubItem = HBMenuItem():New( oMenuItem )
   oSubItem:Caption = "Second"
   oMenuItem:Add( oSubItem )

   oSubItem2 = HBMenuItem():New( oSubItem )
   oSubItem2:Caption = "Some"
   oSubItem:Add( oSubItem2 )

   oSubItem2 = HBMenuItem():New( oSubItem )
   oSubItem2:Caption = "More"
   oSubItem:Add( oSubItem2 )

   oMenuItem = HBMenuItem():New( oMenu )
   oMenuItem:Caption = "Two"
   oMenu:Add( oMenuItem )

   oMenuItem = HBMenuItem():New( oMenu )
   oMenuItem:Caption = "Three"
   oMenu:Add( oMenuItem )

   oForm:Menu = oMenu

   oForm:Caption = "Harbour GUI Framework"

   oForm:ShowModal()

return nil