//
// $Id$
//

// Testing Harbour GUI framework for Win32

function Main()

   local oForm := TForm():New()
   local oMenu := TMenu():New( oForm )
   local oMenuItem, oSubItem, oSubItem2

   oMenuItem = TMenuItem():New( oMenu )
   oMenuItem:Caption = "One"
   oMenu:Add( oMenuItem )

   oSubItem = TMenuItem():New( oMenuItem )
   oSubItem:Caption = "First"
   oMenuItem:Add( oSubItem )

   oSubItem = TMenuItem():New( oMenuItem )
   oSubItem:Caption = "Second"
   oMenuItem:Add( oSubItem )

   oSubItem2 = TMenuItem():New( oSubItem )
   oSubItem2:Caption = "Some"
   oSubItem:Add( oSubItem2 )

   oSubItem2 = TMenuItem():New( oSubItem )
   oSubItem2:Caption = "More"
   oSubItem:Add( oSubItem2 )

   oMenuItem = TMenuItem():New( oMenu )
   oMenuItem:Caption = "Two"
   oMenu:Add( oMenuItem )

   oMenuItem = TMenuItem():New( oMenu )
   oMenuItem:Caption = "Three"
   oMenu:Add( oMenuItem )

   oForm:Menu = oMenu

   oForm:Caption = "Harbour GUI Framework"

   oForm:ShowModal()

return nil