//
// $Id$
//

// Testing Harbour GUI framework for Win32

function Main()

   local oForm := TForm():New()
   local oMenu := TMenu():New( oForm )
   local oMenuItem, oSubItem, oSubItem2

   oMenuItem = TMenuItem():New( oMenu )
   oMenuItem:cCaption = "One"
   oMenu:Add( oMenuItem )

   oSubItem = TMenuItem():New( oMenuItem )
   oSubItem:cCaption = "First"
   oMenuItem:Add( oSubItem )

   oSubItem = TMenuItem():New( oMenuItem )
   oSubItem:cCaption = "Second"
   oMenuItem:Add( oSubItem )

   oSubItem2 = TMenuItem():New( oSubItem )
   oSubItem2:cCaption = "Some"
   oSubItem:Add( oSubItem2 )

   oSubItem2 = TMenuItem():New( oSubItem )
   oSubItem2:cCaption = "More"
   oSubItem:Add( oSubItem2 )

   oMenuItem = TMenuItem():New( oMenu )
   oMenuItem:cCaption = "Two"
   oMenu:Add( oMenuItem )

   oMenuItem = TMenuItem():New( oMenu )
   oMenuItem:cCaption = "Three"
   oMenu:Add( oMenuItem )

   oForm:oMenu = oMenu

   oForm:cCaption = "Harbour GUI Framework"

   oForm:ShowModal()

return nil