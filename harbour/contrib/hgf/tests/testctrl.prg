//
// $Id$
//
// Testing Harbour GUI framework controls

#include "hbclass.ch"

function Main()

   local oForm := HBFormControls():New()
   local oBtn

   oForm:Caption = "Harbour GUI Framework controls"
   oForm:Top     = 175
   oForm:Left    = 197
   oForm:Width   = 382
   oForm:Height  = 249

   oBtn = HBButton():New( oForm )
   oBtn:Caption = "&View as text"
   oBtn:Top     = 50
   oBtn:Left    = 120
   oBtn:Width   = 100
   oBtn:OnClick = "BtnViewAsTextClick"
   oForm:InsertControl( oBtn )

   oBtn = HBButton():New( oForm )
   oBtn:Caption = "&Ok"
   oBtn:Top     = 150
   oBtn:Left    = 70
   oBtn:OnClick = "BtnOkClick"
   oForm:InsertControl( oBtn )

   oBtn = HBButton():New( oForm )
   oBtn:Caption = "&Cancel"
   oBtn:Top     = 150
   oBtn:Left    = 200
   oBtn:OnClick = "BtnCancelClick"
   oForm:InsertControl( oBtn )

   oForm:ShowModal()

return nil

CLASS HBFormControls FROM HBForm

   METHOD BtnViewAsTextClick( oSender ) INLINE MsgInfo( ::SaveToText() )
   METHOD BtnOkClick( oSender )         INLINE MsgInfo( "Ok was pressed" )
   METHOD BtnCancelClick( oSender )     INLINE MsgInfo( "Cancel was pressed" )

ENDCLASS