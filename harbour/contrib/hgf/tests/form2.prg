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

   METHOD Button1Click( oSender )

ENDCLASS

METHOD New() CLASS TForm2

   Super:New()

   ::cCaption = ::ClassName()

return Self

METHOD Button1Click( oSender ) CLASS TForm2

   MsgInfo( oSender:ClassName() )

return nil