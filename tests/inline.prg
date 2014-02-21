//
// Test of inline function
//

#include "hbclass.ch"

PROCEDURE Main()

   LOCAL oForm := TForm():New()

   ? oForm:ClassName()
   oForm:cText := "Let's show a form here :-)"

   oForm:Show()

   RETURN

CREATE CLASS TForm STATIC

   VAR cText
   VAR nTop
   VAR nLeft
   VAR nBottom
   VAR nRight

   METHOD New()
   METHOD Show() INLINE QOut( ::cText )

END CLASS

METHOD New() CLASS TForm

   ::nTop    := 10
   ::nLeft   := 10
   ::nBottom := 20
   ::nRight  := 40

   RETURN Self
